use crate::parser_rt::{JSONValue, JSONText, JSONKeyValuePair};



use serde::ser::{
    self, Serialize, Serializer, SerializeSeq, SerializeTuple, SerializeMap, SerializeStruct,
    SerializeStructVariant, SerializeTupleVariant, SerializeTupleStruct
};
use std::fmt;

pub fn to_json_model<T>(value: &T) -> Result<JSONText, SerdeJSON5Error>
where
    T: Serialize,
{
    let serializer = JSONValueSerializer;
    let val = value.serialize(serializer)?;
    Ok(JSONText{ value: val})
}

pub fn to_string<T>(value: &T) -> Result<String, SerdeJSON5Error>
where
    T: Serialize
{
    let serializer = JSONValueSerializer;
    let maybe_model = value.serialize(serializer);
    match maybe_model {
        Err(e) => {Err(SerdeJSON5Error::Custom(e.to_string()))}
        Ok(model) => {
            Ok(model.to_string())
        }
    }

}

#[derive(Debug)]
pub enum SerdeJSON5Error {
    Custom(String),
}

impl std::error::Error for SerdeJSON5Error {}
impl fmt::Display for SerdeJSON5Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SerdeJSON5Error::Custom(msg) => write!(f, "{}", msg),
        }
    }
}

impl ser::Error for SerdeJSON5Error {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        SerdeJSON5Error::Custom(msg.to_string())
    }
}

/// Used for a normal sequence `[elem, elem, ...]` or a tuple `(elem, ...)`.
pub struct CompoundSeq {
    pub elements: Vec<JSONValue>,
}

impl CompoundSeq {
    fn end_impl(self) -> Result<JSONValue, SerdeJSON5Error> {
        Ok(JSONValue::JSONArray { values: self.elements })
    }

    fn serialize_element_impl<T>(&mut self, value: &T) -> Result<(), SerdeJSON5Error>
    where
        T: ?Sized + Serialize,
    {
        let val = value.serialize(JSONValueSerializer)?;
        self.elements.push(val);
        Ok(())
    }
}

impl SerializeSeq for CompoundSeq {
    type Ok = JSONValue;
    type Error = SerdeJSON5Error;

    fn serialize_element<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + Serialize,
    {
        let val = value.serialize(JSONValueSerializer)?;
        self.elements.push(val);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(JSONValue::JSONArray {
            values: self.elements,
        })
    }
}

// We can reuse the same logic for `SerializeTuple` by implementing that trait for CompoundSeq
impl SerializeTuple for CompoundSeq {
    type Ok = JSONValue;
    type Error = SerdeJSON5Error;

    fn serialize_element<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + Serialize,
    {
        self.serialize_element_impl(value)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.end_impl()
    }
}

impl SerializeTupleStruct for CompoundSeq {
    type Ok = JSONValue;
    type Error = SerdeJSON5Error;

    fn serialize_field<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + Serialize,
    {
        self.serialize_element_impl(value)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.end_impl()
    }
}

/// For a tuple variant (e.g. `MyEnum::Variant(...)`), we store as:
/// `{ "Variant": [ ... elements ... ] }`
pub struct CompoundSeqVariant {
    pub variant: String,
    pub elements: Vec<JSONValue>,
}

impl SerializeTupleVariant for CompoundSeqVariant {
    type Ok = JSONValue;
    type Error = SerdeJSON5Error;

    fn serialize_field<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + Serialize,
    {
        let val = value.serialize(JSONValueSerializer)?;
        self.elements.push(val);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        let pair = JSONKeyValuePair {
            key: JSONValue::Identifier(self.variant),
            value: JSONValue::JSONArray { values: self.elements },
        };
        Ok(JSONValue::JSONObject {
            key_value_pairs: vec![pair],
        })
    }
}


/// For a normal map or a struct
pub struct CompoundMap {
    pub pairs: Vec<JSONKeyValuePair>,
    pub next_key: Option<JSONValue>,
}

impl SerializeMap for CompoundMap {
    type Ok = JSONValue;
    type Error = SerdeJSON5Error;

    fn serialize_key<T>(&mut self, key: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + Serialize,
    {
        // First, serialize the key as a JSONValue
        let k = key.serialize(JSONValueSerializer)?;
        self.next_key = Some(k);
        Ok(())
    }

    fn serialize_value<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + Serialize,
    {
        let v = value.serialize(JSONValueSerializer)?;
        let key = self.next_key.take().ok_or_else(|| {
            ser::Error::custom("serialize_value called before serialize_key")
        })?;

        self.pairs.push(JSONKeyValuePair { key, value: v });
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(JSONValue::JSONObject {
            key_value_pairs: self.pairs,
        })
    }
}

impl SerializeStruct for CompoundMap {
    type Ok = JSONValue;
    type Error = SerdeJSON5Error;

    fn serialize_field<T>(&mut self, field: &'static str, value: &T)
                          -> Result<(), Self::Error>
    where
        T: ?Sized + Serialize,
    {
        // The key is a field name, so store it as an identifier or a double-quoted string
        let key_val = JSONValue::Identifier(field.to_string());
        let val = value.serialize(JSONValueSerializer)?;

        self.pairs.push(JSONKeyValuePair {
            key: key_val,
            value: val,
        });
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(JSONValue::JSONObject {
            key_value_pairs: self.pairs,
        })
    }
}

/// For a struct variant (e.g. `MyEnum::Variant { x: 1, y: 2 }`),
/// we store as `{ "Variant": { x: 1, y: 2 } }`.
pub struct CompoundMapVariant {
    pub variant: String,
    pub pairs: Vec<JSONKeyValuePair>,
}

impl SerializeStructVariant for CompoundMapVariant {
    type Ok = JSONValue;
    type Error = SerdeJSON5Error;

    fn serialize_field<T>(
        &mut self,
        field: &'static str,
        value: &T
    ) -> Result<(), Self::Error>
    where
        T: ?Sized + Serialize,
    {
        let key_val = JSONValue::Identifier(field.to_string());
        let val = value.serialize(JSONValueSerializer)?;
        self.pairs.push(JSONKeyValuePair {
            key: key_val,
            value: val,
        });
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        // Build the object for the fields:
        let obj = JSONValue::JSONObject {
            key_value_pairs: self.pairs,
        };
        // Then wrap that in a top‐level object for the variant:
        Ok(JSONValue::JSONObject {
            key_value_pairs: vec![
                JSONKeyValuePair {
                    key: JSONValue::Identifier(self.variant),
                    value: obj,
                }
            ],
        })
    }
}


pub struct JSONValueSerializer;

impl Serializer for JSONValueSerializer {
    type Ok = JSONValue;              // final successful output is a JSONValue
    type Error = SerdeJSON5Error;     // our error type

    // Types used for sequences, maps, structs, etc.
    type SerializeSeq = CompoundSeq;
    type SerializeTuple = CompoundSeq;
    type SerializeTupleStruct = CompoundSeq;
    type SerializeTupleVariant = CompoundSeqVariant;
    type SerializeMap = CompoundMap;
    type SerializeStruct = CompoundMap;
    type SerializeStructVariant = CompoundMapVariant;

    // ---- Primitives ----
    fn serialize_bool(self, v: bool) -> Result<Self::Ok, Self::Error> {
        Ok(JSONValue::Bool(v))
    }

    fn serialize_i8(self, v: i8) -> Result<Self::Ok, Self::Error> {
        // For example, store it as `Integer` variant:
        Ok(JSONValue::Integer(v.to_string()))
    }

    fn serialize_i16(self, v: i16) -> Result<Self::Ok, Self::Error> {
        Ok(JSONValue::Integer(v.to_string()))
    }

    fn serialize_i32(self, v: i32) -> Result<Self::Ok, Self::Error> {
        Ok(JSONValue::Integer(v.to_string()))
    }

    fn serialize_i64(self, v: i64) -> Result<Self::Ok, Self::Error> {
        Ok(JSONValue::Integer(v.to_string()))
    }

    fn serialize_u8(self, v: u8) -> Result<Self::Ok, Self::Error> {
        Ok(JSONValue::Integer(v.to_string()))
    }

    fn serialize_u16(self, v: u16) -> Result<Self::Ok, Self::Error> {
        Ok(JSONValue::Integer(v.to_string()))
    }

    fn serialize_u32(self, v: u32) -> Result<Self::Ok, Self::Error> {
        Ok(JSONValue::Integer(v.to_string()))
    }

    fn serialize_u64(self, v: u64) -> Result<Self::Ok, Self::Error> {
        Ok(JSONValue::Integer(v.to_string()))
    }

    fn serialize_f32(self, v: f32) -> Result<Self::Ok, Self::Error> {
        self.serialize_f64(v as f64)
    }

    fn serialize_f64(self, v: f64) -> Result<Self::Ok, Self::Error> {
        if v.is_nan() {
            Ok(JSONValue::NaN)
        } else if v.is_infinite() {
            // For simplicity, store +/-∞ as Infinity.
            // Or do a `Unary { operator: Minus, value: Infinity }` if negative.
            Ok(JSONValue::Infinity)
        } else {
            // You could store exponent form separately if you wish
            Ok(JSONValue::Float(v.to_string()))
        }
    }

    fn serialize_char(self, v: char) -> Result<Self::Ok, Self::Error> {
        // We can store it in a string variant, for instance:
        Ok(JSONValue::DoubleQuotedString(v.to_string()))
    }

    fn serialize_str(self, v: &str) -> Result<Self::Ok, Self::Error> {
        // For simplicity, put all strings in DoubleQuotedString:
        Ok(JSONValue::DoubleQuotedString(v.to_string()))
    }

    fn serialize_bytes(self, v: &[u8]) -> Result<Self::Ok, Self::Error> {
        // There's no standard JSON raw binary, so let's store them in a string
        // or perhaps a hex string. For example:
        let hex_str = v.iter().map(|b| format!("{:02X}", b)).collect::<String>();
        Ok(JSONValue::Hexadecimal(hex_str))
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        Ok(JSONValue::Null)
    }

    fn serialize_some<T>(self, value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + Serialize
    {
        value.serialize(self)
    }

    // ---- Special cases: unit, option, etc. ----
    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        Ok(JSONValue::Null)
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<Self::Ok, Self::Error> {
        Ok(JSONValue::Null)
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        // For an enum like E::Variant, store it as an identifier or string:
        Ok(JSONValue::Identifier(variant.to_owned()))
    }

    fn serialize_newtype_struct<T>(
        self,
        _name: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + Serialize,
    {
        // Just serialize the inner value directly
        value.serialize(self)
    }

    fn serialize_newtype_variant<T>(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + Serialize,
    {
        // For `E::Variant(inner)`, produce an object like { "Variant": <inner> }
        let val = value.serialize(JSONValueSerializer)?;
        let pair = JSONKeyValuePair {
            key: JSONValue::Identifier(variant.to_owned()),
            value: val,
        };
        Ok(JSONValue::JSONObject {
            key_value_pairs: vec![pair],
        })
    }

    // ---- Sequences ----
    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        Ok(CompoundSeq {
            elements: Vec::new(),
        })
    }

    fn serialize_tuple(self, _len: usize) -> Result<Self::SerializeTuple, Self::Error> {
        Ok(CompoundSeq {
            elements: Vec::new(),
        })
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        _len: usize
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        Ok(CompoundSeq {
            elements: Vec::new(),
        })
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        Ok(CompoundSeqVariant {
            variant: variant.to_string(),
            elements: Vec::new(),
        })
    }

    // ---- Maps ----
    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        Ok(CompoundMap {
            pairs: Vec::new(),
            next_key: None,
        })
    }

    fn serialize_struct(
        self,
        _name: &'static str,
        _len: usize
    ) -> Result<Self::SerializeStruct, Self::Error> {
        Ok(CompoundMap {
            pairs: Vec::new(),
            next_key: None,
        })
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        Ok(CompoundMapVariant {
            variant: variant.to_string(),
            pairs: Vec::new(),
        })
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use super::*;
    use serde::Serialize;
    #[derive(Debug, Serialize)]
    struct Demo {
        name: String,
        nums: Vec<i32>,
        nested: Option<SubData>,
    }

    #[derive(Debug, Serialize)]
    struct SubData {
        enabled: bool,
        count: u64,
    }
    #[test]
    fn main() {
        let example = Demo {
            name: "Test".to_string(),
            nums: vec![1, 2, 3],
            nested: Some(SubData {
                enabled: true,
                count: 999,
            }),
        };

        match to_json_model(&example) {
            Ok(json_value) => {
                println!("Serialized to JSONValue: {:?}", json_value);
                // If you want a JSONText:
                // let text = JSONText { value: json_value };
            }
            Err(e) => eprintln!("Error: {:?}", e),
        }
    }

    #[test]
    fn test_hashmap() {
        use crate::parser_rt::{JSONValue, JSONKeyValuePair, JSONText};
        let mut example: HashMap<String, String> = HashMap::new();
        example.insert("foo".to_string(), "bar".to_string());
        match to_json_model(&example) {
            Ok(val) => {
                let expected = JSONText{ value: JSONValue::JSONObject { key_value_pairs: vec![JSONKeyValuePair { key: JSONValue::DoubleQuotedString("foo".to_string()), value: JSONValue::DoubleQuotedString("bar".to_string()) }] }};
                assert_eq!(val, expected);
            }
            Err(e) => eprintln!("Error: {:?}", e)
        }
    }
}