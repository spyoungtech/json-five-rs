use serde::de::{self, DeserializeSeed, Deserialize, Deserializer, Error as DeError, MapAccess, SeqAccess, VariantAccess, Visitor};
use std::fmt;
use crate::parser::{JSONText, JSONValue, JSONKeyValuePair, Unary, UnaryOperator, ParsingError, parse_text};
#[derive(Debug)]
pub enum SerdeJSON5Error {
    Custom(String),
}

impl fmt::Display for SerdeJSON5Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SerdeJSON5Error::Custom(msg) => write!(f, "{}", msg),
        }
    }
}

impl std::error::Error for SerdeJSON5Error {}

impl de::Error for SerdeJSON5Error {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        SerdeJSON5Error::Custom(msg.to_string())
    }
}

/// A small helper that wraps our `JSONValue` and implements
/// `serde::Deserializer`.
struct JSONValueDeserializer<'a> {
    input: &'a JSONValue,
}



impl<'de, 'a> Deserializer<'de> for JSONValueDeserializer<'a> {
    type Error = SerdeJSON5Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.input {
            JSONValue::Null => visitor.visit_unit(),
            JSONValue::Bool(b) => visitor.visit_bool(*b),
            JSONValue::DoubleQuotedString(s) | JSONValue::SingleQuotedString(s) => {
                visitor.visit_string(s.clone())
            }
            JSONValue::Identifier(s) => visitor.visit_string(s.clone()),
            JSONValue::JSONObject { key_value_pairs } => {
                // Treat as a map
                let mut map_deserializer = JSONMapAccess {
                    pairs: key_value_pairs,
                    index: 0,
                };
                visitor.visit_map(&mut map_deserializer)
            }
            JSONValue::JSONArray { values } => {
                let mut seq_deserializer = JSONSeqAccess {
                    values,
                    index: 0,
                };
                visitor.visit_seq(&mut seq_deserializer)
            }
            JSONValue::Integer(s) => {
                // Attempt to parse as i64, or fallback to bigger integer handling as needed
                if let Ok(i) = s.parse::<i64>() {
                    visitor.visit_i64(i)
                } else {
                    // if it doesn't parse to i64, try as u64 or fallback to string
                    if let Ok(u) = s.parse::<u64>() {
                        visitor.visit_u64(u)
                    } else {
                        // fallback: treat as a string or produce an error
                        Err(de::Error::custom(format!("Invalid integer {}", s)))
                    }
                }
            }
            JSONValue::Float(s) | JSONValue::Exponent(s) => {
                // For floats (including exponent):
                if let Ok(f) = s.parse::<f64>() {
                    visitor.visit_f64(f)
                } else {
                    Err(de::Error::custom(format!("Invalid float {}", s)))
                }
            }
            JSONValue::Infinity => visitor.visit_f64(f64::INFINITY),
            JSONValue::NaN => visitor.visit_f64(f64::NAN),
            JSONValue::Hexadecimal(s) => {
                // Optionally convert to integer, or treat as string
                if let Ok(hex) = u64::from_str_radix(s, 16) {
                    visitor.visit_u64(hex)
                } else {
                    Err(de::Error::custom(format!("Invalid hex {}", s)))
                }
            }
            JSONValue::Unary { operator, value } => {
                // For demonstration, we’ll just handle plus/minus on numbers:
                let sign = match operator {
                    UnaryOperator::Plus => 1.0,
                    UnaryOperator::Minus => -1.0,
                };
                // Re-enter the deserializer with the underlying value
                let inner_de = JSONValueDeserializer { input: &**value };
                // If you want to handle +foo or -foo as floats (e.g. -3.14)
                // we can parse it out carefully:
                let number: f64 = Deserialize::deserialize(inner_de)?;
                visitor.visit_f64(sign * number)
            }
        }
    }

    // --- The following methods “forward” to deserialize_any by default. ---
    // --- For efficiency/clarity, you can implement them specifically.  ---

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_any(visitor)
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        self.deserialize_any(visitor)
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        self.deserialize_any(visitor)
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        self.deserialize_any(visitor)
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        // Example: if we have an integer string, parse to i64
        match self.input {
            JSONValue::Integer(s) => {
                let i = s.parse::<i64>().map_err(de::Error::custom)?;
                visitor.visit_i64(i)
            }
            _ => self.deserialize_any(visitor),
        }
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        self.deserialize_any(visitor)
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        self.deserialize_any(visitor)
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        self.deserialize_any(visitor)
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        self.deserialize_any(visitor)
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        self.deserialize_any(visitor)
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.input {
            JSONValue::Float(s) | JSONValue::Exponent(s) => {
                let f = s.parse::<f64>().map_err(de::Error::custom)?;
                visitor.visit_f64(f)
            }
            JSONValue::Infinity => visitor.visit_f64(f64::INFINITY),
            JSONValue::NaN => visitor.visit_f64(f64::NAN),
            _ => self.deserialize_any(visitor),
        }
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        self.deserialize_any(visitor)
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        self.deserialize_any(visitor)
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        self.deserialize_any(visitor)
    }

    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        self.deserialize_any(visitor)
    }

    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
        self.deserialize_any(visitor)
    }

    // The rest are standard boilerplate.
    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.input {
            JSONValue::Null => visitor.visit_none(),
            _ => visitor.visit_some(self),
        }
    }

    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if let JSONValue::Null = self.input {
            visitor.visit_unit()
        } else {
            self.deserialize_any(visitor)
        }
    }

    fn deserialize_unit_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_unit(visitor)
    }

    fn deserialize_newtype_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_newtype_struct(self)
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.input {
            JSONValue::JSONArray { values } => {
                let mut seq = JSONSeqAccess {
                    values,
                    index: 0,
                };
                visitor.visit_seq(&mut seq)
            }
            _ => self.deserialize_any(visitor),
        }
    }

    fn deserialize_tuple<V>(
        self,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.input {
            JSONValue::JSONObject { key_value_pairs } => {
                let mut map = JSONMapAccess {
                    pairs: key_value_pairs,
                    index: 0,
                };
                visitor.visit_map(&mut map)
            }
            _ => self.deserialize_any(visitor),
        }
    }

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_map(visitor)
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        // If you do not need enum support, you can skip or error out here.
        // Otherwise interpret your JSONValue in a way that corresponds to an enum
        // in user code.
        use serde::de::IntoDeserializer;
        match self.input {
            JSONValue::Identifier(ident) => {
                // We'll treat the entire enum as a single variant with no payload:
                visitor.visit_enum(ident.as_str().into_deserializer())
            }
            JSONValue::JSONObject { key_value_pairs } if !key_value_pairs.is_empty() => {
                // Possibly the first key is your variant, the value is the data.
                // Example pattern for `Tag: { /* contents */ }` style
                let kv = &key_value_pairs[0];
                if let JSONValue::Identifier(ref variant) = kv.key {
                    visitor.visit_enum(EnumDeserializer {
                        variant,
                        content: &kv.value,
                    })
                } else {
                    Err(de::Error::custom("Invalid enum representation"))
                }
            }
            _ => Err(de::Error::custom("Unsupported enum representation")),
        }
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.input {
            JSONValue::Identifier(s)
            | JSONValue::DoubleQuotedString(s)
            | JSONValue::SingleQuotedString(s) => visitor.visit_str(s),
            _ => self.deserialize_any(visitor),
        }
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_unit()
    }
}

/// Minimal SeqAccess implementation for arrays
struct JSONSeqAccess<'a> {
    values: &'a [JSONValue],
    index: usize,
}

impl<'de, 'a> SeqAccess<'de> for JSONSeqAccess<'a> {
    type Error = SerdeJSON5Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, SerdeJSON5Error>
    where
        T: DeserializeSeed<'de>,
    {
        if self.index < self.values.len() {
            let deserializer = JSONValueDeserializer {
                input: &self.values[self.index],
            };
            self.index += 1;
            seed.deserialize(deserializer).map(Some)
        } else {
            Ok(None)
        }
    }
}

/// Minimal MapAccess implementation for objects
struct JSONMapAccess<'a> {
    pairs: &'a [JSONKeyValuePair],
    index: usize,
}

impl<'de, 'a> MapAccess<'de> for JSONMapAccess<'a> {
    type Error = SerdeJSON5Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, SerdeJSON5Error>
    where
        K: DeserializeSeed<'de>,
    {
        if self.index < self.pairs.len() {
            let deserializer = JSONValueDeserializer {
                input: &self.pairs[self.index].key,
            };
            seed.deserialize(deserializer).map(Some)
        } else {
            Ok(None)
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, SerdeJSON5Error>
    where
        V: DeserializeSeed<'de>,
    {
        let deserializer = JSONValueDeserializer {
            input: &self.pairs[self.index].value,
        };
        self.index += 1;
        seed.deserialize(deserializer)
    }
}

/// If you need to handle complex enum representations:
struct EnumDeserializer<'a> {
    variant: &'a String,
    content: &'a JSONValue,
}

impl<'de, 'a> de::EnumAccess<'de> for EnumDeserializer<'a> {
    type Error = SerdeJSON5Error;
    type Variant = JSONValueDeserializer<'a>;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant), SerdeJSON5Error>
    where
        V: DeserializeSeed<'de>,
    {
        use serde::de::IntoDeserializer;
        let val = seed.deserialize(self.variant.as_str().into_deserializer())?;
        Ok((val, JSONValueDeserializer { input: self.content }))
    }
}

impl<'de, 'a> VariantAccess<'de> for JSONValueDeserializer<'a> {
    type Error = SerdeJSON5Error;

    fn unit_variant(self) -> Result<(), Self::Error> {
        // If the variant is expected to have no value, do nothing:
        Ok(())
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value, Self::Error>
    where
        T: de::DeserializeSeed<'de>,
    {
        // If the variant is a "newtype" (e.g. `MyEnum::Variant(SomeData)`),
        // call deserialize on *this* deserializer to parse it.
        seed.deserialize(self)
    }

    fn tuple_variant<V>(
        self,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        // If the variant is a tuple, we can delegate to `deserialize_seq`.
        self.deserialize_seq(visitor)
    }

    fn struct_variant<V>(
        self,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        // If the variant is a struct, we can delegate to `deserialize_map`.
        self.deserialize_map(visitor)
    }
}

/// Finally, define a convenience from_str for your custom format:
pub fn from_str<'de, T>(s: &'de str) -> Result<T, SerdeJSON5Error>
where
    T: Deserialize<'de>,
{
    // 1) Parse the string into your JSONText
    let parsed = parse_text(s).map_err(|err| SerdeJSON5Error::Custom(err.to_string()))?;

    // 2) Wrap the JSONValue in our deserializer
    let deserializer = JSONValueDeserializer {
        input: &parsed.value,
    };

    // 3) Deserialize into the caller’s type T
    T::deserialize(deserializer)
}

#[cfg(test)]
mod test {
    use serde::Deserialize;
    use super::*;
    #[derive(Debug, Deserialize)]
    struct MyData {
        name: String,
        count: i64,
        maybe: Option<f64>,
    }
    #[test]
    fn main() {
        let source = r#"
        // A possible JSON5 input
        {
          name: 'Hello',
          count: 42,
          maybe: NaN
        }
    "#;

        match from_str::<MyData>(source) {
            Ok(data) => {
                println!("Parsed data: {:?}", data);
            }
            Err(e) => {
                println!("Error: {:?}", e);
            }
        }
    }

}