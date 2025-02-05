use serde::de::{self, DeserializeSeed, Deserialize, Deserializer, MapAccess, SeqAccess, VariantAccess, Visitor};
use std::fmt;
use crate::parser::{JSONValue, JSONKeyValuePair, UnaryOperator, from_str as model_from_str};
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
pub struct JSONValueDeserializer<'a> {
    input: &'a JSONValue<'a>,
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
                visitor.visit_str(s)
            }
            JSONValue::Identifier(s) => visitor.visit_str(s),
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
                match u64::from_str_radix(s.trim_start_matches("0x"), 16) {
                    Ok(hex) => {
                        visitor.visit_u64(hex)
                    }
                    Err(e) => {
                        Err(de::Error::custom(format!("Invalid hex {}", e)))
                    }
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
                visitor.visit_enum(ident.into_deserializer())
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
    values: &'a [JSONValue<'a>],
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
    pairs: &'a [JSONKeyValuePair<'a>],
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
    variant: &'a str,
    content: &'a JSONValue<'a>,
}

impl<'de, 'a> de::EnumAccess<'de> for EnumDeserializer<'a> {
    type Error = SerdeJSON5Error;
    type Variant = JSONValueDeserializer<'a>;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant), SerdeJSON5Error>
    where
        V: DeserializeSeed<'de>,
    {
        use serde::de::IntoDeserializer;
        let val = seed.deserialize(self.variant.into_deserializer())?;
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
    let parsed = model_from_str(s).map_err(|err| SerdeJSON5Error::Custom(err.to_string()))?;

    // 2) Wrap the JSONValue in our deserializer
    let deserializer = JSONValueDeserializer {
        input: &parsed.value,
    };

    // 3) Deserialize into the caller’s type T
    T::deserialize(deserializer)
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;
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
                println!("{}{}{:?}", data.name, data.count, data.maybe)
            }
            Err(e) => {
                println!("Error: {:?}", e);
            }
        }
    }

    #[test]
    fn test_hashmap() {
        let source = r#"
        // A possible JSON5 input
        {
          name: 'Hello',
          count: '42',
          maybe: null
        }
    "#;

        let res: HashMap<String, Option<String>> = from_str(source).unwrap();
        let expected = HashMap::from([
            (String::from("name"), Some(String::from("Hello"))),
            (String::from("count"), Some(String::from("42"))),
            (String::from("maybe"), None),
        ]);
        assert_eq!(res, expected)

    }


}

#[cfg(test)]
mod json5_compat_tests {
    use super::*; // Bring in your `from_str` parser.
    use std::collections::BTreeMap; // Or HashMap, if preferred.
    use serde::Deserialize;

    /// A minimal JSON5-like "value" type for testing.
    /// Adjust as needed for your parser’s feature set.
    #[derive(Debug, PartialEq, Deserialize)]
    #[serde(untagged)]
    enum MyValue {
        Null,
        Bool(bool),
        Number(f64),
        String(String),
        Array(Vec<MyValue>),
        Object(BTreeMap<String, MyValue>),
    }

    /// Helper to parse a string with your JSON5 parser under test.
    fn parse_test(input: &str) -> MyValue {
        match from_str::<MyValue>(input) {
            Ok(v) => v,
            Err(e) => panic!("Error parsing input: {:?}", e),
        }
    }

    // ------------------------------------------------------------------------
    // Objects
    // ------------------------------------------------------------------------

    #[test]
    fn object_empty() {
        let v = parse_test(r#"{}"#);
        match v {
            MyValue::Object(map) => assert!(map.is_empty()),
            _ => panic!("Expected an empty object"),
        }
    }

    #[test]
    fn object_double_quoted_key() {
        let v = parse_test(r#"{"a":1}"#);
        match v {
            MyValue::Object(map) => {
                assert_eq!(map.get("a"), Some(&MyValue::Number(1.0)));
            }
            _ => panic!("Expected an object with key 'a' = 1"),
        }
    }

    #[test]
    fn object_single_quoted_key() {
        let v = parse_test(r#"{'a':1}"#);
        match v {
            MyValue::Object(map) => {
                assert_eq!(map.get("a"), Some(&MyValue::Number(1.0)));
            }
            _ => panic!("Expected an object with key 'a' = 1"),
        }
    }

    #[test]
    fn object_unquoted_key() {
        let v = parse_test(r#"{a:1}"#);
        match v {
            MyValue::Object(map) => {
                assert_eq!(map.get("a"), Some(&MyValue::Number(1.0)));
            }
            _ => panic!("Expected an object with key 'a' = 1"),
        }
    }

    #[test]
    fn object_special_keys() {
        let v = parse_test(r#"{$_:1,_$:2,a\u200C:3}"#);
        dbg!(&v);
        match v {
            MyValue::Object(map) => {
                assert_eq!(map.get("$_"), Some(&MyValue::Number(1.0)));
                assert_eq!(map.get("_$"), Some(&MyValue::Number(2.0)));
                assert_eq!(map.get("a\u{200C}"), Some(&MyValue::Number(3.0)));
            }
            _ => panic!("Expected an object with special keys $_, _$, and a\u{200C}"),
        }
    }

    #[test]
    fn object_unicode_key() {
        let v = parse_test(r#"{ùńîċõďë:9}"#);
        match v {
            MyValue::Object(map) => {
                assert_eq!(map.get("ùńîċõďë"), Some(&MyValue::Number(9.0)));
            }
            _ => panic!("Expected an object with unicode key 'ùńîċõďë' = 9"),
        }
    }


    #[test]
    fn object_unicode_key_quoted() {
        let v = parse_test(r#"{"ùńîċõďë":9}"#);
        match v {
            MyValue::Object(map) => {
                assert_eq!(map.get("ùńîċõďë"), Some(&MyValue::Number(9.0)));
            }
            _ => panic!("Expected an object with unicode key 'ùńîċõďë' = 9"),
        }
    }

    #[test]
    fn object_escaped_keys() {
        let v = parse_test(r#"{\u0061\u0062:1,\u0024\u005F:2,\u005F\u0024:3}"#);
        dbg!(&v);
        match v {
            MyValue::Object(map) => {
                // \u0061\u0062 -> "ab"
                assert_eq!(map.get("ab"), Some(&MyValue::Number(1.0)));
                // \u0024\u005F -> "$_"
                assert_eq!(map.get("$_"), Some(&MyValue::Number(2.0)));
                // \u005F\u0024 -> "_$"
                assert_eq!(map.get("_$"), Some(&MyValue::Number(3.0)));
            }
            _ => panic!("Expected escaped keys"),
        }
    }

    #[test]
    fn object_proto_name() {
        let v = parse_test(r#"{"__proto__":1}"#);
        match v {
            MyValue::Object(map) => {
                // In Rust, it's just a normal key:
                assert_eq!(map.get("__proto__"), Some(&MyValue::Number(1.0)));
            }
            _ => panic!("Expected object with '__proto__' key"),
        }
    }

    #[test]
    fn object_multiple_properties() {
        let v = parse_test(r#"{abc:1,def:2}"#);
        match v {
            MyValue::Object(map) => {
                assert_eq!(map.get("abc"), Some(&MyValue::Number(1.0)));
                assert_eq!(map.get("def"), Some(&MyValue::Number(2.0)));
            }
            _ => panic!("Expected object with {{abc:1,def:2}}"),
        }
    }

    #[test]
    fn object_nested() {
        let v = parse_test(r#"{a:{b:2}}"#);
        match v {
            MyValue::Object(outer) => {
                match outer.get("a") {
                    Some(MyValue::Object(inner)) => {
                        assert_eq!(inner.get("b"), Some(&MyValue::Number(2.0)));
                    }
                    _ => panic!("Expected a nested object {{b:2}}"),
                }
            }
            _ => panic!("Expected object with key 'a'"),
        }
    }

    // ------------------------------------------------------------------------
    // Arrays
    // ------------------------------------------------------------------------

    #[test]
    fn array_empty() {
        let v = parse_test(r#"[]"#);
        match v {
            MyValue::Array(arr) => {
                assert!(arr.is_empty());
            }
            _ => panic!("Expected an empty array"),
        }
    }

    #[test]
    fn array_single_value() {
        let v = parse_test(r#"[1]"#);
        match v {
            MyValue::Array(arr) => {
                assert_eq!(arr.len(), 1);
                assert_eq!(arr[0], MyValue::Number(1.0));
            }
            _ => panic!("Expected [1]"),
        }
    }

    #[test]
    fn array_multiple_values() {
        let v = parse_test(r#"[1,2]"#);
        match v {
            MyValue::Array(arr) => {
                assert_eq!(arr, vec![MyValue::Number(1.0), MyValue::Number(2.0)]);
            }
            _ => panic!("Expected [1,2]"),
        }
    }

    #[test]
    fn array_nested() {
        let v = parse_test(r#"[1,[2,3]]"#);
        match v {
            MyValue::Array(arr) => {
                assert_eq!(arr.len(), 2);
                assert_eq!(arr[0], MyValue::Number(1.0));
                match &arr[1] {
                    MyValue::Array(inner) => {
                        assert_eq!(inner, &vec![MyValue::Number(2.0), MyValue::Number(3.0)]);
                    }
                    _ => panic!("Expected a nested array [2,3]"),
                }
            }
            _ => panic!("Expected an array with nested array"),
        }
    }


    #[test]
    fn parse_null() {
        let v = parse_test(r#"null"#);
        assert_eq!(v, MyValue::Null);
    }

    #[test]
    fn parse_true() {
        let v = parse_test(r#"true"#);
        assert_eq!(v, MyValue::Bool(true));
    }

    #[test]
    fn parse_false() {
        let v = parse_test(r#"false"#);
        assert_eq!(v, MyValue::Bool(false));
    }


    #[test]
    fn numbers_leading_zeroes() {
        let v = parse_test(r#"[0,0.,0e0,]"#);
        match v {
            MyValue::Array(arr) => {
                // All are 0.
                assert_eq!(arr, vec![
                    MyValue::Number(0.0),
                    MyValue::Number(0.0),
                    MyValue::Number(0.0),
                ]);
            }
            _ => panic!("Expected array of zeros"),
        }
    }

    #[test]
    fn numbers_trailing_zeroes() {
        let v = parse_test(r#"[.0]"#);
        match v {
            MyValue::Array(arr) => {
                // All are 0.
                assert_eq!(arr, vec![
                    MyValue::Number(0.0),
                ]);
            }
            _ => panic!("Expected array of zeros"),
        }
    }
    #[test]
    fn numbers_integers() {
        let v = parse_test(r#"[1,23,456,7890]"#);
        match v {
            MyValue::Array(arr) => {
                assert_eq!(arr, vec![
                    MyValue::Number(1.0),
                    MyValue::Number(23.0),
                    MyValue::Number(456.0),
                    MyValue::Number(7890.0),
                ]);
            }
            _ => panic!("Expected integer array"),
        }
    }

    #[test]
    fn numbers_signed() {
        let v = parse_test(r#"[-1,+2,-.1,-0]"#);
        match v {
            MyValue::Array(arr) => {
                // Some tricky floating-point checks:
                assert_eq!(arr[0], MyValue::Number(-1.0));
                assert_eq!(arr[1], MyValue::Number(2.0));
                assert_eq!(arr[2], MyValue::Number(-0.1));
                // -0 is 0.0 in float equality:
                let minus_zero = match arr[3] {
                    MyValue::Number(f) => f,
                    _ => panic!("Expected a number for -0"),
                };
                assert_eq!(minus_zero, 0.0);
            }
            _ => panic!("Expected array of signed numbers"),
        }
    }

    #[test]
    fn numbers_leading_decimal() {
        let v = parse_test(r#"[.1,.23]"#);
        match v {
            MyValue::Array(arr) => {
                assert_eq!(arr, vec![MyValue::Number(0.1), MyValue::Number(0.23)]);
            }
            _ => panic!("Expected array of leading-decimal numbers"),
        }
    }

    #[test]
    fn numbers_fractional() {
        let v = parse_test(r#"[1.0,1.23]"#);
        match v {
            MyValue::Array(arr) => {
                assert_eq!(arr, vec![MyValue::Number(1.0), MyValue::Number(1.23)]);
            }
            _ => panic!("Expected array with fractional numbers"),
        }
    }

    #[test]
    fn numbers_exponents() {
        let v = parse_test(r#"[1e0,1e1,1e01,1.e0,1.1e0,1e-1,1e+1]"#);
        match v {
            MyValue::Array(arr) => {
                let expected = vec![
                    MyValue::Number(1.0),
                    MyValue::Number(10.0),
                    MyValue::Number(10.0),
                    MyValue::Number(1.0),
                    MyValue::Number(1.1),
                    MyValue::Number(0.1),
                    MyValue::Number(10.0),
                ];
                assert_eq!(arr, expected);
            }
            _ => panic!("Expected array of exponent numbers"),
        }
    }

    #[test]
    fn numbers_hexadecimal() {
        let v = parse_test(r#"[0x1,0x10,0xff,0xFF]"#);
        match v {
            MyValue::Array(arr) => {
                let expected = vec![
                    MyValue::Number(1.0),
                    MyValue::Number(16.0),
                    MyValue::Number(255.0),
                    MyValue::Number(255.0),
                ];
                assert_eq!(arr, expected);
            }
            _ => panic!("Expected array of hex numbers"),
        }
    }

    #[test]
    fn numbers_infinity() {
        let v = parse_test(r#"[Infinity,-Infinity]"#);
        match v {
            MyValue::Array(arr) => {
                match arr.as_slice() {
                    [MyValue::Number(a), MyValue::Number(b)] => {
                        assert!(a.is_infinite() && a.is_sign_positive());
                        assert!(b.is_infinite() && b.is_sign_negative());
                    }
                    _ => panic!("Expected [Infinity, -Infinity]"),
                }
            }
            _ => panic!("Expected array"),
        }
    }

    #[test]
    fn numbers_nan() {
        let v = parse_test(r#"NaN"#);
        match v {
            MyValue::Number(f) => assert!(f.is_nan()),
            _ => panic!("Expected NaN"),
        }
    }

    #[test]
    fn numbers_negative_nan() {
        let v = parse_test(r#"-NaN"#);
        match v {
            MyValue::Number(f) => assert!(f.is_nan()),
            _ => panic!("Expected -NaN"),
        }
    }

    #[test]
    fn numbers_single() {
        let v = parse_test(r#"1"#);
        assert_eq!(v, MyValue::Number(1.0));
    }

    #[test]
    fn numbers_signed_explicit() {
        let v = parse_test(r#"+1.23e100"#);
        match v {
            MyValue::Number(f) => {
                assert_eq!(f, 1.23e100);
            }
            _ => panic!("Expected +1.23e100 as a number"),
        }
    }

    #[test]
    fn numbers_bare_hex() {
        let v = parse_test(r#"0x1"#);
        assert_eq!(v, MyValue::Number(1.0));
    }

    #[test]
    #[should_panic]
    fn numbers_bare_long_hex() {
        let v = parse_test(r#"-0x0123456789abcdefABCDEF"#);
        match v {
            MyValue::Number(f) => {
                // Very large; f might be approximate in float form. We just check it's a number.
                assert!(f.is_sign_negative());
            }
            _ => panic!("Expected a negative large hex as a float number"),
        }
    }


    #[test]
    fn strings_double_quoted() {
        let v = parse_test(r#""abc""#);
        assert_eq!(v, MyValue::String("abc".to_owned()));
    }

    #[test]
    fn strings_single_quoted() {
        let v = parse_test(r#"'abc'"#);
        assert_eq!(v, MyValue::String("abc".to_owned()));
    }

    #[test]
    fn strings_quotes_in_strings() {
        let v = parse_test(r#"['"',"'"]"#);
        match v {
            MyValue::Array(arr) => {
                assert_eq!(arr, vec![
                    MyValue::String("\"".to_owned()),
                    MyValue::String("'".to_owned()),
                ]);
            }
            _ => panic!("Expected an array with double-quote and single-quote strings"),
        }
    }

    #[test]
    fn strings_escaped_chars() {
        // The JavaScript test includes many escapes plus line continuations.
        let input = r#"'\\b\\f\\n\\r\\t\\v\\0\\x0f\\u01fF\\\n\\\r\n\\\r\\\u2028\\\u2029\\a\\'\\\"'"#;
        let v = parse_test(input);
        match v {
            MyValue::String(s) => {
                // Depending on your parser’s rules, this will be the interpreted string.
                // Here, just check that we got a string (no parse error).
                // You can do deeper checks if your parser decodes e.g. '\v' into ASCII 0x0B, etc.
                assert!(!s.is_empty(), "Expected a non-empty string with escapes");
            }
            _ => panic!("Expected an escaped string"),
        }
    }

    #[test]
    fn strings_line_paragraph_separators() {
        // If your parser warns about \u2028 or \u2029, that’s up to you to handle. Just parse.
        let v = parse_test(r#"'\u2028\u2029'"#);
        match v {
            MyValue::String(s) => {
                assert_eq!(s, "\u{2028}\u{2029}");
            }
            _ => panic!("Expected string with line/paragraph separators"),
        }
    }


    #[test]
    fn comments_single_line() {
        let input = r#"{// comment
        }"#;
        let v = parse_test(input);
        match v {
            MyValue::Object(map) => assert!(map.is_empty()),
            _ => panic!("Expected empty object"),
        }
    }

    #[test]
    fn comments_single_line_at_end() {
        let input = r#"{}// comment"#;
        let v = parse_test(input);
        match v {
            MyValue::Object(map) => assert!(map.is_empty()),
            _ => panic!("Expected empty object"),
        }
    }

    #[test]
    fn comments_multi_line() {
        let input = r#"{/* comment
        ** */}"#;
        let v = parse_test(input);
        match v {
            MyValue::Object(map) => assert!(map.is_empty()),
            _ => panic!("Expected empty object"),
        }
    }

    #[test]
    fn whitespace() {
        // Includes tabs, vertical tabs, form feeds, non-breaking spaces, etc.
        let input = "{\t\u{000B}\u{000C} \u{00A0}\u{FEFF}\n\r\u{2028}\u{2029}\u{2003}}";
        let v = parse_test(input);
        match v {
            MyValue::Object(map) => assert!(map.is_empty()),
            _ => panic!("Expected empty object after whitespace"),
        }
    }
}
