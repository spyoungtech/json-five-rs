# json-five-rs

This project provides a handwritten JSON5 tokenizer and recursive descent parser compatible with `serde`.

Besides being a reasonably performant parser, this project also aims to provide support for round-trip use cases 
(e.g., formatters, linters) where whitespace and comments need to be preserved, added/removed, or otherwise manipulated.

The default tokenizer and parser/AST implementation are zero-copy implementations. There is also a parser implemented 
to produce AST using owned types for use cases where this is desirable.


# Usage

You can use this lib with `serde` in the typical way:
```rust
use json_five::from_str;
use serde::Deserialize;
#[derive(Debug, Deserialize)]
struct MyData {
    name: String,
    count: i64,
    maybe: Option<f64>,
}

fn main() {
    let source = r#"
    // A possible JSON5 input
    {
      name: 'Hello',
      count: 42,
      maybe: NaN
    }
"#;

    let parsed = from_str::<MyData>(source).unwrap();

    println!("Parsed data: {:?}", parsed);
    // Parsed data: MyData { name: "Hello", count: 42, maybe: Some(NaN) }
}
```


# Notes

## Status

This project is in **very** early phases. Some major pieces of functionality are missing, not least of which includes 
correctly processing escapes from JSON strings to produce Rust string values. Although many test cases are passing, 
it is safe to say it is not yet suitable for production use. 

More thorough testing is needed to ensure that the tokenizer/parser rejects invalid documents.

Questions, discussions, and contributions are welcome. Right now, things are moving fast, so the best way to contribute 
is likely to just [open an issue](https://github.com/spyoungtech/json-five-rs/issues).

## Serde is optional

Using `serde` is actually optional. Some use cases may not require the use of the serializer/deserializer functionality 
and may only need to rely on the tokenizer and/or AST tree features. By default, the serde feature is enabled, 
but this can be disabled.