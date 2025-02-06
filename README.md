# json-five-rs

This project provides a handwritten JSON5 tokenizer and recursive descent parser compatible with `serde`.

[![Crates.io Version](https://img.shields.io/crates/v/json-five)](https://crates.io/crates/json-five/) [![docs.rs](https://img.shields.io/docsrs/json-five)]()

# Key Features

- Compatible with `serde` data model
- Supports round-trip use cases with preservation/editing of whitespace and comments
- Supports formatting (indent, compact formats, etc.) in serialization
- Supports both model-based (AST) edits and token-based round-tripping
- Performance-focused default tokenizer/parser that avoids copying input
- Ergonomics-focused round-trip tokenizer/parser that produce structures with solely owned types for ease of editing
- Supports basic parsing and serialization without `serde` (you may disable the default `serde` feature!)

# Usage

You can use this lib with `serde` in the typical way:
```rust
use json_five::from_str;
use serde::Deserialize;
#[derive(Debug, PartialEq, Deserialize)]
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
    let expected = MyData {name: "Hello".to_string(), count: 42, maybe: Some(NaN)}
    assert_eq!(parsed, expected)
}
```
## Examples

See the `examples/` directory for examples of programs that utilize round-tripping features.

- `examples/json5-doublequote-fixer` gives an example of tokenization-based round-tripping edits
- `examples/json5-trailing-comma-formatter` gives an example of model-based round-tripping edits

## Benchmarking

Benchmarks are available in the `benches/` directory. Test data is in the `data/` directory. A couple of benchmarks use
big files that are not committed to this repo. So run `./data/setupdata.sh` to download the required data files
so that you don't skip the big benchmarks. The benchmarks compare `json_five` (this crate) to
[serde_json](https://github.com/serde-rs/json) and [json5-rs](https://github.com/callum-oakley/json5-rs).

Notwithstanding the general caveats of benchmarks, in initial testing, `json_five` outperforms `json5-rs`.
In typical scenarios: 3-4x performance, it seems. At time of writing (pre- v0) no performance optimizations have been done. I 
expect performance to improve, if at least marginally, in the future.

These benchmarks were run on Windows on an i9-10900K. This table won't be updated unless significant changes happen.

|      test     |   json_five   |   serde_json  | json5         |
|---------------|---------------|---------------|---------------|
|      big      |   580.31 ms   |   150.39 ms   | 3.0861 s      |
|     empty     |   228.62 ns   |   38.786 ns   | 708.00 ns     |
|  medium-ascii |   199.88 ms   |   59.008 ms   | 706.94 ms     |
|     arrays    |   578.24 ns   |   100.95 ns   | 1.3228 µs     |
|    objects    |   922.91 ns   |   205.75 ns   | 2.0748 µs     |
|  nested-array |   22.990 µs   |   5.0483 µs   | 29.356 µs     |
| nested-objects|   50.659 µs   |   14.755 µs   | 132.75 µs     |
|     string    |   421.17 ns   |   91.051 ns   | 3.5691 µs     |
|     number    |   238.75 ns   |   36.179 ns   | 779.13 ns     |



# Round-trip model

The `rt` module contains the round-trip parser. This is intended to be ergonomic for round-trip use cases, although 
it is still very possible to use the default parser (which is more performance-oriented) for certain round-trip use cases. 
The round-trip AST model produced by the round-trip parser includes additional `context` fields that describe the whitespace, comments, 
and (where applicable) trailing commas on each production. Moreover, unlike the default parser, the AST consists 
entirely of owned types, allowing for simplified in-place editing.


The `context` field holds a single field struct that contains the field `wsc` (meaning 'white space and comments') 
which holds a tuple of `String`s that represent the contextual whitespace and comments. The last element in 
the `wsc` tuple in the `context` of `JSONArrayValue` and `JSONKeyValuePair` objects is an `Option<String>` -- which 
is used as a marker to indicate an optional trailing comma and any whitespace that may follow that optional comma.

The `context` field is always an `Option`.

Contexts are associated with the following structs (which correspond to the JSON5 productions) and their context layout:

## `rt::parser::JSONText`

Represents the top-level Text production of a JSON5 document. It consists solely of a single (required) value. 
It may have whitespace/comments before or after the value. The `value` field contains any `JSONValue` and the `context` 
field contains the context struct containing the `wsc` field, a two-length tuple that describes the whitespace before and after the value.
In other words: `{ wsc.0 } value { wsc.1 }`

```rust
use json_five::rt::parser::from_str;
use json_five::rt::parser::JSONValue;

let doc = from_str(" 'foo'\n").unwrap();
let context = doc.context.unwrap();

assert_eq!(&context.wsc.0, " ");
assert_eq!(doc.value, JSONValue::SingleQuotedString("foo".to_string()));
assert_eq!(&context.wsc.1, "\n");
```


## `rt::parser::JSONValue::JSONObject`

Member of the `rt::parser::JSONValue` enum representing [JSON5 objects](https://spec.json5.org/#objects).

There are two fields: `key_value_pairs`, which is a `Vec` of `JSONKeyValuePair`s, and `context` whose `wsc` is 
a one-length tuple containing the whitespace/comments that occur after the opening brace. In non-empty objects, 
the whitespace that precedes the closing brace is part of the last item in the `key_value_pairs` Vec.  
In other words: `LBRACE { wsc.0 } [ key_value_pairs ] RBRACE`  
and: `.context.wsc: (String,)`

### `rt::parser::KeyValuePair`

The `KeyValuePair` struct represents the ['JSON5Member' production](https://spec.json5.org/#prod-JSON5Member). 
It has three fields: `key`, `value`, and `context`. The `key` is a `JSONValue`, in practice limited to `JSONValue::Identifier`, 
`JSONValue::DoubleQuotedString` or a `JSONValue::SingleQuotedString`. The `value` is any `JSONValue`.

Its context describes whitespace/comments that are between the key 
and `:`, between the `:` and the value, after the value, and (optionally) a trailing comma and whitespace trailing the
comma.  
In other words, roughly: `key { wsc.0 } COLON { wsc.1 } value { wsc.2 } [ COMMA { wsc.3 } [ next_key_value_pair ] ]`  
and: `.context.wsc: (String, String, String, Option<String>)`

When `context.wsc.3` is `Some()`, it indicates the presence of a trailing comma (not included in the string) and 
whitespace that follows the comma. This item MUST be `Some()` when it is not the last member in the object.

## `rt::parser::JSONValue::JSONArray`

Member of the `rt::parser::JSONValue` enum representing [JSON5 arrays](https://spec.json5.org/#arrays).

There are two fields on this struct: `values`, which is of type `Vec<JSONArrayValue>`, and `context` which holds 
a one-length tuple containing the whitespace/comments that occur after the opening bracket. In non-empty arrays,
the whitespace that precedes the closing bracket is part of the last item in the `values` Vec.  
In other words: `LBRACKET { wsc.0 } [ values ] RBRACKET`  
and: `.context.wsc: (String,)`


### `rt::parser::JSONArrayValue`

The `JSONArrayValue` struct represents a single member of a JSON5 Array. It has two fields: `value`, which is any 
`JSONValue`, and `context` which contains the contextual whitespace/comments around the member. The `context`'s `wsc` 
field is a two-length tuple for the whitespace that may occur after the value and (optionally) after the comma following the value.  
In other words, roughly: `value { wsc.0 } [ COMMA { wsc.1 } [ next_value ]]`  
and: `.context.wsc: (String, Option<String>)`

When `context.wsc.1` is `Some()` it indicates the presence of the comma (not included in the string) and any whitespace 
following the comma is contained in the string. This item MUST be `Some()` when it is not the last member of the array.

## Other `rt::parser::JSONValue`s



- `JSONValue::Integer(String)`
- `JSONValue::Float(String)`
- `JSONValue::Exponent(String)`
- `JSONValue::Null`
- `JSONValue::Infinity`
- `JSONValue::NaN`
- `JSONValue::Hexadecimal(String)`
- `JSONValue::Bool(bool)`
- `JSONValue::DoubleQuotedString(String)`
- `JSONValue::SingleQuotedString(String)`
- `JSONValue::Unary { operator: UnaryOperator, value: Box<JSONValue> }`
- `JSONValue::Identifier(String)` (for object keys only!).

Where these enum members have `String`s, they represent the object as it was tokenized without any modifications (that 
is, for example, without any escape sequences un-escaped). The single- and double-quoted `String`s do not include the surrounding 
quote characters. These members alone have no `context`.

# round-trip tokenizer

The `rt::tokenizer` module contains some useful tools for round-tripping tokens. The `Token`s produced by the 
rt tokenizer are owned types containing the lexeme from the source. There are two key functions in the tokenizer module:

- `rt::tokenize::source_to_tokens`
- `rt::tokenize::tokens_to_source`

Each `Token` generated from `source_to_tokens` also contains some contextual information, such as line/col numbers, offsets, etc. 
This contextual information is not required for `tokens_to_source` -- that is: you can create new tokens and insert them 
into your tokens array and process those tokens back to JSON5 source without issue.

The `tok_type` attribute leverages the same `json_five::tokenize::TokType` types. Those are:

- `LeftBrace`
- `RightBrace`
- `LeftBracket`
- `RightBracket`
- `Comma`
- `Colon`
- `Name` (Identifiers)
- `SingleQuotedString`
- `DoubleQuotedString`
- `BlockComment`
- `LineComment` note: the lexeme includes the singular trailing newline, if present (e.g., not a comment just before EOF with no newline at end of file)
- `Whitespace`
- `True`
- `False`
- `Null`
- `Integer`
- `Float`
- `Infinity`
- `Nan`
- `Exponent`
- `Hexadecimal`
- `Plus`
- `Minus`
- `EOF`

Note: string tokens will include surrounding quotes.


# Notes

## Status

This project is in **very** early phases. While the crate is usable right now, more thorough testing is needed to 
ensure that the tokenizer/parser rejects invalid documents.

Questions, discussions, and contributions are welcome. Right now, things are moving fast, so the best way to contribute 
is likely to just [open an issue](https://github.com/spyoungtech/json-five-rs/issues).

Expect breaking changes for now, even in patch releases.

## Serde is optional

Using `serde` is actually optional. Some use cases may not require the use of serde's various deserialization methods 
and may only need to rely on the tokenizer and/or AST tree features. By default, the serde feature is enabled, 
but this can be disabled. Even without the `serde` feature, the parser modules provide functions and methods for 
parsing and serialization, including the ability to customize the style.


## TODOs

Some things I need to implement and some things I may or may not implement. In rough priority order:

- [ ] Move documentation from readme to crate documentation
- [ ] Provide methods for safely editing models (e.g., validate that, when serialized, the model will produce a valid JSON5 document) today. This may also let us adjust the visibility of certain attributes.
- [ ] Provide a `json5!` macro similar to `serde_json`'s [`json!` macro](https://docs.rs/serde_json/latest/serde_json/value/index.html)
- [ ] Investigate `no_std` support
- [ ] Optimize the round-trip tokenizer to avoid processing the input twice
- [ ] More serialization formatting options (e.g., prefer single- or double-quoted strings, try to use identifiers where possible, etc.)
- [ ] Incremental parsing. Originally, an incremental tokenizer/parser was actually developed. In testing, speeds were the same or worse. Maybe it could be done in a performant way. But this may be useful for specific use cases, such as memory-constrained environments, very large JSON5 files (why?), or use cases where the input is streamed (say, over the network).
- [x] ~~Publish crate~~
- [x] ~~Benchmarks~~
- [x] ~~Basic formatting options (indent, compact, trailing comma)~~
- [x] ~~Complete logic for serialization of values (specifically: processing all \[unicode\] escape sequences in strings/identifiers and handling certain float formats like `.0` and `1.`)~~
- [x] ~~Come up with a way to reject invalid unicode escape sequences (e.g., when an illegal escape sequence is used at the start of an identifier)~~
- [ ] ~~Validate correctness of the tokenizer (specifically: use of `is_alphabetic` may not comport with the JSON5 spec)~~
