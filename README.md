# json-five-rs

This project provides a handwritten JSON5 tokenizer and recursive descent parser compatible with `serde`.

[![Crates.io Version](https://img.shields.io/crates/v/json-five)](https://crates.io/crates/json-five/) [![docs.rs](https://img.shields.io/docsrs/json-five)](https://docs.rs/json-five/latest/json_five/)

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
      maybe: null
    }
"#;

    let parsed = from_str::<MyData>(source).unwrap();
    let expected = MyData {name: "Hello".to_string(), count: 42, maybe: None};
    assert_eq!(parsed, expected);
}
```

Serializing also works in the usual way. The re-exported `to_string` function comes from the `ser` module and works 
how you'd expect with default formatting.

```rust
use serde::Serialize;
use json_five::to_string;
#[derive(Serialize)]
struct Test {
    int: u32,
    seq: Vec<&'static str>,
}
let test = Test {
    int: 1,
    seq: vec!["a", "b"],
};

let serialized = to_string(&test).unwrap();

let expected = r#"{"int": 1, "seq": ["a", "b"]}"#;
assert_eq!(serialized, expected);
```

You may also use the `to_string_formatted` with a `FormatConfiguration` to control the output format, including 
indentation, trailing commas, and key/item separators. A few useful constructors are available, including 
`::compact()` for the most compact format (no whitespace).

```rust
use serde::Serialize;
use json_five::{to_string_formatted, FormatConfiguration, TrailingComma};
#[derive(Serialize)]
struct Test {
    int: u32,
    seq: Vec<&'static str>,
}
let test = Test {
    int: 1,
    seq: vec!["a", "b"],
};

let config = FormatConfiguration::with_indent(4, TrailingComma::ALL);
let formatted_doc = to_string_formatted(&test, config).unwrap();

let expected = r#"{
    "int": 1,
    "seq": [
        "a",
        "b",
    ],
}"#;

assert_eq!(formatted_doc, expected);
```

## Examples

See the `examples/` directory for examples of programs that utilize round-tripping features.

- `examples/json5-doublequote-fixer` gives an example of tokenization-based round-tripping edits
- `examples/json5-trailing-comma-formatter` gives an example of model-based round-tripping edits


# Benchmarking

Benchmarks are available in the `benches/` directory. Test data is in the `data/` directory. A couple of benchmarks use
big files that are not committed to this repo. So run `./data/setupdata.sh` to download the required data files
so that you don't skip the big benchmarks. The benchmarks compare `json_five` (this crate) to
[serde_json](https://github.com/serde-rs/json) and [json5-rs](https://github.com/callum-oakley/json5-rs).

Notwithstanding the general caveats of benchmarks, in initial testing, `json_five` definitively outperforms `json5-rs`.
In typical scenarios observations have been 3-4x performance, and up to 20x faster in some synthetic tests on extremely large data. 
At time of writing (pre- v0) no performance optimizations have been done. I expect performance to improve, 
if at least marginally, in the future.

These benchmarks were run on Windows on an i9-10900K with rustc 1.83.0 (90b35a623 2024-11-26). This table won't be updated unless significant changes happen.


| test                       | json_five | json5     | serde_json |
|----------------------------|-----------|-----------|------------|
| big (25MB)                 | 580.31 ms | 3.0861 s  | 150.39 ms  |
| medium-ascii (5MB)         | 199.88 ms | 706.94 ms | 59.008 ms  |
| empty                      | 228.62 ns | 708.00 ns | 38.786 ns  |
| arrays                     | 578.24 ns | 1.3228 µs | 100.95 ns  |
| objects                    | 922.91 ns | 2.0748 µs | 205.75 ns  |
| nested-array               | 22.990 µs | 29.356 µs | 5.0483 µs  |
| nested-objects             | 50.659 µs | 132.75 µs | 14.755 µs  |
| string                     | 421.17 ns | 3.5691 µs | 91.051 ns  |
| number                     | 238.75 ns | 779.13 ns | 36.179 ns  |
| deserialize (size 10)      | 6.9898µs  | 58.398µs  | 886.33ns   |
| deserialize (size 100)     | 66.005µs  | 830.79µs  | 9.9705µs   |
| deserialize (size 1000)    | 599.39µs  | 8.4952ms  | 69.110µs   |
| deserialize (size 10000)   | 5.9841ms  | 82.591ms  | 734.40µs   |
| deserialize (size 100000)  | 66.841ms  | 955.37ms  | 11.638ms   |
| deserialize (size 1000000) | 674.13ms  | 9.5758s   | 119.03ms   |
| serialize (size 10)        | 2.3496µs  | 48.915µs  | 891.85ns   |
| serialize (size 100)       | 19.602µs  | 458.98µs  | 6.7109µs   |
| serialize (size 1000)      | 194.19µs  | 4.6035ms  | 62.667µs   |
| serialize (size 10000)     | 2.2104ms  | 47.253ms  | 761.10µs   |
| serialize (size 100000)    | 24.418ms  | 502.35ms  | 11.410ms   |
| serialize (size 1000000)   | 245.26ms  | 4.6211s   | 115.84ms   |




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

