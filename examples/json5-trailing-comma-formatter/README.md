# JSON5 Formatter Example

This example shows how to use the round-trip parser to apply formatting changes to a JSON5 document.

The formatter in this example will add trailing commas to all objects and arrays.

```rust
// src/main.rs
fn main() {
    let doc = r#"// My Document
{
    breakfast: [
        'bacon',
        'eggs',
        'spam'  // <-- a trailing comma will be added here
    ],
    objekt: {
        nested: "value" // <-- and here
    } // <--- and here
}"#;
    println!("{}", format_str(doc));

}
```

output

```text
// My Document
{
    breakfast: [
        bacon,
        eggs,
        spam,  // <-- a trailing comma will be added here
    ],
    objekt: {
        nested: value, // <-- and here
    }, // <--- and here
}

```