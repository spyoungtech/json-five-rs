# json5-doublequote-fixer

This example shows how to use the round-trip tokenizer to edit/format a JSON5 document.

e.g.

```rust
// src/main
fn main() {
    let doc = r#"// My Document
{
    breakfast: [ // <-- the `breakfast` key and all items will get double-quoted
        'bacon',
        'eggs',
        'spam'
    ],
    objekt: { // < -- this key, too
        nested: 'inner \"escaped double quotes\" will not be double-escaped',
        unescape: 'inner \'unnecessary escapes\' will be removed'
    }
}"#;
    println!("{}", format_str(doc));
}
```

output:
```text
// My Document
{
    "breakfast": [ // <-- the `breakfast` key and all items will get double-quoted
        "bacon",
        "eggs",
        "spam"
    ],
    "objekt": { // < -- this key, too
        "nested": "inner \"escaped double quotes\" will not be double-escaped",
        "unescape": "inner 'unnecessary escapes' will be removed"
    }
}
```
