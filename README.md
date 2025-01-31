# json-five-rs

This project provides a handwritten JSON5 tokenizer and recursive descent parser compatible with `serde`.

Besides being a reasonably performant parser, this project also aims to provide support for round-trip use cases 
(e.g., formatters, linters) where whitespace and comments need to be preserved, added/removed, or otherwise manipulated.

The default tokenizer and parser/AST implementation are zero-copy implementations. There is also a parser implemented 
to produce AST using owned types for use cases where this is desirable.



# Notes

## Serde is optional

Using `serde` is actually optional. Some use cases may not require the use of the serializer/deserializer functionality 
and may only need to rely on the tokenizer and/or AST tree features. By default, the serde feature is enabled, 
but this can be disabled.