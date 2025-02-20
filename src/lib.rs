/// The default performance-focused tokenizer
pub mod tokenize;

/// Convenience functions and utilities
mod utils;


/// The default performance-focused parser
pub mod parser;

/// The deserialization module, for `serde` compatibility (optional feature)
#[cfg(feature = "serde")]
pub mod de;

/// The serialization module, for `serde` compatibility (optional feature)
#[cfg(feature = "serde")]
pub mod ser;

/// The `serde` deserializer
#[cfg(feature = "serde")]
pub use de::{from_str, JSONValueDeserializer};

/// the `serde` serializer
#[cfg(feature = "serde")]
pub use ser::{to_string, to_string_formatted, Serializer};

/// turn your strings into abstract JSON5 model (AST)
pub use parser::from_str as model_from_str;

/// Turn [crate::tokenize::Tokens] into AST
pub use parser::from_tokens as model_from_tokens;

/// formatting configuration for use with [crate::to_string_formatted]
pub use parser::{FormatConfiguration, TrailingComma};

/// turn str/bytes into [crate::tokenize::Tokens]
pub use tokenize::{tokenize_bytes, tokenize_str};

/// round-trip source (str) into [crate::rt::tokenize::Token]s
pub use rt::tokenize::{tokens_to_source, source_to_tokens};

/// The round-tripping module
pub mod rt {
    pub mod parser;
    pub mod tokenize;
}

#[doc = include_str!("../README.md")]
#[cfg(doctest)]
pub struct ReadmeDoctests;