#![doc = include_str!("../README.md")]

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

#[cfg(feature = "serde")]
pub use de::{from_str, from_bytes, JSONValueDeserializer};

#[cfg(feature = "serde")]
pub use ser::{to_string, to_string_formatted, Serializer};


pub use parser::{from_str as model_from_str, from_bytes as model_from_bytes};

pub use parser::from_tokens as model_from_tokens;

pub use parser::{FormatConfiguration, TrailingComma};

pub use tokenize::{tokenize_bytes, tokenize_str, tokenize_rt_str, tokenize_rt_bytes};

pub use rt::tokenize::{tokens_to_source, source_to_tokens};


/// The round-tripping module
pub mod rt {
    pub mod parser;
    pub mod tokenize;
}
