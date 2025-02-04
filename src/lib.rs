pub mod tokenize;
mod utils;
pub mod parser;
#[cfg(feature = "serde")]
pub mod de;
#[cfg(feature = "serde")]
pub mod ser;


#[cfg(feature = "serde")]
pub use de::{from_str, JSONValueDeserializer};
#[cfg(feature = "serde")]
pub use ser::{to_string, Serializer};

pub use parser::from_str as model_from_str;
pub use parser::from_tokens as model_from_tokens;
pub use tokenize::{tokenize_bytes, tokenize_str};

pub mod rt {
    pub mod parser;
    pub mod tokenize;
}