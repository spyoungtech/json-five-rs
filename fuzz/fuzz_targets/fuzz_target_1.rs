#![no_main]

use libfuzzer_sys::fuzz_target;
use json_five::tokenize::tokenize_bytes;
use json_five::parser::from_tokens;
fuzz_target!(|data: &[u8]| {
    let result = tokenize_bytes(data);
    if let Ok(tokens) = result {
        let _ = from_tokens(&tokens);
    }
});
