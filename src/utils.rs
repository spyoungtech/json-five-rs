pub(crate) fn get_byte_to_code_point(haystack: &str) -> Vec<usize> {
    // copied from https://github.com/G-Research/ahocorasick_rs/blob/034e3f67e12198c08137bb9fb3153cb01cf5da31/src/lib.rs#L72-L88

    // Map UTF-8 byte index to Unicode code point index

    let mut byte_to_code_point = vec![usize::MAX; haystack.len() + 1];
    let mut max_codepoint = 0;
    for (codepoint_off, (byte_off, _)) in haystack.char_indices().enumerate() {
        byte_to_code_point[byte_off] = codepoint_off;
        max_codepoint = codepoint_off;
    }
    // End index is exclusive (e.g. 0:3 is first 3 characters), so handle
    // the case where pattern is at end of string.
    if !haystack.is_empty() {
        byte_to_code_point[haystack.len()] = max_codepoint + 1;
    }
    byte_to_code_point
}

pub(crate) fn get_partial_byte_to_code_point(haystack: &str, stop_byte_offset: usize) -> Vec<usize> {
    // A slight optimization of `get_byte_to_code_point` that lets you stop at a certain
    // byte offset instead of processing the whole haystack
    // Useful for when an error message is localized to a particular token
    // For example, if the offset conversion needed toward the beginning of a very long haystack
    assert!(stop_byte_offset < haystack.len());
    let mut byte_to_code_point = vec![usize::MAX; stop_byte_offset + 1];
    let mut max_codepoint = 0;
    for (codepoint_off, (byte_off, _)) in haystack.char_indices().enumerate() {
        byte_to_code_point[byte_off] = codepoint_off;
        max_codepoint = codepoint_off;
        if byte_off >= stop_byte_offset {
            break
        }
    }
    // End index is exclusive (e.g. 0:3 is first 3 characters), so handle
    // the case where pattern is at end of string.
    if !haystack.is_empty() {
        byte_to_code_point[haystack.len()] = max_codepoint + 1;
    }
    byte_to_code_point
}


pub(crate) fn get_line_col_char(doc: &str, byte_offset: usize) -> (usize, usize, usize) {
    let mut lineno: usize = 1;
    let mut colno: usize = 1;
    assert!(byte_offset < doc.len());
    for (codepoint_off, (byte_off, char)) in doc.char_indices().enumerate() {
        colno += 1;
        if char == '\n' {
            lineno += 1;
            colno = 1;
        }
        if byte_off < byte_offset {
            continue
        }
        if byte_off == byte_offset {
            return (lineno, colno, codepoint_off)
        }
        if byte_off > byte_offset {
            panic!("Byteoffset lands in the middle of a character")
        }
    }
    panic!("Reached end of document")
}

