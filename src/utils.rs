pub(crate) fn get_line_col_char(doc: &str, byte_offset: usize) -> (usize, usize, usize) {
    let mut lineno: usize = 1;
    let mut colno: usize = 0;
    if byte_offset == 0 {
        return (1, 1, 0)
    }
    assert!(byte_offset < doc.len(), "requested byteoffset {} is not less than doc length ({})", byte_offset, doc.len());
    for (codepoint_off, (byte_off, char)) in doc.char_indices().enumerate() {
        colno += 1;
        if char == '\n' {
            if byte_off == byte_offset {
                return (lineno, colno, codepoint_off)
            }
            lineno += 1;
            colno = 0;
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

