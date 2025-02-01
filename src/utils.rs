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


pub (crate) fn escape_double_quoted(input: &str) -> String {
    // In the worst case (every char requires a backslash), the output could
    // be roughly twice the length of `input`.
    let mut escaped = String::with_capacity(input.len() * 2);

    for c in input.chars() {
        match c {
            '"'  => { escaped.push('\\'); escaped.push('"');  }
            '\\' => { escaped.push('\\'); escaped.push('\\'); }
            '\n' => { escaped.push('\\'); escaped.push('n');  }
            '\r' => { escaped.push('\\'); escaped.push('r');  }
            '\t' => { escaped.push('\\'); escaped.push('t');  }
            '/'  => { escaped.push('\\'); escaped.push('/');  }
            '\u{0008}' => { escaped.push('\\'); escaped.push('b'); }
            '\u{000c}' => { escaped.push('\\'); escaped.push('f'); }
            _ => escaped.push(c),
        }
    }

    escaped
}

pub (crate) fn escape_single_quoted(input: &str) -> String {
    let mut escaped = String::with_capacity(input.len() * 2);
    for c in input.chars() {
        match c {
            '\''  => { escaped.push('\\'); escaped.push('"');  }
            '\\' => { escaped.push('\\'); escaped.push('\\'); }
            '\n' => { escaped.push('\\'); escaped.push('n');  }
            '\r' => { escaped.push('\\'); escaped.push('r');  }
            '\t' => { escaped.push('\\'); escaped.push('t');  }
            '/'  => { escaped.push('\\'); escaped.push('/');  }
            '\u{0008}' => { escaped.push('\\'); escaped.push('b'); }
            '\u{000c}' => { escaped.push('\\'); escaped.push('f'); }
            _ => escaped.push(c),
        }
    }
    escaped
}