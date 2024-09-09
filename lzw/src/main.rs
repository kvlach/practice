use std::collections::HashMap;
use std::fs::File;
use std::io::{Read, Write};

const INITIAL_CHARS: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ ,!";

fn encode(input: &str, path: &str) {
    let mut f = File::create(path).unwrap();

    let mut table: HashMap<String, u16> = HashMap::new();

    for (i, c) in INITIAL_CHARS.chars().enumerate() {
        table.insert(c.to_string(), i as u16);
    }

    let mut s = "".to_string();
    for c in input.chars() {
        let mut sc = s.clone();
        sc.push(c);

        if table.contains_key(&sc) {
            s = sc
        } else {
            if let Some(index) = table.get(&s) {
                f.write_all(&index.to_le_bytes()).expect("failed to write");
            } else {
                panic!("Expected to find string: {}", s);
            }
            if table.len() > (1 << 16) {
                panic!("Table too large");
            }
            table.insert(sc, table.len() as u16);
            s = c.to_string();
        }
    }

    if let Some(index) = table.get(&s) {
        f.write_all(&index.to_le_bytes()).expect("failed to write");
    } else {
        panic!("Expected to find string: {}", s);
    }

    f.flush().unwrap();
}

fn decode(path: &str) -> String {
    let mut f = File::open(path).unwrap();

    let mut table: HashMap<u16, String> = HashMap::new();

    for (i, c) in INITIAL_CHARS.chars().enumerate() {
        table.insert(i as u16, c.to_string());
    }

    let mut buf = [0u8; 2];

    f.read_exact(&mut buf).expect("failed to read bytes");
    let mut prev_index = u16::from_le_bytes(buf);
    let mut prev = table.get(&prev_index).unwrap();
    let mut current_index: u16;

    let mut output = "".to_string();

    output.push_str(prev.as_str());

    while f.read_exact(&mut buf).is_ok() {
        current_index = u16::from_le_bytes(buf);
        let current = table.get(&current_index).unwrap();
        output.push_str(current.as_str());
        let c = current.chars().next().unwrap();
        prev.push(c);
        table.insert(table.len() as u16, prev.to_string());
    }

    output
}

fn main() {
    encode("Hello, world!", "lzw.bin");
    println!("{}", decode("lzw.bin"));
}
