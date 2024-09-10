use std::collections::HashMap;
use std::fs::File;
use std::io::{Read, Write};

fn encode(input: &str, path: &str) {
    let mut f = File::create(path).unwrap();
    let mut table: HashMap<String, u16> = HashMap::new();

    for (i, c) in "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ !,"
        .chars()
        .enumerate()
    {
        table.insert(c.to_string(), i as u16);
    }

    let mut s = "".to_string();

    for c in input.chars() {
        let mut sc = s.clone();
        sc.push(c);

        if table.contains_key(&sc) {
            s = sc;
        } else {
            if let Some(index) = table.get(&s) {
                f.write_all(&index.to_le_bytes()).unwrap();
            } else {
                panic!("string not found: {}", s);
            }
            table.insert(sc, table.len() as u16);
            s = c.to_string();
        }
    }

    if let Some(index) = table.get(&s) {
        f.write_all(&index.to_le_bytes()).unwrap();
    } else {
        panic!("string not found: {}", s);
    }

    // f.flush();
}

fn decode(path: &str) -> String {
    let mut f = File::open(path).unwrap();

    let mut table: HashMap<u16, String> = HashMap::new();

    for (i, c) in "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ !,"
        .chars()
        .enumerate()
    {
        table.insert(i as u16, c.to_string());
    }

    let mut output = "".to_string();

    let mut curr_index: u16;
    let mut prev: String;
    let mut curr: String;

    let mut buf = [0u8; 2];

    f.read_exact(&mut buf).unwrap();

    let prev_index = u16::from_le_bytes(buf);
    prev = table.get(&prev_index).unwrap().to_string();
    output.push_str(prev.as_str());

    while f.read_exact(&mut buf).is_ok() {
        curr_index = u16::from_le_bytes(buf);
        curr = table.get(&curr_index).unwrap().to_string();
        output.push_str(curr.as_str());

        let c = curr.chars().next().unwrap();
        prev.push(c);
        table.insert(table.len() as u16, prev);
        prev = curr;
    }

    output
}

fn main() {
    encode("Hello, world!", "lzw.bin");
    println!("{}", decode("lzw.bin"));
}
