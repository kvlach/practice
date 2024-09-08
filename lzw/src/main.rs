use std::collections::HashMap;
use std::fs::File;
use std::io::{Read, Write};

fn encode(input: &str, mut f: &File) {
    let mut table: HashMap<String, u16> = HashMap::new();

    for (i, c) in "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ ,!"
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
            if table.len() > 4096 {
                panic!("table too large");
            }

            if let Some(index) = table.get(&s) {
                f.write_all(&index.to_le_bytes()).expect("write failed");
            } else {
                panic!("couldn't find index of {}", s);
            }

            table.insert(sc, table.len() as u16);
            s = c.to_string();
        }
    }
}

fn decode(mut f: &File) {
    let mut table: HashMap<u16, String> = HashMap::new();

    for (i, c) in "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ ,!"
        .chars()
        .enumerate()
    {
        table.insert(i as u16, c.to_string());
    }

    let mut buf = [0u8; 2];

    let mut current_index: u16;
    let mut prev: String = "".to_string();
    while let Ok(_) = f.read_exact(&mut buf) {
        current_index = u16::from_le_bytes(buf);
        print!("{}", current_index);
        let entry = table.get(&current_index).expect("couldn't find index");
        print!("{}", entry);
        prev.push(entry.chars().next().unwrap());
        table.insert(table.len() as u16, prev.clone());
    }
}

fn main() {
    let f = File::create("lzw.bin").expect("failed to create file");
    encode("Hello, world!", &f);
    decode(&f);
}
