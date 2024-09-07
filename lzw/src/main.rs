use std::collections::HashMap;
use std::fs;
use std::io::Write;

fn encode(input: &str) {
    let mut file = fs::OpenOptions::new()
        .write(true)
        .create(true)
        .open("lzw.bin")
        .expect("failed to create file");

    let mut table: HashMap<String, u16> = HashMap::new();

    for (i, c) in "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ! ,"
        .chars()
        .enumerate()
    {
        table.insert(c.to_string(), i as u16);
    }

    let mut s: String = "".to_string();

    for c in input.chars() {
        let mut sc = s.clone();
        sc.push(c);

        // println!("{:?}", sc);

        if table.contains_key(&sc) {
            s = sc;
        } else {
            if let Some(index) = table.get(&s) {
                file.write_all(&index.to_le_bytes().to_vec());
            } else {
                panic!("something went wrong: {}", s);
            }
            table.insert(sc, table.len() as u16);
            s = "".to_string();
            s.push(c);
        }
    }

    if let Some(index) = table.get(&s) {
        file.write_all(&index.to_le_bytes().to_vec());
    } else {
        panic!("something went wrong");
    }

    println!("");

    for (k, v) in table.iter() {
        if k.len() > 1 {
            println!("{} = {}", k, v);
        }
    }

    // println!("{:?}", table);
}

fn main() {
    encode("Hello, world! Hello World! Hello World! Hello World! Hello World! Hello World! Hello World! Hello World! Hello World! Hello World! Hello World!");
    // encode("banana_bandana");
}
