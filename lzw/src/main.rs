use std::collections::HashMap;

fn encode(input: String) {
    let mut table: HashMap<&str, usize> = HashMap::new();
    // table.insert(k, v);

    // for (i, c) in "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".char_indices() {
    //     table.insert(c as u32, i);
    // }

    // println!("{:?}", table);

    // let mut table = vec![];

    // let mut p = input.chars().next().unwrap() as u32;
    let mut p = "";

    for c in input.chars() {
        let pc = p.push;
        if table.contains_key(&pc) {
            p = pc;
        } else {
            // println!("{:?}", table.get(&p).expect("key not found"));
            if let Some(cnt) = table.get(&p) {
                print!("{}", cnt);
            } else {
                print!("{}", 0);
            }

            if let Some(cnt) = table.get(&pc) {
                table.insert(pc, cnt + 1);
            } else {
                table.insert(pc, 1);
            }
            p = c as u32;
        }
    }
    // println!("{:?}", table.get(&p).expect("key not found"));
}

fn main() {
    encode("Hello, world!");
}
