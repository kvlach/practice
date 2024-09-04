use std::collections::HashMap;

const TABLE: [char; 27] = [
    '@', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R',
    'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
];

fn encode(input: &str) {
    let table: HashMap<char, u8> = HashMap::new();

    for i in 0u8..255 {
        // table[(i as char)] = i;
        table.insert(i as char, i)
    }

    println!("{:?}", table);

    // let p = input.chars().next().unwrap();
    // println!("{:?}", p);

    // for c in input.chars() {
    //     println!("{:?}",);
    //     if (p as u8) + (c as u8) {
    //         unimplemented!();
    //         p = ((p as u8) + (c as u8)) as char
    //     } else {
    //     }
    // }
}

fn main() {
    encode("abcdef");
}
