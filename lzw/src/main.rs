use std::collections::HashMap;
// use std::collections::HashSet;

fn encode(_input: &str) {
    let input = _input.to_string();

    let mut table: HashMap<&str, usize> = HashMap::new();
    table.insert("a", 0);
    table.insert("b", 1);
    table.insert("d", 2);
    table.insert("n", 3);
    table.insert("_", 4);

    // let mut table = vec!["a", "b", "d", "n", "_"];

    // let mut table: HashSet<&str> = HashSet::new();
    // table.insert("a");
    // table.insert("b");
    // table.insert("d");
    // table.insert("n");
    // table.insert("_");

    let mut s: String = "".to_string();
    for ch in input.chars() {
        // let mut sch = s.clone();
        // sch.push(ch);

        let sch = format!("{}{}", s, ch).as_str();

        // let sch_str = sch.as_str();

        if table.contains_key(sch) {
            s = sch.to_string();
        } else {
            if let Some(index) = table.get(s.as_str()) {
                print!("{}", index);
            } else {
                panic!("something went wrong");
            }
            table.insert(sch, table.len());
            s = ch.to_string();
        }
    }
}

fn main() {
    encode("banana_bandana");
}
