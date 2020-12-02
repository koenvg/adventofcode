use regex::Regex;
use std::fs;

fn parse_line(line: &str) -> (usize, usize, String, String) {
    let re = Regex::new(r"(\d*)-(\d*) ([a-z]): (.*)").unwrap();

    let cap = re.captures(line).unwrap();
    let min: usize = cap[1].parse().expect("Jos en jef");
    let max: usize = cap[2].parse().expect("Jos en jef");
    let mandatory_character = String::from(&cap[3]);
    let password = String::from(&cap[4]);
    return (min, max, mandatory_character, password);
}

fn validate_password2(line: &str) -> bool {
    let (min, max, mandatory_character, password) = parse_line(line);

    let charAtMin = String::from(password.chars().nth(min - 1).unwrap());
    let charAtMax = String::from(password.chars().nth(max - 1).unwrap());

    return (charAtMin == mandatory_character && charAtMax != mandatory_character)
        || (charAtMax == mandatory_character && charAtMin != mandatory_character);
}

fn validate_password(line: &str) -> bool {
    let (min, max, mandatory_character, password) = parse_line(line);

    let amount = password.matches(&mandatory_character).count();

    if amount < min || amount > max {
        return false;
    }
    return true;
}

fn main() {
    let content =
        fs::read_to_string("./src/input.txt").expect("Something went wrong reading the file");
    let lines: Vec<&str> = content.split("\n").collect();

    let mut amount_valid: i32 = 0;
    let mut amount_valid_part2: i32 = 0;
    for line in lines {
        let is_valid = validate_password(line);
        let is_valid_part2 = validate_password2(line);
        if is_valid_part2 {
            amount_valid_part2 += 1;
        }
        if is_valid {
            amount_valid += 1;
        }
    }

    println!("Amount of passwords valid {}", amount_valid);
    println!("Amount of passwords valid Part 2 {}", amount_valid_part2);
}
