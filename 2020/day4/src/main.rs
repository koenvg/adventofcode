use regex::Regex;
use std::fs;

fn is_valid_passport(passport: &str) -> bool {
    let mandatory_fields = vec!["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"];

    for mandatory_field in mandatory_fields {
        if !passport.contains(&format!("{}:", mandatory_field)) {
            return false;
        }
    }
    return true;
}

fn is_valid_passport_part_2(passport: &str) -> bool {
    let rules = vec![
        r"byr:(19[2-9][0-9]|200[0-2])",
        r"iyr:(201[0-9]|2020)",
        r"eyr:(202[0-9]|2030)",
        r"hgt:(1([5-8][0-9]|9[0-3])cm|(59|6[0-9]|7[0-6])in)",
        r"hcl:#([0-9]|[a-f]){6}",
        r"ecl:(amb|blu|brn|gry|grn|hzl|oth)( |\n|$)",
        r"pid:[0-9]{9}( |\n|$)",
    ];

    for rule in rules {
        let re = Regex::new(rule).unwrap();

        if !re.is_match(passport) {
            return false;
        }
    }
    return true;
}

fn main() {
    let content =
        fs::read_to_string("./src/input.txt").expect("Something went wrong reading the file");

    let passports: Vec<&str> = content.split("\n\n").collect();

    let mut valid_passports = 0;
    let mut valid_passports_part_2 = 0;

    for passport in passports {
        if is_valid_passport(passport) {
            valid_passports += 1
        }

        if is_valid_passport_part_2(passport) {
            valid_passports_part_2 += 1;
        }
    }

    println!("Valid passports {}", valid_passports);
    println!("Valid passports part 2 {}", valid_passports_part_2);
}
