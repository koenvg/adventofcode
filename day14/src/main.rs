use regex::Regex;
use std::collections::HashMap;
use std::fs;

fn get_masked_number(mask: &str, number: &i64) -> i64 {
    let binary_format = format!("{:b}", number);

    let mut masked_number = String::new();

    for i in 0..mask.len() {
        let binary_index = binary_format.len() as i32 - mask.len() as i32 + i as i32;

        let mask_char = mask.chars().nth(i).unwrap();

        if mask_char == 'X' {
            if binary_index < 0 {
                masked_number.push('0');
            } else {
                let n = binary_format.chars().nth(binary_index as usize).unwrap();
                masked_number.push(n);
            }
        } else {
            masked_number.push(mask_char)
        }
    }

    return i64::from_str_radix(&masked_number, 2).unwrap();
}

fn part1(docking_instructions: &Vec<(String, Vec<(usize, i64)>)>) {
    let mut memory: HashMap<usize, i64> = HashMap::new();
    for (mask, assignments) in docking_instructions {
        for (memory_index, assignment) in assignments {
            let masked_number = get_masked_number(mask, assignment);
            memory.insert(*memory_index, masked_number);
        }
    }

    let mut sum = 0;
    for (i, masked_number) in memory {
        sum += masked_number
    }

    println!("Part 1: {}", sum)
}
fn main() {
    let content = fs::read_to_string("./src/input.txt").unwrap();
    let re = Regex::new(r"mem\[(\d*)\] = (\d*)").unwrap();
    let mask_re = Regex::new(r".* = ([X|0|1]*)").unwrap();

    let mut masks: Vec<(String, Vec<(usize, i64)>)> = Vec::new();

    for part in content.split("\nmask") {
        let lines: Vec<&str> = part.split("\n").collect();
        let cap = mask_re.captures(lines[0]).unwrap();

        let memory_assignments: Vec<(usize, i64)> = lines[1..lines.len()]
            .to_vec()
            .iter()
            .map(|line| {
                let captures = re.captures(line).unwrap();
                return (captures[1].parse().unwrap(), captures[2].parse().unwrap());
            })
            .collect();
        masks.push((cap[1].to_string(), memory_assignments))
    }

    part1(&masks);
}
