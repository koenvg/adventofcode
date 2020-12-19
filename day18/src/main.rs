use std::fs;

fn parse_expression(expression: &str) -> i64 {
    let mut result: i64 = 0;
    let mut right = String::new();
    let mut operator = '+';

    let mut level = 0;

    for c in format!("{} ", expression).chars() {
        match c {
            '(' => {
                if level > 0 {
                    right.push(c);
                }
                level += 1;
                continue;
            }
            ')' => {
                level -= 1;
                if level == 0 {
                    let parsed_result = parse_expression(&right).to_string();
                    println!("Parsing {} result {} ", right, parsed_result);
                    right = parsed_result;
                } else {
                    right.push(c);
                }
                continue;
            }
            _ => {
                if level > 0 {
                    right.push(c);
                    continue;
                }
            }
        }
        match c {
            ' ' => {
                if right.len() == 0 {
                    continue;
                }
                let right_number: i64 = right.parse().unwrap();

                match operator {
                    '+' => {
                        result = result + right_number;
                    }
                    '*' => result = result * right_number,
                    _ => {}
                }
                right = String::new();
            }
            '+' | '*' => {
                operator = c;
            }
            _ => {
                right.push(c);
            }
        }
    }

    return result;
}
fn part1() {
    let content = fs::read_to_string("./src/input.txt").unwrap();

    let mut sum = 0;
    for line in content.lines() {
        let result = parse_expression(&format!("{}", line));

        println!("{} = {}", line, result);
        sum += result;
    }

    println!("Part1: {}", sum);
}
fn main() {
    part1();
}
