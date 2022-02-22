use std::collections::HashMap;
use std::fs;

fn get_combinations(
    rule: &str,
    rules: &HashMap<&str, &str>,
    max_depth: usize,
    current_depth: usize,
) -> Vec<String> {
    let mut combinations: Vec<String> = Vec::new();

    println!("current_depth {}", current_depth);
    if max_depth < current_depth {
        return vec!["".to_string()];
    }
    if rule == "\"a\"" {
        return vec!["a".to_string()];
    } else if rule == "\"b\"" {
        return vec!["b".to_string()];
    }

    for chain in rule.split(" | ") {
        let mut chain_combinations = vec![String::new()];
        for part in chain.split(" ") {
            let combos = get_combinations(
                rules.get(part).unwrap(),
                rules,
                max_depth,
                current_depth + 1,
            );
            let mut new_chain_combinations: Vec<String> = Vec::new();

            for chain_combination in chain_combinations {
                for combo in &combos {
                    new_chain_combinations.push(format!("{}{}", chain_combination, combo))
                }
            }

            chain_combinations = new_chain_combinations;
        }
        combinations.extend(chain_combinations.iter().cloned())
    }

    // println!("Rule: {} combinations: {}", rule, combinations.concat());
    return combinations;
}

fn part1(rules: &HashMap<&str, &str>, messages: &Vec<&str>) {
    let possible_combinations = get_combinations(rules.get("0").unwrap(), rules, 9999, 0);

    println!("Possible combinations: {}", possible_combinations.len());
    let number = messages
        .iter()
        .filter(|m| possible_combinations.contains(&m.to_string()))
        .count();

    println!("Number of valid messages part 1: {}", number)
}

fn part2(rules: &HashMap<&str, &str>, messages: &Vec<&str>) {
    let mut new_rules = rules.clone();

    let mut max_message_length = 0;
    for message in messages {
        if max_message_length < message.len() {
            max_message_length = message.len();
        }
    }

    new_rules.insert("8", "42 | 42 8");
    new_rules.insert("11", "42 31 | 42 11 31");

    let possible_combinations = get_combinations(new_rules.get("0").unwrap(), &new_rules, 5, 0);

    println!("Possible combinations: {}", possible_combinations.len());

    let number = messages
        .iter()
        .filter(|m| possible_combinations.contains(&m.to_string()))
        .count();

    println!("Number of valid messages: {}", number)
}

fn main() {
    let content = fs::read_to_string("./src/input.txt").unwrap();

    let mut messages: Vec<&str> = Vec::new();
    let mut rules: HashMap<&str, &str> = HashMap::new();

    let mut parsing_rules = true;

    for line in content.lines() {
        if line == "" {
            parsing_rules = false;
            continue;
        }
        if parsing_rules {
            let split: Vec<&str> = line.split(": ").collect();
            let rule = split[1];
            rules.insert(split[0], rule);
        } else {
            messages.push(line);
        }
    }

    // part1(&rules, &messages);
    part2(&rules, &messages);
}
