use multimap::MultiMap;
use regex::Regex;
use std::collections::HashMap;
use std::fs;

fn is_valid(
    num: &usize,
    (_name, (r1_min, r1_max, r2_min, r2_max)): &(String, (usize, usize, usize, usize)),
) -> bool {
    return (num >= r1_min && num <= r1_max) || (num >= r2_min && num <= r2_max);
}

fn part1(
    rules: &Vec<(String, (usize, usize, usize, usize))>,
    other_tickets: &Vec<Vec<usize>>,
) -> Vec<Vec<usize>> {
    let mut valid_tickets: Vec<Vec<usize>> = Vec::new();
    let mut error_rate = 0;

    for ticket in other_tickets {
        let mut valid_ticket = true;
        for num in ticket {
            let mut valid = false;
            for rule in rules {
                if is_valid(num, rule) {
                    valid = true;
                }
            }

            if !valid {
                error_rate += num;
                valid_ticket = false;
            }
        }
        if valid_ticket {
            valid_tickets.push(ticket.to_vec());
        }
    }

    println!("Part 1, The error rate is {}", error_rate);

    return valid_tickets;
}

fn part2(
    rules: &Vec<(String, (usize, usize, usize, usize))>,
    valid_tickets: &Vec<Vec<usize>>,
    my_ticket: &Vec<usize>,
) {
    println!("{}", valid_tickets.len());
    let mut rule_set: MultiMap<usize, &String> = MultiMap::new();
    for rule in rules {
        let name = &rule.0;
        for i in 0..my_ticket.len() {
            let mut all_valid = true;
            for ticket in valid_tickets {
                if !is_valid(&ticket[i], rule) {
                    // println!("INVALID {}", &ticket[i]);
                    all_valid = false;
                }
            }
            if all_valid {
                rule_set.insert(i, name);
            }
        }
    }
    let mut used_rules: Vec<&&String> = Vec::new();
    let mut rule_index: HashMap<&&String, usize> = HashMap::new();
    loop {
        for i in 0..my_ticket.len() {
            let possible_solutions: Vec<&&std::string::String> = rule_set
                .get_vec(&i)
                .unwrap()
                .iter()
                .filter(|r| !used_rules.contains(r))
                .collect();

            if possible_solutions.len() == 1 {
                used_rules.push(possible_solutions[0]);
                rule_index.insert(possible_solutions[0], i);
            }
        }
        if used_rules.len() == rules.len() {
            break;
        }
    }

    let mut result: i64 = 1;

    for (rule_name, index) in rule_index {
        if rule_name.starts_with("departure") {
            result *= my_ticket[index] as i64;
        }
    }

    println!("Result part2: {}", result);
}

fn main() {
    let rules_re = Regex::new(r"([\w ]*): (\d*)-(\d*) or (\d*)-(\d*)").unwrap();
    let content = fs::read_to_string("./src/input.txt").unwrap();

    let mut rules: Vec<(String, (usize, usize, usize, usize))> = Vec::new();
    let mut my_ticket: Vec<usize> = Vec::new();
    let mut other_tickets: Vec<Vec<usize>> = Vec::new();

    let mut parsing_rules = true;
    let mut parsing_my_ticket = false;
    let mut parsing_other_tickets = false;
    for line in content.lines() {
        if line == "" {
            parsing_rules = false;
            parsing_my_ticket = false;
            continue;
        }
        if parsing_rules {
            let caps = rules_re.captures(line).unwrap();
            let rule_name = caps[1].to_string();
            let r1_min: usize = caps[2].parse().unwrap();
            let r1_max: usize = caps[3].parse().unwrap();
            let r2_min: usize = caps[4].parse().unwrap();
            let r2_max: usize = caps[5].parse().unwrap();
            rules.push((rule_name, (r1_min, r1_max, r2_min, r2_max)));
            continue;
        }

        if parsing_my_ticket {
            my_ticket = line.split(',').map(|n| n.parse().unwrap()).collect();
            continue;
        }

        if parsing_other_tickets {
            other_tickets.push(line.split(',').map(|n| n.parse().unwrap()).collect());
            continue;
        }

        if line == "your ticket:" {
            parsing_my_ticket = true;
            continue;
        }
        if line == "nearby tickets:" {
            parsing_other_tickets = true;
        }
    }

    let mut valid_tickets = part1(&rules, &other_tickets);
    valid_tickets.push(my_ticket.to_vec());
    part2(&rules, &valid_tickets, &my_ticket);
}
