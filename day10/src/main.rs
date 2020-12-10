use std::collections::HashMap;
use std::fs;

fn part1(adapter_ratings: &Vec<i32>) {
    let mut number_of_1_jolt_difference = 0;
    let mut number_of_3_jolt_difference = 0;

    for i in 1..adapter_ratings.len() {
        let diff = adapter_ratings[i] - adapter_ratings[i - 1];

        if diff == 1 {
            number_of_1_jolt_difference += 1;
        } else if diff == 3 {
            number_of_3_jolt_difference += 1;
        }
    }

    println!(
        "Result part1 {}",
        number_of_1_jolt_difference * number_of_3_jolt_difference
    );
}

fn count_possibilities(
    i: usize,
    adapter_ratings: &Vec<i32>,
    cache: &mut HashMap<usize, i64>,
) -> i64 {
    if i == adapter_ratings.len() - 1 {
        return 1;
    }
    match cache.get(&i) {
        Some(v) => return *v,
        None => {
            let rating = adapter_ratings[i];
            let mut possibilities = 0;
            for j in i + 1..adapter_ratings.len() {
                if adapter_ratings[j] - rating < 4 {
                    possibilities += count_possibilities(j, adapter_ratings, cache);
                }
            }
            cache.insert(i, possibilities);
            return possibilities;
        }
    }
}

fn part2(adapter_ratings: &Vec<i32>) {
    let mut cache: HashMap<usize, i64> = HashMap::new();
    let combinations = count_possibilities(0, adapter_ratings, &mut cache);
    println!("Possible combinations {}", combinations);
}

fn main() {
    let content = fs::read_to_string("./src/input.txt").unwrap();

    let mut input: Vec<i32> = content.lines().map(|line| line.parse().unwrap()).collect();
    // The starting outlet
    input.push(0);
    input.sort();
    // The laptop
    input.push(input[input.len() - 1] + 3);

    part1(&input);
    part2(&input);
}
