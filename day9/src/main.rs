use std::fs;

fn find_pairs_that_sums_up_to(numbers: &[i64], sum: i64) -> Vec<(i64, i64)> {
    let mut pairs: Vec<(i64, i64)> = Vec::new();
    for i in 0..numbers.len() {
        for l in i + 1..numbers.len() {
            if numbers[i] + numbers[l] == sum {
                pairs.push((numbers[i], numbers[l]))
            }
        }
    }
    return pairs;
}

fn part1(xmas: &Vec<i64>) -> Option<i64> {
    let preamble_size = 25;
    for i in preamble_size..xmas.len() {
        let number = xmas[i];

        if find_pairs_that_sums_up_to(&xmas[i - preamble_size..i], number).len() == 0 {
            println!("Found the flaw {}", number);
            return Some(number);
        }
    }
    return None;
}

fn sum(numbers: &Vec<i64>) -> i64 {
    let mut sum_of_numbers: i64 = 0;
    for num in numbers {
        sum_of_numbers += num;
    }
    return sum_of_numbers;
}

fn part2(xmas: &Vec<i64>, flaw: i64) {
    for i in 0..xmas.len() {
        let number = xmas[i];
        let mut numbers_in_sum: Vec<i64> = Vec::new();
        numbers_in_sum.push(number);

        for j in i + 1..xmas.len() {
            numbers_in_sum.push(xmas[j]);

            let current_sum = sum(&numbers_in_sum);
            if current_sum == flaw {
                numbers_in_sum.sort();
                println!(
                    "Weakness: {}",
                    numbers_in_sum.first().unwrap() + numbers_in_sum.last().unwrap()
                )
            } else if current_sum > flaw {
                break;
            }
        }
    }
}

fn main() {
    let content = fs::read_to_string("./src/input.txt").unwrap();

    let xmas: Vec<i64> = content.lines().map(|line| line.parse().unwrap()).collect();

    match part1(&xmas) {
        Some(flaw) => part2(&xmas, flaw),
        None => {}
    }
}
