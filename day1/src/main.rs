use std::fs;

fn print_power_where_sum_is(numbers: Vec<i32>, sum: i32) {
    for i in 0..numbers.len() {
        for l in i + 1..numbers.len() {
            for k in l + 1..numbers.len() {
                let a = numbers[i];
                let b = numbers[l];
                let c = numbers[k];
                if a + b + c == sum {
                    println!("Number {} * {} * {} = {}", a, b, c, a * b * c);
                }
            }
        }
    }
}

fn main() {
    let contents =
        fs::read_to_string("./src/input.txt").expect("Something went wrong reading the file");

    let mut list_with_numbers: Vec<i32> = Vec::new();
    let lines: Vec<&str> = contents.split("\n").collect();

    for line in lines {
        list_with_numbers.push(line.parse().expect("Jos en jef"));
    }
    print_power_where_sum_is(list_with_numbers, 2020);
}
