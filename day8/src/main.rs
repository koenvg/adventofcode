use std::fs;

fn run_program(instructions: &Vec<(&str, i32)>) -> (bool, i32) {
    let mut visited_indexes: Vec<i32> = Vec::new();
    let mut i: i32 = 0;
    let mut accumulator: i32 = 0;

    while i < instructions.len() as i32 && !visited_indexes.contains(&i) {
        visited_indexes.push(i);
        let instruction = instructions[i as usize];
        match instruction.0 {
            "jmp" => {
                i += instruction.1;
            }
            "acc" => {
                i += 1;
                accumulator += instruction.1;
            }
            _ => {
                i += 1;
            }
        }
    }

    if i == instructions.len() as i32 {
        println!("Finished the program normally with acc {}", accumulator);
        return (true, accumulator);
    } else {
        println!(
            "Finished the program detecting an infinite loop with acc {}",
            accumulator
        );
        return (false, accumulator);
    }
}

fn part1(instructions: &Vec<(&str, i32)>) {
    run_program(instructions);
}

fn part2(instructions: &Vec<(&str, i32)>) {
    for i in 0..instructions.len() {
        let instruction = instructions[i];
        if instruction.0 == "nop" {
            let mut new_instructions = instructions.to_vec();
            new_instructions[i] = ("jmp", instruction.1);
            run_program(&new_instructions);
        } else if instruction.0 == "jmp" {
            let mut new_instructions = instructions.to_vec();
            new_instructions[i] = ("nop", instruction.1);
            run_program(&new_instructions);
        }
    }
}

fn main() {
    let content = fs::read_to_string("./src/input.txt").unwrap();

    let instructions: Vec<(&str, i32)> = content
        .lines()
        .map(|line| {
            let jos: Vec<&str> = line.split(" ").collect();
            return (jos[0], jos[1].parse().unwrap());
        })
        .collect();

    part1(&instructions);
    part2(&instructions);
}
