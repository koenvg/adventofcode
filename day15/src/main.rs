use std::collections::HashMap;

fn speak_number(number: usize, turn: usize, spoken_numbers: &mut HashMap<usize, (i32, i32)>) {
    match spoken_numbers.get(&number) {
        Some((_prev, last)) => {
            spoken_numbers.insert(number, (*last, turn as i32));
        }
        None => {
            spoken_numbers.insert(number, (-1, turn as i32));
        }
    }
}

fn game(turns: usize) {
    // let start: Vec<usize> = vec![0, 3, 6];
    let start: Vec<usize> = vec![14, 3, 1, 0, 9, 5];
    let mut spoken_numbers: HashMap<usize, (i32, i32)> = HashMap::new();
    let mut last_spoken_number: usize = 0;
    for i in 0..start.len() {
        last_spoken_number = start[i];
        spoken_numbers.insert(start[i], (-1, i as i32 + 1));
    }

    for i in start.len() + 1..turns + 1 {
        let (prev, last) = spoken_numbers.get(&last_spoken_number).unwrap();
        if *prev == -1 {
            last_spoken_number = 0;
        } else {
            last_spoken_number = (last - prev) as usize;
        }
        speak_number(last_spoken_number, i, &mut spoken_numbers);
    }

    println!("Last spoken number: {}", last_spoken_number)
}

fn part1() {
    println!("==== PART1 ====");
    game(2020);
}

fn part2() {
    println!("\n==== PART2 ====");
    game(30000000);
}
fn main() {
    part1();
    part2();
}
