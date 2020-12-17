use std::collections::HashMap;
use std::fs;

fn part1(board: HashMap<(i32, i32, i32, i32), char>) {
    let mut mutating_board = board;
    for step in 0..6 {
        let mut new_board: HashMap<(i32, i32, i32, i32), char> = HashMap::new();
        for ((x, y, z, w), charge) in &mutating_board {
            let mut active_neighbors = 0;

            for dx in -1..2 {
                for dy in -1..2 {
                    for dz in -1..2 {
                        if dx != 0 || dy != 0 || dz != 0 {
                            match mutating_board.get(&(&(x + dx, y + dy, z + dz, *w))) {
                                Some(val) => {
                                    if *val == '#' {
                                        active_neighbors += 1;
                                    }
                                }
                                None => {}
                            }
                        }
                    }
                }
            }

            if *charge == '#' && !(active_neighbors == 2 || active_neighbors == 3) {
                new_board.insert((*x, *y, *z, *w), '.');
            } else if *charge == '.' && active_neighbors == 3 {
                new_board.insert((*x, *y, *z, *w), '#');
            } else {
                new_board.insert((*x, *y, *z, *w), *charge);
            }
        }
        mutating_board = new_board;
        // print_board(&mutating_board);
        println!("Iteration {}", step)
    }

    let mut count = 0;

    for (_coord, c) in mutating_board {
        if c == '#' {
            count += 1;
        }
    }

    println!("\nPART 1 COUNT: {}", count);
}

fn part2(board: HashMap<(i32, i32, i32, i32), char>) {
    let mut mutating_board = board;
    for step in 0..6 {
        let mut new_board: HashMap<(i32, i32, i32, i32), char> = HashMap::new();
        for ((x, y, z, w), charge) in &mutating_board {
            let mut active_neighbors = 0;

            for dx in -1..2 {
                for dy in -1..2 {
                    for dz in -1..2 {
                        for dw in -1..2 {
                            if dx != 0 || dy != 0 || dz != 0 || dw != 0 {
                                match mutating_board.get(&(x + dx, y + dy, z + dz, w + dw)) {
                                    Some(val) => {
                                        if *val == '#' {
                                            active_neighbors += 1;
                                        }
                                    }
                                    None => {}
                                }
                            }
                        }
                    }
                }
            }

            if *charge == '#' && !(active_neighbors == 2 || active_neighbors == 3) {
                new_board.insert((*x, *y, *z, *w), '.');
            } else if *charge == '.' && active_neighbors == 3 {
                new_board.insert((*x, *y, *z, *w), '#');
            } else {
                new_board.insert((*x, *y, *z, *w), *charge);
            }
        }
        mutating_board = new_board;
        // print_board(&mutating_board);
        println!("Iteration {}", step)
    }

    let mut count = 0;

    for (_coord, c) in mutating_board {
        if c == '#' {
            count += 1;
        }
    }

    println!("\nPART 2 COUNT: {}", count);
}

fn main() {
    let content = fs::read_to_string("./src/input.txt").unwrap();

    let mut board: HashMap<(i32, i32, i32, i32), char> = HashMap::new();

    let range = 15;
    for x in -range..range {
        for y in -range..range {
            for z in -7..7 {
                for w in -7..7 {
                    let coordinates = (x as i32, y as i32, z as i32, w as i32);
                    board.insert(coordinates, '.');
                }
            }
        }
    }

    content.lines().enumerate().for_each(|(y, line)| {
        for x in 0..line.len() {
            let coordinates = (x as i32, y as i32, 0, 0);
            board.insert(coordinates, line.chars().nth(x).unwrap());
        }
    });

    // part1(board);
    part2(board);
}
