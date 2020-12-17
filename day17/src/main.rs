use std::collections::HashMap;
use std::fs;

fn coordinates_key((x, y, z): &(i32, i32, i32)) -> String {
    return format!("{}_{}_{}", z, y, x);
}
fn print_board(board: &HashMap<String, ((i32, i32, i32), char)>) {
    let mut board_vec: Vec<(&String, &((i32, i32, i32), char))> = board.iter().collect();

    board_vec.sort_by(|(key1), (key2)| {
        return key1.cmp(&key2);
    });

    let mut prev_y = -999;
    let mut prev_z = -999;
    for (key, ((_x, y, z), c)) in board_vec {
        if &prev_y != y {
            print!("\n");
        }
        if &prev_z != z {
            print!("\nZ={}\n", z);
        }
        print!("{}", c);

        prev_y = *y;
        prev_z = *z;
    }
}

fn part1(board: HashMap<String, ((i32, i32, i32), char)>) {
    let mut mutating_board = board;
    for _ in 0..6 {
        let mut new_board: HashMap<String, ((i32, i32, i32), char)> = HashMap::new();
        for (key, ((x, y, z), charge)) in &mutating_board {
            let mut active_neighbors = 0;

            for dx in -1..2 {
                for dy in -1..2 {
                    for dz in -1..2 {
                        if dx != 0 || dy != 0 || dz != 0 {
                            match mutating_board.get(&coordinates_key(&(x + dx, y + dy, z + dz))) {
                                Some((_, val)) => {
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
                new_board.insert(key.to_string(), ((*x, *y, *z), '.'));
            } else if *charge == '.' && active_neighbors == 3 {
                new_board.insert(key.to_string(), ((*x, *y, *z), '#'));
            } else {
                new_board.insert(key.to_string(), ((*x, *y, *z), *charge));
            }
        }
        mutating_board = new_board;
        // print_board(&mutating_board);
    }

    let mut count = 0;

    for (key, (coord, c)) in mutating_board {
        if c == '#' {
            count += 1;
        }
    }

    println!("\nCOUNT: {}", count);
}

fn main() {
    let content = fs::read_to_string("./src/input.txt").unwrap();

    let mut board: HashMap<String, ((i32, i32, i32), char)> = HashMap::new();

    let range = 15;
    for x in -range..range {
        for y in -range..range {
            for z in -range..range {
                let coordinates = (x as i32, y as i32, z as i32);
                board.insert(coordinates_key(&coordinates), (coordinates, '.'));
            }
        }
    }

    content.lines().enumerate().for_each(|(y, line)| {
        for x in 0..line.len() {
            let coordinates = (x as i32, y as i32, 0);
            board.insert(
                coordinates_key(&coordinates),
                (coordinates, line.chars().nth(x).unwrap()),
            );
        }
    });

    part1(board);
}
