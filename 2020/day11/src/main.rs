use std::cmp;
use std::fs;

fn look_at_visible_seats<F>((x, y): (usize, usize), seat_map: &Vec<String>, mut look: F)
where
    F: FnMut(char),
{
    let directions = vec![
        (-1, -1),
        (-1, 0),
        (-1, 1),
        (0, -1),
        (0, 1),
        (1, -1),
        (1, 0),
        (1, 1),
    ];

    for (x_step, y_step) in directions {
        let mut current_x = x as i32;
        let mut current_y = y as i32;
        loop {
            current_x += x_step;
            current_y += y_step;

            if current_x < 0
                || current_x >= seat_map[0].len() as i32
                || current_y < 0
                || current_y >= seat_map.len() as i32
            {
                break;
            }
            let seat = seat_map[current_y as usize]
                .chars()
                .nth(current_x as usize)
                .unwrap();

            if seat != '.' {
                look(seat);
                break;
            }
        }
    }
}

fn look_at_adjacent_seats<F>((x, y): (usize, usize), seat_map: &Vec<String>, mut look: F)
where
    F: FnMut(char),
{
    let min_y = cmp::max(0, y as i32 - 1) as usize;
    let max_y = cmp::min(seat_map.len() - 1, y + 1);
    let min_x = cmp::max(0, x as i32 - 1) as usize;
    let max_x = cmp::min(seat_map[0].len() - 1, x + 1);

    for seat_y in min_y..max_y + 1 {
        let row = &seat_map[seat_y];
        for seat_x in min_x..max_x + 1 {
            if seat_x == x && seat_y == y {
                continue;
            }
            look(row.chars().nth(seat_x).unwrap())
        }
    }
}

fn count_adjacent_occupied_seats((x, y): (usize, usize), seat_map: &Vec<String>) -> usize {
    let mut occupied_seat_count = 0;

    let count_occupied_seats = |seat| {
        if seat == '#' {
            occupied_seat_count += 1;
        }
    };
    look_at_visible_seats((x, y), seat_map, count_occupied_seats);
    return occupied_seat_count;
}

fn part1_and_2(seat_map: Vec<String>) {
    let mut map = seat_map.to_vec();
    loop {
        let mut mutations = 0;
        let mut new_map: Vec<String> = Vec::new();
        for y in 0..map.len() {
            let row = &map[y];
            let mut new_row = String::new();
            for x in 0..row.len() {
                let seat = map[y].chars().nth(x).unwrap();
                let occupied_seat_count = count_adjacent_occupied_seats((x, y), &map);

                if seat == 'L' && occupied_seat_count == 0 {
                    new_row.push('#');
                    mutations += 1;
                } else if seat == '#' && occupied_seat_count > 4 {
                    new_row.push('L');
                    mutations += 1;
                } else {
                    new_row.push(row.chars().nth(x).unwrap())
                }
            }
            new_map.push(new_row);
        }
        map = new_map;

        if mutations == 0 {
            break;
        }
    }

    let mut occupied_seats = 0;
    for row in map {
        occupied_seats += row.matches('#').count();
    }

    println!("Occupied seats: {}", occupied_seats);
}

fn main() {
    let content = fs::read_to_string("./src/input.txt").unwrap();
    let rows: Vec<String> = content.lines().map(|line| line.to_string()).collect();

    part1_and_2(rows)
}
