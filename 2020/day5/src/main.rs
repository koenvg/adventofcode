use math::round;
use std::fs;

fn get_middle(min: i32, max: i32) -> i32 {
    return round::half_up((min + max) as f64 / 2.0, 0) as i32;
}

fn get_seat_id(seat: &str) -> i32 {
    let (rowChars, colChars) = seat.split_at(7);

    let mut row = 0;
    let mut col = 0;

    let mut max = 127;

    let mut index = 0;
    for row_code in rowChars.chars() {
        index += 1;
        if row_code == 'B' {
            row = row + round::half_up(max as f64 / (2 as i32).pow(index) as f64, 0) as i32;
        }
    }

    max = 7;
    index = 0;
    for col_code in colChars.chars() {
        index += 1;
        if col_code == 'R' {
            col = col + round::half_up(max as f64 / (2 as i32).pow(index) as f64, 0) as i32;
        }
    }

    println!(
        "Seat: {} row: {} col: {} ID: {}",
        seat,
        row,
        col,
        row * 8 + col
    );
    return row * 8 + col;
}

fn main() {
    let content =
        fs::read_to_string("./src/input.txt").expect("Something went wrong reading the file");

    let mut max_seat_id = 0;
    let mut seat_ids: Vec<i32> = Vec::new();
    for line in content.lines() {
        let seat_id = get_seat_id(line);
        seat_ids.push(seat_id);
        if max_seat_id < seat_id {
            max_seat_id = seat_id
        }
    }

    let mut missing_seat_id = 0;
    seat_ids.sort();
    let mut prev_ID = 0;
    for id in seat_ids {
        if (id - prev_ID == 2) {
            println!("BAM!!! ID: {} prevID: {}", id, prev_ID);
            missing_seat_id = prev_ID + 1;
        }
        prev_ID = id;
    }

    println!("Max seat ID {}", max_seat_id);
    println!("Missing seat ID {}", missing_seat_id);
}
