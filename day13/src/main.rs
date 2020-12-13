use std::fs;

fn part1(now: i32, busses: &Vec<i32>) {
    // depart_at, bus_id
    let mut departure: (i32, i32) = (0, 0);

    for bus in busses {
        let departs_at = (now as f32 / *bus as f32).ceil() as i32 * bus;

        if departure.0 == 0 || departure.0 - now > departs_at - now {
            departure = (departs_at, *bus);
        }
    }

    println!(
        "Earliest bus: {} at {} combined {}",
        departure.1,
        departure.0,
        (departure.0 - now) * departure.1
    );
}
fn main() {
    let content = fs::read_to_string("./src/input.txt").unwrap();
    let lines: Vec<&str> = content.lines().collect();

    let now: i32 = lines[0].parse().unwrap();
    let busses: Vec<i32> = lines[1]
        .split(",")
        .filter(|bus| bus != &"x")
        .map(|bus| bus.parse().unwrap())
        .collect();

    part1(now, &busses);
}
