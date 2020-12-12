use std::fs;

fn part1(actions: Vec<(&str, i32)>) {
    let mut x = 0;
    let mut y = 0;
    let mut angle = 0;

    for (action, amount) in actions {
        match action {
            "N" => y += amount,
            "S" => y -= amount,
            "E" => x += amount,
            "W" => x -= amount,
            "R" => angle = (angle - amount) % 360,
            "L" => angle = (angle + amount) % 360,
            "F" => {
                if angle == 0 {
                    x += amount;
                } else if angle == 90 || angle == -270 {
                    y += amount;
                } else if angle == 180 || angle == -180 {
                    x -= amount;
                } else {
                    y -= amount;
                }
            }
            _ => {}
        }
    }

    print!(
        "X: {}, Y: {} Manhattan distance: {} \n",
        x,
        y,
        x.abs() + y.abs()
    );
}

fn main() {
    let content = fs::read_to_string("./src/input.txt").unwrap();

    let actions: Vec<(&str, i32)> = content
        .lines()
        .map(|line| {
            let (action, amount) = line.split_at(1);
            return (action, amount.parse().unwrap());
        })
        .collect();

    part1(actions);
}
