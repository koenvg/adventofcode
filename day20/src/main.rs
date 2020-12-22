use std::collections::HashMap;
use std::fs;
fn get_edges(tile: Vec<Vec<char>>) -> (String, String, String, String) {
    let mut top = String::new();
    let mut right = String::new();
    let mut bottom = String::new();
    let mut left = String::new();

    for c in &tile[0] {
        top.push(*c);
    }
    for c in &tile[tile.len() - 1] {
        bottom.push(*c);
    }

    for row in &tile {
        right.push(row[row.len() - 1]);
    }
    for row in &tile {
        left.push(row[0]);
    }

    return (top, right, bottom, left);
}

fn part1(image_tiles: &HashMap<&str, Vec<Vec<char>>>) {
    for (name, tile) in image_tiles {}
}

fn main() {
    let content = fs::read_to_string("./src/input.txt").unwrap();

    let mut tiles: HashMap<&str, Vec<Vec<char>>> = HashMap::new();

    let mut image_parts: Vec<Vec<char>> = Vec::new();
    let mut part_name = "";

    for line in content.lines() {
        if line == "" {
            continue;
        }
        if line.starts_with("Tile ") {
            if part_name != "" {
                tiles.insert(part_name, image_parts.clone());
                image_parts = Vec::new();
            }
            part_name = &line[5..9];
            continue;
        }

        image_parts.push(line.chars().collect())
    }

    part1(&tiles)
}
