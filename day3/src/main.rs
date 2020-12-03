use std::fs;

fn traverse_forest_and_count_trees<M>(forest: &Vec<&str>, traverser: M) -> i64
where
    M: Fn(usize, usize) -> (usize, usize),
{
    let tree = '#';
    let mut number_of_trees = 0;
    let (mut x, mut y) = traverser(0, 0);
    while y < forest.len() {
        let row = forest.get(y).unwrap();
        let position: char = row.chars().nth(x % row.len()).unwrap();
        if position == tree {
            number_of_trees += 1;
        }
        let new_coordinates = traverser(x, y);
        x = new_coordinates.0;
        y = new_coordinates.1;
    }

    println!("Number of trees in forest {}", number_of_trees);
    return number_of_trees;
}

fn main() {
    let content =
        fs::read_to_string("./src/input.txt").expect("Something went wrong reading the file");
    let forest: Vec<&str> = content.split("\n").collect();

    let sum: i64 = traverse_forest_and_count_trees(&forest, |x, y| (x + 1, y + 1))
        * traverse_forest_and_count_trees(&forest, |x, y| (x + 3, y + 1))
        * traverse_forest_and_count_trees(&forest, |x, y| (x + 5, y + 1))
        * traverse_forest_and_count_trees(&forest, |x, y| (x + 7, y + 1))
        * traverse_forest_and_count_trees(&forest, |x, y| (x + 1, y + 2));

    println!("The product is {} ", sum);
}
