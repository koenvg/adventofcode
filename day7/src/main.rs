use regex::Regex;
use std::fs;

struct Node {
    bag_name: String,
    children: Vec<Node>,
}

fn get_node<'a>(name: &str, nodes: &'a Vec<Node>) -> &'a Node {
    for node in nodes {
        if node.bag_name == name {
            return node;
        }
    }
    let newNode = Node {
        bag_name: name.to_string(),
        children: Vec::new(),
    };
    nodes.push(newNode);
    return &newNode;
}

fn part1(content: String) {
    let mut nodes: Vec<Node> = Vec::new();

    let re = Regex::new("(.*) bags contain (.*)").unwrap();
    let bagRe = Regex::new("\\d (.*) bags").unwrap();
    for line in content.lines() {
        let captures = re.captures(line).unwrap();
        let bag = &captures[1];
        let contents: Vec<&str> = captures[2].split(",").collect();

        let node = get_node(bag, &nodes);

        for content in contents {
            if bagRe.is_match(content) {
                let content_bag_name = &bagRe.captures(content).unwrap()[1];
                println!("Content {}", content_bag_name)
            }
        }
        println!("{}", bag);
    }
}

fn main() {
    let content =
        fs::read_to_string("./src/input.txt").expect("Something went wrong reading the file");

    part1(content);
}
