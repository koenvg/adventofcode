use std::fs;

fn unique(value: String) -> String {
    let mut unique_string = String::new();

    for item in value.chars() {
        if !unique_string.contains(item) {
            unique_string.push(item);
        }
    }
    return unique_string;
}

fn answers_they_agree_on(group: &str) -> String {
    let mut agreed_answers = String::new();
    let people: Vec<&str> = group.split("\n").collect();

    for answer in people[0].chars() {
        let mut do_people_agree = true;
        for person in &people {
            if !person.contains(answer) {
                do_people_agree = false;
                break;
            }
        }
        if do_people_agree {
            agreed_answers.push(answer);
        }
    }

    println!("Group {} \n agree on {}", group, agreed_answers);
    return agreed_answers;
}

fn main() {
    let content =
        fs::read_to_string("./src/input.txt").expect("Something went wrong reading the file");

    let groups: Vec<&str> = content.split("\n\n").collect();

    let mut sum_unique_answers = 0;
    let mut sum_unanimous_answers = 0;
    for group in groups {
        let unique_answers = unique(group.replace("\n", ""));
        sum_unique_answers += unique_answers.len();
        sum_unanimous_answers += answers_they_agree_on(group).len()
    }

    println!("Sum unique answers per group {}", sum_unique_answers);
    println!("Sum unanimous answers per group {}", sum_unanimous_answers);
}
