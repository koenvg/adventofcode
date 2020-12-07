const fs = require("fs");

const content = fs.readFileSync(__dirname + "/input.txt", "utf-8");

const canContainBag = (name, node) => {
  return node.children.some(({ child }) => {
    if (child.bagName === name) {
      return true;
    }
    return canContainBag(name, child);
  });
};

const numberOfBagsItContains = (node, amount = { total: 0 }) => {
  node.children.forEach((node) => {
    amount.total += node.amount;
    for (let i = 0; i < node.amount; i++) {
      numberOfBagsItContains(node.child, amount);
    }
  });
  return amount.total;
};

let nodes = [];

const getNode = (name) => {
  const node = nodes.find((n) => n.bagName === name);
  if (node) {
    return node;
  }
  const newNode = {
    bagName: name,
    children: [],
  };

  nodes.push(newNode);
  return newNode;
};

content.split("\n").forEach((line) => {
  const [_, bagName, contents] = line.match(/(.*) bags contain (.*)/);

  const node = getNode(bagName);

  contents
    .split(", ")
    .filter((content) => content !== "no other bags.")
    .map((content) => content.match(/(\d) (.*) bag/))
    .forEach((match) => {
      const [_, amount, bagNameContent] = match;
      const contentNode = getNode(bagNameContent);
      node.children.push({
        child: contentNode,
        amount: parseInt(amount, 10),
      });
    });
});

const bagsThatCanContainShinyGold = nodes.filter((node) => {
  return canContainBag("shiny gold", node);
});

console.log(
  "Bags that can contain shiny gold",
  bagsThatCanContainShinyGold.length
);

const shinyGoldBag = nodes.find((node) => node.bagName === "shiny gold");

console.log("Shiny gold is a big bag: ", numberOfBagsItContains(shinyGoldBag));
