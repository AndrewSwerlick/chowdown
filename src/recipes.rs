use comrak::nodes::{AstNode, ListType, NodeValue};
use comrak::{format_commonmark, parse_document, Arena, ComrakOptions};
use derive_builder::Builder;
use fraction::Ratio;
use inflector::Inflector;
use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;
use scraper::{Html, Selector};
use select::document::Document;
use select::predicate::{Predicate, Attr, Class, Name};

use std::convert::TryFrom;
use std::fmt;

#[derive(Parser)]
#[grammar = "ingredient.pest"]
pub struct IngredientParser;

#[derive(Debug, Clone, PartialEq)]
pub enum Quantity {
    Integer(u8),
    // Represented as the numerator of a fraction over 24.
    // 24 is chose because its the LCD of common cooking fractions
    // like 1/2, 1/4, and 1/8.
    Fractional(u8),
    Descriptive(String),
}

impl From<Pair<'_, Rule>> for Quantity {
    fn from(amount: Pair<'_, Rule>) -> Quantity {
        let parsed = amount.into_inner().next().unwrap();
        match parsed.as_rule() {
            Rule::non_integer => parse_non_integer(parsed),
            Rule::integer => Quantity::Integer(parsed.as_str().parse().unwrap()),
            Rule::descriptive => Quantity::Descriptive(parsed.as_str().to_string()),
            _ => Quantity::Descriptive("".to_string()),
        }
    }
}

impl fmt::Display for Quantity {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Quantity::Integer(v) => v.fmt(f),
            Quantity::Fractional(v) => {
                let fraction = Ratio::new(*v, 24);
                let integer = fraction.trunc().to_integer();
                let fract = fraction.fract();
                match integer {
                    0 => fract.fmt(f),
                    _ => write!(f, "{int} {fract}", int = integer, fract = fract),
                }
            }
            Quantity::Descriptive(v) => v.fmt(f),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Unit {
    Tablespoon,
    Teaspoon,
    Cup,
    Ounce,
    Gram,
    Each,
    Imprecise(String),
}

impl Unit {
    fn to_inflected_string(&self, quantity: &Quantity) -> String {
        let is_plural = match quantity {
            Quantity::Integer(v) => v > &1,
            Quantity::Fractional(v) => (*v as f32) / (24 as f32) > 1.0,
            Quantity::Descriptive(_) => true,
        };
        match (&self, is_plural) {
            (Unit::Each, _) => self.to_string(),
            (_, true) => self.to_string().to_plural(),
            (_, false) => self.to_string(),
        }
    }
}

impl From<String> for Unit {
    fn from(text: String) -> Self {
        let downcased = text.to_lowercase();
        let normalized = downcased.trim().trim_end_matches(".");
        match normalized {
            "tablespoon" | "tbsp" => Unit::Tablespoon,
            "teaspoon" | "tsp" => Unit::Teaspoon,
            "cup" | "c" => Unit::Cup,
            "ounce" | "oz" => Unit::Ounce,
            "gram" => Unit::Gram,
            _ => Unit::Imprecise(text),
        }
    }
}

impl fmt::Display for Unit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Unit::Tablespoon => write!(f, "tablespoon"),
            Unit::Teaspoon => write!(f, "teaspoon"),
            Unit::Cup => write!(f, "cup"),
            Unit::Gram => write!(f, "gram"),
            Unit::Each => write!(f, ""),
            Unit::Ounce => write!(f, "ounce"),
            Unit::Imprecise(v) => v.fmt(f),
        }
    }
}

#[derive(Debug, Clone, Builder)]
pub struct Amount {
    quantity: Quantity,
    unit: Unit,
    alternate_measurement: Option<String>,
}

impl fmt::Display for Amount {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match (&self.unit, self.alternate_measurement.as_ref()) {
            (Unit::Each, Some(m)) => write!(
                f,
                "{quantity} {measure}",
                quantity = self.quantity,
                measure = m,
            ),
            (Unit::Each, None) => write!(f, "{quantity}", quantity = self.quantity),
            (_, Some(m)) => write!(
                f,
                "{quantity} {unit} {measure}",
                quantity = self.quantity,
                unit = &self.unit.to_inflected_string(&self.quantity),
                measure = m
            ),
            (_, None) => write!(
                f,
                "{quantity} {unit}",
                quantity = self.quantity,
                unit = &self.unit.to_inflected_string(&self.quantity)
            ),
        }
    }
}

#[derive(Debug, Builder, Clone)]
pub struct Ingredient {
    raw: String,
    item: String,
    amount: Amount,
}

impl From<String> for Ingredient {
    fn from(text: String) -> Self {
        let downcased = text.to_ascii_lowercase();

        let parsed = IngredientParser::parse(Rule::ingredient, &downcased)
            .expect("Ingredient parsing failed")
            .next()
            .unwrap();

        let mut builder = IngredientBuilder::default();
        let mut a_builder = AmountBuilder::default();

        a_builder
            .alternate_measurement(None)
            .unit(Unit::Each)
            .quantity(Quantity::Integer(1));

        for pair in parsed.into_inner() {
            match pair.as_rule() {
                Rule::amount => {
                    a_builder.quantity(Quantity::from(pair));
                }
                Rule::maybe_plural_unit => {
                    a_builder.unit(Unit::from(
                        pair.into_inner().next().unwrap().as_str().to_string(),
                    ));
                }
                Rule::alt_measurement => {
                    a_builder.alternate_measurement(Some(pair.as_str().to_string()));
                }
                Rule::item => {
                    builder.item(pair.as_str().to_string());
                }
                _ => (),
            }
        }
        builder.raw(text.clone());
        builder.amount(a_builder.build().unwrap());
        builder.build().unwrap()
    }
}

impl fmt::Display for Ingredient {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{amount} {item}", amount = self.amount, item = self.item)
    }
}

#[derive(Debug)]
pub struct Recipe {
    ingredients: Vec<(String, Vec<Ingredient>)>,
    instructions: Vec<String>,
}

impl fmt::Display for Recipe {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut text = String::new();
        for (title, ingredients) in &self.ingredients {
            text.push_str(&format!("# {}\n\n", title));
            for ingredient in ingredients {
                text.push_str("- ");
                text.push_str(&ingredient.to_string());
                text.push_str("\n");
            }
        }
        text.push_str("\n# Instructions\n\n");
        for (i, instruction) in self.instructions.iter().enumerate() {
            text.push_str(&format!("{}. ", i + 1));
            text.push_str(&instruction.to_string());
            text.push_str("\n");
        }
        text.fmt(f)
    }
}

fn collect_nodes<'a>(node: &'a AstNode<'a>, items: &mut Vec<&AstNode<'a>>) {
    for c in node.children() {
        items.push(c);
        collect_nodes(c, items);
    }
}

impl TryFrom<String> for Recipe {
    type Error = &'static str;

    fn try_from(text: String) -> Result<Self, Self::Error> {
        let arena = Arena::new();
        let root = parse_document(&arena, &text, &ComrakOptions::default());
        let mut nodes = Vec::new();
        collect_nodes(root, &mut nodes);

        let ingredients_lists: Vec<(String, Vec<Ingredient>)> = nodes
            .into_iter()
            .filter(|node| match node.data.borrow().value {
                NodeValue::List(list) => list.list_type == ListType::Bullet,
                _ => false,
            })
            .map(|list_node| {
                let header = list_node.preceding_siblings().find(|sib_node| {
                    match sib_node.data.borrow().value {
                        NodeValue::Heading(_) => true,
                        _ => false,
                    }
                });
                let header_text = match header {
                    None => "Ingredients".to_string(),
                    Some(header_node) => {
                        let mut header_text = vec![];
                        format_commonmark(header_node, &ComrakOptions::default(), &mut header_text)
                            .expect("error converinting back to markdown");
                        String::from_utf8(header_text).unwrap()
                    }
                };

                let list = list_node
                    .children()
                    .map(|c| {
                        let mut text = vec![];
                        match c.data.borrow().value {
                            NodeValue::Item(_) => format_commonmark(
                                c.children().next().unwrap(),
                                &ComrakOptions::default(),
                                &mut text,
                            )
                            .expect("Error converting back to markdown"),
                            _ => (),
                        };
                        Ingredient::from(String::from_utf8(text).unwrap().trim().to_string())
                    })
                    .collect();
                (header_text, list)
            })
            .collect();

        let recipie = Recipe {
            ingredients: ingredients_lists,
            instructions: ["".to_string()].to_vec(),
        };
        // TODO return error if we can't find any ingredients or instructions
        Ok(recipie)
    }
}

fn parse_non_integer(non_integer: Pair<'_, Rule>) -> Quantity {
    // There are a lot of unwraps in this function, but I think they're
    // All justified. In general the input for this function should
    // be constrained by the fact that these pairs come from our pest rules
    // which ensures they'll be safe for the various parsing operations we perform.
    // If any of these parsing or tree walking unwraps fails, that suggests a
    // problem with our pest rules.
    let sum = non_integer
        .into_inner()
        .map(|part| match part.as_rule() {
            Rule::integer => {
                let num: u8 = part.as_str().parse().unwrap();
                num * 24
            }
            Rule::fraction => {
                let fraction = part.into_inner().next().unwrap();
                match fraction.as_rule() {
                    Rule::multi_part => {
                        let mut parts = fraction.into_inner();
                        let numerator: u8 = parts.next().unwrap().as_str().parse().unwrap();
                        let denominator: u8 = parts.next().unwrap().as_str().parse().unwrap();
                        24 / denominator * numerator
                    }
                    Rule::unicode => {
                        let character = fraction.as_str();
                        match character {
                            "⅛" => 3,
                            "¼" => 6,
                            "⅓" => 8,
                            "⅜" => 9,
                            "½" => 12,
                            "⅔" => 16,
                            "¾" => 18,
                            _ => 0,
                        }
                    }
                    _ => 0,
                }
            }
            _ => 0,
        })
        .sum();
    Quantity::Fractional(sum)
}

pub fn extract_recipe_from_html(html: String, selector_str: &str) -> Result<Recipe, String> {
    let document = Document::from(&html);
    let selector = Selector::parse(format!("{} ul", selector_str).as_str()).unwrap();
    let ingredient_lists = document.select(&selector);

    let ingredients = ingredient_lists.map(|list| {
        let header_selector = Selector::parse(format!("{} ul", selector_str).as_str()).unwrap();

        // find header 
        // build list
    })
    let items
        .map(|ingredient| Ingredient::from(ingredient.text().collect::<Vec<_>>().join("")))
        .collect();

    let instructions_selector = Selector::parse(format!("{} ol", selector_str).as_str()).unwrap();
    let instructions = document
        .select(&instructions_selector)
        .map(|instructions| instructions.text().collect::<Vec<_>>().join(""))
        .collect();

    Ok(Recipe {
        ingredients: [("Ingredients".to_string(), items)].to_vec(),
        instructions: instructions,
    })
}

#[cfg(test)]
mod tests {
    use crate::recipes::{extract_recipe_from_html, Ingredient, Recipe};
    use difference::{Changeset, Difference};
    use std::convert::TryFrom;
    use std::fs;
    use term;

    fn diff_text(left: &String, right: &String) {
        // diffing
        let Changeset { diffs, .. } = Changeset::new(left, right, "");
        let mut t = term::stdout().unwrap();

        for i in 0..diffs.len() {
            match diffs[i] {
                Difference::Same(ref x) => {
                    t.reset().unwrap();
                    writeln!(t, " {}", x);
                }
                Difference::Add(ref x) => {
                    t.fg(term::color::GREEN).unwrap();
                    writeln!(t, "+{}", x);
                }
                Difference::Rem(ref x) => {
                    t.fg(term::color::RED).unwrap();
                    writeln!(t, "-{}", x);
                }
            }
        }
    }

    #[test]
    fn scratch() {
        let ingred =
            Ingredient::try_from("This is not an ingredient".to_string()).expect("Failed to parse");
        println!("{:#?}", ingred)
    }

    #[test]
    fn test_recipe_from_string() {
        let recipe = Recipe::try_from(fs::read_to_string("fixtures/sample.md").unwrap()).unwrap();
        assert_eq!(recipe.ingredients.len(), 2)
    }

    #[test]
    fn test_extract_recipe_from_html() {
        let html = fs::read_to_string("fixtures/cookie-mango-peanut-tofu.html").unwrap();
        let recipie = extract_recipe_from_html(html, "#tasty-recipes-33936").unwrap();
        println!("{:#?}", recipie);
        assert_eq!(recipie.ingredients.first().unwrap().1.len(), 21);
    }

    #[test]
    fn test_to_string() {
        let html = fs::read_to_string("fixtures/cookie-mango-peanut-tofu.html").unwrap();
        let recipie = extract_recipe_from_html(html, "#tasty-recipes-33936").unwrap();
        let text = recipie.to_string();
        let fixture_text = fs::read_to_string("fixtures/cookie-mango-peanut-tofu.md").unwrap();

        assert_eq!(text.trim().to_string(), fixture_text.trim().to_string());
    }
}
