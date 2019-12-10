use derive_builder::Builder;
use fraction::Ratio;
use inflector::Inflector;
use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;
use scraper::{Html, Selector};
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

#[derive(Debug, Builder)]
pub struct Ingredient {
    raw: String,
    item: String,
    amount: Amount,
}

impl From<String> for Ingredient {
    fn from(text: String) -> Self {
        let downcased = text.to_ascii_lowercase();
        let parsed = IngredientParser::parse(Rule::ingredient, &downcased)
            .expect("could not parser ingredient")
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
    ingredients: Vec<Ingredient>,
    instructions: Vec<String>,
}

impl fmt::Display for Recipe {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut text = String::new();
        text.push_str("# Ingredients\n\n");
        for ingredient in &self.ingredients {
            text.push_str("- ");
            text.push_str(&ingredient.to_string());
            text.push_str("\n");
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

// impl From<String> for Recipe {
//     fn from(text: String) -> Self {

//     }
// }

fn parse_non_integer(non_integer: Pair<'_, Rule>) -> Quantity {
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

pub fn extract_recipe_from_html(html: String) -> Result<Recipe, String> {
    let document = Html::parse_document(&html);
    let selector = Selector::parse(".tasty-recipe-ingredients li").unwrap();
    let items = document
        .select(&selector)
        .map(|ingredient| Ingredient::from(ingredient.text().collect::<Vec<_>>().join("")))
        .collect();

    let instructions_selector = Selector::parse(".tasty-recipe-instructions li").unwrap();
    let instructions = document
        .select(&instructions_selector)
        .map(|instructions| instructions.text().collect::<Vec<_>>().join(""))
        .collect();

    Ok(Recipe {
        ingredients: items,
        instructions: instructions,
    })
}

#[cfg(test)]
mod tests {
    use crate::recipes::{extract_recipe_from_html};
    use difference::{Changeset, Difference};
    use std::fs;
    use term;

    fn diff_text(left: &String, right: &String){
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
    fn scratch() {}

    #[test]
    fn test_extract_recipe_from_html() {
        let html = fs::read_to_string("fixtures/cookie-mango-peanut-tofu.html").unwrap();
        let recipie = extract_recipe_from_html(html).unwrap();
        assert_eq!(recipie.ingredients.len(), 21);
    }

    #[test]
    fn test_to_string() {
        let html = fs::read_to_string("fixtures/cookie-mango-peanut-tofu.html").unwrap();
        let recipie = extract_recipe_from_html(html).unwrap();
        let text = recipie.to_string();
        let fixture_text = fs::read_to_string("fixtures/cookie-mango-peanut-tofu.md").unwrap();

        assert_eq!(text.trim().to_string(), fixture_text.trim().to_string());
    }
}
