use std::fmt::Display;
/// sized string for strings on the stack with a limited size.
use std::string::ToString;
const SIZE: usize = 32;
#[derive(Debug, Clone, Copy)]
pub struct SizedString {
    size: usize,
    contents: [char; SIZE],
}

impl SizedString {
    /// Create a new sized string that can be copied and transfered on the stack.
    pub fn new<T: ToString>(token: T) -> Result<Self, &'static str> {
        let content = token.to_string();
        if content.len() > SIZE {
            return Err("Too long to be tokenized");
        }
        let mut chars = ['\0'; SIZE];
        for (i, val) in content.chars().enumerate() {
            chars[i] = val;
        }
        Ok(SizedString {
            size: content.len(),
            contents: chars,
        })
    }
}

impl Display for SizedString {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let as_str: String = self.contents[0..self.size].iter().collect();
        write!(f, "{}", as_str)
    }
}
