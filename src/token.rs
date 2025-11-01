#[derive(Debug, PartialEq)]

pub enum TokenType {
    Illegal,
    Eof,
    Ident,
    Int,

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    // Comparisons
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,

    Comma,
    Semicolon,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,

    // Keywords
    Function,
    Let,
    If,
    Else,
    Return,
    True,
    False,
}

#[derive(Debug, PartialEq)]
pub struct Token<'a> {
    token_type: TokenType,
    literal: &'a str,
}

impl Token<'_> {
    pub fn token_type(&self) -> &TokenType {
        &self.token_type
    }

    pub fn literal(&self) -> &str {
        self.literal
    }

    pub fn from_char(ch: u8) -> Token<'static> {
        match ch {
            b'=' => Token::assign(),
            b'+' => Token::plus(),
            b'-' => Token::minus(),
            b'!' => Token::bang(),
            b'*' => Token::asterisk(),
            b'/' => Token::slash(),
            b'<' => Token::less_than(),
            b'>' => Token::greater_than(),
            b'(' => Token::lparen(),
            b')' => Token::rparen(),
            b'{' => Token::lbrace(),
            b'}' => Token::rbrace(),
            b',' => Token::comma(),
            b';' => Token::semicolon(),
            _ => Token::illegal(),
        }
    }

    pub fn from_keyword(keyword: &str) -> Token {
        match keyword {
            "let" => Token::r#let(),
            "fn" => Token::function(),
            "if" => Token::r#if(),
            "else" => Token::r#else(),
            "return" => Token::r#return(),
            "true" => Token::r#true(),
            "false" => Token::r#false(),
            other => Token {
                token_type: Ident,
                literal: other,
            },
        }
    }

    pub fn from_number(number: &str) -> Token {
        Token {
            token_type: Int,
            literal: number,
        }
    }

    pub fn symbols() -> &'static [u8] {
        b"=+-!*/<>(){},;"
    }
}

macro_rules! implement_create_token {
    ($(($token_type:expr, $token_literal:literal, $name:ident)),* $(,)?) => {
        $(
            impl Token<'static> {
                pub fn $name() -> Token<'static> {
                    Token {
                        token_type: $token_type,
                        literal: $token_literal,
                    }
                }
            }
        )*
    };
}

use TokenType::*;
implement_create_token! {
    (Illegal, "illegal", illegal),
    (Eof, "", eof),

    // Operators
    (Assign, "=", assign),
    (Plus, "+", plus),
    (Minus, "-", minus),
    (Bang, "!", bang),
    (Asterisk, "*", asterisk),
    (Slash, "/", slash),

    // Comparisons
    (LessThan, "<", less_than),
    (GreaterThan, ">", greater_than),
    (Equal, "==", equal),
    (NotEqual, "!=", not_equal),

    (Comma, ",", comma),
    (Semicolon, ";", semicolon),
    (Lparen, "(", lparen),
    (Rparen, ")", rparen),
    (Lbrace, "{", lbrace),
    (Rbrace, "}", rbrace),

    // Keywords
    (Function, "fn", function),
    (Let, "let", r#let),
    (If, "if", r#if),
    (Else, "else", r#else),
    (Return, "return", r#return),
    (True, "true", r#true),
    (False, "false", r#false),
}
