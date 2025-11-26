use std::fmt::{self, Display};

use crate::ast::Precedence;
#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash)]

pub enum TokenType {
    Illegal,
    Eof,
    Ident,
    Int,
    String,

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

    // Delimiters
    Comma,
    Semicolon,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Lbracket,
    Rbracket,

    // Keywords
    Function,
    Let,
    If,
    Else,
    Return,
    True,
    False,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash)]
pub struct Token<'a> {
    pub token_type: TokenType,
    pub literal: &'a str,
}

impl Token<'_> {
    pub fn token_type(&self) -> &TokenType {
        &self.token_type
    }

    pub fn literal(&self) -> &str {
        self.literal
    }

    pub fn from_char(ch: &u8) -> Token<'static> {
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
            b'[' => Token::lbracket(),
            b']' => Token::rbracket(),
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

    pub fn from_string(string: &str) -> Token {
        Token {
            token_type: String,
            literal: string,
        }
    }

    pub fn is_eof(&self) -> bool {
        self.token_type == Eof
    }

    pub fn precedence(&self) -> Precedence {
        match self.token_type {
            TokenType::Equal | TokenType::NotEqual => Precedence::Equals,
            TokenType::LessThan | TokenType::GreaterThan => Precedence::LessGreater,
            TokenType::Plus | TokenType::Minus => Precedence::Sum,
            TokenType::Asterisk | TokenType::Slash => Precedence::Product,
            TokenType::Lparen => Precedence::Call,
            TokenType::Lbracket => Precedence::Index,
            _ => Precedence::Lowest,
        }
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

    // Delimiters
    (Comma, ",", comma),
    (Semicolon, ";", semicolon),
    (Lparen, "(", lparen),
    (Rparen, ")", rparen),
    (Lbrace, "{", lbrace),
    (Rbrace, "}", rbrace),
    (Lbracket, "[", lbracket),
    (Rbracket, "]", rbracket),

    // Keywords
    (Function, "fn", function),
    (Let, "let", r#let),
    (If, "if", r#if),
    (Else, "else", r#else),
    (Return, "return", r#return),
    (True, "true", r#true),
    (False, "false", r#false),
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.literal())
    }
}
