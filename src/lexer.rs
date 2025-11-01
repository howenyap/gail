use crate::token::Token;

pub struct Lexer<'a> {
    input: &'a str,
    bytes: &'a [u8],
    position: usize,
    read_position: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let bytes = input.as_bytes();
        Self {
            input,
            bytes,
            position: 0,
            read_position: 1.min(bytes.len()),
        }
    }

    pub fn next_token(&mut self) -> Token<'a> {
        self.skip_whitespace();

        if self.is_eof() {
            return Token::eof();
        }

        if self.is_symbol() {
            match (self.curr_char(), self.peek_char()) {
                (Some(b'='), Some(b'=')) => {
                    self.read_char();
                    self.read_char();
                    return Token::equal();
                }
                (Some(b'!'), Some(b'=')) => {
                    self.read_char();
                    self.read_char();
                    return Token::not_equal();
                }
                (Some(ch), _) => {
                    let token = Token::from_char(*ch);
                    self.read_char();
                    return token;
                }
                _ => {}
            }
        }

        if self.is_letter() {
            return Token::from_keyword(self.read_identifier());
        }

        if self.is_digit() {
            return Token::from_number(self.read_number());
        }

        Token::illegal()
    }

    fn read_char(&mut self) -> Option<&u8> {
        if let Some(ch) = self.bytes.get(self.position) {
            self.position = self.read_position;
            self.read_position += 1;
            Some(ch)
        } else {
            None
        }
    }

    fn curr_char(&self) -> Option<&u8> {
        self.bytes.get(self.position)
    }

    fn peek_char(&self) -> Option<&u8> {
        self.bytes.get(self.read_position)
    }

    fn read_identifier(&mut self) -> &'a str {
        let start = self.position;

        while self.is_letter() {
            self.read_char();
        }

        &self.input[start..self.position]
    }

    fn read_number(&mut self) -> &'a str {
        let start = self.position;

        while self.is_digit() {
            self.read_char();
        }

        &self.input[start..self.position]
    }

    fn is_digit(&self) -> bool {
        self.curr_char()
            .map(|ch| ch.is_ascii_digit())
            .unwrap_or(false)
    }

    fn is_symbol(&self) -> bool {
        self.curr_char()
            .map(|ch| Token::symbols().contains(ch))
            .unwrap_or(false)
    }

    fn is_letter(&self) -> bool {
        self.curr_char()
            .map(|ch| ch.is_ascii_alphabetic() || *ch == b'_')
            .unwrap_or(false)
    }

    fn is_eof(&self) -> bool {
        self.curr_char().is_none()
    }

    fn skip_whitespace(&mut self) {
        while self
            .curr_char()
            .map(|ch| ch.is_ascii_whitespace())
            .unwrap_or(false)
        {
            self.read_char();
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.next_token();
        if token.token_type() == &crate::token::TokenType::Eof {
            None
        } else {
            Some(token)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::TokenType::*;

    #[test]
    fn test_next_token() {
        let input = r#"let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);

!-/*5;

5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}

10 == 10;
10 != 9;
"#;

        let mut lexer = Lexer::new(input);
        let tests = vec![
            (Let, "let"),
            (Ident, "five"),
            (Assign, "="),
            (Int, "5"),
            (Semicolon, ";"),
            (Let, "let"),
            (Ident, "ten"),
            (Assign, "="),
            (Int, "10"),
            (Semicolon, ";"),
            (Let, "let"),
            (Ident, "add"),
            (Assign, "="),
            (Function, "fn"),
            (Lparen, "("),
            (Ident, "x"),
            (Comma, ","),
            (Ident, "y"),
            (Rparen, ")"),
            (Lbrace, "{"),
            (Ident, "x"),
            (Plus, "+"),
            (Ident, "y"),
            (Semicolon, ";"),
            (Rbrace, "}"),
            (Semicolon, ";"),
            (Let, "let"),
            (Ident, "result"),
            (Assign, "="),
            (Ident, "add"),
            (Lparen, "("),
            (Ident, "five"),
            (Comma, ","),
            (Ident, "ten"),
            (Rparen, ")"),
            (Semicolon, ";"),
            (Bang, "!"),
            (Minus, "-"),
            (Slash, "/"),
            (Asterisk, "*"),
            (Int, "5"),
            (Semicolon, ";"),
            (Int, "5"),
            (LessThan, "<"),
            (Int, "10"),
            (GreaterThan, ">"),
            (Int, "5"),
            (Semicolon, ";"),
            (If, "if"),
            (Lparen, "("),
            (Int, "5"),
            (LessThan, "<"),
            (Int, "10"),
            (Rparen, ")"),
            (Lbrace, "{"),
            (Return, "return"),
            (True, "true"),
            (Semicolon, ";"),
            (Rbrace, "}"),
            (Else, "else"),
            (Lbrace, "{"),
            (Return, "return"),
            (False, "false"),
            (Semicolon, ";"),
            (Rbrace, "}"),
            (Int, "10"),
            (Equal, "=="),
            (Int, "10"),
            (Semicolon, ";"),
            (Int, "10"),
            (NotEqual, "!="),
            (Int, "9"),
            (Semicolon, ";"),
            (Eof, ""),
        ];

        for (i, (expected_type, expected_literal)) in tests.iter().enumerate() {
            let token = lexer.next_token();
            eprintln!("token {}: {:?}", i, token);
            assert_eq!(token.token_type(), expected_type);
            assert_eq!(token.literal(), *expected_literal);
        }
    }
}
