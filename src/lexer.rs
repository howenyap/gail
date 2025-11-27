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
        let position = 0;
        let read_position = 1.min(bytes.len());

        Self {
            input,
            bytes,
            position,
            read_position,
        }
    }

    pub fn next_token(&mut self) -> Token<'a> {
        self.skip_whitespace();

        let Some(ch) = self.curr_char() else {
            return Token::eof();
        };

        match ch {
            b'=' => {
                if self.peek_char() == Some(&b'=') {
                    self.read_char();
                    self.read_char();
                    Token::equal()
                } else {
                    self.read_char();
                    Token::assign()
                }
            }
            b'!' => {
                if self.peek_char() == Some(&b'=') {
                    self.read_char();
                    self.read_char();
                    Token::not_equal()
                } else {
                    self.read_char();
                    Token::bang()
                }
            }
            b'<' => {
                if self.peek_char() == Some(&b'=') {
                    self.read_char();
                    self.read_char();
                    Token::less_than_or_equal()
                } else {
                    self.read_char();
                    Token::less_than()
                }
            }
            b'>' => {
                if self.peek_char() == Some(&b'=') {
                    self.read_char();
                    self.read_char();
                    Token::greater_than_or_equal()
                } else {
                    self.read_char();
                    Token::greater_than()
                }
            }
            b'"' => self.read_string(),
            b'+' | b'-' | b'*' | b'/' | b'(' | b')' | b'{' | b'}' | b'[' | b']' | b',' | b';' => {
                let token = Token::from_char(ch);
                self.read_char();
                token
            }
            ch if ch.is_ascii_alphabetic() => Token::from_keyword(self.read_identifier()),
            ch if ch.is_ascii_digit() => Token::from_number(self.read_number()),
            _ => Token::illegal(),
        }
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

    fn read_string(&mut self) -> Token<'a> {
        let start = self.position;

        loop {
            if self.is_eof() {
                return Token::illegal();
            }

            self.read_char();

            if self.is_quote() {
                self.read_char();
                break;
            }
        }

        Token::from_string(&self.input[start + 1..self.position - 1])
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
        matches!(self.curr_char(), Some(ch) if ch.is_ascii_digit())
    }

    fn is_letter(&self) -> bool {
        matches!(self.curr_char(), Some(ch) if ch.is_ascii_alphabetic() || *ch == b'_')
    }

    fn is_quote(&self) -> bool {
        matches!(self.curr_char(), Some(ch) if *ch == b'"')
    }

    fn is_eof(&self) -> bool {
        self.curr_char().is_none()
    }

    fn skip_whitespace(&mut self) {
        while matches!(self.curr_char(), Some(ch) if ch.is_ascii_whitespace()) {
            self.read_char();
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.next_token();
        if token.is_eof() { None } else { Some(token) }
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
"foobar";
"foo bar";
[1, 2];
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
            (String, "foobar"),
            (Semicolon, ";"),
            (String, "foo bar"),
            (Semicolon, ";"),
            (Lbracket, "["),
            (Int, "1"),
            (Comma, ","),
            (Int, "2"),
            (Rbracket, "]"),
            (Semicolon, ";"),
            (Eof, ""),
        ];

        for (expected_type, expected_literal) in tests.iter() {
            let token = lexer.next_token();
            assert_eq!(expected_type, token.token_type());
            assert_eq!(*expected_literal, token.literal());
        }
    }
}
