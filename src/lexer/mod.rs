use crate::{parser::ast::Const, symbols::Builtin, Either, Interner, Position, Span, Spanned};
use std::{iter::Peekable, str::Chars};

mod token;
pub use token::Token;

pub struct Lexer<'a, 'sym> {
    source: &'a str,
    interner: &'sym mut Interner,
    input: Peekable<Chars<'a>>,
    current: Position,
    pos: usize,
}

impl<'a, 'sym> Lexer<'a, 'sym> {
    pub fn new(source: &'a str, interner: &'sym mut Interner) -> Self {
        Self {
            source,
            interner,
            input: source.chars().peekable(),
            current: Position::zero(),
            pos: 0,
        }
    }

    fn lex(&mut self) -> Option<Spanned<Token>> {
        self.consume_delimiter();
        let pos = self.current;

        let out = match self.peek()? {
            ';' => self.emit(pos, Token::Semi),
            ',' => self.emit(pos, Token::Comma),
            '\'' => self.emit(pos, Token::Apostrophe),
            '_' => self.emit(pos, Token::Wildcard),
            '(' => {
                let alt = self.emit(pos, Token::LParen);
                match self.peek() {
                    Some('*') => return self.eat_comment(),
                    _ => alt,
                }
            }
            ')' => self.emit(pos, Token::RParen),
            '{' => self.emit(pos, Token::LBrace),
            '}' => self.emit(pos, Token::RBrace),
            'λ' => self.emit(pos, Token::Fn),
            '∀' => self.emit(pos, Token::Forall),
            '#' => {
                self.consume();
                match self.peek()? {
                    '"' => return self.lex_char_literal(),
                    _ => Spanned::new(Token::Selector, Span::new(pos, self.current)),
                }
            }
            '"' => return self.lex_string_literal(),
            x if x.is_ascii_alphabetic() => self.lex_keyword(),
            x if x.is_numeric() => return self.lex_number(),
            x if Self::is_valid_symbol(x) => self.lex_symbolic(),
            ch => {
                self.consume();
                Spanned::new(Token::Invalid(ch), Span::new(self.current, self.current))
            }
        };

        Some(out)
    }

    fn emit(&mut self, pos: Position, token: Token) -> Spanned<Token> {
        self.consume().unwrap();
        Spanned::new(token, Span::new(pos, self.current))
    }

    fn lex_symbolic(&mut self) -> Spanned<Token> {
        let (s, span) = self.consume_while(Self::is_valid_symbol);
        let kind = match self.interner.intern(s) {
            Builtin::SYM_ARROW => Token::Arrow,
            Builtin::SYM_FATARROW => Token::FatArrow,
            Builtin::SYM_COLON => Token::Colon,
            Builtin::SYM_PIPE => Token::Pipe,
            Builtin::SYM_EQUAL => Token::Equals,
            Builtin::SYM_DOT => Token::Dot,
            Builtin::SYM_FLEX => Token::Flex,
            s => Token::SymIdent(s),
        };
        Spanned::new(kind, span)
    }

    fn lex_keyword(&mut self) -> Spanned<Token> {
        let (word, span) = self.consume_while(Self::is_valid_ident);
        let kind = match self.interner.intern(word) {
            Builtin::SYM_ABSTYPE => Token::Abstype,
            Builtin::SYM_AND => Token::And,
            Builtin::SYM_ANDALSO => Token::Andalso,
            Builtin::SYM_AS => Token::As,
            Builtin::SYM_CASE => Token::Case,
            Builtin::SYM_DATATYPE => Token::Datatype,
            Builtin::SYM_DO => Token::Do,
            Builtin::SYM_ELSE => Token::Else,
            Builtin::SYM_END => Token::End,
            Builtin::SYM_EXCEPTION => Token::Exception,
            Builtin::SYM_FN => Token::Fn,
            Builtin::SYM_FUN => Token::Fun,
            Builtin::SYM_FUNCTOR => Token::Functor,
            Builtin::SYM_HANDLE => Token::Handle,
            Builtin::SYM_IF => Token::If,
            Builtin::SYM_IN => Token::In,
            Builtin::SYM_INFIX => Token::Infix,
            Builtin::SYM_INFIXR => Token::Infixr,
            Builtin::SYM_LET => Token::Let,
            Builtin::SYM_LOCAL => Token::Local,
            Builtin::SYM_NONFIX => Token::Nonfix,
            Builtin::SYM_OF => Token::Of,
            Builtin::SYM_OP => Token::Op,
            Builtin::SYM_OPEN => Token::Open,
            Builtin::SYM_ORELSE => Token::Orelse,
            Builtin::SYM_PRIMITIVE => Token::Primitive,
            Builtin::SYM_RAISE => Token::Raise,
            Builtin::SYM_REC => Token::Rec,
            Builtin::SYM_THEN => Token::Then,
            Builtin::SYM_TYPE => Token::Type,
            Builtin::SYM_VAL => Token::Val,
            Builtin::SYM_WITH => Token::With,
            Builtin::SYM_WITHTYPE => Token::Withtype,
            Builtin::SYM_WHILE => Token::While,
            Builtin::SYM_SIG => Token::Sig,
            Builtin::SYM_SIGNATURE => Token::Signature,
            Builtin::SYM_STRUCT => Token::Struct,
            Builtin::SYM_STRUCTURE => Token::Structure,
            word => Token::Ident(word),
        };
        Spanned::new(kind, span)
    }

    fn lex_string_literal(&mut self) -> Option<Spanned<Token>> {
        self.consume()?;
        let (str, span) = self.consume_while(|c| c != '"');
        self.consume()?;
        let kind = Token::Const(Const::String(self.interner.intern(str)));
        Some(Spanned::new(kind, span))
    }

    fn lex_char_literal(&mut self) -> Option<Spanned<Token>> {
        let pos = match self.consume_expect(|c| c == '"')? {
            Either::Left(pos) => pos,
            Either::Right(err) => return Some(err),
        };
        let ch = self.consume()?;

        let out = match self.consume_expect(|c| c == '"')? {
            Either::Left(..) => {
                let kind = Token::Const(Const::Char(ch));
                let span = Span::new(pos, self.current);
                Spanned::new(kind, span)
            }
            Either::Right(err) => err,
        };
        Some(out)
    }

    fn lex_number(&mut self) -> Option<Spanned<Token>> {
        let (data, span) = self.consume_while(char::is_numeric);
        data.parse()
            .ok()
            .map(Const::Int)
            .map(Token::Const)
            .map(|kind| Spanned::new(kind, span))
    }

    fn eat_comment(&mut self) -> Option<Spanned<Token>> {
        self.consume_while(|c| c != '*');
        self.consume()?;
        if let Some(')') = self.peek() {
            self.consume();
            return self.lex();
        }
        self.eat_comment()
    }

    fn peek(&mut self) -> Option<char> {
        self.input.peek().copied()
    }

    fn consume(&mut self) -> Option<char> {
        match self.input.next()? {
            '\n' => {
                self.current.line += 1;
                self.current.col = 0;
                self.pos += 1;
                Some('\n')
            }
            ch => {
                self.current.col += 1;
                self.pos += 1;
                Some(ch)
            }
        }
    }

    fn consume_while<F: Fn(char) -> bool>(&mut self, f: F) -> (&'a str, Span) {
        let pos = self.pos;
        let start = self.current;
        while let Some(c) = self.peek() {
            if !f(c) {
                break;
            }

            if self.consume().is_none() {
                break;
            }
        }
        (&self.source[pos..self.pos], Span::new(start, self.current))
    }

    fn consume_delimiter(&mut self) {
        let _ = self.consume_while(char::is_whitespace);
    }

    fn consume_expect<F: Fn(char) -> bool>(
        &mut self,
        f: F,
    ) -> Option<Either<Position, Spanned<Token>>> {
        let start = self.current;
        let out = match self.consume()? {
            c if f(c) => Either::Left(start),
            c => Either::Right(Spanned::new(
                Token::Invalid(c),
                Span::new(start, self.current),
            )),
        };
        Some(out)
    }

    fn is_valid_symbol(ch: char) -> bool {
        const VALID: &str = "!%&$#+-/:<=>?@~`^|\\.";
        VALID.contains(ch)
    }

    fn is_valid_ident(ch: char) -> bool {
        match ch {
            x if x.is_ascii_alphabetic() || x == '_' || x == '\'' => true,
            _ => false,
        }
    }
}

impl<'a, 'sym> Iterator for Lexer<'a, 'sym> {
    type Item = Spanned<Token>;
    fn next(&mut self) -> Option<Self::Item> {
        self.lex()
    }
}
