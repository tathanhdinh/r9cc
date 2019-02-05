use crate::{preprocess, CharacterType, TokenType};
// use crate::CharacterType;
// use crate::TokenType;

use std::{
    collections::HashMap,
    fmt::Display,
    fs::File,
    io::{BufReader, Read},
    path::{self, Path},
    // rc::Rc,
};

// pub fn tokenize(path: String, ctx: &mut preprocess::Preprocessor) -> Vec<Token> {
pub fn tokenize<'a, T: AsRef<Path>>(
    path: T,
    ctx: &mut preprocess::Preprocessor<T>,
) -> Vec<Token<'a, T>> {
    let mut tokenizer = Tokenizer::new(path);
    // tokenizer.canonicalize_newline();
    // tokenizer.remove_backslash_newline();
    // tokenizer.scan(&keyword_map());
    // tokenizer.scan(&KEYWORD_MAP);

    tokenizer.tokens = preprocess::preprocess(tokenizer.tokens, ctx);
    tokenizer.strip_newlines_tokens();
    tokenizer.join_string_literals();
    tokenizer.tokens
}

lazy_static! {
    static ref KEYWORD_MAP: HashMap<&'static str, TokenType> = {
        let mut map = HashMap::new();
        map.insert("_Alignof", TokenType::Alignof);
        map.insert("break", TokenType::Break);
        map.insert("char", TokenType::Char);
        map.insert("void", TokenType::Void);
        map.insert("do", TokenType::Do);
        map.insert("else", TokenType::Else);
        map.insert("extern", TokenType::Extern);
        map.insert("for", TokenType::For);
        map.insert("if", TokenType::If);
        map.insert("int", TokenType::Int);
        map.insert("return", TokenType::Return);
        map.insert("sizeof", TokenType::Sizeof);
        map.insert("struct", TokenType::Struct);
        map.insert("typedef", TokenType::Typedef);
        map.insert("while", TokenType::While);
        map
    };
}

// fn keyword_map() -> HashMap<&'static str, TokenType> {
//     let mut map = HashMap::new();
//     map.insert("_Alignof", TokenType::Alignof);
//     map.insert("break", TokenType::Break);
//     map.insert("char", TokenType::Char);
//     map.insert("void", TokenType::Void);
//     map.insert("do", TokenType::Do);
//     map.insert("else", TokenType::Else);
//     map.insert("extern", TokenType::Extern);
//     map.insert("for", TokenType::For);
//     map.insert("if", TokenType::If);
//     map.insert("int", TokenType::Int);
//     map.insert("return", TokenType::Return);
//     map.insert("sizeof", TokenType::Sizeof);
//     map.insert("struct", TokenType::Struct);
//     map.insert("typedef", TokenType::Typedef);
//     map.insert("while", TokenType::While);
//     map
// }

#[derive(Debug, Clone)]
pub struct Token<'a, T> {
    pub ty: TokenType, // Token type

    // For preprocessor
    pub stringize: bool,

    // For error reporting
    // pub buf: Rc<Vec<char>>,
    pub buf: &'a [char],
    // pub filename: Rc<String>,
    pub filename: T,
    pub start: usize,
    pub end: usize,
}

// impl<T: AsRef<Path>> Default for Token<T> {
//     fn default() -> Self {
//         Token {
//             ty: TokenType::Int,
//             buf: Rc::new(vec![]),
//             // filename: Rc::new("".to_string()),
//             filename: Default::default(),
//             start: 0,
//             end: 0,
//             stringize: false,
//         }
//     }
// }

impl<'a, T: Display> Token<'a, T> {
    // pub fn new(ty: TokenType, start: usize, filename: Rc<String>, buf: Rc<Vec<char>>) -> Self {
    pub fn new(ty: TokenType, start: usize, filename: T, buf: &'a [char]) -> Self {
        Token {
            ty,
            stringize: false,
            buf,
            filename,
            start,
            end: start,
            // ..Default::default()
        }
    }

    pub fn bad_token(&self, msg: &str) -> ! {
        print_line(&*self.buf, self.filename, self.start);
        panic!("{}", msg);
    }

    pub fn tokstr(&self) -> String {
        self.buf[self.start..self.end].iter().collect()
    }

    pub fn get_line_number(&self) -> usize {
        self.buf[..self.end].iter().filter(|c| *c == &'\n').count()
    }

    pub fn is_ident(&self, s: &str) -> bool {
        match self.ty {
            TokenType::Ident(ref name) => name == s,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
struct Symbol {
    name: &'static str,
    ty: TokenType,
}

impl Symbol {
    fn new(name: &'static str, ty: TokenType) -> Self {
        Symbol { name, ty }
    }
}

lazy_static! {
    static ref SYMBOLS: Vec<Symbol> = [
        Symbol::new("<<=", TokenType::ShlEQ),
        Symbol::new(">>=", TokenType::ShrEQ),
        Symbol::new("!=", TokenType::NE),
        Symbol::new("&&", TokenType::Logand),
        Symbol::new("++", TokenType::Inc),
        Symbol::new("--", TokenType::Dec),
        Symbol::new("->", TokenType::Arrow),
        Symbol::new("<<", TokenType::SHL),
        Symbol::new("<=", TokenType::LE),
        Symbol::new("==", TokenType::EQ),
        Symbol::new(">=", TokenType::GE),
        Symbol::new(">>", TokenType::SHR),
        Symbol::new("||", TokenType::Logor),
        Symbol::new("*=", TokenType::MulEQ),
        Symbol::new("/=", TokenType::DivEQ),
        Symbol::new("%=", TokenType::ModEQ),
        Symbol::new("+=", TokenType::AddEQ),
        Symbol::new("-=", TokenType::SubEQ),
        Symbol::new("&=", TokenType::BitandEQ),
        Symbol::new("^=", TokenType::XorEQ),
        Symbol::new("|=", TokenType::BitorEQ),
    ]
    .to_vec();
}

// Tokenizer
struct Tokenizer<'a, T: AsRef<Path>> {
    // p: Rc<Vec<char>>,
    p: Box<[char]>,
    // p: &'a [char],
    pos: usize,
    tokens: Vec<Token<'a, path::Display<'a>>>,

    // Error reporting
    // filename: Option<T>,
    filename: T,
}

impl<'a, T: AsRef<Path>> Tokenizer<'a, T> {
    // fn new(filename: Rc<String>) -> Self {
    fn new(filename: T) -> Self {
        let read_chars = {
            let reader = BufReader::new(File::open(filename).expect("Unable to open file"));
            let mut buffer = String::new();
            reader
                .read_to_string(&mut buffer)
                .expect("Invalid character detected");
            let lines = buffer.lines().map(|s| s.to_owned()).collect::<Vec<_>>();
            lines
                .join("\n")
                .chars()
                .collect::<Vec<char>>()
                .into_boxed_slice()
            // buffer.split(|p| p == "\r\n")
            // buffer.chars().collect::<Vec<char>>().into_boxed_slice()
            // Self::read(reader)
        };

        // canonicalize newline

        Tokenizer {
            p: read_chars,
            filename,
            pos: 0,
            // tokens: Vec::<Token<'a, path::Display<'a>>>::new(),
            tokens: vec![],
        }

        // Tokenizer {
        //     p: Rc::new(Self::read_file(filename).chars().collect()),
        //     filename,
        //     pos: 0,
        //     tokens: vec![],
        // }
    }

    // fn read(input: impl Read) -> Box<[char]> {
    //     let mut buffer = String::new();
    //     input.read_to_string(&mut buffer);
    //     buffer.chars().collect::<Vec<char>>().into_boxed_slice()

    //     // let mut fp = io::stdin();
    //     // // if filename != &"-".to_string() {
    //     // if filename != Path::new("-") {
    //     //     let mut fp = File::open(filename).expect("file not found");
    //     //     fp.read_to_string(&mut input)
    //     //         .expect("something went wrong reading the file");
    //     //     return input;
    //     // }
    //     // fp.read_to_string(&mut input)
    //     //     .expect("something went wrong reading the file");
    //     // input
    // }

    fn new_token(&'a self, ty: TokenType) -> Token<'a, path::Display> {
        // Token::new(ty, self.pos, self.filename, self.p.clone())
        Token::new(ty, self.pos, self.filename.as_ref().display(), &self.p)
    }

    // This does not support non-ASCII characters.
    fn get_character(&self, advance_from_pos: usize) -> Option<CharacterType> {
        self.p.get(self.pos + advance_from_pos).map(|ch| {
            if ch == &'\n' {
                CharacterType::NewLine
            } else if ch == &' ' || ch == &'\t' {
                CharacterType::Whitespace
            } else if ch.is_alphabetic() || ch == &'_' {
                CharacterType::Alphabetic
            } else if ch.is_ascii_digit() {
                CharacterType::Digit
            } else {
                CharacterType::NonAlphabetic(*ch)
            }
        })
    }

    fn scan(
        &'a mut self,
        keywords: &HashMap<&'static str, TokenType>,
    ) -> Vec<Token<'a, path::Display>> {
        'outer: while let Some(head_char) = self.get_character(0) {
            match head_char {
                CharacterType::NewLine => {
                    let mut t = self.new_token(TokenType::NewLine);
                    self.pos += 1;
                    t.end = self.pos;
                    self.tokens.push(t);
                }
                CharacterType::Whitespace => self.pos += 1,
                CharacterType::Alphabetic => self.ident(&keywords),
                CharacterType::Digit => self.number(),

                CharacterType::NonAlphabetic('\'') => self.char_literal(),
                CharacterType::NonAlphabetic('\"') => self.string_literal(),
                CharacterType::NonAlphabetic('/') => match self.p.get(self.pos + 1) {
                    Some('/') => self.line_comment(),
                    Some('*') => self.block_comment(),
                    Some('=') => {
                        let mut t = self.new_token(TokenType::DivEQ);
                        self.pos += 2;
                        t.end = self.pos;
                        self.tokens.push(t);
                    }
                    // This is Dividing operator
                    _ => {
                        let mut t = self.new_token(TokenType::Div);
                        self.pos += 1;
                        t.end = self.pos;
                        self.tokens.push(t);
                    }
                },
                CharacterType::NonAlphabetic(c) => {
                    // Multi-letter symbol
                    for symbol in SYMBOLS.iter() {
                        let name = symbol.name;
                        let len = name.len();
                        if self.pos + len > self.p.len() {
                            continue;
                        }

                        let first = &self.p[self.pos..self.pos + len];
                        if name != first.iter().collect::<String>() {
                            continue;
                        }

                        let mut t = self.new_token(symbol.ty.clone());
                        self.pos += len;
                        t.end = self.pos;
                        self.tokens.push(t);
                        continue 'outer;
                    }

                    // Single-letter symbol
                    if let Some(ty) = TokenType::new_single_letter(c) {
                        let mut t = self.new_token(ty);
                        self.pos += 1;
                        t.end = self.pos;
                        self.tokens.push(t);
                        continue 'outer;
                    }
                    self.bad_position("Unknown symbol.");
                }
                CharacterType::Unknown(_) => self.bad_position("Unknwon character type."),
            }
        }

        // self.tokens.clone()
        self.tokens
    }

    fn line_comment(&mut self) {
        while self.p.get(self.pos) != Some(&'\n') {
            self.pos += 1;
        }
    }

    fn block_comment(&mut self) {
        self.pos += 2;
        loop {
            if let Some(two_char) = self.p.get(self.pos..self.pos + 2) {
                self.pos += 1;
                if two_char == ['*', '/'] {
                    self.pos += 1;
                    return;
                }
            } else {
                self.bad_position("unclosed comment");
            }
        }
    }

    fn escaped(c: char) -> Option<char> {
        // Issue: https://github.com/rust-lang/rfcs/issues/751
        match c {
            // 'a' => Some('\a'),
            // 'b' => Some('\b'),
            // 'f' => Some('\f'),
            'n' => Some('\n'),
            'r' => Some('\r'),
            't' => Some('\t'),
            // 'v' => Some('\v'),
            _ => None,
        }
    }

    fn char_literal(&'a mut self) {
        self.pos += 1;
        let result: char;
        let c = self.p.get(self.pos).expect("premature end of input");
        if c != &'\\' {
            result = *c;
            self.pos += 1;
        } else {
            self.pos += 1;
            let c2 = self.p.get(self.pos).unwrap();
            result = if let Some(esc) = Self::escaped(*c2) {
                esc
            } else {
                *c2
            };
            self.pos += 1;
        }

        if self.p.get(self.pos) != Some(&'\'') {
            panic!("unclosed character literal");
        }

        let mut t = self.new_token(TokenType::Num(result as u8 as i32));
        self.pos += 1;
        t.end = self.pos + 1;
        self.tokens.push(t);
    }

    fn string_literal(&'a mut self) {
        self.pos += 1;
        let mut sb = String::new();
        let mut len = 0;
        loop {
            let mut c2 = self.p.get(self.pos + len).expect("PREMATURE end of input");
            if c2 == &'"' {
                len += 1;
                self.pos += len;
                let mut t = self.new_token(TokenType::Str(sb, len));
                t.start = self.pos - len - 1;
                t.end = self.pos + 1;
                self.tokens.push(t);
                return;
            }

            if c2 != &'\\' {
                len += 1;
                sb.push(c2.clone());
                continue;
            }

            len += 1;
            c2 = self.p.get(self.pos + len).unwrap();
            if let Some(esc) = Self::escaped(*c2) {
                sb.push(esc);
            } else {
                sb.push(c2.clone());
            }
            len += 1;
        }
    }

    fn ident(&'a mut self, keywords: &HashMap<&'static str, TokenType>) {
        let mut len = 1;
        while let Some(c2) = self.p.get(self.pos + len) {
            if c2.is_alphabetic() || c2.is_ascii_digit() || c2 == &'_' {
                len += 1;
                continue;
            }
            break;
        }

        let name: String = self.p[self.pos..self.pos + len].iter().collect();
        let mut t;
        if let Some(keyword) = keywords.get(name.as_str()) {
            t = self.new_token(keyword.clone());
        } else {
            t = self.new_token(TokenType::Ident(name.clone()));
        }
        self.pos += len;
        t.end = self.pos;
        self.tokens.push(t);
    }

    fn number(&'a mut self) {
        match self.p.get(self.pos..self.pos + 2) {
            Some(&['0', 'x']) | Some(&['0', 'X']) => {
                self.pos += 2;
                self.parse_number(16);
            }
            Some(&['0', _]) => {
                self.parse_number(8);
            }
            _ => self.parse_number(10),
        }
    }

    fn parse_number(&'a mut self, base: u32) {
        let mut sum: i32 = 0;
        let mut len = 0;
        for c in self.p[self.pos..].iter() {
            if let Some(val) = c.to_digit(base) {
                sum = sum * base as i32 + val as i32;
                len += 1;
            } else {
                break;
            }
        }
        let mut t = self.new_token(TokenType::Num(sum as i32));
        self.pos += len;
        t.end = self.pos;
        self.tokens.push(t);
    }

    // fn canonicalize_newline(&mut self) {
    //     let mut pos = 0;
    //     while pos < self.p.len() {
    //         if self.p[pos] == '\r' && self.p[pos + 1] == '\n' {
    //             Rc::get_mut(&mut self.p).unwrap().remove(pos);
    //             Rc::get_mut(&mut self.p).unwrap().remove(pos);
    //         }
    //         pos += 1;
    //     }
    // }

    // Quoted from 9cc
    // > Concatenates continuation lines. We keep the total number of
    // > newline characters the same to keep the line counter sane.
    // fn remove_backslash_newline(&mut self) {
    //     let mut pos = 0;
    //     let mut cnt = 0;
    //     while pos < self.p.len() {
    //         if self.p[pos] == '\\' && self.p[pos + 1] == '\n' {
    //             cnt += 1;
    //             Rc::get_mut(&mut self.p).unwrap().remove(pos);
    //             Rc::get_mut(&mut self.p).unwrap().remove(pos);
    //             pos += 1;
    //         } else if self.p[pos] == '\n' {
    //             for _ in 0..cnt {
    //                 Rc::get_mut(&mut self.p).unwrap().insert(pos, '\n');
    //                 pos += 1;
    //             }
    //             pos += 1;
    //             cnt = 0;
    //         } else {
    //             pos += 1;
    //         }
    //     }
    // }

    fn append(&'a mut self, x_str: &str, y_str: &str, start: usize) -> Token<'a, path::Display> {
        let concated = format!("{}{}", x_str, y_str);
        let l = concated.len() + 1; // Because `+1` has `\0`.
        Token::new(
            TokenType::Str(concated, l),
            start,
            // self.filename.clone(),
            self.filename.as_ref().display(),
            // self.p.clone(),
            &self.p,
        )
    }

    fn join_string_literals(&'a mut self) {
        let mut v = vec![];
        let mut last_may: Option<Token<'a, path::Display>> = None;

        // for t in self.tokens.clone().into_iter() {
        for t in self.tokens.into_iter() {
            if let Some(ref last) = last_may {
                if let (TokenType::Str(ref last_str, _), TokenType::Str(ref t_str, _)) =
                    (&last.ty, &t.ty)
                {
                    let new = self.append(last_str, t_str, last.start);
                    v.pop();
                    v.push(new);
                    continue;
                }
            }

            // last_may = Some(t.clone());
            last_may = Some(t);
            v.push(t);
        }
        self.tokens = v;
    }

    fn strip_newlines_tokens(&mut self) {
        self.tokens = self
            .tokens
            // .clone()
            .into_iter()
            .filter(|t| t.ty != TokenType::NewLine)
            .collect()
    }

    fn bad_position(&self, msg: &'static str) {
        // print_line(&self.p, &self.filename, self.pos);
        print_line(&self.p, self.filename.as_ref().display(), self.pos);
        panic!(msg);
    }
}

// Finds a line pointed by a given pointer from the input file
// to print it out.
fn print_line<T: Display>(buf: &[char], path: T, pos: usize) {
    let mut p = 0;
    let mut start = 0;
    let mut line = 0;
    let mut col = 0;
    for c in buf.iter() {
        if c == &'\n' {
            start = pos + 1;
            line += 1;
            col = 0;
            p += 1;
            continue;
        }

        if p != pos {
            col += 1;
            p += 1;
            continue;
        }

        print!("error at {}:{}:{}\n\n", path, line + 1, col);
        break;
    }

    for p in buf[start..].iter() {
        if p == &'\n' {
            break;
        }
        print!("{}", p);
    }
    println!();
    for _ in 0..col - 1 {
        print!(" ");
    }
    print!("^\n\n");
}
