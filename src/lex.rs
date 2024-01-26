use std::fmt;
use std::str;

pub enum Char<'a> {
    Set(&'a[u8]),
    Range(u8, u8),
    Alt(&'a[Char<'a>]),
    NoneOf(&'a[u8]),
    Any
}

pub const QUOTE_CHAR: Char = Char::Set(&[b'"']);
pub const LATIN_SMALL: Char = Char::Range(b'a', b'z');
pub const LATIN_CAPITAL: Char = Char::Range(b'A', b'Z');
pub const LATIN: Char = Char::Alt(&[LATIN_SMALL, LATIN_CAPITAL]);
pub const DIGIT: Char = Char::Range(b'0', b'9');
pub const UNIX_NAME_CHAR: Char = Char::Alt(&[LATIN_SMALL, LATIN_CAPITAL, DIGIT, Char::Set(&[b'_'])]);

impl<'a> Char<'a> {
    pub fn test(&self, byte: u8) -> bool {
        match self {
            Self::Set(opts) => opts.iter().any(|c| *c == byte),
            Self::Range(a, b) => (*a..*b+1).any(|c| c == byte),
            Self::Alt(opts) => opts.iter().any(|cd| cd.test(byte)),
            Self::NoneOf(opts) => !opts.iter().any(|c| *c == byte),
            Self::Any => true
        }
    }

    pub fn test_opt(&self, byte: u8) -> Option<()> {
        if self.test(byte) { Some(()) }
        else { None }
    }
}

#[test]
fn chardef_matches_one_byte() {
    assert_eq!(LATIN_SMALL.test(b'a'), true);
    assert_eq!(LATIN_SMALL.test(b'c'), true);
    assert_eq!(LATIN_SMALL.test(b'z'), true);
    assert_eq!(LATIN_CAPITAL.test(b'Z'), true);
    assert_eq!(LATIN_CAPITAL.test(b'C'), true);
    assert_eq!(LATIN_CAPITAL.test(b'a'), false);
    assert_eq!(LATIN_SMALL.test(b'_'), false);
    assert_eq!(LATIN_SMALL.test(b'A'), false);
    assert_eq!(LATIN.test(b'Z'), true);
    assert_eq!(LATIN.test(b'C'), true);
    assert_eq!(LATIN.test(b'a'), true);
    assert_eq!(UNIX_NAME_CHAR.test(b'_'), true);
    assert_eq!(Char::Any.test(b'_'), true);
    assert_eq!(Char::Any.test(b'Q'), true);
    assert_eq!(Char::Any.test(b'v'), true);
    assert_eq!(Char::Any.test(11), true);
}

pub enum Seq<'a> {
    One(Char<'a>),
    Many(&'a Seq<'a>),
    Any(&'a Seq<'a>),
    Or(&'a [Seq<'a>]),
    Composition(&'a[Seq<'a>])
}

#[derive(PartialEq, Debug)]
pub struct Substr<'a> {
    str: &'a [u8],
    start: usize,
    end: usize,
    line: usize
}

impl <'a> Substr<'a> {
    pub fn start(str: &'a [u8], len: usize) -> Self {
        Substr { str, start: 0, end: len, 
                 line: str[0..len].iter()
                                   .filter(|c| **c == b'\n')
                                   .count() }
    }

    pub fn head(&self) -> &'a [u8] { &self.str[self.start..self.end] }
    pub fn tail(&self) -> &'a [u8] { &self.str[self.end..] }

    pub fn token(&self, typ: &'a Seq) -> Token<'a> {
        Token { src: self.head(), line: self.line + 1, typ }
    }

    pub fn then<F>(&self, f:F) -> Option<Self>
            where F: Fn(&'a [u8]) -> Option<Substr<'a>> {
        f(self.tail())
            .map(|ans| Substr { str: self.str, 
                                start: self.start, 
                                end: self.end + ans.end,
                                line: self.line + ans.line }) 
    }
}

impl <'a> Seq<'a> {
    fn scan(&self, rawstr: &'a [u8]) -> Option<Substr<'a>> {
        match self {
            Self::One(cd) => rawstr.get(0)
                                   .and_then(|c| cd.test_opt(*c)
                                                   .map(|_| Substr::start(rawstr, 1))),

            Self::Many(seq) => {
                let mut acc = seq.scan(rawstr);
                match acc.as_mut() {
                    Some(x) => {
                        loop { 
                            match x.then(|x| seq.scan(x)) {
                                Some(y) => { *x = y },
                                None => { break; }
                            }
                        }
                    },
                    None => { }
                }

                acc
            },

            Self::Any(seq) => {
                let mut acc = seq.scan(rawstr);
                match acc.as_mut() {
                    Some(x) => {
                        loop { 
                            match x.then(|x| seq.scan(x)) {
                                Some(y) => { *x = y },
                                None => { break; }
                            }
                        }
                    },
                    None => {return Some(Substr::start(rawstr, 0))}
                }

                acc
            },

            Self::Or(opts) => {
                for opt in opts.iter() {
                    let ans = opt.scan(rawstr);
                    if ans.is_some() {
                        return ans;
                    }
                }

                None
            },

            Self::Composition(parts) => {
                parts.iter()
                     .fold(Some(Substr::start(rawstr, 0)),
                           |acc: Option<Substr>, part| acc.and_then(|x| x.then(|x| part.scan(x))))
            }
        }
    }
}

pub const LATIN_WORD: Seq = Seq::Many(&Seq::One(LATIN));
pub static UNIX_WORD: Seq = Seq::Many(&Seq::One(UNIX_NAME_CHAR));
pub const ESC: Seq = Seq::Composition(&[Seq::One(Char::Set(&[b'\\'])),
                                      Seq::One(Char::Any)]);

static DOUBLE_QUOTE: Seq = Seq::Composition(&[Seq::One(QUOTE_CHAR),
                                                 Seq::Any(&Seq::Or(&[ESC, Seq::One(Char::NoneOf(&[b'"']))])),
                                                 Seq::One(QUOTE_CHAR)]);

#[test]
fn seq_matches () {
    let a = b"foobar";
    assert_eq!(Seq::One(LATIN).scan(a), Some(Substr::start(a, 1)));
    assert_eq!(Seq::One(LATIN).scan(b"_foobar"), None);

    assert_eq!(LATIN_WORD.scan(a), Some(Substr::start(a, 6)));

    let b = b"foo_bar99";
    assert_eq!(UNIX_WORD.scan(b), Some(Substr::start(b, 9)));

    let c = "\"你好\" chacha".as_bytes();
    assert_eq!(DOUBLE_QUOTE.scan(c), Some(Substr::start(c, 8)));
    
    let d = b"\"\" chacha";
    assert_eq!(DOUBLE_QUOTE.scan(d), Some(Substr::start(d, 2)));

    assert_eq!(Seq::One(LATIN).scan(c), None);

    let e = b"\"foo \\\"bar\\\" \" kan";
    assert_eq!(DOUBLE_QUOTE.scan(e), Some(Substr::start(e, 14)));
}

static SPACE: Seq = Seq::Many(&Seq::One(Char::Set(&[b' ', b'\t', b'\n'])));

pub struct Token<'a> {
    src: &'a [u8],
    typ: &'a Seq<'a>,
    line: usize
}

fn ptr_eq<T>(a: &T, b: &T) -> bool {
    a as *const T == b as *const T
}

impl<'a> PartialEq for Token<'a> {
    fn eq (&self, other: &Token<'a>) -> bool {
        self.src == other.src && ptr_eq(self.typ, other.typ)
    }
}

impl<'a> fmt::Debug for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}<{}>", self.typ as *const Seq<'a> as usize, str::from_utf8(self.src).unwrap())
    }
}

pub struct Lexer<'a> {
    opts: &'a [&'a Seq<'a>]
}

impl<'a> Lexer<'a> {
    fn new(opts: &'a [&'a Seq<'a>]) -> Self { Lexer { opts } }

    fn scan(&'a self, rawstr: &'a [u8]) -> Lex<'a> {
        Lex::start(self, rawstr)
    }
}

#[derive(PartialEq,Debug,Clone,Copy)]
pub enum LexState<'a> {
    Ok,
    Eof,
    InvalidChar(&'a [u8])
}

pub struct Lex<'a> {
    lex: &'a Lexer<'a>,
    cursor: Substr<'a>,
    state: LexState<'a>
}

impl<'a> Lex <'a> {
    fn start(lex: &'a Lexer, rawstr: &'a [u8]) -> Self {
        Lex { lex, cursor: Substr::start(rawstr, 0), 
                 state: LexState::Ok }
    }

    fn state(&self) -> LexState<'a> {
        self.state
    }
}

impl <'a> Iterator for Lex<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let tail = self.cursor.tail();
        if tail.len() == 0 {
            self.state = LexState::Eof;
            return None;
        }

        for opt in self.lex.opts {
            if let Some(m) = opt.scan(tail) {
                self.cursor = m;
                return Some(self.cursor.token(opt));
            }
        }

        self.state = LexState::InvalidChar(
            if tail.len() > 10 { &tail[0..10] } else { tail }
        );
        None
    }
}

#[test]
fn lex_iterates_seqs () {
    let toks = [&UNIX_WORD, &SPACE];
    let lex = Lexer::new(&toks);
    let mut it = lex.scan(b"foo42 bar_11 baz");
    assert_eq!(it.next(), Some(Token { src: b"foo42", line:1, typ: &UNIX_WORD }));
    assert_eq!(it.next(), Some(Token { src: b" ", line:1, typ: &SPACE }));
    assert_eq!(it.next(), Some(Token { src: b"bar_11", line:1, typ: &UNIX_WORD }));
    assert_eq!(it.next(), Some(Token { src: b" ", line:1, typ: &SPACE }));
    assert_eq!(it.next(), Some(Token { src: b"baz", line:1, typ: &UNIX_WORD }));
    assert_eq!(it.next(), None);
    assert_eq!(it.state(), LexState::Eof);
}

#[test]
fn lex_provides_feedback() {
    let toks = [&UNIX_WORD, &DOUBLE_QUOTE, &SPACE];
    let lex = Lexer::new(&toks);
    let mut it = lex.scan("foo \"bar\"ąęś baz".as_bytes());
    assert_eq!(it.next(), Some(Token { src: b"foo", line:1, typ: &UNIX_WORD }));
    assert_eq!(it.next(), Some(Token { src: b" ", line:1, typ: &SPACE }));

    assert_eq!(it.state(), LexState::Ok);

    assert_eq!(it.next(), Some(Token { src: b"\"bar\"", line:1, typ: &DOUBLE_QUOTE }));
    assert_eq!(it.next(), None);

    assert_eq!(it.state(), LexState::InvalidChar("ąęś baz".as_bytes()));
}

#[test]
fn lex_tracks_line_numbers () {
    let toks = [&UNIX_WORD, &DOUBLE_QUOTE, &SPACE];
    let lex = Lexer::new(&toks);
    let mut it = lex.scan(b"foo\nbar\nbaz\t\"coo\"");
    assert_eq!(it.next(), Some(Token { src: b"foo", line:1, typ: &UNIX_WORD }));
    assert_eq!(it.next(), Some(Token { src: b"\n", line:2, typ: &SPACE }));
    assert_eq!(it.next(), Some(Token { src: b"bar", line:2, typ: &UNIX_WORD }));
    assert_eq!(it.next(), Some(Token { src: b"\n", line:3, typ: &SPACE }));
    assert_eq!(it.next(), Some(Token { src: b"baz", line:3, typ: &UNIX_WORD }));
    assert_eq!(it.next(), Some(Token { src: b"\t", line:3, typ: &SPACE }));
    assert_eq!(it.next(), Some(Token { src: b"\"coo\"", line:3, typ: &DOUBLE_QUOTE }));
    assert_eq!(it.next(), None);
    assert_eq!(it.state(), LexState::Eof);
}
