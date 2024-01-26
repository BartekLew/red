use std::fmt;
use std::str;

pub trait OrdSet: Ord+Sized+Copy {
    fn next(&self) -> Self;
}

impl OrdSet for u8 {
    fn next(&self) -> u8 { self+1 }
} 

pub enum Class<'a, T:OrdSet> {
    Set(&'a[T]),
    Range(T, T),
    Alt(&'a[Class<'a, T>]),
    NoneOf(&'a [T]),
    Any
}

pub const QUOTE_CHAR: Class<u8> = Class::Set(&[b'"']);
pub const LATIN_SMALL: Class<u8> = Class::Range(b'a', b'z');
pub const LATIN_CAPITAL: Class<u8> = Class::Range(b'A', b'Z');
pub const LATIN: Class<u8> = Class::Alt(&[LATIN_SMALL, LATIN_CAPITAL]);
pub const DIGIT: Class<u8> = Class::Range(b'0', b'9');
pub const UNIX_NAME_CHAR: Class<u8> = Class::Alt(&[LATIN_SMALL, LATIN_CAPITAL, DIGIT, Class::Set(&[b'_'])]);

impl<'a, T:OrdSet> Class<'a, T> {
    pub fn test(&self, val: T) -> bool {
        match self {
            Self::Set(opts) => opts.iter().any(|c| *c == val),
            Self::Range(start, limit) => {
                let mut i = *start;
                while i <= *limit {
                    if i == val { return true; }
                    i = i.next();
                } 
                return false;
            },
            Self::Alt(opts) => opts.iter().any(|cd| cd.test(val)),
            Self::NoneOf(opts) => !opts.iter().any(|c| *c == val),
            Self::Any => true
        }
    }

    pub fn test_opt(&self, val: T) -> Option<()> {
        if self.test(val) { Some(()) }
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
    assert_eq!(Class::Any.test(b'_'), true);
    assert_eq!(Class::Any.test(b'Q'), true);
    assert_eq!(Class::Any.test(b'v'), true);
    assert_eq!(Class::Any.test(11), true);
}

pub enum Seq<'a,T:OrdSet> {
    One(Class<'a, T>),
    Many(&'a Seq<'a,T>),
    Any(&'a Seq<'a,T>),
    Or(&'a [Seq<'a,T>]),
    Composition(&'a[Seq<'a,T>])
}

pub trait Lined : PartialEq {
    fn lines_count(&self) -> usize;
}

impl Lined for u8 {
    fn lines_count(&self) -> usize {
        if *self == b'\n' { 1 } else { 0 }
    }
}

#[derive(PartialEq, Debug)]
pub struct Subseq<'a, T: Lined> {
    str: &'a [T],
    start: usize,
    end: usize,
    line: usize
}

impl <'a, T:Lined> Subseq<'a, T> {
    pub fn start(str: &'a [T], len: usize) -> Self {
        Subseq { str, start: 0, end: len, 
                 line: str[0..len].iter().fold(0, |a, v| a + v.lines_count()) }
    }

    pub fn head(&self) -> &'a [T] { &self.str[self.start..self.end] }
    pub fn tail(&self) -> &'a [T] { &self.str[self.end..] }

    pub fn then<F>(&self, f:F) -> Option<Self>
            where F: Fn(&'a [T]) -> Option<Subseq<'a, T>> {
        f(self.tail())
            .map(|ans| Subseq { str: self.str, 
                                start: self.start, 
                                end: self.end + ans.end,
                                line: self.line + ans.line }) 
    }
}

impl <'a> Subseq <'a, u8> {
    pub fn token(&self, typ: &'a Seq<u8>) -> Token<'a> {
        Token { src: self.head(), line: self.line + 1, typ }
    }
}

impl <'a, T:OrdSet+Lined> Seq<'a, T> {
    fn scan(&self, rawstr: &'a [T]) -> Option<Subseq<'a,T>> {
        match self {
            Self::One(cd) => rawstr.get(0)
                                   .and_then(|c| cd.test_opt(*c)
                                                   .map(|_| Subseq::start(rawstr, 1))),

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
                    None => {return Some(Subseq::start(rawstr, 0))}
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
                     .fold(Some(Subseq::start(rawstr, 0)),
                           |acc: Option<Subseq<T>>, part| acc.and_then(|x| x.then(|x| part.scan(x))))
            }
        }
    }
}

pub const LATIN_WORD: Seq<u8> = Seq::Many(&Seq::One(LATIN));
pub static UNIX_WORD: Seq<u8> = Seq::Many(&Seq::One(UNIX_NAME_CHAR));
pub const ESC: Seq<u8> = Seq::Composition(&[Seq::One(Class::Set(&[b'\\'])),
                                            Seq::One(Class::<u8>::Any)]);

static DOUBLE_QUOTE: Seq<u8> = Seq::Composition(&[Seq::One(QUOTE_CHAR),
                                                  Seq::Any(&Seq::Or(&[ESC, 
                                                                      Seq::One(Class::NoneOf(&[b'"']))])),
                                                  Seq::One(QUOTE_CHAR)]);

#[test]
fn seq_matches () {
    let a = b"foobar";
    assert_eq!(Seq::One(LATIN).scan(a), Some(Subseq::start(a, 1)));
    assert_eq!(Seq::One(LATIN).scan(b"_foobar"), None);

    assert_eq!(LATIN_WORD.scan(a), Some(Subseq::start(a, 6)));

    let b = b"foo_bar99";
    assert_eq!(UNIX_WORD.scan(b), Some(Subseq::start(b, 9)));

    let c = "\"你好\" chacha".as_bytes();
    assert_eq!(DOUBLE_QUOTE.scan(c), Some(Subseq::start(c, 8)));
    
    let d = b"\"\" chacha";
    assert_eq!(DOUBLE_QUOTE.scan(d), Some(Subseq::start(d, 2)));

    assert_eq!(Seq::One(LATIN).scan(c), None);

    let e = b"\"foo \\\"bar\\\" \" kan";
    assert_eq!(DOUBLE_QUOTE.scan(e), Some(Subseq::start(e, 14)));
}

static SPACE: Seq<u8> = Seq::Many(&Seq::One(Class::Set(&[b' ', b'\t', b'\n'])));

pub struct Token<'a> {
    src: &'a [u8],
    typ: &'a Seq<'a,u8>,
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
        write!(f, "{}<{}>", self.typ as *const Seq<'a, u8> as usize, str::from_utf8(self.src).unwrap())
    }
}

pub struct Lexer<'a> {
    opts: &'a [&'a Seq<'a,u8>]
}

impl<'a> Lexer<'a> {
    fn new(opts: &'a [&'a Seq<'a, u8>]) -> Self { Lexer { opts } }

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
    cursor: Subseq<'a, u8>,
    state: LexState<'a>
}

impl<'a> Lex <'a> {
    fn start(lex: &'a Lexer, rawstr: &'a [u8]) -> Self {
        Lex { lex, cursor: Subseq::start(rawstr, 0), 
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
