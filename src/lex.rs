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
    Const(&'a [T]),
    Or(&'a [&'a Seq<'a,T>]),
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

            Self::Const(seq) => {
                if &rawstr[0..seq.len()] == *seq {
                    Some(Subseq::start(rawstr, seq.len()))
                } else {
                    None
                }
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

pub static DOUBLE_QUOTE: Seq<u8> =
    Seq::Composition(&[Seq::One(QUOTE_CHAR),
                       Seq::Any(&Seq::Or(&[&ESC, 
                                           &Seq::One(Class::NoneOf(&[b'"']))])),
                       Seq::One(QUOTE_CHAR)]);

pub static BRACK: Seq<u8> =
    Seq::Composition(&[Seq::One(Class::Set(&[b'('])),
                       Seq::Any(&Seq::Or(&[&BRACK, &Seq::One(Class::NoneOf(&[b'(', b')']))])),
                       Seq::One(Class::Set(&[b')']))]);

pub static CURLY: Seq<u8> =
    Seq::Composition(&[Seq::One(Class::Set(&[b'{'])),
                       Seq::Any(&Seq::Or(&[&CURLY, &Seq::One(Class::NoneOf(&[b'{', b'}']))])),
                       Seq::One(Class::Set(&[b'}']))]);

pub static SQBRACK: Seq<u8> =
    Seq::Composition(&[Seq::One(Class::Set(&[b'['])),
                       Seq::Any(&Seq::Or(&[&SQBRACK, &Seq::One(Class::NoneOf(&[b'[', b']']))])),
                       Seq::One(Class::Set(&[b']']))]);

pub static PTBRACK: Seq<u8> =
    Seq::Composition(&[Seq::One(Class::Set(&[b'<'])),
                       Seq::Any(&Seq::Or(&[&PTBRACK, &Seq::One(Class::NoneOf(&[b'<', b'>']))])),
                       Seq::One(Class::Set(&[b'>']))]);

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

#[test]
fn seq_matches_brackets() {
    let a = b"(foo, bar, (bazqff), uqq, (bi, ca, yo))";
    assert_eq!(BRACK.scan(a), Some(Subseq::start(a, 39)));

    let b = b"<Akkkh<> 11>";
    assert_eq!(PTBRACK.scan(b), Some(Subseq::start(b, 12)));

    let c = b"[132, a[44]]";
    assert_eq!(SQBRACK.scan(c), Some(Subseq::start(c, 12)));

    let d = b"{12, sqq({42, 2})}";
    assert_eq!(CURLY.scan(d), Some(Subseq::start(d, 18)));
}

pub static SPACE: Seq<u8> = Seq::Many(&Seq::One(Class::Set(&[b' ', b'\t', b'\n'])));

#[derive(Clone, Copy)]
pub struct Token<'a> {
    src: &'a [u8],
    typ: &'a Seq<'a,u8>,
    line: usize
}

impl<'a> Token<'a> {
    pub fn line(&self) -> usize { self.line }

    pub fn test(&self, t: &'a Seq<'a, u8>, val: &'a[u8]) -> bool {
        ptr_eq(t, self.typ) && self.src == val
    }

    pub fn of_type(&self, t: &'a Seq<'a, u8>) -> bool {
        ptr_eq(t, self.typ)
    }
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
    pub const fn new(opts: &'a [&'a Seq<'a, u8>]) -> Self { Lexer { opts } }

    pub fn scan(&'a self, rawstr: &'a [u8]) -> Lex<'a> {
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
    pub fn start(lex: &'a Lexer, rawstr: &'a [u8]) -> Self {
        Lex { lex, cursor: Subseq::start(rawstr, 0), 
                 state: LexState::Ok }
    }

    pub fn state(&self) -> LexState<'a> {
        self.state
    }
}

impl <'a> Iterator for Lex<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.state != LexState::Ok {
            return None
        }

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

pub struct RustBlock<'a> {
    pub name: &'a str,
    pub body: &'a[u8]
}

static RUST_LEX: Lexer = Lexer::new(&[&UNIX_WORD, &SPACE, &CURLY, &PTBRACK]);

#[derive(Debug)]
pub struct SyntaxError<'a> {
    pub last: Option<Token<'a>>
}

impl <'a> SyntaxError<'a> {
    pub fn from_it_next<I>(last: Option<Token<'a>>, it: &mut I)
            -> Result<Token<'a>, SyntaxError<'a>> 
            where I: Iterator<Item=Token<'a>> {

        match it.next() {
            Some(x) => Ok(x),
            None => Err(SyntaxError { last })
        }
    }
}

impl<'a> RustBlock<'a> {
    pub fn scan(body: &'a[u8]) -> Result<Self,SyntaxError<'a>> {
        let mut it = RUST_LEX.scan(body)
                             .filter(|x| !x.of_type(&SPACE));
        let initial = SyntaxError::from_it_next(None, &mut it)?;

        let strct = match initial.test(&UNIX_WORD, b"pub") {
            true => SyntaxError::from_it_next(None, &mut it)?,
            false => initial
        };

        if strct.test(&UNIX_WORD, b"struct") {
            let name = SyntaxError::from_it_next(Some(initial), &mut it)?;
            let mut x;
            loop { 
                x = SyntaxError::from_it_next(Some(name.clone()), &mut it)?;
                if !x.of_type(&SPACE) {
                    break;
                }
            }

            if x.of_type(&CURLY) {
                return Ok(RustBlock { name: str::from_utf8(name.src).unwrap(), 
                                      body: &x.src[1..x.src.len()-1] });
            } else {
                return Err(SyntaxError { last: Some(name) })
            }
        }

        return Err(SyntaxError { last: Some(initial) })
    }
}

#[test]
fn seq_matches_rust_blocks() {
    let src = b"pub struct aa { qq: String }";
    let rb = RustBlock::scan(src).unwrap();
    assert_eq!(rb.name, "aa");
    assert_eq!(rb.body, b" qq: String ");
}
