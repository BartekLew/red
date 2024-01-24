
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
    end: usize
}

impl <'a> Substr<'a> {
    pub fn start(str: &'a [u8], len: usize) -> Self {
        Substr { str, start: 0, end: len }
    }

    pub fn head(&self) -> &'a [u8] { &self.str[self.start..self.end] }
    pub fn tail(&self) -> &'a [u8] { &self.str[self.end..] }

    pub fn then<F>(&self, f:F) -> Option<Self>
            where F: Fn(&'a [u8]) -> Option<Substr<'a>> {
        f(self.tail())
            .map(|ans| Substr { str: self.str, start: self.start, end: self.end + ans.end }) 
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
pub const UNIX_WORD: Seq = Seq::Many(&Seq::One(UNIX_NAME_CHAR));
pub const ESC: Seq = Seq::Composition(&[Seq::One(Char::Set(&[b'\\'])),
                                      Seq::One(Char::Any)]);

pub const DOUBLE_QUOTE: Seq = Seq::Composition(&[Seq::One(QUOTE_CHAR),
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
