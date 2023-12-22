use std::num;
use core::fmt;

pub trait Number: Sized {
    fn parse(src: &str) -> Result<Self, num::ParseIntError>;
}

impl Number for u64 {
    fn parse(src: &str) -> Result<Self, num::ParseIntError> {
        u64::from_str_radix(src, 10)
    }
}

pub fn warn_err<T, E:fmt::Display>(r: Result<T,E>) -> Option<T> {
    match r {
        Ok(v) => Some(v),
        Err(e) => {
            eprintln!("warning: {}", e);
            None
        }
    }
}

pub struct Matcher<'a, T> {
    tail: &'a str,
    val: Option<T>
}

impl <'a, T> Matcher<'a,T> {
    fn pop<R>(&self, len: usize, val: R) -> Matcher<'a, R> {
        Matcher { tail: &self.tail[len..], val: Some(val) }
    }

    fn derive<R>(self, val: Option<R>) -> Matcher<'a, R> {
        Matcher { tail: self.tail, val }
    }

    pub fn drop_val(self) -> Matcher<'a, ()> {
        self.derive(Some(()))
    }

    pub fn map<R,F>(self, f: F) -> Matcher<'a, R>
            where F: FnOnce(T) -> Option<R> {
        match self {
            Matcher { tail, val: None } => Matcher { tail, val: None },
            Matcher { tail, val: Some(v) } => Matcher { tail, val: (f(v)) }
        }
    }

    fn then<R,F>(self, f: F) -> Matcher<'a, R> 
            where F: FnOnce(T) -> Matcher<'a, R> {
        match self.val {
            None => Matcher { tail: self.tail, val: None },
            Some(v) => f(v) 
        }
    }

    pub fn maybe<F>(self, f:F) -> Self
            where F: Fn(Matcher<'a,()>) -> Self {
        match f(Matcher::new(self.tail)) {
            v if v.val.is_none() => self,
            v => v
        }
    }

    fn fail<R>(mut self, offset: usize) -> Matcher<'a, R> {
        if self.tail.len() >= offset {
            self.tail = &self.tail[offset..];
        }

        Matcher { tail: self.tail, val: None }
    }

    pub fn tail(&self) -> &str {
        self.tail
    }

    pub fn skip_after<F>(mut self, f:F) -> Self
            where F: Fn(char) -> bool {

        match Matcher::new(self.tail)
                      .class(|_,i| !f(i)) {
            Matcher { tail, val: Some(_) } => {
                if tail.len() > 0 {
                    self.tail = &tail[1..]
                } else {
                    self.tail = tail
                }
            },
            Matcher { tail, .. } => self.tail = tail
        }

        self
    }

    pub fn result(self) -> Option<T> {
        self.val
    }

    pub fn space(self) -> Matcher<'a, T> {
        if self.val.is_none() {
            return self
        }

        Matcher::new(self.tail)
               .class(|_,c| c.is_whitespace())
               .map(|_| self.val)
    }

    pub fn add_word(self) -> Matcher<'a, (T, &'a str)> {
        let tail = self.tail;
        self.then(|base|
            Matcher::new(tail)
                  .word()
                  .map(|v| Some((base, v))))
    }
}

impl <'a> Matcher<'a, ()> {
    pub fn new(tail: &'a str) -> Self {
        Matcher { tail, val: Some(()) }
    }

    pub fn line<R,F>(self, f:F) -> Matcher<'a, R>
            where F: Fn(Matcher<'a, ()>) -> Matcher<'a, R> {
        match f(Matcher::new(self.tail)) {
            v if v.val.is_none() => self.derive(None).skip_after(|c| c == '\n'),
            v => v.skip_after(|c| c == '\n')
        }
    }

    pub fn const_str(self, refstr: &'a str) -> Matcher<'a, ()> {
        if self.val.is_none() {
            return self;
        }

        let len = self.tail.len();
        if len < refstr.len() {
            self.fail(1)
        } else if &self.tail[0..refstr.len()] == refstr {
            self.pop(refstr.len(), ())
        } else {
            self.fail(1)
        }
    }

    pub fn class<F>(self, f: F) -> Matcher<'a, &'a str>
            where F: Fn(usize, char) -> bool {
        if self.val.is_none() {
            return self.derive(None);
        }

        let mut n = 0;
        while n < self.tail.len() {
            let c = self.tail.chars().nth(n).unwrap();
            if f(n, c) {
                n += 1;
            } else {
                break;
            }
        }

        if n > 0 {
            self.pop(n, &self.tail[0..n])
        } else {
            self.fail(1)
        }
    }

    pub fn number<R:Number>(self) -> Matcher<'a,R> {
        self.class(|_, c| c.is_ascii_digit())
            .map(|s| warn_err(R::parse(s)))
    }

    pub fn word(self) -> Matcher<'a, &'a str> {
        self.class(|n, c| (c.is_alphanumeric() || c == '_')
                       && (n != 0 || !c.is_ascii_digit()))
    }

    pub fn search<R,F>(self, f: F) -> Search<'a, R, F> 
            where F: Fn(Matcher<'a, ()>) -> Matcher<'a, R> {
        Search { m: self, f }
    }

    pub fn or<R,F,F2>(self, f1: F, f2: F2) -> Matcher<'a, R>
            where F: Fn(Matcher<'a, ()>) -> Matcher<'a, R>,
                  F2: Fn(Matcher<'a, ()>) -> Matcher<'a, R> {
        match f1(Matcher::new(self.tail)) {
            Matcher { tail: _, val: None } => f2(self),
            m => m
        }
    }
}

impl <'a> Matcher<'a, &'a str> {
    pub fn class<F>(self, f: F) -> Self
            where F: Fn(usize, char) -> bool {
        if self.val.is_none() {
            return self;
        }

        match Matcher::new(self.tail)
                      .class(f) {
            Matcher{ tail, val: Some(v) } => {
                unsafe {
                    let base = self.val.unwrap();
                    let len = base.len();
                    let extref = std::slice::from_raw_parts(base.as_bytes().as_ptr(), len + v.len());
                    Matcher { tail, val: Some(std::str::from_utf8_unchecked(&*extref)) }
                }
            }, 

            _ => self
        }
    }
}

pub struct Search<'a, T, F> 
            where F: Fn(Matcher<'a, ()>) -> Matcher<'a, T> {
    m: Matcher<'a, ()>,
    f: F
}

impl <'a, T, F> Iterator for Search<'a, T, F>
        where F: Fn(Matcher<'a, ()>) -> Matcher<'a, T> {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        while self.m.tail.len() > 0 {
            match (self.f)(Matcher::new(self.m.tail)) {
                Matcher { tail, val: None } => {
                    self.m.tail = tail;
                },
                ans => {
                    self.m.tail = ans.tail;
                    return ans.val;
                }
                
            }
        }

        return None
    }
}

#[test]
fn matcher_matches_const() {
    let source = "foo bar";
    let ans = Matcher::new(source)
                .const_str("foo");

    assert_eq!(ans.tail, " bar");
    assert_eq!(ans.result(), Some(()));
}

#[test]
fn matcher_matches_word() {
    let source = "foo bar baz";
    let ans = Matcher::new(source)
                     .word();

    assert_eq!(ans.tail, " bar baz");
    assert_eq!(ans.result(), Some("foo"));

    let ans = Matcher::new("cooo_foo")
                     .word();
    assert_eq!(ans.tail, "");
    assert_eq!(ans.result(), Some("cooo_foo"));
    
    let ans = Matcher::new("99cooo_foo")
                     .word();
    assert_eq!(ans.tail(), "9cooo_foo");
    assert_eq!(ans.result(), None);
}

#[test]
fn matcher_matches_whitespace() {
    let ans = Matcher::new("  foobar 32")
                      .space()
                      .word();
    assert_eq!(ans.tail(), " 32");
    assert_eq!(ans.result(), Some("foobar"));
}

#[test]
fn matcher_strings_concatenate() {
    let ans = Matcher::new("  432foobar_")
                     .space() 
                     .class(|_, c| c.is_ascii_digit())
                     .class(|_, c| c.is_ascii_alphabetic());

    assert_eq!(ans.tail(), "_");
    assert_eq!(ans.val, Some("432foobar"));
}

#[test]
fn matcher_searches_in_string() {
    let mut ans = Matcher::new("  foo bar")
                         .search(|m| m.word());

    assert_eq!(ans.next(), Some("foo"));
    assert_eq!(ans.m.tail, " bar");

    assert_eq!(ans.next(), Some("bar"));
    assert_eq!(ans.m.tail(), "");

    assert_eq!(ans.next(), None);
}

#[test]
fn matcher_makes_tupples () {
    let ans = Matcher::new("425 foo\n661 bar\ncoo\n777 baz")
                     .number::<u64>()
                     .space()
                     .add_word();

    assert_eq!(ans.tail(), "\n661 bar\ncoo\n777 baz");
    assert_eq!(ans.result(), Some((425, "foo")));
}

#[test]
fn matcher_matches_lines () {
    let mut it = Matcher::new("425 foo\n661 bar\ncoo 123 bam\n777 baz")
                        .search(|m| m.number::<u64>()
                                     .space()
                                     .add_word()
                                     .skip_after(|c| c == '\n'));

    assert_eq!(it.next(), Some((425, "foo")));
    assert_eq!(it.next(), Some((661, "bar")));
    assert_eq!(it.next(), Some((777, "baz")));
    assert_eq!(it.m.tail(), "");
}

#[test]
fn matcher_refuses_to_match_on_bad_state() {
    let ans = Matcher::new("foo 123 bar")
                     .number::<u64>()
                     .add_word();

    assert_eq!(ans.tail(), "oo 123 bar");
    assert_eq!(ans.result(), None);

    let ans = Matcher::new("foo 123 bar")
                     .const_str("baah:")
                     .word();

    assert_eq!(ans.tail(), "oo 123 bar");
    assert_eq!(ans.result(), None);
}

#[test]
fn match_warning () {
    let ans = Matcher::new("foo bar\nwarning: blah\nbbb\n")
                    .const_str("warning:")
                    .space()
                    .class(|_,c| c != '\n')
                    .skip_after(|c| c == '\n');

    assert_eq!(ans.tail, "warning: blah\nbbb\n");
    assert_eq!(ans.result(), None);
}

#[test]
fn matcher_supports_alternative () {
    let mut it = Matcher::new("foo:423 baz:222 boo:42; moo:11 bar:111")
                        .search(|m| m.or(|m| m.const_str("foo:"),
                                         |m| m.const_str("bar:"))
                                     .number::<u64>());
    assert_eq!(it.next(), Some(423));
    assert_eq!(it.next(), Some(111));
    assert_eq!(it.m.tail(), "");
}

#[test]
fn matcher_supports_per_line_match() {
    let mut it = Matcher::new("l: cat cat 123\ncoo coo442\nl: zaza")
                        .search(|m| m.line(|m| m.const_str("l: ")
                                                .word()));
    
    assert_eq!(it.next(), Some("cat"));
    assert_eq!(it.next(), Some("zaza"));
    assert_eq!(it.next(), None);
}

#[test]
fn matcher_supports_optionality() {
    fn act<'a>(m: Matcher<'a, ()>) -> Matcher<'a, &'a str> {
             m.const_str("error")
              .maybe(|m| m.const_str("[")
                          .word().drop_val()
                          .const_str("]"))
              .const_str(":")
              .space()
              .class(|_,c| c != '\n')
    }

    assert_eq!(act(Matcher::new("error: foo bar baz"))
                          .result(),
               Some("foo bar baz"));

    assert_eq!(act(Matcher::new("error[caacoo]: foo bar baz"))
                          .result(),
               Some("foo bar baz"));

    assert_eq!(act(Matcher::new("error[caac: foo bar baz"))
                          .result(),
               None);
}
