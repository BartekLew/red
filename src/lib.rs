
pub struct Matcher<'a, T> {
    tail: &'a str,
    val: Option<T>
}

impl <'a, T> Matcher<'a,T> {
    fn derive<R>(self, v:Option<R>) -> Matcher<'a, R> {
        Matcher { tail: self.tail, val: v }
    }

    fn pop<R>(&self, len: usize, val: R) -> Matcher<'a, R> {
        Matcher { tail: &self.tail[len..], val: Some(val) }
    }

    pub fn result(self) -> Option<T> {
        self.val
    }
}

impl <'a> Matcher<'a, ()> {
    pub fn new(tail: &'a str) -> Self {
        Matcher { tail, val: None }
    }

    pub fn const_str(self, refstr: &'a str) -> Matcher<'a, ()> {
        if self.tail.len() < refstr.len() {
            self.derive(None)
        } else if &self.tail[0..refstr.len()] == refstr {
            self.pop(refstr.len(), ())
        } else {
            self.derive(None)
        }
    }

    pub fn class<F>(self, f: F) -> Matcher<'a, &'a str>
            where F: Fn(usize, char) -> bool {
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
            self.derive(None)
        }
    }

    pub fn space(self) -> Matcher<'a, ()> {
        self.class(|_,c| c.is_whitespace()).derive(Some(()))
    }

    pub fn word(self) -> Matcher<'a, &'a str> {
        self.class(|n, c| (c.is_alphanumeric() || c == '_')
                       && (n != 0 || !c.is_ascii_digit()))
    }

    pub fn search<R,F>(self, f: F) -> Search<'a, R, F> 
            where F: Fn(Matcher<'a, ()>) -> Matcher<'a, R> {
        Search { m: self, f }
    }
}

impl <'a> Matcher<'a, &'a str> {
    pub fn class<F>(self, f: F) -> Self
            where F: Fn(usize, char) -> bool {
        if self.val.is_none() {
            return self
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
        for i in 0..self.m.tail.len() {
            match (self.f)(Matcher::new(&self.m.tail[i..])) {
                Matcher { val: None, .. } => {},
                Matcher { tail, val } => {
                    self.m = Matcher::new(tail);
                    return val;
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
    assert_eq!(ans.tail, "99cooo_foo");
    assert_eq!(ans.result(), None);
}

#[test]
fn matcher_matches_whitespace() {
    let ans = Matcher::new("  foobar 32")
                      .space()
                      .word();
    assert_eq!(ans.tail, " 32");
    assert_eq!(ans.result(), Some("foobar"));
}

#[test]
fn matcher_strings_concatenate() {
    let ans = Matcher::new("  432foobar_")
                     .space() 
                     .class(|_, c| c.is_ascii_digit())
                     .class(|_, c| c.is_ascii_alphabetic());

    assert_eq!(ans.tail, "_");
    assert_eq!(ans.val, Some("432foobar"));
}

#[test]
fn matcher_searches_in_string() {
    let mut ans = Matcher::new("  foo bar")
                         .search(|m| m.word());

    assert_eq!(ans.next(), Some("foo"));
    assert_eq!(ans.m.tail, " bar");

    assert_eq!(ans.next(), Some("bar"));
    assert_eq!(ans.m.tail, "");

    assert_eq!(ans.next(), None);
}

