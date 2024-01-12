use std::io;
use std::io::Read;
use std::str;
use std::fmt;

mod file;

mod matcher;
use matcher::Matcher;
use matcher::Value;

struct Warning<'a> {
    line: u64,
    text: &'a str,
    file: &'a str,
    offset: u64,
    msg: &'a str
}

impl <'a> Value<'a> for Warning<'a> {
    fn parse(m: Matcher<'a, ()>) -> Matcher<'a, Warning> {
        let m = m.line(|m| m.or(|m| m.const_str("warning:"),
                        |m| m.const_str("error")
                             .maybe(|m| m.const_str("[")
                                         .class(|_,c| c != ']')
                                         .const_str("]")
                                         .drop_val())
                             .const_str(":"))
                    .space()
                    .class(|_,c| c != '\n')
                    .space()
                    .const_str("--> ")
                    .add_class(|_,c| c != ':')
                    .const_str(":")
                    .add::<u64>()
                    .const_str(":")
                    .add::<u64>());
         match m.as_tupple() {
            (tail, Some((((text, file), line), offset))) => 
                Matcher { tail: "", val: Some(Warning { text, file, line, offset, msg: tail }) },
            (tail, None) => Matcher {tail, val: None}
         } 

    }
}

impl<'a> Warning <'a> {
    fn drop_if<F>(self, f:F) -> Option<Warning<'a>>
            where F:Fn(Matcher<'a, ()>) -> Matcher<'a, ()> {
        if Matcher::new(self.text).search(f).next().is_none() {
            Some(self)
        } else {
            None
        }
    }

    fn select(self) -> Option<Warning<'a>> {
        self.drop_if(|m|
                m.or(|m| m.const_str("never used"),
                     |m| m.const_str("generated ")
                          .value::<u64>()
                          .drop_val()
                          .const_str(" warnings")))
    }
}

impl<'a> fmt::Display for Warning<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}({}:{}): {}\n{}", self.file, self.line, self.offset, self.text, self.msg)
    }
}

struct TestStat {
    ok: u64, err: u64
}

impl <'a> Value<'a> for TestStat {
    fn parse(m:Matcher<'a, ()>) -> Matcher<'a, Self> {
        m.line(|m| m.const_str("test result: ")
                    .word()
                    .drop_val()
                    .const_str(". ")
                    .value::<u64>()
                    .const_str(" passed; ")
                    .add::<u64>()
                    .map(|(ok, err)| Some(TestStat { ok, err })))
    }
}

impl TestStat {
    pub fn select(self) -> Option<TestStat> {
        if self.err > 0 {
            Some(self)
        } else {
            None
        }
    }
}

impl<'a> fmt::Display for TestStat {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let total = self.ok + self.err;
        if total > 0 {
            write!(f, "tests {}/{} {}%", self.ok, total, self.ok*100/total)
        } else {
            write!(f, "no tests to run")
        }
    }
}

pub struct TestFail <'a> {
    pub case: &'a str,
    pub input: String,
    pub file: &'a str,
    pub line: u64,
    pub col: u64
}

impl <'a> Value<'a> for TestFail<'a> {
    fn parse(m:Matcher<'a, ()>) -> Matcher<'a, Self> {
        let m2 = m.dupl()
                  .line(|m| m.const_str("---- ")
                            .word()
                            .const_str(" stdout ----\n"));

        if m2.is_err() { return m2.derive(None) }

        let (tail, val) =  m2.as_tupple();
        let case = val.unwrap();
        let mut out = String::with_capacity(2048);

        for text in Matcher::new(tail)
                            .search(|m| m.class(|_,c| c != '\n')
                                         .const_str("\n")) {
            let mut it = 
                Matcher::new(text)
                        .search(|m| m.word()
                                     .const_str(":")
                                     .add::<u64>()
                                     .const_str(":")
                                     .add::<u64>()
                                     .map(|((file, line), col)| Some((file, line, col))));

            if let Some((file, line, col)) = it.next() {
                return Matcher { val: Some(TestFail { case, file, line, col, input: out + "\n" + text }),
                                                      tail: it.m.tail };
            } else {
                if out.len() == 0 {
                    out = out + text;
                }
                else {
                    out = out + "\n" + text;
                }
            }
        }

        let (tail, _val) = m.as_tupple();
        Matcher { tail: &tail[1..], val: None }
    }
}

impl<'a> fmt::Display for TestFail<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\n{}({}:{}): case {} failed:\n{}\n", self.file,
                  self.line, self.col, self.case, self.input)
    }
}


enum Info<'a> {
    Code(Warning<'a>),
    Test(TestStat),
    Fail(TestFail<'a>)
}

impl<'a> Info<'a> {
    fn scan(msg: Matcher<'a, ()>) -> Option<Info> {
        if let Some(txt) = msg.dupl().search(|m| m.value::<Warning<'a>>()).next() {
            txt.select().map(|w| Info::Code(w))
        } else if let Some(fail) = msg.dupl().search(|m| m.value::<TestFail>()).next() {
            Some(Info::Fail(fail))
        } else if let Some(testrep) = msg.dupl().search(|m| m.value::<TestStat>()).next() {
            testrep.select().map(|w| Info::Test(w))
        } else {
            None
        }
    }
}

impl<'a> fmt::Display for Info<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Code(i) => i.fmt(f),
            Self::Test(i) => i.fmt(f),
            Self::Fail(i) => i.fmt(f),
        }
    }
}

fn main() {
    let mut buff: [u8;2028] = [0;2028];
    loop {
        match io::stdin().read(&mut buff) {
            Ok(n) if n > 0 => {
                let fixed_buff = String::from_utf8_lossy(&mut buff);
                for msg in Matcher::new(fixed_buff.as_ref())
                                   .split("\n\n") {
                    if let Some(msg) = Info::scan(msg) {
                        println!("{}", msg);
                    }
                }
            },
            Ok(_) => { break; },
            Err(e) => {
                eprintln!("Error: {}", e);
                break;
            }
        }
    }
}

