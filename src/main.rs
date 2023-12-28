use std::io;
use std::io::Read;
use std::str;
use std::fmt;

mod lib;
use lib::Matcher;

struct Warning<'a> {
    line: u64,
    text: &'a str,
    file: &'a str,
    offset: u64
}

impl <'a> Warning<'a> {
    pub fn scan(m: Matcher<'a, ()>) -> Matcher<'a, Warning> {
        m.line(|m| m.or(|m| m.const_str("warning:"),
                        |m| m.const_str("error:"))
                    .space()
                    .class(|_,c| c != '\n')
                    .space()
                    .const_str("--> ")
                    .add_class(|_,c| c != ':')
                    .const_str(":")
                    .add::<u64>()
                    .const_str(":")
                    .add::<u64>()
                    .map(|(((text,file),line),offset)| Some(Warning { text, file, line, offset })))
    }
}

impl<'a> Warning <'a> {
    fn skip_messages<F>(self, f:F) -> Option<Warning<'a>>
            where F:Fn(Matcher<'a, ()>) -> Matcher<'a, ()> {
        if Matcher::new(self.text).search(f).next().is_none() {
            Some(self)
        } else {
            None
        }
    }
}

impl<'a> fmt::Display for Warning<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}({}:{}): {}", self.file, self.line, self.offset, self.text)
    }
}

struct TestStat {
    ok: u64, err: u64
}

impl TestStat {
    pub fn scan(m:Matcher<'_, ()>) -> Matcher<'_, Self> {
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

enum Info<'a> {
    Code(Warning<'a>),
    Test(TestStat)
}

impl<'a> Info<'a> {
    fn scan(msg: Matcher<'a, ()>) -> Option<Info> {
        if let Some(txt) = msg.dupl()
                              .search(|m| Warning::scan(m))
                              .next() {
            txt.skip_messages(|m|
                    m.or(|m| m.const_str("function is never used"),
                         |m| m.const_str("generated ")
                              .value::<u64>()
                              .drop_val()
                              .const_str(" warnings")))
               .map(|w| Info::Code(w))
        } else if let Some(testrep) = msg.dupl().search(|m| TestStat::scan(m)).next() {
            if testrep.err > 0 {
                Some(Info::Test(testrep))
            } else {
                None
            }
        } else {
            None
        }
    }
}

impl<'a> fmt::Display for Info<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Code(i) => i.fmt(f),
            Self::Test(i) => i.fmt(f)
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

