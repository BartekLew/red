use std::io;
use std::io::Read;
use std::str;
use std::fmt;

use red::matcher::Matcher;
use red::matcher::Value;

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
                             .class(|_,c| !c.is_whitespace())
                             .const_str(" stdout ----"));

        if m2.is_err() { return m2.derive(None) }

        let (tail, val) =  m2.as_tupple();
        let case = val.unwrap();

        let mut it = Matcher::new(tail)
                            .search(|m| m.class(|_,c| c != ':' && !c.is_whitespace())
                                         .const_str(":")
                                         .add::<u64>()
                                         .const_str(":")
                                         .add::<u64>()
                                         .map(|((file, line), col)| Some((file, line, col))));

        if let Some((file, line, col)) = it.next() {
            return Matcher { val: Some(TestFail { case, file, line, col, 
                                                  input: String::from(tail)}),
                             tail: &tail[0..0]}
        }

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
            Some(Info::Code(txt))
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

fn fill_buff(buff: &mut [u8], offset: usize) -> Option<usize> {
    match io::stdin().read(&mut buff[offset..]) {
        Ok(n) if n == 0 => None,
        Ok(n) => Some(n),
        Err(e) => {
            eprintln!("{}", e);
            None
        }
    }
}

const BUFF_SIZE : usize = 1024;
fn main() {
    let mut buff: [u8;BUFF_SIZE] = [0;BUFF_SIZE];
    let mut offset = 0;
    loop {
        match fill_buff(&mut buff, offset) {
            Some(n) => {
                let fixed_buff = String::from_utf8_lossy(&buff[0..offset+n]);
                let mut lines = Matcher::new(fixed_buff.as_ref())
                                       .split("\n\n");
                while let Some(msg) = lines.next() {
                    if let Some(msg) = Info::scan(msg) {
                        println!("{}", msg);
                    }
                }

                let taillen = lines.tail().as_bytes().len();
                let basepos = offset+n - taillen;
                if taillen > 0 {
                    for i in 0..taillen {
                        buff[i] = buff[basepos+i];
                    }
                }

                offset = taillen;
            },
            None => break
        }
    }
}

