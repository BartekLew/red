use std::io;
use std::io::Read;
use std::str;
use std::fmt;

mod lib;
use lib::Matcher;

struct Warning<'a> {
    line: u64,
    text: &'a str,
    file: &'a str
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
                    .map(|((text,file),line)| Some(Warning { text, file, line })))
    }
}

impl<'a> fmt::Display for Warning<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}({}): {}", self.file, self.line, self.text)
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
                    if let Some(txt) = msg.dupl().search(|m| Warning::scan(m)).next() {
                        if Matcher::new(txt.text)
                                  .search(|m| m.or(|m| m.const_str("function is never used"),
                                                   |m| m.const_str("generated ")
                                                        .value::<u64>()
                                                        .drop_val()
                                                        .const_str(" warnings")))
                                  .next().is_none() {
                            println!("{}", txt);
                        }
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

