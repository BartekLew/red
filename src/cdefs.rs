// This util provides rust headers for a C struct
// struct fields come from stdin.

use std::io;
use std::io::Read;
use std::fmt;

mod lib;
use lib::*;

fn load_stdin() -> Result<String, String> {
    let mut remains = 20480;
    let mut offset = 0;
    let mut ans = String::with_capacity(remains);
    unsafe {ans.as_mut_vec().set_len(remains)};
    loop {
        match unsafe { io::stdin().read(&mut (ans.as_mut_vec())[offset..remains]) } {
            Ok(n) => {
                if n == 0 {
                    unsafe { ans.as_mut_vec().set_len(offset) };
                    return Ok(ans);
                }
                offset += n;
                remains -= n;
                if remains < 100 {
                    unsafe { ans.as_mut_vec().set_len(offset+20480) };
                    remains = 20480;
                } else {
                    unsafe { ans.as_mut_vec().set_len(offset+remains) };
                }
            }, Err(e) => {
                return Err(format!("{}", e));
            }
        }

    }
}

struct CFieldDef<'a> {
    typ: &'a str,
    name: &'a str
}

impl<'a> Value<'a> for CFieldDef<'a> {
    fn parse(m: Matcher<'a, ()>) -> Matcher<'a, CFieldDef> {
        m.word()
         .space()
         .add_word()
         .const_str(";")
         .skip_after(|c| c == '\n')
         .map(|(typ, name)| Some(CFieldDef{ typ, name }))
    }
}

impl<'a> fmt::Display for CFieldDef<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.typ)
    }
}

fn main() {
    match load_stdin() {
        Ok(s) => {
            let fields : Vec<CFieldDef> =
                Matcher::new(s.as_str())
                       .search(|m| m.maybe(|m| m.space())
                                    .value::<CFieldDef>())
                       .collect();
    
            println!("struct S {{");
            for f in fields.iter() {
                println!("    {} {};", f.typ, f.name);
            }
            println!("}}\n");

            println!("int main() {{");
            for f in fields.iter() {
                println!("    printf(\"{}: %ld\\n\", offsetof(S, {}), sizeof({}));",
                         f.name, f.name, f.typ);
            }
            println!("}}");
        }, Err(e) => {
            println!("'{:?}'", e);
        }
    }
}
