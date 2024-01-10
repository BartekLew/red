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
            let mut struc_m : Search<&str,_> =
                Matcher::new(s.as_str())
                        .search(|m| m.const_str("struct")
                                     .space()
                                     .word()
                                     .space()
                                     .const_str("{")); 
            match struc_m.next() {
                Some(s) => {
                    let fields : Vec<CFieldDef> =
                    struc_m.m.search(|m| m.maybe(|m| m.space())
                                          .value::<CFieldDef>())
                             .collect();
    
                    println!("#include<stdio.h>");
                    println!("#include<stddef.h>");
                    println!("#include<sys/types.h>");
                    println!("#include<sys/stat.h>\n");

                    println!("struct {} {{", s);
                    for f in fields.iter() {
                        println!("    {} {};", f.typ, f.name);
                    }
                    println!("}};\n");
    
                    println!("char *type4size(size_t len) {{");
                    println!("    if(len == 4) return \"u32\";");
                    println!("    if(len == 8) return \"u64\";");
                    println!("    return \"UNKNOWNTYPE\";\n}}");

                    println!("int main() {{");
                    println!("    int offset = 0;");
                    println!("    int newoff;");
                    println!("    printf(\"struct {} {{\\n\");", s);
                    for f in fields.iter() {

                        println!("    newoff = offsetof(struct {}, {});", s, f.name);
                        println!("    if(newoff > offset) {{");
                        println!("        printf(\"    _: %s,\\n\", type4size(newoff - offset));");
                        println!("        offset = newoff;\n    }}");
                        println!("    printf(\"    _{}: %s,\\n\", type4size(sizeof({})));", f.name, f.typ);
                        println!("    offset += sizeof({});", f.typ);
                    }
                    println!("    printf(\"}}\");\n}}");
                }, None => {}
            }
        }, Err(_) => {}
    }
}
