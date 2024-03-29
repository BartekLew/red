use core::str::Utf8Error;
use std::slice;
use std::str;
use std::fs;
use std::io::Read;
use cstr::cstr;
use std::ffi::CStr;

use crate::matcher::*;

extern "C" {
    fn open(fname: *const i8, flags: i32, mode: i32) -> i32;
    fn fstat(fd: i32, buff: &mut Stat) -> i32;
    fn ftruncate(fd: i32, size: usize) -> i32;
    fn mmap(addr: *mut u8, size: usize, protection: i64, flags: i64,
            fd: i32, offset: u64) -> *mut u8;
    fn munmap(fd: i32, offset: usize);
    fn strlen(str: *const u8) -> usize;
    fn strerror(errno: i64) -> *const u8;
    fn __errno_location() -> *const i64;

}

fn libc_err() -> &'static str {
    unsafe {
        let err = strerror(*(__errno_location()));
        let len = strlen(err);
        str::from_utf8(slice::from_raw_parts(err,len)).unwrap()
    }
}

#[derive(Clone,Copy)]
pub enum Mode { ReadOnly, ReadWrite }

impl Mode {
    pub fn libc_flags(&self) -> i32 {
        match self {
            Self::ReadOnly => O_RDONLY,
            Self::ReadWrite => O_RDWR | O_CREAT
        }
    }

    pub fn libc_mode(&self) -> i32 {
        match self {
            Self::ReadOnly => 0o400,
            Self::ReadWrite => 0o644 
        }
    }

    pub fn mmap_prot(&self) -> i64 {
        match self {
            Self::ReadOnly => PROT_READ,
            Self::ReadWrite => PROT_READ | PROT_WRITE
        }
    }
}

struct File<'a> {
    name: &'a str,
    buff: &'a mut [u8],
    fd:   i32,
    mode: Mode
}

const O_RDONLY: i32 = 0;
const O_RDWR: i32 = 02;
const O_CREAT: i32 = 0o100;
const PROT_READ : i64 = 1;
const PROT_WRITE : i64 = 2;
const MAP_SHARED: i64 = 1;

impl<'a> File<'a> {
    pub fn open(name: &CStr, mode: Mode) -> Result<File,String> {
        let rname = name.to_str().unwrap();
        unsafe {
            let fd = open(name.as_ptr(), mode.libc_flags(), mode.libc_mode());
            if fd < 0 {
                return Err(format!("Can't open file '{}': {}", rname, libc_err()));
            }

            match Stat::from_fd(fd).map(|x| x.size()) {
                Ok(size) => {
                    Self::mmap(fd, size, mode)
                        .map(|buff| File { name: rname, buff, fd, mode })
                },
                Err(e) => Err(e)
            }
        }
    }

    pub fn resize(&mut self, size:usize) -> Result<(), String> {
        unsafe{
            if ftruncate(self.fd, size) != 0 {
                return Err(format!("ftruncate() failed: {}", libc_err()));
            }
        }
        
        self.buff = Self::mmap(self.fd, size, self.mode).unwrap();
        Ok(())
    }

    fn mmap(fd: i32, size: usize, mode: Mode) -> Result<&'static mut [u8], String> {
        if size == 0 { return Ok(&mut [0;0]); }
        unsafe {
            let addr = mmap(0 as *mut u8, size, mode.mmap_prot(), MAP_SHARED, fd, 0);
            if addr != usize::MAX as *mut u8 {
                let buff = slice::from_raw_parts_mut (addr, size);
                Ok(buff)
            }
            else {
                Err(format!("mmap() failed: {}", libc_err()))
            }
        }
    }

    pub fn as_text(&self) -> Result<&str, Utf8Error> {
        str::from_utf8(self.buff)
    }

    pub fn write_at(&mut self, offset: usize, buff: &[u8]) -> Result<(),String> {
        let endpos = offset + buff.len();
        if endpos > self.buff.len() {
            unsafe {
                munmap(self.fd, self.buff.len());
                self.resize(endpos)?;
            }
        }

        for i in 0..buff.len() {
            self.buff[offset+i] = buff[i];
        }

        Ok(())
    }

    pub fn lines(&self, start: usize, end: usize) -> Result<&str,String> {
        let sbuff = str::from_utf8(self.buff)
                       .map_err(|e| format!("{}", e))?;

        let mut m = Matcher::new(sbuff);
        for i in 1..start {
            m = m.skip_after(|c| c == '\n');
            if m.tail().len() == 0 {
                return Err(format!("Start line {}:{} doesn't exist, there are only {} lines",
                                   self.name, start, i))
            }
        }

        let mut m2 = Matcher::new(m.tail);
        for _ in 0..(end-start) {
            m2 = m2.skip_after(|c| c == '\n');
        }

        let tail = m.cut_off(m2).as_tupple().0;
        Ok(tail)
    }
}

impl<'a> Drop for File<'a> {
    fn drop(&mut self) {
        unsafe {
            munmap(self.fd, self.buff.len());
        }
    }
}

fn null<T>() -> *mut T {
    0 as *mut T
}

#[repr(C)]
struct Stat {
    _st_dev: u64,
    _st_ino: u64,
    _st_mode: u32,
    _x1: u32,
    _st_nlink: u64,
    _st_uid: u32,
    _st_gid: u32,
    _st_rdev: u64,
    st_size: u64,
    _st_blksize: u64,
    _st_blocks: u64
}

impl Stat {
    fn zero () -> Self {
        Stat { _st_dev: 0, _st_ino: 0, _st_mode: 0, _x1: 0, _st_nlink: 0,
               _st_uid: 0, _st_gid: 0, _st_rdev: 0, st_size: 0, _st_blksize: 0,
               _st_blocks: 0 }
    }

    fn from_fd(fd: i32) -> Result<Self,String> {
        unsafe {
            let mut ans = Stat::zero();
            let ret = fstat(fd, &mut ans);
            if ret == 0 {
                Ok(ans)
            }
            else {
                Err(format!("Can't stat on {}.", fd))
            }
        }
    }

    fn size(&self) -> usize {
        self.st_size as usize
    }
}

#[test]
fn file_can_open_and_edit_file () {
    {
        let mut f = File::open(cstr!("/tmp/foo.x"), Mode::ReadWrite).unwrap();
        f.resize(0).unwrap();

        f.write_at(0, "foobaz\n".as_bytes()).unwrap();
        assert_eq!(f.buff.len(), 7);
        assert_eq!(f.as_text(), Ok("foobaz\n"));

        f.write_at(6, "coo\n".as_bytes()).unwrap();
        assert_eq!(f.buff.len(), 10);
        assert_eq!(f.as_text(), Ok("foobazcoo\n"));
    }

    let mut buff : [u8;40] = [0;40];
    let n = fs::File::open("/tmp/foo.x").unwrap()
              .read(&mut buff)
              .unwrap();

    assert_eq!(&buff[..n], "foobazcoo\n".as_bytes());
}

#[test]
fn file_can_select_lines () {
    let f = File::open(cstr!("test_data/file.rs"), Mode::ReadOnly)
                 .unwrap();

    assert_eq!(f.lines(10,13).unwrap(),
               concat!("    fn open(fname: *const i8, flags: i32, mode: i32) -> i32;\n",
                       "    fn fstat(fd: i32, buff: &mut Stat) -> i32;\n",
                       "    fn ftruncate(fd: i32, size: usize) -> i32;\n"));
}
