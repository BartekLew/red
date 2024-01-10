#!/bin/bash

# This is example run of cdefs bin in this project.
# The script takes C definitions on input and produces
# rust code need to use it from there.
# In following, ./x will produce rust binding for sys_stat struct.

set -ufex -o pipefail

cargo run --bin cdefs << !EOF > x.c
struct sys_stat {
               dev_t     st_dev;         /* ID of device containing file */
               ino_t     st_ino;         /* Inode number */
               mode_t    st_mode;        /* File type and mode */
               nlink_t   st_nlink;       /* Number of hard links */
               uid_t     st_uid;         /* User ID of owner */
               gid_t     st_gid;         /* Group ID of owner */
               dev_t     st_rdev;        /* Device ID (if special file) */
               off_t     st_size;        /* Total size, in bytes */
               blksize_t st_blksize;     /* Block size for filesystem I/O */
               blkcnt_t  st_blocks;      /* Number of 512B blocks allocated */
!EOF

gcc x.c -o x
./x
