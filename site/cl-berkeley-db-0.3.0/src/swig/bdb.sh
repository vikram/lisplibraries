
swig -cffi -importall -I/usr/include -I/usr/include/linux -I/usr/lib/gcc/i686-pc-linux-gnu/4.1.1/include -generate-typedef bdb.i 
ex cffi-oracle-bdb.lisp -s << EOF
:/;SNIP-START/,/;SNIP-END/d
:/;;;SWIG wrapper code starts here/,/;;;SWIG wrapper code ends here/d
:wq
EOF
mv cffi.lisp ..
#swig -cffi -importall -I/usr/include -I/usr/include/linux -I/usr/lib/gcc/i686-pc-linux-gnu/4.1.1/include -generate-typedef -dump_top db.i
#swig -sexp -importall -I/usr/include -I/usr/include/linux -I/usr/lib/gcc/i686-pc-linux-gnu/4.1.1/include db2.i
