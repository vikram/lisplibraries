
#redefine variables to you system
SHELL=/bin/sh
DB_HOME=/usr
DB_LIBDIR=${DB_HOME}/lib/
DB_INCDIR=${DB_HOME}/include/

BDB_LIB_INST_DIR=~/opt/lib

all: libbdb.so

install: libbdb.so
	cp libbdb.so ${BDB_LIB_INST_DIR}

uninstall:
	rm ${BDB_LIB_INST_DIR}/libbdb.so

libbdb.so: src/bdb.c
	gcc -shared -Wall -L${DB_LIBDIR} -I${DB_INCDIR} -fPIC -O3 -o $@ $< -ldb -lm
