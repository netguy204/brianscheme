bin_PROGRAMS = bschpb

bschpb_SOURCES = bsch.c interp.c types.c read.c gc.c vm.c hashtab.c bffi.c pool.c socket.c tlsf.c

AM_CPPFLAGS = $(libffi_CFLAGS)
bschpb_LDADD = $(libffi_LIBS) -lltdl -lz # fixme: move to configure.ac
