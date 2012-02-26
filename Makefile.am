bin_PROGRAMS = bschpb

bschpb_SOURCES = bsch.c interp.c types.c read.c \
	gc.c vm.c hashtab.c bffi.c pool.c \
	socket.c tlsf.c fancystack.c interp.h types.h \
	read.h gc.h vm.h hashtab.h bffi.h \
	pool.h socket.h tlsf.h fancystack.h \
	*.sch clos/*.sch tests/*.sch

AM_CPPFLAGS = $(libffi_CFLAGS)
bschpb_LDADD = $(libffi_LIBS) -lltdl -lz -lm # fixme: move to configure.ac
