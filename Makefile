.POSIX:
.SUFFIXES: .o .c

PREFIX = /usr/local
BINDIR = $(PREFIX)/bin

SRC      = main.c util.c parse.c cfg.c mem.c ssa.c alias.c load.c copy.c \
           fold.c live.c spill.c rega.c gas.c
AMD64SRC = amd64/targ.c amd64/sysv.c amd64/isel.c amd64/emit.c
ARM64SRC = arm64/targ.c arm64/abi.c arm64/isel.c arm64/emit.c
RV64SRC  = rv64/targ.c rv64/abi.c rv64/isel.c rv64/emit.c
SRCALL   = $(SRC) $(AMD64SRC) $(ARM64SRC) $(RV64SRC)

AMD64OBJ = $(AMD64SRC:.c=.o)
ARM64OBJ = $(ARM64SRC:.c=.o)
RV64OBJ  = $(RV64SRC:.c=.o)
OBJ      = $(SRC:.c=.o) $(AMD64OBJ) $(ARM64OBJ) $(RV64OBJ)

CFLAGS = $(CPPFLAGS) -Wall -Wextra -std=c99 -g -Wpedantic

qbe: $(OBJ)
	$(CC) $(LDFLAGS) $(OBJ) -o $@

.c.o:
	$(CC) $(CFLAGS) -c $< -o $@

$(OBJ): all.h ops.h
$(AMD64OBJ): amd64/all.h
$(ARM64OBJ): arm64/all.h
$(RV64OBJ): rv64/all.h
main.o: config.h

config.h:
	@case `uname` in                               \
	*Darwin*)                                      \
		echo "#define Defasm Gasmacho";        \
		echo "#define Deftgt T_amd64_sysv";    \
		;;                                     \
	*)                                             \
		echo "#define Defasm Gaself";          \
		case `uname -m` in                     \
		*aarch64*)                             \
			echo "#define Deftgt T_arm64"; \
			;;                             \
		*riscv64*)                             \
			echo "#define Deftgt T_rv64";  \
			;;                             \
		*)                                     \
			echo "#define Deftgt T_amd64_sysv";\
			;;                             \
		esac                                   \
		;;                                     \
	esac > $@

install: qbe
	mkdir -p "$(DESTDIR)$(BINDIR)"
	install -m755 qbe "$(DESTDIR)$(BINDIR)/qbe"

uninstall:
	rm -f "$(DESTDIR)$(BINDIR)/qbe"

clean:
	rm -f *.o */*.o qbe

clean-gen: clean
	rm -f config.h

check: qbe
	tools/test.sh all

check-arm64: qbe
	TARGET=arm64 tools/test.sh all

check-rv64: qbe
	TARGET=rv64 tools/test.sh all

src:
	@echo $(SRCALL)

80:
	@for F in $(SRCALL);                       \
	do                                         \
		awk "{                             \
			gsub(/\\t/, \"        \"); \
			if (length(\$$0) > $@)     \
				printf(\"$$F:%d: %s\\n\", NR, \$$0); \
		}" < $$F;                          \
	done

.PHONY: clean clean-gen check check-arm64 src 80 install uninstall
