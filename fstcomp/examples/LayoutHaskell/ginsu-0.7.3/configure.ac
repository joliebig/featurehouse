AC_INIT(ginsu,0.7.3)
AC_CONFIG_SRCDIR(Main.hs)
AM_INIT_AUTOMAKE(no-dependencies)
AM_CONFIG_HEADER([config.h])

AC_CANONICAL_HOST

dnl Let aclocal use macros in ./config directory
ACLOCAL="${ACLOCAL} -I ./ac-macros"


AC_PROGRAM_REQUIRE(hc,ghc,     [  --with-hc=<haskell compiler>    Specify location of ghc.])
AC_ARG_WITH(hcflags,
            [  --with-hcflags=HCFLAGS  specify flags for Haskell compiler],
	    [HCFLAGS=$withval])

AC_PROGRAM_REQUIRE(hsc2hs,hsc2hs,     [  --with-hsc2hs=<hsc2hs location>    Specify location of hsc2hs.])

AC_PROG_CC
AM_PROG_CC_C_O

mutt_cv_curses=/usr
AC_ARG_WITH(curses, [  --with-curses=DIR          Where ncurses is installed ],
        [if test $withval != yes; then
                mutt_cv_curses=$withval
        fi
        if test x$mutt_cv_curses != x/usr; then
                LDFLAGS="-L${mutt_cv_curses}/lib $LDFLAGS"
                CPPFLAGS="$CPPFLAGS -I${mutt_cv_curses}/include"
        fi])

AC_CHECK_FUNC(initscr,,[
cf_ncurses="ncurses"
for lib in ncurses ncursesw
do
        AC_CHECK_LIB($lib, waddnwstr, [cf_ncurses="$lib"; break])
done
AC_CHECK_LIB($cf_ncurses, initscr,
        [MUTTLIBS="$MUTTLIBS -l$cf_ncurses"
        if test "$cf_ncurses" = ncursesw; then
                AC_CHECK_HEADERS(ncursesw/ncurses.h,[cf_cv_ncurses_header="ncursesw/ncurses.h"])
        else
                AC_CHECK_HEADERS(ncurses/ncurses.h,[cf_cv_ncurses_header="ncurses/ncurses.h"],[
                        AC_CHECK_HEADERS(ncurses.h,[cf_cv_ncurses_header="ncurses.h"])
                ])
        fi],

        [CF_CURSES_LIBS])
        ])

# old_LIBS="$LIBS"
LIBS="$LIBS $MUTTLIBS"
CF_CHECK_FUNCDECLS([#include <${cf_cv_ncurses_header-curses.h}>], start_color typeahead bkgdset curs_set meta use_default_colors resizeterm)
if test "$ac_cv_func_decl_start_color" = yes; then
        AC_DEFINE(HAVE_COLOR,1,[ Define if your curses library supports color. ])
fi
# if test "$ac_cv_func_decl_resizeterm" = yes; then
#         MUTT_LIB_OBJECTS="$MUTT_LIB_OBJECTS resize.o"
# fi
# LIBS="$old_LIBS"

dnl AC_CHECK_CURSES
dnl CHECK_ZLIB

AC_caolan_CHECK_PACKAGE(openssl,EVP_SealInit,crypto,openssl/crypto.h,, [
	AC_MSG_ERROR(cannot find crypto library; is OpenSSL installed?)
])

AC_PROG_INSTALL
AC_PATH_PROGS(SH, sh)
AC_PATH_PROGS(RM, rm)


OPT_ARGS="-O2 -fvia-C"

AC_ARG_ENABLE(unopt,[  --enable-unopt         disable optimization],OPT_ARGS="-fvia-C")

AC_ARG_ENABLE(profile,[  --enable-profile        compile in profiling support],PROFILE_ARGS="-prof -auto-all -osuf .prof.o -hisuf .prof.hi ")

AC_SUBST(HC)
AC_SUBST(HCFLAGS)
AC_SUBST(HSC2HS)
AC_SUBST(PROFILE_ARGS)
AC_SUBST(OPT_ARGS)

AC_SUBST(CURSES_LIBS)
AC_SUBST(CURSES_INCLUDEDIR)

AC_CHECK_FUNCS([waddnwstr mbsnrtowcs wcsnrtombs use_default_colors resizeterm])



AC_OUTPUT(
	Makefile
	Version.hs
	ginsu.spec
)
