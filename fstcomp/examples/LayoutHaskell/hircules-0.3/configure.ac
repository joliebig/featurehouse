dnl hircules
dnl Copyright (c) 2003 Jens-Ulrik Petersen
dnl 
dnl This program is free software; you can redistribute it and/or
dnl modify it under the terms of the GNU General Public
dnl License as published by the Free Software Foundation,
dnl version 2.
dnl 
dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
dnl Library General Public License for more details.
dnl 
dnl You should have received a copy of the GNU General Public
dnl License along with this library (COPYING); if not, write to the Free
dnl Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

dnl ######################################################################
dnl Process this file with autoconf to produce a configure script.
dnl ######################################################################


dnl Initialise.
AC_INIT(hircules, 0.3)
AM_INIT_AUTOMAKE
AC_CONFIG_SRCDIR(Main.hs)
RELEASE=1

dnl Select a specific Haskell compiler and/or flags.
AC_ARG_WITH(hc, 
            [  --with-hc=HC            use Haskell compiler HC],
	    [HC=$withval])
AC_ARG_WITH(hcflags, 
            [  --with-hcflags=HCFLAGS  specify flags for Haskell compiler],
	    [HCFLAGS=$withval])
AC_ARG_WITH(lambdabot, 
            [  --with-lambdabot=PATH  use lambdabot modules],
	    [LAMBDABOT=$withval])

dnl Checks for programs.
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PATH_PROG(SED,sed)
AC_PATH_PROG(TAR,tar)
AC_PATH_PROG(TOUCH,touch)

AC_EXEEXT

dnl Haskell compiler
dnl (The next command is not executed, if $HC is already set.)
AC_PATH_PROG(HC, $HC ghc, ghc-not-found)
if test $HC = ghc-not-found; then
 AC_MSG_ERROR([
Could not find GHC!  This is currently the only supported compiler.
** Check \"http://haskell.org/ghc/\". **])
fi

dnl Needed substitution.
AC_SUBST(HCFLAGS)
AC_SUBST(RELEASE)
AC_SUBST(EXEEXT)
AC_SUBST(LAMBDABOT)

dnl write the results...
AC_OUTPUT([
  Makefile
  hircules.spec
  lib/Version.hs
])

dnl ...and chat with the user
echo "********************************************"
echo "Configuration completed successfully." 
echo "Next run \"make\"."
echo "********************************************"
