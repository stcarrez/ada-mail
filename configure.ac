dnl Autoconf configure script for mail

dnl Process this file with autoconf to produce a configure script.

AC_INIT(src/mail.ads)

# Current release settings
MAIL_MAJOR_VERSION=0
MAIL_MINOR_VERSION=1
MAIL_MICRO_VERSION=0
MAIL_VERSION=$MAIL_MAJOR_VERSION.$MAIL_MINOR_VERSION.$MAIL_MICRO_VERSION

# Set the version number of the project
AC_SUBST(MAIL_VERSION)
AC_SUBST(MAIL_MAJOR_VERSION)
AC_SUBST(MAIL_MINOR_VERSION)
AC_SUBST(MAIL_MICRO_VERSION)

AM_GNAT_LIBRARY_PROJECT(mail)

AM_GNAT_FIND_ADA_UTIL([mail])

AC_OUTPUT(
Makefile.conf
mailada.gpr
mailada_tests.gpr
mailada_tools.gpr
)
