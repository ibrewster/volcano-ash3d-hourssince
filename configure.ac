#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([volcano-ash3d-hourssince], [1.0], [BUG-REPORT-ADDRESS])

# Checks for programs.
AC_PROG_AWK
AC_PATH_PROG([GFORTRAN_DIR],[gfortran])
AC_SUBST([GFORTRAN_DIR])

#Check for /usr/local/ Directory
AC_CHECK_FILE([/usr/local],[OTHERLIBS="-L/usr/local/lib"; OTHERINC="-I/usr/local/include";])
AC_SUBST([OTHERLIBS])
AC_SUBST([OTHERINC])

# Checks for libraries.
LDFLAGS=${LDFLAGS_BACKUP}

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

AC_CONFIG_FILES([makefile])
AC_OUTPUT
