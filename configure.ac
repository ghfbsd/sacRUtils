dnl Configure input for SAC R tools - George Helffrich/U. Bristol
AC_INIT(sacRUtils,0.1)
dnl AC_CONFIG_AUX_DIR(utils)
AC_PROG_CC(cc gcc)
AC_PROG_F77(gfortran g77 f77)
AC_CHECK_PROG(HAVE_R,R,yes,no)
AC_CHECK_PROG(HAVE_SAC,sac,yes,no)
test $HAVE_R = no && AC_MSG_WARN(No R installed -- hope this is ok...)
test $HAVE_SAC = no && AC_MSG_WARN(No SAC installed -- hope this is ok...)
dnl AC_PROG_RANLIB
dnl AC_PROG_INSTALL
dnl AC_DEFINE([HAVE_LIBXML],[0],[whether using libxml])
AC_F77_LIBRARY_LDFLAGS
dnl Have to undo damage done by F77_LIBRARY_LDFLAGS on MacOS systems
KLUDGE= KEEP=
for L in $ac_cv_flibs $ac_cv_f77_libs ; do
   case $L in
   -l*) ;;
   -L*) KEEP=1 ;;
   *) ;;
   esac
   test -z "${KEEP}" || KLUDGE="${KLUDGE} $L"
done
sym=neither
test -z "$ac_cv_flibs" || sym=ac_cv_flibs
test -z "$ac_cv_f77_libs"  || sym=ac_cv_f77_libs
: ${sym}="${KLUDGE}" ; FLIBS="${KLUDGE}"
dnl Set default FFLAGS to what we want
KEEP=
for L in $FFLAGS ; do
   case $L in
   -g* | -O*) ;;
   *) KEEP="${KEEP} $L"
   ;;
   esac
done
FFLAGS=`echo "${KEEP}"`

AC_SUBST(FLIBS)
AC_SUBST(FRTLIBS)
dnl AC_SUBST(ADD_CFLAGS)
AC_SUBST(SACDIR)
AC_ARG_VAR(BINDIR,[SAC program and utility executable code])
AC_ARG_VAR(LIBDIR,[SAC library code (sac.a)])
AC_ARG_VAR(SACAUX,[SAC auxiliary files (defaults to LIBDIR/aux)])
SACDIR=$PWD
BINDIR=${BINDIR:-${exec_prefix}}
LIBDIR=${LIBDIR:-${libdir}}
SACAUX=${SACAUX:-${LIBDIR}/aux}

dnl Additional flags based on system type
if (uname -s > /dev/null) ; then
   case `uname -s` in
   Darwin) : # HAVE_COCOA=mac
      case `uname -r` in
      7*) : # LIBS="${LIBS} -lcc_dynamic"
        ;;
      8*) # LIBCRT="-lcrt1.o"
        ;;
      esac
      ;;
   *) ;;
   esac
   case `uname -s` in
   FreeBSD|Linux) : ;; # ADD_CFLAGS="${ADD_CFLAGS} -DTRY_PROC" ;;
   *) ;;
   esac
   case `uname -r -s` in
   "SunOS 4.1."*) : ;; # FOPT=-O ADD_CFLAGS="${ADD_CFLAGS} -DDUMMY_XRTNS" ;;
   *) ;;
   esac
fi

dnl Library routines
AC_PATH_XTRA

dnl Fortran/C interoperability
dnl Check whether fortran compiler is 32 or 64 bit mode by default; adjust
dnl CFLAGS if so.
dnl AC_LANG_PUSH([Fortran 77])
dnl AC_LANG_CONFTEST([      end])
dnl ARCHBITS=32
dnl for arg in `$F77 $FFLAGS -v -o conftest conftest.f 2>&1 | $EGREP 'as[[ 	]]+-arch ' | awk '{print $3}'` ; do
dnl    case "${arg}" in
dnl       i386) CFLAGS="${CFLAGS} -m32";;
dnl       x86_64) CFLAGS="${CFLAGS} -m64" ARCHBITS="${ARCHBITS} 64";;
dnl       ppc64) CFLAGS="${CFLAGS} -m64"  ARCHBITS="${ARCHBITS} 64";;
dnl    esac
dnl    EXARCH="${arg}"
dnl done
dnl AC_MSG_CHECKING(Fortran symbol naming)
dnl AC_F77_FUNC(init_sac,SYMBOL)
dnl case "$SYMBOL" in
dnl    init_sac_) 
dnl       AC_MSG_RESULT(they end with _ - fine)
dnl       ;;
dnl    init_sac__) ADD_CFLAGS="${ADD_CFLAGS} -DFSYMSFX=2"
dnl       AC_MSG_RESULT(they end with __ - fine)
dnl       ;;
dnl    init_sac) ADD_CFLAGS="${ADD_CFLAGS} -DIBM"
dnl       AC_MSG_RESULT(they are unchanged - fine)
dnl       ;;
dnl    *) AC_MSG_RESULT(no known method - good luck!)
dnl       ;;
dnl esac
dnl AC_LANG_POP([Fortran 77])
dnl Locate -lg2c and -lfrtbegin on g77/gfortran systems
gver=`gfortran -v 2>&1 | awk '/gcc version.*GCC/{print $3}'`
for L in ${FLIBS} ; do
   case $L in
   -L*) LI=`echo $L | awk '{print substr($0,3)}'`
      echo $FFLAGS | fgrep -q -- -m64 && LI="${LI}/x86_64"
      # test -f $LI/libg2c.a && FRTLIBS="${FRTLIBS} $LI/libg2c.a"
      # test -f $LI/libfrtbegin.a && FRTLIBS="${FRTLIBS} $LI/libfrtbegin.a"
      test -f $LI/libgfortran.a && FRTLIBS="${FRTLIBS} $LI/libgfortran.a"
      # test -f $LI/libfortranbegin.a && FRTLIBS="${FRTLIBS} $LI/libforrtranbegin.a"
      case "${gver}" in
         4.[1-5]*)            ##gfortran build fault
            # test -f $LI/libgcc_eh.a && FRTLIBS="${FRTLIBS} $LI/libgcc_eh.a"
            ;;
      esac
      ;;
   *) ;;
   esac
done
test "${FLIBS-unset}" = unset && FLIBS=
test "${FRTLIBS-unset}" = unset && FRTLIBS=
test _$F77 = _gfortran && FRTLIBS="${FRTLIBS} -static-libgfortran"

dnl SAC library routine checks
dnl This fails due to mismatch between endianness of lib and C compiler - skip
dnl AC_CHECK_LIB(xapiir,xapiir_,[],[],[${FRTLIBS}])

dnl AC_LANG_PUSH([Fortran 77])
dnl SV_FFLAGS="${FFLAGS}" F77BC=
dnl Check for Fortran bounds-check option
dnl for F in fbounds-check C unavailable-option ; do
dnl    FFLAGS="-c -$F"
dnl AC_TRY_COMPILE([],[
dnl       integer array(2)
dnl       array(1)=1
dnl       array(2)=2
dnl ],[break])
dnl done
dnl test "$F" = "unavailable-option" || F77BC="-$F"
dnl Check for Fortran backslash option
dnl FFLAGS="-c -fbackslash"
dnl AC_TRY_COMPILE([],[
dnl       i = index(' \t','t')
dnl ],[ADD_FFLAGS="-fbackslash"],[ADD_FFLAGS=])
dnl Check for Open-MP capable Fortran
dnl HAVE_OPENMP=without FLAG_OPENMP=DUMMY
dnl for F in fopenmp xopenmp openmp mp omp qsmp=omp unavailable-option ; do
dnl    FFLAGS="-c -$F"
dnl AC_TRY_COMPILE([],[
dnl       implicit none
dnl C$    logical test
dnl       test = .true.
dnl ],[break])
dnl done
dnl test "$F" = "unavailable-option" || \
dnl    ADD_FFLAGS="${ADD_FFLAGS} -$F" \
dnl    HAVE_OPENMP=with FLAG_OPENMP=-$F
dnl Check Fortran for brief debug info availability
dnl FFLAGS="-g -feliminate-unused-debug-symbols"
dnl AC_TRY_COMPILE([],[
dnl       integer array(2)
dnl       array(1)=1
dnl       array(2)=2
dnl ],[FDEBUG=" -feliminate-unused-debug-symbols"],[FDEBUG=])
dnl Check for need for Fortran equivalence/common table size options.
dnl g77 ignores -N options with a warning, but does not fail to compile.
dnl Thus, the check is kludgey here.
dnl rm -f conftest.$ac_objext conftest.$ac_ext
dnl F77SIZE="-Nq300 -Nn2000"
dnl cat >conftest.$ac_ext <<_ACEOF
dnl       program main
dnl       end
dnl _ACEOF
dnl KEEP=`eval "$F77 -c ${F77SIZE} conftest.$ac_ext 2>&1  1>/dev/null"`
dnl if test $? -eq 0 -a -s conftest.$ac_ext ; then
dnl    for L in $KEEP ; do
dnl       case $L in
dnl         unrecognized) F77SIZE= ;;
dnl         *) ;;
dnl       esac
dnl    done
dnl else
dnl    F77SIZE=
dnl fi
dnl rm -f conftest.$ac_objext conftest.$ac_ext
dnl AC_LANG_POP(Fortran 77)
dnl FFLAGS="${SV_FFLAGS} ${F77SIZE}"

dnl Optional compilation defines
dnl AC_ARG_ENABLE(debug,  
dnl    [  --enable-debug          compiles programs with debugging enabled]
dnl )
dnl AC_ARG_ENABLE(opt,  
dnl    [  --disable-opt           compiles programs with optimizing disabled]
dnl )

AC_OUTPUT(Makefile)
echo "Configured with Fortran libraries" $FLIBS $FRTLIBS
