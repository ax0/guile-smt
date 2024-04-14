AC_INIT([guile-smt], [0.1])
AM_INIT_AUTOMAKE([-Wall -Werror foreign -Wno-portability])
AUTOMAKE_OPTIONS = -Wno-portability

GUILE_SITE_DIR

wont_work=""

# guile-gdbm check
GUILE_MODULE_AVAILABLE(have_guile_gdbm, (gdbm))
AM_CONDITIONAL([guile_gdbm], [test "$have_guile_gdbm" = "yes"])

test $have_guile_gdbm = no &&
    wont_work="(smt db gdbm) $wont_work"

# Native hash library check
AC_CHECK_LIB(guile_smt_hash, c_hash_leaf, [have_guile_smt_hash=yes])
AM_CONDITIONAL([guile_smt_hash], [test "$have_guile_smt_hash" = "yes"])

test ! "$have_guile_smt_hash" = "yes" &&
    wont_work="(smt hash bn254 poseidon2) (smt instance bn254 poseidon2) $wont_work"

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])

AC_OUTPUT

if test ! "$wont_work" = "" ; then
    p="         ***"
    echo
    echo "$p"
    echo "$p NOTE:"
    echo "$p The following modules will not be compiled due to missing dependencies:"
    echo "$p   $wont_work"
    echo "$p"
    echo
fi


