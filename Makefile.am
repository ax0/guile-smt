include build-ax/guile.am

SOURCES =					\
	smt.sld					\
	smt/aux.sld				\
	smt/db.sld				\
	smt/instance.sld			\
	smt/instance/bn254.sld			\
	smt/proof.sld				\
	smt/types.sld

if guile_gdbm
SOURCES += smt/db/gdbm.sld
endif

if guile_smt_hash
SOURCES +=					\
	smt/hash/bn254/poseidon2.sld		\
	smt/instance/bn254/poseidon2.sld
endif

EXTRA_DIST =					\
	README.org				\
	autogen.sh
