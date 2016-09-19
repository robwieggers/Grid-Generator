#  VERSION : 08.09.2016 21:37
#  based on the SOLPS makefiles

SRCDIR = ${TOPDIR}/src
DEFINES = -D__FILENAME__='"$(subst $(SRCDIR)/,,$(abspath $<))"'
SRCLOCAL = ${TOPDIR}/src/local
BASEDIR = ${TOPDIR}/base
OBJDIR = ${BASEDIR}/${OBJECTCODE}

ifeq ($(shell [ -d ${SRCLOCAL} ] && echo yes || echo no ),yes)
INCLUDE = -I${SRCLOCAL}
else
INCLUDE =
endif
#INCLUDE += -I${SRCDIR}/common -I${TOPDIR}/include.local -I${TOPDIR}/include

ifeq ($(shell [ -d ${SRCLOCAL} ] && echo yes || echo no ),yes)
VPATH =${SRCLOCAL}:${SRCDIR}/main:${SRCDIR}/configuration:${SRCDIR}/common:${SRCDIR}/quadrangular:${SRCDIR}/triangular
else
VPATH =${SRCDIR}/main:${SRCDIR}/configuration:${SRCDIR}/common:${SRCDIR}/quadrangular:${SRCDIR}/triangular
endif

ifeq ($(shell [ -e ${BASEDIR}/LISTOBJ ] && echo yes || echo no ),yes)
include ${BASEDIR}/LISTOBJ
endif
include ${TOPDIR}/config/compile
MAKES = ${TOPDIR}/Makefile ${TOPDIR}/config/compile ${TOPDIR}/config/compiler.${OBJECTCODE}

MODULES := ${patsubst ${SRCDIR}/*/%Mod.f90,%,${shell echo ${SRCDIR}/*/*Mod.f90}}
CLASSES := ${patsubst ${SRCDIR}/*/%Class.f90,%,${shell echo ${SRCDIR}/*/*Class.f90}}
#MODLOCASE:=$(shell echo $(MODULES)| tr A-Z a-z)
#MODMODS = ${MODLOCASE:%=${OBJDIR}/%.${MOD}}
MODMODS = ${MODULES:%.o=${OBJDIR}/%.${MOD}}
MODOBJS = ${MODULES:%.o=${OBJDIR}/%.o}
CLASSMODS = ${CLASSES:%.o=${OBJDIR}/%.${MOD}}
CLASSOBJS = ${CLASSES:%.o=${OBJDIR}/%.o}

ALLOBJS = ${OBJS:%.o=${OBJDIR}/%.o}

SRCF = ${OBJS:%.o=%.f90}

PROG_MN = gridGenerator.exe

EXCLUDELIST = ${patsubst %.exe, %.o, ${PROG_MN} }
EXELIST = ${patsubst %.exe, %.o, ${PROG_MN} }

MNEXE = ${patsubst %.exe, ${OBJDIR}/%.exe, ${PROG_MN}}

.PHONY: DEFAULT NOPLOT ALL clean realclean depend listobj tags
DEFAULT: ${MNEXE}
ALL: ${MNEXE}
NOPLOT: ${MNEXE}
MAIN: ${MNEXE}

MNEXTRA =

${MNEXE}: ${OBJDIR}/%.exe: ${OBJDIR}/%.o ${OBJDIR}/libgridgenerator.a ${MNEXTRA}
	${LD} ${LDOPTS} -o $@ $^ ${LDLIBES} ${LDOPTSend}

ifeq (${MOD},o)
${OBJDIR}/libgridgenerator.a: $(filter-out ${MODOBJS} ${CLASSOBJS},${ALLOBJS})
else
${OBJDIR}/libgridgenerator.a: $(filter-out ${MODOBJS} ${CLASSOBJS},${ALLOBJS})
endif
	${BLD} $@ $?

# targets 'clean' and 'realclean' clean up the directory.
clean : 
	-mkdir ${OBJDIR}/.delete
	-mv -i ${OBJDIR}/*.o ${OBJDIR}/*.${MOD} ${OBJDIR}/*.f90 ${OBJDIR}/.delete
	-rm -rf ${OBJDIR}/.delete &

realclean : clean
	rm -f ${OBJDIR}/*.a ${OBJDIR}/*.exe

depend: ${BASEDIR}/LISTOBJ ${OBJS:.o=.f90} ${EXELIST:.o=.f90}
	`which makedepend` -p'$${OBJDIR}/' ${DEFINES} -f- ${INCLUDE} $^ | \
	sed 's,^$${OBJDIR}/[^ ][^ ]*/,\$${OBJDIR}/,' > ${OBJDIR}/dependencies 
ifneq (${MOD},o)
	`which makedepend` -p'$${OBJDIR}/' ${DEFINES} -f- ${INCLUDE} $^ -o.${MOD} | \
	sed 's,^$${OBJDIR}/[^ ][^ ]*/,\$${OBJDIR}/,' >> ${OBJDIR}/dependencies 
endif
ifeq ($(shell [ -d ${SRCLOCAL} ] && echo yes || echo no ),yes)
	egrep -i '^ {2,}use [a-z]' ${SRCLOCAL}/*.f90 ${SRCDIR}/*/*.f90 | grep 'Class$$\|Mod$$' | grep -v '!IGNORE' | awk '{sub("\.f90",".o",$$1);sub("^.*/","$${OBJDIR}/",$$1); print $$1,"$${OBJDIR}/"$$3".${MOD}"}' >> ${OBJDIR}/dependencies
	egrep -i '^ {2,}use [a-z]' ${SRCLOCAL}/*.f90 ${SRCDIR}/*/*.f90 | grep 'Class$$\|Mod$$' | grep -v '!IGNORE' | awk '{sub("\.f90",".${MOD}",$$1);sub("^.*/","$${OBJDIR}/",$$1); print $$1,"$${OBJDIR}/"$$3".${MOD}"}' >> ${OBJDIR}/dependencies
#	egrep -i '^ {2,}use ' ${SRCLOCAL}/*.f90 ${SRCDIR}/*/*.f90 | grep -v 'IGNORE' | awk '{sub("\.f90",".o",$$1);sub("^.*/","$${OBJDIR}/",$$1); print $$1,"$${OBJDIR}/"$$3".${MOD}"}' >> ${OBJDIR}/dependencies
#	egrep -i '^ {2,}use ' ${SRCLOCAL}/*.f90 ${SRCDIR}/*/*.f90 | grep -v 'IGNORE' | awk '{sub("\.f90",".${MOD}",$$1);sub("^.*/","$${OBJDIR}/",$$1); print $$1,"$${OBJDIR}/"$$3".${MOD}"}' >> ${OBJDIR}/dependencies
else
	egrep -i '^ {2,}use [a-z]' ${SRCDIR}/*/*.f90 | grep 'Class\|Mod' | grep -v '!IGNORE' | awk '{sub("\.f90",".o",$$1);sub("^.*/","$${OBJDIR}/",$$1); print $$1,"$${OBJDIR}/"$$3".${MOD}"}' >> ${OBJDIR}/dependencies
	egrep -i '^ {2,}use [a-z]' ${SRCDIR}/*/*.f90 | grep 'Class\|Mod' | grep -v '!IGNORE' | awk '{sub("\.f90",".${MOD}",$$1);sub("^.*/","$${OBJDIR}/",$$1); print $$1,"$${OBJDIR}/"$$3".${MOD}"}' >> ${OBJDIR}/dependencies
#	egrep -i '^ {2,}use ' ${SRCDIR}/*/*.f90 | grep -v 'IGNORE' | awk '{sub("\.f90",".o",$$1);sub("^.*/","$${OBJDIR}/",$$1); print $$1,"$${OBJDIR}/"$$3".${MOD}"}' >> ${OBJDIR}/dependencies
#	egrep -i '^ {2,}use ' ${SRCDIR}/*/*.f90 | grep -v 'IGNORE' | awk '{sub("\.f90",".${MOD}",$$1);sub("^.*/","$${OBJDIR}/",$$1); print $$1,"$${OBJDIR}/"$$3".${MOD}"}' >> ${OBJDIR}/dependencies
endif

tags:
ifeq ($(shell [ -d ${SRCLOCAL} ] && echo yes || echo no ),yes)
	rm ${BASEDIR}/TAGS ; etags -o ${BASEDIR}/TAGS ${SRCLOCAL}/*.h ${SRCLOCAL}/*.f90 ${SRCDIR}/common/*.h ${SRCDIR}/*/*.f90
else
	rm ${BASEDIR}/TAGS ; etags -o ${BASEDIR}/TAGS ${SRCDIR}/common/*.h ${SRCDIR}/*/*.f90
endif

listobj:
	@rm -f ${BASEDIR}/LISTOBJ; touch ${BASEDIR}/LISTOBJ; l="OBJS ="; \
	for d in `echo "${VPATH}" | tr : \ `; do \
		l="$$l `(cd $$d > /dev/null; echo *.f90)`"; \
	done; \
	E="-e 's/\.f90/\.o/g'" ; for f in ${EXELIST}; do \
		E="$$E -e 's/ $$f//'"; \
	done; \
	echo "$$l" | eval sed "$$E" > ${BASEDIR}/LISTOBJ

${BASEDIR}/LISTOBJ: listobj

${OBJDIR}/dependencies: ${SRCDIR}
ifeq ($(shell [ -d ${BASEDIR} ] && echo yes || echo no ),no)
	-mkdir ${BASEDIR}
	echo ${BASEDIR}
endif
ifeq ($(shell [ -d ${OBJDIR} ] && echo yes || echo no ),no)
	-mkdir ${OBJDIR}
	echo ${OBJDIR}
endif
ifeq ($(shell [ -d ${SRCDIR}/${OBJECTCODE} ] && echo yes || echo no ),no)
#	ln -sf ${OBJDIR} ${SRCDIR}/${OBJECTCODE}
endif
	touch ${OBJDIR}/dependencies
	${MAKE} depend

include ${OBJDIR}/dependencies

echo:
#	@echo VPATH=${VPATH}
#	@echo OBJS=${OBJS}
#	@echo SOBJS=${SOBJS}
#	@echo DOBJS=${DOBJS}
	@echo MODULES=${MODULES}
	@echo MODOBJS=${MODOBJS}
	@echo MODMODS=${MODMODS}
	@echo CLASSES=${CLASSES}
	@echo CLASSOBJS=${CLASSOBJS}
	@echo CLASSMODS=${CLASSMODS}
	@echo $(filter-out ${MODOBJS},${ALLOBJS})
	@echo $(filter-out ${CLASSOBJS},${ALLOBJS})
	@echo $(filter-out ${MODOBJS} ${CLASSOBJS},${ALLOBJS})
#	@echo $(filter-out ${MODOBJS},${ALLOBJS})
	@echo EXCLUDELIST=${EXCLUDELIST}
	@echo EXELIST=${EXELIST}
#	@echo GREXE=${GREXE}
	@echo MNEXE=${MNEXE}
#	@echo XDEXE=${XDEXE}
#	@echo OTEXE=${OTEXE}
#	@echo ${SRCF}
#	@echo ${OBJDEST}
# DO NOT DELETE
