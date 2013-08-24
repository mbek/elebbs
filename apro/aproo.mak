#
#                            APROO.MAK 2.03
#             Make file used to build Async Professional 2.03
#                          (OOP interface)
#
# This make file is designed to be used only with Borland's MAKE utility; it
# does NOT work with Microsoft's MAKE. To use this file, enter
#
#     MAKE -fAPROO.MAK
#
# at the DOS command line.
#
# --------------------------------------------------------------------- Defines

# Set this to 1 for Turbo Assembler, or comment it out for Microsoft Assembler.
tasm=1

# Set this (in addition to the above) to use TASM 2.0 or later
tasm2=1

# Uncomment the following line if you have an assembler.
HaveAssembler=1

# Uncomment the next line to build protected mode units and demos
# pmode=1

# ------------------------------------------------------------------- Macros
# If your assembler or compiler is not located in a directory on the DOS path,
# or if you are running under DOS 2.x, insert the complete pathname of the
# relevant executable file in the following definitions. For example, if
# your copy of BPC.EXE is located in the directory C:\BP\BIN then you would
# change the following line to
#
#     compile=C:\BP\BIN\BPC.EXE /M /$D- /$L- /L
#
# You may also want to change the default compilation directives by adding them
# at the end of command line.

!if $d(tasm)
assemble=TASM.EXE
!else
assemble=MASM.EXE
!endif

!if $d(tasm2)
mpass=/m2
!else
mpass=
!endif

# You may wish to modify these command line options or compiler names.
# Note that if you switch BPC to TPC you must remove the /CD option below.
!if $d(pmode)
compile=BPC.EXE /CP /M /$D- /$L- /L
!else
compile=BPC.EXE /CD /M /$D- /$L- /L
!endif

# Specify the path of the utility used to convert a binary file into an
# OBJ file suitable for linking. This is used only for APFAX.FNT here.

binobj=BINOBJ.EXE

# ---------------------------------------- Force evaluation of all dependencies

!if $d(pmode)
dummy: allobj alltpp alldemo
!else
dummy: allobj alltpu alldemo
!endif

allobj: apuart1.obj apuart2.obj apuart3.obj apuart4.obj apuart5.obj \
        apuart6.obj apuart7.obj apuart9.obj oolzh.obj \
        fastw1.obj aplha.obj apfax.obj

!if $d(pmode)
alltpp: apmisc.tpp apport.tpp apuart.tpp apint14.tpp \
        oocom.tpp oomodem.tpp ooxmodem.tpp ooymodem.tpp \
        oozmodem.tpp ookermit.tpp apansi.tpp aptimer.tpp \
        ooascii.tpp apmisc.tpp ooarchiv.tpp oolzh.tpp \
        oozip.tpp ootfdd.tpp apdigi14.tpp apfossil.tpp \
        ooemu.tpp \
        oofaxcvt.tpp ooabsfax.tpp oofax12.tpp oofaxcas.tpp oofaxprn.tpp \
        ooini.tpp ooinidb.tpp oomoddb.tpp oomodem2.tpp

!else

alltpu: apmisc.tpu apport.tpu apuart.tpu apint14.tpu \
        oocom.tpu oomodem.tpu ooxmodem.tpu ooymodem.tpu \
        oozmodem.tpu ookermit.tpu apansi.tpu aptimer.tpu \
        ooascii.tpu apmisc.tpu ooarchiv.tpu oolzh.tpu \
        oozip.tpu ootfdd.tpu apdigi14.tpu apfossil.tpu \
        ooemu.tpu \
        oofaxcvt.tpu ooabsfax.tpu oofax12.tpu oofaxcas.tpu oofaxprn.tpu \
        ooini.tpu ooinidb.tpu oomoddb.tpu oomodem2.tpu

!endif

alldemo: comtesto.exe fxo.exe uartid.exe \
         lzhvo.exe lzhxo.exe lzho.exe \
         zipvo.exe zipxo.exe zipo.exe \
         simpcom.exe \
         simprcvo.exe simpsndo.exe \
         cvt2faxo.exe prnfaxo.exe \
         fax2pcxo.exe showfaxo.exe \
         moddemo.exe

# -------------------------------------------------------------- Implicit rules

!if $d(HaveAssembler)
.asm.obj:
  $(assemble) $.;
!endif

.pas.exe:
  $(compile) $*

.pas.tpu:
  $(compile) $*

.pas.tpp:
  $(compile) $*

# ---------------------------------------------------- Assembly Language Source

!if $d(HaveAssembler)

apuart1.obj : apuart.asm apconfig.1
  copy apconfig.1 apconfig.asm
  $(assemble) $(mpass) apuart.asm, apuart1.obj ;

apuart2.obj : apuart.asm apconfig.2
  copy apconfig.2 apconfig.asm
  $(assemble) $(mpass) apuart.asm, apuart2.obj ;

apuart3.obj : apuart.asm apconfig.3
  copy apconfig.3 apconfig.asm
  $(assemble) $(mpass) apuart.asm, apuart3.obj ;

apuart4.obj : apuart.asm apconfig.4
  copy apconfig.4 apconfig.asm
  $(assemble) $(mpass) apuart.asm, apuart4.obj ;

apuart5.obj : apuart.asm apconfig.5
  copy apconfig.5 apconfig.asm
  $(assemble) $(mpass) apuart.asm, apuart5.obj ;

apuart6.obj : apuart.asm apconfig.6
  copy apconfig.6 apconfig.asm
  $(assemble) $(mpass) apuart.asm, apuart6.obj ;

apuart7.obj : apuart.asm apconfig.7
  copy apconfig.7 apconfig.asm
  $(assemble) $(mpass) apuart.asm, apuart7.obj ;

apuart9.obj : apuart.asm apconfig.9
  copy apconfig.9 apconfig.asm
  $(assemble) $(mpass) apuart.asm, apuart9.obj ;

fastw1.obj: fastw1.asm

oolzh.obj: oolzh.asm

aplha.obj: aplha.asm

apfax.obj: apfax.fnt
  $(binobj) apfax.fnt apfax.obj BoundFont

!endif

# ---------------------------------------------------- Library Units

!if $d(pmode)
apmisc.tpp: apmisc.pas apmisc.pa0 apmisc.pa1 apdefine.inc

aptimer.tpp: aptimer.pas aptimer.pa0 apdefine.inc

apport.tpp: apport.pas apport.pa0 apmisc.tpp apdefine.inc

apuart.tpp: apuart.pas apuart.pa0 apuart1.obj apuart2.obj apuart3.obj \
            apuart4.obj apuart5.obj apuart6.obj apuart7.obj apuart9.obj \
            apdefine.inc apmisc.tpp apport.tpp

apint14.tpp: apint14.pas apint14.pa0 apdefine.inc apmisc.tpp apport.tpp

apfossil.tpp: apfossil.pas apfossil.pa0 apdefine.inc apmisc.tpp apport.tpp

apdigi14.tpp: apdigi14.pas apdigi14.pa0 apdefine.inc apmisc.tpp apport.tpp

apansi.tpp: apansi.pas apmisc.tpp apport.tpp

oocom.tpp: oocom.pas oocom.pa1 oocom.pa2 apdefine.inc \
           apmisc.tpp apport.tpp aptimer.tpp \
           apuart.tpp apint14.tpp apfossil.tpp apdigi14.tpp

ooabspcl.tpp: ooabspcl.pas apdefine.inc apport.tpp apmisc.tpp \
              aptimer.tpp oocom.tpp

ooxmodem.tpp: ooxmodem.pas apdefine.inc apport.tpp apmisc.tpp \
              aptimer.tpp oocom.tpp ooabspcl.tpp

ooymodem.tpp: ooymodem.pas apdefine.inc apport.tpp apmisc.tpp \
              aptimer.tpp oocom.tpp ooabspcl.tpp ooxmodem.tpp

oozmodem.tpp: oozmodem.pas oozmodem.pa1 oozmodem.pa2 apdefine.inc \
              apport.tpp apmisc.tpp aptimer.tpp oocom.tpp ooabspcl.tpp

ookermit.tpp: ookermit.pas apdefine.inc ookermit.pa1 ookermit.pa2 \
              apport.tpp apmisc.tpp aptimer.tpp oocom.tpp ooabspcl.tpp

ooascii.tpp: ooascii.pas apdefine.inc apport.tpp apmisc.tpp \
             aptimer.tpp oocom.tpp ooabspcl.tpp

oomodem.tpp: oomodem.pas apdefine.inc oomodem.pa1 \
             apport.tpp apmisc.tpp aptimer.tpp oocom.tpp

ooemu.tpp: ooemu.pas apdefine.inc apmisc.tpp

termwin.tpp: termwin.pas apdefine.inc termwin.pcd \
             apport.tpp apuart.tpp apmisc.tpp \
             oocom.tpp ooemu.tpp

comutil.tpp: comutil.pas apdefine.inc apport.tpp oocom.tpp oomodem.tpp \
             ooabspcl.tpp ooxmodem.tpp

ooarchiv.tpp: ooarchiv.pas apdefine.inc apmisc.tpp

oolzh.tpp: oolzh.pas oolzh.pa1 oolzh.pa2 oolzh.pa3 \
           oolzh.obj aplha.obj \
           apmisc.tpp ooarchiv.tpp

oozip.tpp: oozip.pas apdefine.inc oozip.pa1 oozip.pa2 \
           oozip.pa3  oozip.pa4 apmisc.tpp ooarchiv.tpp

ootfdd.tpp: ootfdd.pas apdefine.inc apmisc.tpp apport.tpp oocom.tpp

oofaxcvt.tpp: oofaxcvt.pas apdefine.inc apmisc.tpp

ooabsfax.tpp: ooabsfax.pas apdefine.inc apmisc.tpp aptimer.tpp \
              apport.tpp oocom.tpp oofaxcvt.tpp

oofax12.tpp: oofax12.pas apdefine.inc apmisc.tpp aptimer.tpp \
             apport.tpp oocom.tpp ooabsfax.tpp oofaxcvt.tpp

oofaxcas.tpp: oofaxcas.pas apdefine.inc apmisc.tpp aptimer.tpp \
              apport.tpp oocom.tpp ooabsfax.tpp oofaxcvt.tpp

oofaxprn.tpp: oofaxprn.pas apdefine.inc apmisc.tpp oofaxcvt.tpp

ooini.tpp: ooini.pas apmisc.tpp

ooinidb.tpp: ooinidb.pas apmisc.tpp \
             apport.tpp ooini.tpp

oomoddb.tpp: oomoddb.pas apmisc.tpp \
             apport.tpp ooinidb.tpp

oomodem2.tpp: oomodem2.pas apmisc.tpp \
              apport.tpp aptimer.tpp oocom.tpp \
              oomoddb.tpp
!else
apmisc.tpu: apmisc.pas apmisc.pa0 apmisc.pa1 apdefine.inc

aptimer.tpu: aptimer.pas aptimer.pa0 apdefine.inc

apport.tpu: apport.pas apport.pa0 apmisc.tpu apdefine.inc

apuart.tpu: apuart.pas apuart.pa0 apuart1.obj apuart2.obj apuart3.obj \
            apuart4.obj apuart5.obj apuart6.obj apuart7.obj apuart9.obj \
            apdefine.inc apmisc.tpu apport.tpu

apint14.tpu: apint14.pas apint14.pa0 apdefine.inc apmisc.tpu apport.tpu

apfossil.tpu: apfossil.pas apfossil.pa0 apdefine.inc apmisc.tpu apport.tpu

apdigi14.tpu: apdigi14.pas apdigi14.pa0 apdefine.inc apmisc.tpu apport.tpu

apansi.tpu: apansi.pas apmisc.tpu apport.tpu

oocom.tpu: oocom.pas oocom.pa1 oocom.pa2 apdefine.inc \
           apmisc.tpu apport.tpu aptimer.tpu \
           apuart.tpu apint14.tpu apfossil.tpu apdigi14.tpu

ooabspcl.tpu: ooabspcl.pas apdefine.inc apport.tpu apmisc.tpu \
              aptimer.tpu oocom.tpu

ooxmodem.tpu: ooxmodem.pas apdefine.inc apport.tpu apmisc.tpu \
              aptimer.tpu oocom.tpu ooabspcl.tpu

ooymodem.tpu: ooymodem.pas apdefine.inc apport.tpu apmisc.tpu \
              aptimer.tpu oocom.tpu ooabspcl.tpu ooxmodem.tpu

oozmodem.tpu: oozmodem.pas oozmodem.pa1 oozmodem.pa2 apdefine.inc \
              apport.tpu apmisc.tpu aptimer.tpu oocom.tpu ooabspcl.tpu

ookermit.tpu: ookermit.pas apdefine.inc ookermit.pa1 ookermit.pa2 \
              apport.tpu apmisc.tpu aptimer.tpu oocom.tpu ooabspcl.tpu

ooascii.tpu: ooascii.pas apdefine.inc apport.tpu apmisc.tpu \
             aptimer.tpu oocom.tpu ooabspcl.tpu

oomodem.tpu: oomodem.pas apdefine.inc oomodem.pa1 \
             apport.tpu apmisc.tpu aptimer.tpu oocom.tpu

ooemu.tpu: ooemu.pas apdefine.inc apmisc.tpu

termwin.tpu: termwin.pas apdefine.inc termwin.pcd \
             apport.tpu apuart.tpu apmisc.tpu \
             oocom.tpu ooemu.tpu

comutil.tpu: comutil.pas apdefine.inc apport.tpu oocom.tpu oomodem.tpu \
             ooabspcl.tpu ooxmodem.tpu

ooarchiv.tpu: ooarchiv.pas apdefine.inc apmisc.tpu

oolzh.tpu: oolzh.pas oolzh.pa1 oolzh.pa2 oolzh.pa3 \
           oolzh.obj aplha.obj \
           apmisc.tpu ooarchiv.tpu

oozip.tpu: oozip.pas apdefine.inc oozip.pa1 oozip.pa2 \
           oozip.pa3 oozip.pa4 apmisc.tpu ooarchiv.tpu

ootfdd.tpu: ootfdd.pas apdefine.inc apmisc.tpu apport.tpu oocom.tpu

oofaxcvt.tpu: oofaxcvt.pas apdefine.inc apmisc.tpu

ooabsfax.tpu: ooabsfax.pas apdefine.inc apmisc.tpu aptimer.tpu \
              apport.tpu oocom.tpu oofaxcvt.tpu

oofax12.tpu: oofax12.pas apdefine.inc apmisc.tpu aptimer.tpu \
             apport.tpu oocom.tpu ooabsfax.tpu oofaxcvt.tpu

oofaxcas.tpu: oofaxcas.pas apdefine.inc apmisc.tpu aptimer.tpu \
              apport.tpu oocom.tpu ooabsfax.tpu oofaxcvt.tpu

oofaxprn.tpu: oofaxprn.pas apdefine.inc apmisc.tpu oofaxcvt.tpu

ooini.tpu: ooini.pas apmisc.tpu

ooinidb.tpu: ooinidb.pas apmisc.tpu \
             apport.tpu ooini.tpu

oomoddb.tpu: oomoddb.pas apmisc.tpu \
             apport.tpu ooinidb.tpu

oomodem2.tpu: oomodem2.pas apmisc.tpu \
              apport.tpu aptimer.tpu oocom.tpu \
              oomoddb.tpu

!endif

# ---------------------------------------------------- Demo Programs

!if $d(pmode)
comtesto.exe: apport.tpp apuart.tpp apint14.pas apuart1.obj \
              apuart2.obj apuart3.obj apuart4.obj apuart5.obj \
              apuart6.obj apuart7.obj apuart9.obj \
              apmisc.tpp aptimer.tpp apdefine.inc oocom.tpp \
              comtesto.pas

fxo.exe: apdefine.inc fxo.pas \
         apmisc.tpp aptimer.tpp apport.tpp \
         apuart.tpp apuart1.obj apuart2.obj apuart3.obj \
         apuart4.obj apuart5.obj apuart6.obj apuart7.obj apuart9.obj \
         oocom.tpp ooabspcl.tpp ooxmodem.tpp \
         ooymodem.tpp oozmodem.tpp ookermit.tpp ooascii.tpp oobplus.tpp

uartid.exe: apport.tpp apdefine.inc apuart.tpp uartid.pas

simpcom: simpcom.pas apdefine.inc \
         fastw1.tpp apport.tpp apuart.tpp apmisc.tpp aptimer.tpp \
         oocom.tpp oomodem.tpp ooabspcl.tpp \
         ooxmodem.tpp ooymodem.tpp oozmodem.tpp ookermit.tpp \
         oobplus.tpp ooascii.tpp \
         apansi.tpp comutil.tpp

lzhvo.exe: lzhvo.pas apdefine.inc apmisc.tpp ooarchiv.tpp oolzh.tpp

lzhxo.exe: lzhxo.pas apdefine.inc apmisc.tpp ooarchiv.tpp oolzh.tpp

lzho.exe:  lzho.pas apdefine.inc apmisc.tpp ooarchiv.tpp oolzh.tpp

zipvo.exe: zipvo.pas apdefine.inc apmisc.tpp ooarchiv.tpp oozip.tpp

zipxo.exe: zipxo.pas apdefine.inc apmisc.tpp ooarchiv.tpp oozip.tpp

zipo.exe:  zipo.pas apdefine.inc apmisc.tpp ooarchiv.tpp oozip.tpp

simpsndo.exe: simpsndo.pas apdefine.inc apmisc.tpp \
              aptimer.tpp apport.tpp apuart.tpp \
              oocom.tpp oofaxcvt.tpp ooabsfax.tpp \
              oofax12.tpp oofaxcas.tpp

simprcvo.exe: simprcvo.pas apdefine.inc apmisc.tpp \
              apport.tpp apuart.tpp \
              oocom.tpp oofaxcvt.tpp ooabsfax.tpp \
              oofax12.tpp oofaxcas.tpp

cvt2faxo.exe: cvt2faxo.pas apdefine.inc apmisc.tpp \
              oofaxcvt.tpp

showfaxo.exe: showfaxo.pas apdefine.inc apmisc.tpp oofaxcvt.tpp

prnfaxo.exe: prnfaxo.pas apdefine.inc apmisc.tpp oofaxcvt.tpp oofaxprn.tpp

fax2pcxo.exe: fax2pcxo.pas apdefine.inc apmisc.tpp oofaxcvt.tpp

moddemo.exe: moddemo.pas apdefine.inc apmisc.tpp \
             apport.tpp ooini.tpp ooinidb.tpp oomoddb.tpp

!else

comtesto.exe: apport.tpu apuart.tpu apint14.pas apuart1.obj \
              apuart2.obj apuart3.obj apuart4.obj apuart5.obj \
              apuart6.obj apuart7.obj apuart9.obj \
              apmisc.tpu aptimer.tpu apdefine.inc oocom.tpu \
              comtesto.pas

fxo.exe: apdefine.inc fxo.pas \
         apmisc.tpu aptimer.tpu apport.tpu \
         apuart.tpu apuart1.obj apuart2.obj apuart3.obj \
         apuart4.obj apuart5.obj apuart6.obj apuart7.obj apuart9.obj \
         oocom.tpu ooabspcl.tpu ooxmodem.tpu \
         ooymodem.tpu oozmodem.tpu ookermit.tpu ooascii.tpu oobplus.tpu

uartid.exe: apport.tpu apdefine.inc apuart.tpu uartid.pas

simpcom: simpcom.pas apdefine.inc \
         fastw1.tpu apport.tpu apuart.tpu apmisc.tpu aptimer.tpu \
         oocom.tpu oomodem.tpu ooabspcl.tpu \
         ooxmodem.tpu ooymodem.tpu oozmodem.tpu ookermit.tpu \
         oobplus.tpu ooascii.tpu \
         apansi.tpu comutil.tpu

lzhvo.exe: lzhvo.pas apdefine.inc apmisc.tpu ooarchiv.tpu oolzh.tpu

lzhxo.exe: lzhxo.pas apdefine.inc apmisc.tpu ooarchiv.tpu oolzh.tpu

lzho.exe:  lzho.pas apdefine.inc apmisc.tpu ooarchiv.tpu oolzh.tpu

zipvo.exe: zipvo.pas apdefine.inc apmisc.tpu ooarchiv.tpu oozip.tpu

zipxo.exe: zipxo.pas apdefine.inc apmisc.tpu ooarchiv.tpu oozip.tpu

zipo.exe:  zipo.pas apdefine.inc apmisc.tpu ooarchiv.tpu oozip.tpu

simpsndo.exe: simpsndo.pas apdefine.inc apmisc.tpu \
              aptimer.tpu apport.tpu apuart.tpu \
              oocom.tpu oofaxcvt.tpu ooabsfax.tpu \
              oofax12.tpu oofaxcas.tpu

simprcvo.exe: simprcvo.pas apdefine.inc apmisc.tpu \
              apport.tpu apuart.tpu \
              oocom.tpu oofaxcvt.tpu ooabsfax.tpu \
              oofax12.tpu oofaxcas.tpu

cvt2faxo.exe: cvt2faxo.pas apdefine.inc apmisc.tpu \
              oofaxcvt.tpu

showfaxo.exe: showfaxo.pas apdefine.inc apmisc.tpu oofaxcvt.tpu

prnfaxo.exe: prnfaxo.pas apdefine.inc apmisc.tpu oofaxcvt.tpu oofaxprn.tpu

fax2pcxo.exe: fax2pcxo.pas apdefine.inc apmisc.tpu oofaxcvt.tpu

moddemo.exe: moddemo.pas apdefine.inc apmisc.tpu \
             apport.tpu ooini.tpu ooinidb.tpu oomoddb.tpu

!endif
