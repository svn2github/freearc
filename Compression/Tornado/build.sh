#!/bin/sh

# Tornado build script for Unix, written by Joachim Henke

PARM='-D_FILE_OFFSET_BITS=64 -DFREEARC_UNIX -lm -otor main.cpp'
PARM_ADDR='-m32'
PARM_DEBUG=''
PARM_ENDIANNES='-DFREEARC_INTEL_BYTE_ORDER'
PARM_FULL=''
PARM_OPT='-O3 --param inline-unit-growth=999 -fomit-frame-pointer -fstrict-aliasing -ffast-math -fforce-addr -fno-exceptions -fno-rtti -fno-threadsafe-statics -fwhole-program -combine'
PARM_PROF='-funroll-loops'
PARM_STATS=''
PARM_TIME='-lrt'
TEST_FILE='tor'

for P; do
	if [ "$P" = "${P##-}" ]; then
		TEST_FILE="$P"
		PARM_PROF='-fprofile-use'
	else
		case $P in
		'-64')
			PARM_ADDR='-DFREEARC_64BIT -m64'
			;;
		'-be')
			PARM_ENDIANNES='-DFREEARC_MOTOROLA_BYTE_ORDER'
			;;
		'-debug')
			PARM_DEBUG='-DDEBUG -g'
			PARM_OPT=''
			;;
		"-flags="*)
			PARM="${P#*=} "
			;;
		'-full')
			PARM_FULL='-DFULL_COMPILE'
			;;
		'-profiled')
			PARM_PROF='-fprofile-use'
			;;
		'-stats')
			PARM_STATS='-DSTATS'
			;;
		'-notiming')
			PARM_TIME='-DFREEARC_NO_TIMING'
			;;
		'-h')
			echo 'usage: build.sh [OPTIONS] [FILE_FOR_PROFILING]'
			echo 'possible options: -full -profiled -64 -be -notiming -stats -debug -flags="<gcc flags>"'
			echo '  -full      -  generate code for all possible coder/matchfinder combinations'
			echo '  -profiled  -  optimize program using FILE_FOR_PROFILING as sample data'
			echo '  -64        -  compile 64-bit version'
			echo '  -be        -  compile for big-endian CPUs (PowerPC, Sparc, Motorola...)'
			echo '  -notiming  -  remove speed calculation code'
			echo '  -stats     -  print program''s internal compression ststistics'
			echo '  -debug     -  add debugging info and printfs'
			exit 0
			;;
		*)
			echo "unknown option: $P"
			exit 1
		esac
	fi
done
PARM="$PARM $PARM_ENDIANNES $PARM_ADDR $PARM_TIME $PARM_OPT"

if [ "$PARM_PROF" = '-fprofile-use' ]; then
	echo 'compiling binary for profiling...'
	gcc $PARM -fprofile-generate || exit 1
	for M in `seq 1 11`; do
		echo "profiling method $M..."
		./tor -$M -q <$TEST_FILE >temp.tor && \
		./tor -d -q <temp.tor >/dev/null || break
	done
	rm -f temp.tor
fi
echo 'compiling final binary...'
gcc $PARM $PARM_PROF -s
