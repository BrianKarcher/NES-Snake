TARGET=none
CONFIG=nes.cfg
OBJECTFILE=./snake.o
OUTFILE=./snake.nes
DBGFILE=./snake.dbg
MAPFILE=./snake.map
LABELFILE=./snake.lbl
LINKERFILE=./snake.lnk
SOURCE=init.asm

.PHONY: build clean run env-emulator-path

build:
	ca65 -g -l ${LINKERFILE} -t ${TARGET} ${SOURCE} -o ${OBJECTFILE}
	ld65 -Ln ${LABELFILE} -m ${MAPFILE} -vm --dbgfile ${DBGFILE} -o ${OUTFILE} -C ${CONFIG} ${OBJECTFILE} ${TARGET}.lib

run: env-emulator-path
	${EMULATOR_PATH} ${OUTFILE}

env-emulator-path:
ifndef EMULATOR_PATH
	$(error EMULATOR_PATH is not set)
endif