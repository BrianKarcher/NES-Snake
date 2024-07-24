# Define the assembler and linker
AS = ca65
LD = ld65

SOURCES = init.asm constants.asm game.asm board.asm
OBJECTS = $(SOURCES:.asm=.o)
OUTPUT = snake.nes

TARGET=none
CONFIG=nes.cfg
OBJECTFILE=./snake.o
OUTFILE=./snake.nes
DBGFILE=./snake.dbg
MAPFILE=./snake.map
LABELFILE=./snake.lbl
LINKERFILE=./snake.lnk
SOURCE=init.asm

# Assembly rules
%.o: %.asm
	$(AS) -o $@ $<

# Link rule
$(OUTPUT): $(OBJECTS)
	$(LD) -o $(OUTPUT) $(OBJECTS) -Ln ${LABELFILE} -m ${MAPFILE} -vm --dbgfile ${DBGFILE} -C ${CONFIG}

.PHONY: build clean run env-emulator-path

build: $(OUTPUT)

# ca65 constants.asm
# ca65 ${SOURCE}
# ld65 -Ln ${LABELFILE} -m ${MAPFILE} -vm --dbgfile ${DBGFILE} -o ${OUTFILE} -C ${CONFIG} ${OBJECTFILE} constants.o ${TARGET}.lib

run: env-emulator-path
	${EMULATOR_PATH} ${OUTFILE}

env-emulator-path:
ifndef EMULATOR_PATH
	$(error EMULATOR_PATH is not set)
endif

# ca65 -g -l ${LINKERFILE} -t ${TARGET} ${SOURCE} -o ${OBJECTFILE}
# ca65 -g -l constants.lnk -t ${TARGET} constants.asm -o constants.o