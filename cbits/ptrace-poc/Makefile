
EXE=pt-test
OPEN=open

${EXE}: ppoc.cpp
	g++ -Wall -g -std=c++11 $^ -o $@

${OPEN}: open.c
	gcc -Wall -g $^ -o ${OPEN}

run: ${EXE} ${OPEN}
	./${EXE} /usr/bin/time -p ./${OPEN}

clean:
	@rm -f ${EXE} ${OPEN}
