#/bin/bash

RED='\033[0;31m'
GREEN='\033[0;32m'
NOCOLOR='\033[0m'

for TESTFILE in ./tests/*.emi
do
	echo -n Compiling test ${TESTFILE}... 
	bin/Compiler ${TESTFILE} &> ./output/test_output_compilation.txt
	if [ $? -eq 0 ]
	then
		printf "${GREEN}SUCCESS${NOCOLOR}   "
	else
		printf "${RED}FAILED!${NOCOLOR}\n"
		cat ./output/test_output_compilation.txt
		exit
	fi

	echo -n Running... 
	output/out &> ./output/test_output_run.txt
	if [ $? -eq 0 ]
	then
		printf "${GREEN}SUCCESS${NOCOLOR}\n"
	else
		printf "${RED}FAILED!${NOCOLOR}\n"
		cat ./output/test_output_run.txt
		exit
	fi
done

for ERRORTESTFILE in ./errortests/*.emi
do
	echo -n Compiling error test ${ERRORTESTFILE}... 
	bin/Compiler ${ERRORTESTFILE} &> /dev/null
	if [ $? -ne 0 ]
	then
		printf "${GREEN}SUCCESS${NOCOLOR}\n"
	else
		printf "${RED}FAILED!${NOCOLOR}\n"
	fi
done
