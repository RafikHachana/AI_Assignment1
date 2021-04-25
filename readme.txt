This file includes the instructions on how to run the prolog code:

1. You can copy paste the whole code to the SWI website, or run it locally with SWI-prolog
2. In case of running it locally, you can use these commands in the terminal:
	
	Run the code with user input:

		swipl -s knowledge.pl -g executeInput -t halt

	Run the code with a randomly generated map with 1 covid:


		swipl -s knowledge.pl -g executeRandom_1C -t halt

	Run the code with a randomly generated map with 2 covids:


		swipl -s knowledge.pl -g executeRandom_2C -t halt

3. In case you're running the code on the website, you will need to restart a new instance each time to avoid bugs related to cash.
	You can use the following queries:

	Run the code with user input:

		executeInput.

	Run the code with a randomly generated map with 1 covid:


		executeRandom_1C.

	Run the code with a randomly generated map with 2 covids:


		executeRandom_2C.


The code will run both algorithms with both variants.

Note that formatting is very important when doing input. If at some point the map because invalid because of input (overlapping cells, or cells in the covid zone) you will have to start input all over again.