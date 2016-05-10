source := turing-machine.pl
output := turing-machine
goal := start

all:
	swipl -q -g $(goal) -o $(output) -c $(source)

clean:
	@- $(RM) $(output)
