.PHONY: clean test all

test:
	./test.sh

test-ecl:
	./test-ecl.sh

docker:
	docker build -t cl-oju .

all:
	make clean test

