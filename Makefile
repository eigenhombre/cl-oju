.PHONY: clean test all

test:
	./test.sh

docker:
	docker build -t cl-oju .

all:
	make clean test

