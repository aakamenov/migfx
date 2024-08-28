build:
	odin build ./src -out:graphics -o:speed -vet-unused -show-timings
run:
	odin run ./src -out:graphics -o:speed -show-timings
check:
	odin check ./src -no-entry-point
test:
    odin test ./migpu/pool -out:tests -show-timings
