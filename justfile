build:
	odin build ./src -out:graphics -o:speed -vet-unused -show-timings -define:WGPU_GFLW_GLUE_SUPPORT_WAYLAND=true
run:
	odin run ./src -out:graphics -o:speed -show-timings -define:WGPU_GFLW_GLUE_SUPPORT_WAYLAND=true
check:
	odin check ./src
test:
	odin test ./src -out:graphics -show-timings
