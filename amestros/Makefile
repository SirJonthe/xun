kernel_files=xun/amestros/kernel/main.xb,\
	xun/amestros/kernel/time.xb,\
	xun/amestros/kernel/device.xb,\
	xun/amestros/kernel/firmware.xb,\
	xun/amestros/kernel/syscall.xb,\
	xun/amestros/kernel/string.xb,\
	xun/amestros/kernel/stdio.xb,\
	xun/amestros/kernel/shell.xb

kernel: debug | release | debug_macos | release_macos
	# (WHATEVER WE HAVE) -xb input==$(kernel_files) output=bin/kernel.bin

userland:
	# compile each userland binary and move them to a file structure mirroring Unix
	# bundle as TAR

distro: kernel, userland