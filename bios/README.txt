[X] Insert font pixels in the binary.
	[X] Use existing font.
[X] Set up some data in RAM for monochrome rendering of text
	[X] Reserve memory in array
[ ] Font rendering routines
	[ ] Main routines for messages
[ ] Print all available devices connected to the machine
[ ] Require a monitor and keyboard to be connected
	[ ] If not, "Connect monitor and/or keyboard and reboot."
[ ] Write some code to detect a disk reader, and load the contents byte for byte to RAM.
	[ ] Fail if there is no disk reader attached
	[ ] If no disk is available, display "Insert boot disk and reboot."
	[ ] If a disk is available, display "Loading booting disk..."
[ ] Load the data on the disk to the stack, and launch it as a program