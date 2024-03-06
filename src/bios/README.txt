[X] Insert font pixels in the binary.
	[X] Use existing font.
[X] Set up some data in RAM for monochrome rendering of text
	[X] Reserve memory in array
[ ] Font rendering routines
	[ ] Debug rendering
		[ ] iterate through all words in FONT_PX 0..n
		[ ] each word on the screen adds 1 to y pitch (YP).
		[ ] every FONT_CHAR_PX_HEIGHT YP resets Y origin (YO) and X offset (XO) increases by 1.
		[ ] every FONT_CHAR_PX_HEIGHT*10 YO increases by VIDMEM_YSTRIDE_WORDS*FONT_CHAR_PX_HEIGHT and XO resets to 0.
	[ ] Main routines for messages
[ ] Print all available devices connected to the machine
[ ] Write some code to detect a disk reader, and load the contents byte for byte to RAM.
	[ ] Fail if there is no disk reader attached
	[ ] If no disk is available, display "Insert boot disk and reboot."
	[ ] If a disk is available, display "Loading booting disk..."