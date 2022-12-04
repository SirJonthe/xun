debug:
	g++ -I. -o bin/debug main.cpp xun.cpp xasm.cpp xdev/xclock.cpp xdev/xdev.cpp xdev/xdisk.cpp xdev/xkeyb.cpp xdev/xdisp.cpp MiniLib/MTL/mtlString.cpp MiniLib/MTL/mtlParser.cpp MiniLib/MTL/mtlPath.cpp -lSDL

release:
	g++ -I. -o bin/release -O3 main.cpp xun.cpp xasm.cpp xdev/xclock.cpp xdev/xdev.cpp xdev/xdisk.cpp xdev/xkeyb.cpp xdev/xdisp.cpp  MiniLib/MTL/mtlString.cpp MiniLib/MTL/mtlParser.cpp MiniLib/MTL/mtlPath.cpp -lSDL