beginfig(1);
	defaultfont := "bchr8r";
	defaultscale := 2;
	
	numeric x,y;
	numeric ss,sd,sz,sh;
	
	x := 0.5in; y := 0.5in;
	ss := 1in; sd := 2in; sz := 3in; sh := 0.5in;
	
	draw (x,y)--(x+sd,y)--(x+sd,y+sz)--(x,y+sz)--cycle;
	draw (x,y+ss)--(x+sd,y+ss);
	draw (x,y+sd)--(x+sd,y+sd);
	
	label("Runtime", (x+ss,y+sd+sh));
	label("Named Chunks", (x+ss,y+ss+sh));
	label("Top-level Code", (x+ss,y+sh));
endfig;

end