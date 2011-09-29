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

beginfig(2);
	numeric x, y, bx, bs;
	numeric s, ss, sss, ssss, sh;

	s := 18pt; ss := s*2; sss := s*3; ssss := s*4;
	sh := s/2; sq := s/3;

	bx := 9pt; by := 9pt;

	x := bx; y := by;
	draw (x , y)--(x+ss,y)--(x+ss,y+s)--(x,y+s)--cycle;
	x := x+ssss+ss;
	draw (x,y)--(x+ss,y)--(x+ss,y+s)--(x,y+s)--cycle;
	x := x+ssss+ss;
	draw (x,y)--(x+ss,y)--(x+ss,y+s)--(x,y+s)--cycle;

	x := bx+ss; y := by+sh;
	drawarrow (x,y)--(x+ssss,y);
	x := x+ssss+ss;
	drawarrow (x,y)--(x+ssss,y);

	x := bx+s; 
	label(btex {\tt WEB} etex, (x,y));
	x := x+ss*3; 
	label(btex \TeX etex, (x,y));
	x := x+ss*3;
	label(btex {\rm PDF} etex, (x,y));

	x := bx+ssss; y := y+sq;
	label(btex Chez{\tt WEAVE} etex, (x,y));
	x := x+ss*3;
	label(btex Xe\TeX etex, (x,y));
endfig;

end
