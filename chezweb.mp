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

beginfig(3);
	numeric x,y, q, s, v;
	numeric l,ll,lll,llll;

	s := 50pt; v := 0.3in; q := 6pt;
	l := v*4; ll := v*3; lll := v*2; llll := v*1;

	x := s*4.5; y := l;
	label(btex Control Codes etex, (x,y));
	y := y-q;
	draw (x,y)--(s*1.5,ll);
	draw (x,y)--(s*3,ll);
	draw (x,y)--(s*4.5,ll);
	draw (x,y)--(s*7.5,ll);

	x := s*1.5; y := ll-q;
	draw (x,y-q)--(x-s/2,lll); 
	draw (x,y-q)--(x+s/2,lll);
	label(btex Data etex, (x,y)); x := s*3;

	draw (x,y-q)--(x-s/2,lll);
	draw (x,y-q)--(x,lll);
	draw (x,y-q)--(x+s/2,lll);
	label(btex Code etex, (x,y)); x := s*4.5;

	draw (x,y-q)--(x-s/2,lll);
	draw (x,y-q)--(x+s/2,lll);
	label(btex Delimiter etex, (x,y)); x := s*7.5;

	draw (x,y-q)--(x-s*2,lll);
	draw (x,y-q)--(x-s,lll);
	draw (x,y-q)--(x,lll);
	draw (x,y-q)--(x+s,lll);
	label(btex Annotation etex, (x,y));

	x := s*1.5-s/2; y := lll-q;
	label(btex {\tt @\ } etex, (x,y)); x := x+s;
	label(btex {\tt @*} etex, (x,y));

	x := s*3-s/2;
	label(btex {\tt @p} etex, (x,y)); x := x+s/2;
	label(btex {\tt @<} etex, (x,y)); x := x+s/2;
	label(btex {\tt @(} etex, (x,y)); x := x+s/2;
	
	x := s*4.5-s/2;
	label(btex {\tt @>} etex, (x,y)); x := x+s;
	label(btex {\tt @>=} etex, (x,y)); x := x+s;

	x := s*7.5-s*2;
	label(btex {\tt @c} etex, (x,y)); x := x+s;
	label(btex {\tt @q} etex, (x,y)); x := x+s;
	label(btex Index etex, (x,y)); x := x+s;
	label(btex {\tt @i} etex, (x,y)); x := x+s;

	x := s*7.5; y := y-q;
	draw (x,y)--(x-s/2,llll);
	draw (x,y)--(x,llll);
	draw (x,y)--(x+s/2,llll);

	x := s*7.5-s/2; y := llll-q;
	label(btex {\tt @\^\ } etex, (x,y)); x := x+s/2;
	label(btex {\tt @:} etex, (x,y)); x := x+s/2;
	label(btex {\tt @.} etex, (x,y)); x := x+s/2;
endfig;

beginfig(4);
	numeric x, y, bx, bs;
	numeric s, ss, sss, ssss, sh;

	s := 18pt; ss := s*2; sss := s*3; ssss := s*4;
	sh := s/2; sq := s/3;

	bx := 9pt; by := 9pt;

	x := bx; y := by;
	draw (x , y)--(x+ss,y)--(x+ss,y+s)--(x,y+s)--cycle;
	x := x+ssss+ss;
	draw (x,y)--(x+ss,y)--(x+ss,y+s)--(x,y+s)--cycle;

	x := bx+ss; y := by+sh;
	drawarrow (x,y)--(x+ssss,y);

	x := bx+s; 
	label(btex {\tt WEB} etex, (x,y));
	x := x+ss*3; 
	label(btex {\tt SCM} etex, (x,y));

	x := bx+ssss; y := y+sq;
	label(btex Chez{\tt TANGLE} etex, (x,y));
endfig;

end
