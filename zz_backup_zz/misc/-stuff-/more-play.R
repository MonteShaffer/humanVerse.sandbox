EQUIVALENT

if( !exists("algo", inherits = FALSE ) )			
						{ 
						algo 		= "md5"; 
						}
if(!is.set(algo, deep.scan = FALSE, inherits = FALSE))
	{
	algo = "md5";
	}
					
https://stackoverflow.com/questions/4214793/mathjax-jsmath-alternatives


https://katex.org/

https://www.math.union.edu/~dpvc/jsMath/welcome.html

https://www.math.union.edu/~dpvc/jsMath/examples/Henrici.html

https://bookdown.org/yihui/rmarkdown-cookbook/diagrams.html

https://www.spinellis.gr/sw/textproc/bib2xhtml




months(Sys.time())

month.abb
?Constants
pi - 4*(4*atan(1/5) - atan(1/239))

format(ISOdate(2000, 1:12, 1), "%B")
format(ISOdate(2000, 1:12, 1), "%b")

MONTHS_ = format(ISOdate(2000, 1:12, 1), "%b")
DAYS_ = format(ISOdate(2000, 1, 1:10), "%a")[1:7]

format(ISOdate(2000, 1, 1:10), "%a")

format(today, format="%a")

julian(Sys.Date(), -2440588) # from a day

julian(ISOdate(2000, 1, 1));

weeks(.leap.seconds)
months(.leap.seconds)


> getwd();
[1] "C:/Users/Monte J. Shaffer/Documents"













# > row
# [1] "333332" "2513"   "3"      "17"     "Mon"    "76" 






format(ISOdate(1600, 8, 5), "%a")   # Sat


julian(ISOdate(2000, 1, 1));
julian(ISOdate(1970, 1, 1));

julian(ISOdate(1970, 1, 1), 
		origin=as.POSIXct("170-01-01", tz = "GMT"));


########### 
	
julian.isLeap = function(cyear) 
	{ 
	i = (cyear %%4 == 0); 
	i;
	}

gregorian.isLeap = function(cyear) 
	{
	k = (cyear %%400 == 0);		if(k) { return(TRUE); }
	j = (cyear %%100 == 0);		if(j) { return(FALSE); }
	i = (cyear %%4 == 0);		
	i;
	}

MONTHS_ = format(ISOdate(2000, 1:12, 1), "%b");
LENS_ = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
DAYS_ = format(ISOdate(2000, 1, 1:10), "%a")[1:7];

ytype = "Julian"
cyear = 1600;
cleap = (cyear %%4 == 0);
cmonth = 8; # August
clen = LENS_[cmonth];
cday = 5;  # Teusday
str.cday = "Tue";
idx.cday = which(DAYS_ == str.cday);
doy = 218;  # FROM https://www.timeanddate.com/date/weekday.html

dir = "FORWARD"

### 154,130 days
#n = 222333;
n = 333333;
n = 433333;



row = c("IDX", "YYYY", "MM", "DD", "DOW", "DOY");
file = paste0(getwd(), "/", "ruthven-",n,".txt");
	cat( paste0(row, collapse="|"), "\n", sep="", 
							file=file, append=FALSE);



##############################

i = 0;
if(cleap) { LENS_[2] = 29; } else { LENS_[2] = 28; }
while(i < n)
	{
	row = c(i, cyear, cmonth, cday, str.cday, doy);
	cat( paste0(row, collapse="|"), "\n", sep="", 
							file=file, append=TRUE);
	
	i = 1+i;
	doy = 1 + doy;
	if(i %% 370 == 0) 
		{ 
		cat("\n =====   ", cyear, " ===== \n"); 
		flush.console(); 
		}
		
	cday = 1 + cday;
	if(cday > clen) 
		{ 
		cday = 1; 
		cmonth = 1 + cmonth;
		if(cmonth > 12)
			{
			cmonth = 1;
			cyear = 1 + cyear;
			doy = 1;
			cleap = (cyear %%4 == 0);
			if(cleap) { LENS_[2] = 29; } else { LENS_[2] = 28; }
			}
		clen = LENS_[cmonth];
		}

	idx.cday = 1 + idx.cday;
	if(idx.cday > 7) { idx.cday = 1;}
	str.cday = DAYS_[idx.cday];
	}





# > row
# [1] "433332" "2786"   "12"     "29"     "Sat"    "363" 



# =====    2208  ===== 
# > 
# > 
# > row
# [1] "222332" "2209"   "4"      "22"     "Sun"    "112" 



## read from pipe


readFromPipe = function(file, header=TRUE, quote="", sep="|", comment.char="#")
  {
  utils::read.csv(file, header=header, quote=quote, sep=sep, comment.char = comment.char);
  }
  
  

df = readFromPipe(file);
 df$IDX = as.integer(df$IDX);
ndf = subset(df, YYYY >= -7525);
 saveRDS(ndf, "-7525.rds");  
  
























#######  REVERSE








MONTHS_ = format(ISOdate(2000, 1:12, 1), "%b");
LENS_ = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
DAYS_ = format(ISOdate(2000, 1, 1:10), "%a")[1:7];

ytype = "Julian"
cyear = 1600;
cleap = (cyear %%4 == 0);
cmonth = 8; # August
clen = LENS_[cmonth];
cday = 5;  # Teusday
str.cday = "Tue";
idx.cday = which(DAYS_ == str.cday);
doy = 218;  # FROM https://www.timeanddate.com/date/weekday.html

dir = "BACKWARDS"

### 154,130 days
#n = -3222333;
n = -3333333;

# n = -500;

row = c("IDX", "YYYY", "MM", "DD", "DOW", "DOY");
file = paste0(getwd(), "/", "ruthvenR-",n,".txt");
	cat( paste0(row, collapse="|"), "\n", sep="", 
							file=file, append=FALSE);



##############################




i = 0;
if(cleap) { LENS_[2] = 29; } else { LENS_[2] = 28; }
while(i > n)
	{
	row = c(i, cyear, cmonth, cday, str.cday, doy);
	cat( paste0(row, collapse="|"), "\n", sep="", 
							file=file, append=TRUE);
	
	i = i-1;
	doy = doy-1;
	if(i %% 370 == 0) 
		{ 
		cat("\n =====   ", cyear, " ===== \n"); 
		flush.console(); 
		}
		
	cday = cday - 1;
	if(cday < 1) 
		{ 
		# cday = 1;   # LENS_2[cmonth]
		cmonth = cmonth - 1;
		if(cmonth < 1)
			{
			cmonth = 12;
			cyear = cyear - 1;
			cleap = (cyear %%4 == 0);
			if(cleap) { LENS_[2] = 29; } else { LENS_[2] = 28; }
			doy = sum(LENS_);
			}
		clen = LENS_[cmonth];
		cday = clen;
		}

	idx.cday = idx.cday - 1;
	if(idx.cday < 1) { idx.cday = 7;}
	str.cday = DAYS_[idx.cday];
	}











# row
# [1] "-3333332" "-7526"    "6"        "6"        "Sun"      "157" 










## read from pipe


readFromPipe = function(file, header=TRUE, quote="", sep="|", comment.char="#")
  {
  utils::read.csv(file, header=header, quote=quote, sep=sep, comment.char = comment.char);
  }
  

df = readFromPipe(file);
 df$IDX = as.integer(df$IDX);
ndf = subset(df, YYYY <= 2525);
 saveRDS(ndf, "2525.rds");  
mdf = readRDS("2525.rds");
rdf = rbind(mdf, ndf);

rdf = rbind(mdf, ndf);

# one row has IDX = 0

xdf = rdf[!duplicated(rdf), ];   	# seems slowish ...
									# has to hold !duplicated in RAM


# unique()

## SORT by ISO ... YYYY, MM, DD  ... 
## DESC    2525, 12, 31 ... -7575, 1, 1


# very fast 
xdf.sorted = xdf[with(xdf, order(-YYYY, -MM, -DD)), ];
rownames(xdf.sorted) = 1:dim(xdf.sorted)[1];

idx = which(xdf.sorted$IDX == 0);
xdf.sorted[idx, ];

heads.inRange = function(obj, n=6, row.num=1)
	{
	idx = row.num;
	set = idx:(idx+n);
	obj[set, ];
	}
	
tails.inRange = function(obj, n=6, row.num=1)
	{
	idx = row.num;
	set = (idx-n):idx;
	obj[set, ];
	}	


saveRDS(xdf.sorted, "ruthven.rds");

df = xdf.sorted; n = 6; row.idx = 338005;

df.printHead = function(df, n=6, row.idx=1, ...)
	{
	idx = row.idx;  		## offset, e.g, SKIP
	nrow = nrow(df);
	# tails 			
		lower = (idx - n); 	if(lower < 1) 	 { lower = 1; }
		upper = lower + n; 	if(upper > nrow) { upper = nrow; }
							if(upper >= idx) { upper = idx - 1; }
	tails = df[ lower:upper, ];  rownames(tails) = lower:upper;
	print(tails, col.names=TRUE, col.at="top", ...);
	cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~", "\n");
	# one
		one = df[idx, ]; rownames(one) = idx;
	print(one, col.names=FALSE, ...);
	cat("\n");
	cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~", "\n");	
	# heads 	
		upper = (idx + n); 	if(upper > nrow) { upper = nrow; }
		lower = upper - n; 	if(lower < 1) { lower = 1; }
							if(lower <= idx) { lower = idx + 1; }
	heads = df[ lower:upper , ];	rownames(heads) = lower:upper;
	print(heads, col.names=TRUE, col.at="bottom", ...);
	invisible(list("tails"=tails, "one"=one, "heads"=heads));
	}
	


dim(iris);
df.printHead(iris, n=6, row.idx=4);
df.printHead(iris, n=6, row.idx=148);





    Sepal.Length Sepal.Width Petal.Length Petal.Width   Species
142          6.9         3.1          5.1         2.3 virginica
143          5.8         2.7          5.1         1.9 virginica
144          6.8         3.2          5.9         2.3 virginica
145          6.7         3.3          5.7         2.5 virginica
146          6.7         3.0          5.2         2.3 virginica
147          6.3         2.5          5.0         1.9 virginica
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    Sepal.Length Sepal.Width Petal.Length Petal.Width   Species
148          6.5           3          5.2           2 virginica

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    Sepal.Length Sepal.Width Petal.Length Petal.Width   Species
149          6.2         3.4          5.4         2.3 virginica
150          5.9         3.0          5.1         1.8 virginica




    Sepal.Length Sepal.Width Petal.Length Petal.Width   Species
142          6.9         3.1          5.1         2.3 virginica
143          5.8         2.7          5.1         1.9 virginica
144          6.8         3.2          5.9         2.3 virginica
145          6.7         3.3          5.7         2.5 virginica
146          6.7         3.0          5.2         2.3 virginica
147          6.3         2.5          5.0         1.9 virginica
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

148          6.5           3          5.2           2 virginica

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

149          6.2         3.4          5.4         2.3 virginica
150          5.9         3.0          5.1         1.8 virginica
    Sepal.Length Sepal.Width Petal.Length Petal.Width   Species



df.printHead(iris, 6, 5)

headsAndTails.inRange(xdf.sorted, 6, 338005);
	
	
	### 		## ??? A = B = C didn't work 
	
	
	

# heads.tails = function() ... show range ...











































num.round = function(n, by=5, method="round")
	{
	byidx = (n %% by == 0); # these already are indexed well 
    new = by * as.integer((n + by) / by);
    res = n;
    res[!byidx] = new[!byidx];
    res;
	# round(n/by) * by;
	#ceiling(n/by) * by;
	#floor(n/by) * by;
	# maybe if negative do ceiling, positive do floor ...
	# user wants basically an UP/DOWN feature ... 
	}

# https://stackoverflow.com/a/15272920/184614
console.readkey = function(msg="Press [ENTER] to continue, [ESC] to exit")
	{
	cat (msg);
	line = readline();
	# maybe do something like onkeypress (see stackoverflow METHOD 3)
	# if [-] slow down the auto.play ... [+] speed up 
	}

# http://rfunction.com/archives/1302
# par mar
snails.pace = function() {} 
# x = snails.pace(auto.play=1/50, s.par=TRUE);
# x = snails.pace(auto.play=1/50, s.par=TRUE, intro.pause=FALSE);
# Symbola 
# https://fontlibrary.org/en/font/symbola
snails.pace = function(snails = 6, finish.line = 8, moves = 200,
							auto.play = NULL, intro.pause = TRUE,
							ymax = 2 + snails,
							s.par = FALSE, s.pch=(10+snails), s.cex=snails,
							snail.col = c("orange", "blue", "pink", "green", "yellow", "red"), ...
						)
	{
	old.par = par(no.readonly = TRUE);
	move.history = NULL;
	on.exit({par(new = FALSE); par(old.par); invisible(move.history)}); # add=TRUE to on.exit ... always, maybe like dput(all)
	snail.x = 0*(1:snails); 
	if(ymax < snails) { ymax = snails; }
	y.d = ymax - snails; 
	scale.y = ymax / snails;
	snail.y = (scale.y*(1:snails)) - (1/(snails/2) * y.d);
	snail.y = snail.y - 1; # moved the rect to top, so adjust 
	
	# RANDOM colors ... `sample(colors(), snails);`
	n.col = length(snail.col);
	n.missing = snails - n.col;
	### collision is possible, highly improbable
	if(n.missing > 0) { snail.col = c(snail.col, sample(colors(), n.missing)); }
		
	snail.rank = 0*snail.x; 
	crank = 1; # current rank 	
	move.number = 0;
	n = 0; # current number randomized (color)
	snail.lab = "";
	
	snails.round = function(n, by = 5)
		{
		ceiling(n/by) * by;
		}

	
	snails.plot = function() 
		{ 
		xmax = max(finish.line, max(snail.x) );
		# define reasonable xmax if overpainting 
		# SIM to solve ?
		if(s.par) { xmax = (1/0.7)* moves/snails;  }		
		
		# U+1F40C [snail]
		plot(snail.x, snail.y, 
				col=snail.col, 
				pch=s.pch, cex=s.cex, 
				xlim=c(-1, snails.round(xmax, 5) ), 
				ylim=c(-1, ymax ), 
				axes=FALSE, 
				frame.plot=FALSE, 
				xlab="", ylab="",
				main=""
				); 
			
		#axis(gr.side("bottom")); 
		axis(1);
			has.rank = (snail.rank != 0);
			snail.lab = paste0(snail.x, "*", snail.rank);
			snail.lab[!has.rank] = snail.x[!has.rank];
			assign("snail.lab", snail.lab, envir=parent.env(environment()) );
		# overlay "points" again so trail doesn't have text ...
		# maybe not even use plot ?
		if(s.par)
			{
			points(snail.x, snail.y, col=snail.col, pch=s.pch, cex=1.3*s.cex);
			}
		# place text with current number PLUS * rank if finish.line 
		text(snail.x, y=snail.y, labels=snail.lab, col="black"); 
		abline(v = finish.line, col="gray", lty="dashed");
		
		# main in plot is updating, so place a textbox (white) to overwrite?
		status=paste0("Move #", move.number, " of ", moves);
			r.col = "white"; if(n != 0) { r.col = snail.col[n]; }
		# berryFunctions, roundedRectangle 
		# just draw inner rectangles with missing corners and add 1/4 radius circles ... gr.circle (should be a function)
		# https://stackoverflow.com/questions/34522732/changing-fonts-in-ggplot2#
		# Also, windowsFonts disappeared from grDevices after 3.4.1. The code here needs updating. – smci  Aug 16, 2018 at 22:19
		# windowsFonts()
		# library(extrafont); font_import(); loadfonts(device = "win");
		# Another option is to use showtext package which supports more types of fonts (TrueType, OpenType, Type 1, web fonts, etc.) and more graphics devices, and avoids using external software such as Ghostscript.
		# library(showtext); font_add_google("Montserrat", "Montserrat")
		# font_paths()  ; font_files()  ; font_add("Palatino", "pala.ttf")
		# http://www.cookbook-r.com/Graphs/Fonts/
		# https://rdrr.io/cran/berryFunctions/man/roundedRect.html
		# http://www.statisticstoproveanything.com/2013/09/using-custom-images-as-pch-values-in-r.html
		# https://cran.r-project.org/web/packages/utf8/vignettes/utf8.html
		# U+1F40C [snail]
		# font_add("Symbola", "Symbola.ttf")
		# https://www.fileformat.info/info/unicode/char/1f40c/fontsupport.htm
		# windowsFonts()
		# sysfonts::font_add_google("Cousine", "Cousine")
		# font_families()
		# https://statr.me/
		# https://yixuan.blog/cleveland-r-meetup/pretty.html#1
		# showtext_auto()
		#  par(family = "Cousine")
		# plot(mtcars$wt, mtcars$mpg, pch="ȯ", col = "red", cex = 2, family="Cousine")
		# points(mtcars$wt, mtcars$mpg, pch="\u25D3", col = "blue", cex = 2, family="Symbola")
		# points(mtcars$wt, mtcars$mpg, pch="\u1F40C", col = "orange", cex = 2, family="Symbola")
		# U+1F40C
		# length is the current color's position
			xlen = finish.line; if( n != 0 ) { xlen = snail.x[n]; }
								if(xlen == 0) { xlen = finish.line; }
		# white out overlay 
		rect(0,ymax, xmax, (ymax-1), border=NA, col="white");
		rect(0,ymax, xlen, (ymax-1), border=NA, col=r.col);
		text(0, y=(ymax-1/3), labels=status, col="black", pos=4); # to the right 
		text(xlen, y=(ymax-2/3), labels=status, col="white", pos=2); # to the left 
		
		if(s.par) { par(new = TRUE); }
		}
	snails.update = function() 
		{
		if(intro.pause && move.number < 2)
			{
			x = readline(prompt="Press [enter] to continue, [ESC] to quit");
			} else if(is.null(auto.play))
				{
				x = readline(prompt="Press [enter] to continue, [ESC] to quit");
				} 
		n = sample(1:snails, 1);
		assign("n", n, envir=parent.env(environment()) );
		
		snail.x[n] = 1 + snail.x[n];
		if( (snail.rank[n] == 0) && (snail.x[n] >= finish.line) )
			{ 			
			snail.rank[n] = crank;
			crank = 1 + crank; 			
			# update to MAIN environment
			assign("snail.rank", snail.rank, envir=parent.env(environment()) );
			assign("crank", crank, envir=parent.env(environment()) );
			}		
		snail.x;
		}

	par(new = FALSE); 	
	# c(bottom, left, top, right)
	par(mar=c(2,1,1,1));
	snails.plot(); 
	while(move.number < moves)
		{
		move.number = 1 + move.number;
		snail.x = snails.update();
		move.history = c(move.history, snail.col[n]);
		snails.plot();	
		if(!is.null(auto.play)) 
			{ 
			dev.flush();
			Sys.sleep(auto.play); 
			}
		}
	attr(move.history, "color") = snail.col;
	attr(move.history, "info") = snail.lab;
	# https://stackoverflow.com/a/31298923/184614
	# attr(move.history, "info") = setNames(as.list(snail.lab), snail.col);
	invisible(move.history);	
	}








