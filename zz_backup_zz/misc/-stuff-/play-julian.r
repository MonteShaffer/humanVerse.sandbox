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





### months 
### daysBetween(d1, d2)... rules ... JULIAN until 9/2/1752; GREG after (as 9/14/1752) ... 11 days lost ... 9/3-9/13 should throw ALARM (NA)
### calendar.rule[["british]][1] => "JULIAN until 1752-09-02 [ISO]
###                          [2] => "GREG since 1752-09-14 [ISO]
### what about FRANCE ... [1] Julian until ..., [2] Greg until [3] French until ... [n] GREG since ...
### define the French Republic Calendar ... MAP over GREG ...










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
		if(doy < 1) { doy = sum(LENS_); } # need this? bug in GREG?
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

# get into weeds, compute strlen(cols), omit some columns
# allow for vertical isolation as well
# print.matrix ...
df.printHead = function(df, n=6, row.idx=1, ...)
	{
	idx = row.idx;  		## offset, e.g, SKIP
	nrow = nrow(df);
	# tails 			
		lower = (idx - n); 	if(lower < 1) 	 { lower = 1; }
		upper = lower + n; 	if(upper > nrow) { upper = nrow; }
							if(upper >= idx) { upper = idx - 1; }
		diff = upper - lower; 
	if(diff < 0) { cat("\n", "NOTHING TO PRINT", "\n"); tails = NULL; } 
	else {
		tails = df[ lower:upper, ];  rownames(tails) = lower:upper;
		print(tails, col.names=TRUE, col.at="top", ...);
		}
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
	
	diff = upper - lower; 
	if(diff < 0) { cat("\n", "NOTHING TO PRINT", "\n"); heads = NULL;} 
	else {
		heads = df[ lower:upper , ];	rownames(heads) = lower:upper;
		print(heads, col.names=TRUE, col.at="bottom", ...);
		}
	
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





df$leap = as.integer(df$YYYY %% 4 == 0);
> head(df)
     IDX YYYY MM DD DOW DOY leap
1 338004 2525 12 31 Thu 365    0
2 338003 2525 12 30 Wed 364    0
3 338002 2525 12 29 Tue 363    0
4 338001 2525 12 28 Mon 362    0
5 338000 2525 12 27 Sun 361    0
6 337999 2525 12 26 Sat 360    0


IDX = f(YYYY, MM, DD, leap)

df$IDX2 = df$IDX + 0.5;

options(digits=22);
## WITH leap year, R^2 == 1 ... linear relationship
df.lm = lm(IDX2 ~ YYYY + MM + DD + leap, data = df);
summary(df.lm);
# why is summary slow?
# df.lm.s = summary(df.lm);

df.lm.s$coefficients
df.lm.s$coefficients[,1]


 x = df.lm.s$coefficients[,1]
> dput(x)
c(INT = -584648.838950183, YYYY = 365.25000001543, 
MM = 30.4397976184598, DD = 0.997931853508713, leap = 0.34021685613482


SET DD = 1, leap = 0.34, MM = 30.440, YYYY = 365.25, INT = -584648.849

# classifier? ensemble

SET DD = 1, leap = 0.33, YYYY = 365.25,  ... LM on MM and INT only

df$DD2 = df$DD * 1;
df$leap2 = df$leap * 0.33;
df$YYYY2 = df$YYYY* 365.25;



options(digits=22);
## WITH leap year, R^2 == 1 ... linear relationship
df.lm = lm(IDX2 ~ YYYY2 + MM + DD2 + leap2, data = df);
# why is summary slow?
( df.lm.s = summary(df.lm) );



df$IDX3 = df$IDX2 + 584649;
options(digits=22);
## WITH leap year, R^2 == 1 ... linear relationship
df.lm = lm(IDX3 ~ YYYY + MM + DD + leap, data = df);
# why is summary slow?
( df.lm.s = summary(df.lm) );

df.lm.s$coefficients
df$predict.lm = predict(df.lm, df)  # not using holdout
df$diff.lm = df$IDX3 - df$predict.lm;
# this break;
# plot(df$YYYY, df$diff.lm);




options(digits=22);
## WITH leap year, R^2 == 1 ... linear relationship
df.lm = lm(IDX ~ YYYY, data = df);
# why is summary slow?
( df.lm.s = summary(df.lm) );

df.lm.s$coefficients
df$predict.lm = predict(df.lm, df)  # not using holdout
df$diff.lm = df$IDX - df$predict.lm;
# this break;
# plot(df$YYYY, df$diff.lm);


	df$a = as.integer( (14 - df$MM) / 12 );
	df$y = df$YYYY + 4800 - df$a;
	df$m = df$MM + 12 * df$a - 3;
	
	# daysBeforeMonth ... March 1 offset ... = 0
	df$d = as.integer( (153*df$m+2)/5 );

	e = (df$YYYY <= -4800);
	df$e = 0;
	df$e[e] = -1;
	
	df$JD = df$e + df$DD + df$d + df$y * 365 + as.integer(df$y/4) - 32000 - 2305758;
	
# https://www.slideshare.net/chenshuo/datetime-julian-date SLIDE 19, inverse
# https://www.slideshare.net/chenshuo/datetime-julian-date SLIDE 8
# 
	
df$diff.JD = (df$JD - df$IDX)
df0 = subset(df, diff.JD == 0)
df1 = subset(df, diff.JD == 1)
	# range(df1$YYYY)
	[1] -7525 -4800
	
	
idx = which(df$YYYY == -4712 & df$MM == 1 & df$DD == 1);

idx = which(df$YYYY == -7 & df$MM == 4 & df$DD == 5);
df.printHead(ndf, 10, idx);  # ndf with fewer cols 

df$JD = date.toJulianDayNumber(df$YYYY, df$MM, df$DD) - 4237058;	
df$diff.JD = (df$JD - df$IDX);
hist(df$diff.JD);

#df0 = subset(df, diff.JD == 0);
df1 = subset(df, diff.JD != 0);
dim(df1);
# table(df1$leap, df1$diff.JD);
# range(df1$YYYY);

# with e, plus leap year adjustment ... equally +/- 1 on DIFF
## only MM = 1 or MM = 2 ... 
 81099
 
# WITH e
248871  # half million fixed, quarter million to go 
		# what does df1 have in common ... hist(df1$MM) ... etc doesn't show anything obvious ...  a bit heavier on first day of month
		# ONLY off by -1 ... 
		# logistic regression ?
		# if I take out my logic for e ... I have more that are off by 1
		
df1$log = 1;	# I can map to negative 1?
# this seems a bit unequal ... DRILLDOWN ...  hist(df1$a)


# let's see if PARAMS changed on subset ... only off by 1...
options(digits=22);
## WITH leap year, R^2 == 1 ... linear relationship
df1.lm = lm(IDX ~ YYYY + MM + DD + leap, data = df1);
# why is summary slow?
( df1.lm.s = summary(df1.lm) );

df1.lm.s$coefficients
df1$predict.lm = predict(df1.lm, df1)  # not using holdout
df1$diff.lm = df1$IDX3 - df1$predict.lm;
# this break;

# leap == 2 on the coefficient ... was 1/3 ... there it is ...
# ALT-TAB stopped working ... 

	

# WITHOUT e 
 dim( subset(df, df$YYYY <= -4800) )
[1] 995672     28
> dim(df1)
[1] 746801     28



	
	
	
	From and including: Saturday, January 1, 2000
To, but not including Monday, December 31, 2525

Result: 192,117 days
	
	
	
	# from GREG?
	JD =  day 	+ as.integer( (153*m+2)/5 )  + y*365 + as.integer(y/4)  - 32083;
	









df.predict = function(year, month, day, leap, return="actual")
	{
	res = (365.25*year + 30.440*month + 1*day + 0.34*leap - 584648.849);
	if(return == "integer") { res = as.integer(res); }
	if(return == "round") { res = round(res); }
	if(return == "ceiling") { res = ceil(res); }
	res;	
	}
	
df$predict = df.predict(df$YYYY, df$MM, df$DD, df$leap);
df$predict.int = df.predict(df$YYYY, df$MM, df$DD, df$leap, "integer");
df$predict.r = df.predict(df$YYYY, df$MM, df$DD, df$leap, "round");

df$diff = df$predict - df$IDX;
df$diff.int = df$predict.int - df$IDX;
df$diff.r = df$predict.r - df$IDX;
## CAN BE OFF by +/-2 days 
## HOW?
	


# https://www.machinelearningplus.com/machine-learning/complete-introduction-linear-regression-r/

df$lm.predict = predict(df.lm, df)  # not using holdout



df.predict = function(year, month, day, leap, return="actual")
	{
	
	b = c(	INT = -584648.838950183, 
			YYYY = 365.25000001543, 
			MM = 30.4397976184598, 
			DD = 0.997931853508713, 
			leap = 0.34021685613482 );
	B = as.data.frame(t(b)); # NAMES to list


	res = (B$YYYY*year + B$MM*month + B$DD*day + B$leap*leap + B$INT);
	if(return == "integer") { res = as.integer(res); }
	if(return == "round") { res = round(res); }
	res;	
	}

	
df$predict = df.predict(df$YYYY, df$MM, df$DD, df$leap);
df$predict.int = df.predict(df$YYYY, df$MM, df$DD, df$leap, "integer");
df$predict.r = df.predict(df$YYYY, df$MM, df$DD, df$leap, "round");

df$diff = df$predict - df$IDX;
df$diff.int = df$predict.int - df$IDX;
df$diff.r = df$predict.r - df$IDX;
## CAN BE OFF by +/-2 days [on INT]
range(df$diff);
















## NO LEAP, still a large R^2
df.lm = lm(IDX ~ YYYY + MM + DD, data = df);
summary(df.lm);
# why is summary slow?
# df.lm.s = summary(df.lm);

df.lm.s$coefficients
options(digits=22);
df.lm.s$coefficients[,1]


































ndf$a = a; ndf$b = b; ndf$c = c; ndf$d = d; ndf$e = e; ndf$m = m;
ndf$jyear = jyear; ndf$jmonth = jmonth; ndf$jday = jday; 

 ndf$EQUAL = ( (ndf$YYYY == ndf$jyear) & (ndf$MM == ndf$jmonth) & (ndf$DD == ndf$jday) )

neq = which(ndf$EQUAL == FALSE);
length(neq);

df.printHead(ndf, 6, neq[123]);

[1] 120612
60306 ... maybe 31 or 30 ... how should it know ?




