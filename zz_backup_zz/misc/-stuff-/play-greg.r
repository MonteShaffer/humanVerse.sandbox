

# day 0 ... Thursday, 14 September 1752
ytype = "Gregorian"
cyear = 1752;
cleap = gregorian.isLeap(cyear);
cmonth = 9; # August
clen = LENS_[cmonth];
cday = 14;  
str.cday = "Thu";
idx.cday = which(DAYS_ == str.cday);
doy = 247;  # FROM https://www.timeanddate.com/date/weekday.html

cyear = 1600;
cleap = (cyear %%4 == 0);
cmonth = 8; # August
clen = LENS_[cmonth];
cday = 5;  # Teusday
str.cday = "Tue";
idx.cday = which(DAYS_ == str.cday);
doy = 218;

# DEFAULT anchors to JULIAN CALENDAR DATE: 
#				August 5, 1600 (Tuesday) ==> RUTHVEN EPOCH 
# Historical Date Found in Documents (saying Tuesday)
# cdoy (Current Day of Year) ... important for first year calculations 
# 		## FROM https://www.timeanddate.com/date/weekday.html
# Above link also has Aug 5, 1600 as a Tuesday 
#
date.generateProleptic = function() {}
date.generateProleptic = function(n, dir="FORWARD", 
									path = getwd(),
									filename = "RUTHVEN_{n}_{dir}.txt",
									ytype="julian", 
									cyear = 1600,
									cmonth = 8,   # August 
									cday = 5,
									str.cday = "Tue",
									cdoy = 218 
								)
	{	
	ytyp = prep.arg(ytype, 1);
	# Jan, Feb, Mar, ...
	MONTHS_ = format(ISOdate(2000, 1:12, 1), "%b");
	LENS_ = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
	# Mon, Tue, Wed, ...
	DAYS_ = format(ISOdate(2000, 1, 1:10), "%a")[1:7];

	
	# current idx of str.cday (name in week)
	idx.cday = which(DAYS_ == str.cday);
	# current leap year day (0 or 1)... extensible to allow others
	cleapdays = date.calculateLeapDays(cyear, ytype, "integer");
	LENS_[2] = 28 + cleapdays; # 28 or 29 .. 
	# current length of a month
	clen = LENS_[cmonth];
	
	
	DIRE = toupper(prep.arg(dir, 4));
	dir = "FORWARD"; if(DIRE == "BACK") { dir = "BACKWARD"; }
		filename = str.replace( "{n}", n, filename );
		filename = str.replace( "{dir}", dir, filename );
		filename = paste0(path, "/", filename);  # is trailing slash required, will it break?
	## HEADER 		
	row = c("IDX", "YYYY", "MM", "DD", "DOW", "DOY");
	cat( paste0(row, collapse="|"), "\n", sep="", 
			file=filename, append=FALSE);
			
	if(DIRE == "FORW") # FORWARD in TIME, ASCENDING
		{
		if(is.negative(n)) { n = -1* n; }  # n is POSITIVE
		i = 0;
		while(i < n)
			{
			# write current row 
			row = c(i, cyear, cmonth, cday, str.cday, cdoy);
			cat( paste0(row, collapse="|"), "\n", sep="", 
					file=filename, append=TRUE);
					
			# increment	
			i = 1+i;
			cdoy = 1 + cdoy;
			if(i %% 365 == 0) 
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
					cdoy = 1;
					cleap = date.calculateLeapDays(cyear, ytype, "integer");
					LENS_[2] = 28 + cleapdays; # 28 or 29 .. 
					}
				clen = LENS_[cmonth];
				}
			# day name of week [DOW] on continuous 7-day loop	
			idx.cday = 1 + idx.cday;
			if(idx.cday > 7) { idx.cday = 1;}
			str.cday = DAYS_[idx.cday];
			}		
		}
		
	if(DIRE == "BACK") # BACKWARD in TIME, DESCENDING
		{
		if(is.positive(n)) { n = -1* n; }  # n is NEGATIVE
		i = 0;
		while(i > n)
			{
			row = c(i, cyear, cmonth, cday, str.cday, cdoy);
			cat( paste0(row, collapse="|"), "\n", sep="", 
					file=filename, append=TRUE);
			
			# DECREMENT 
			i = i-1;
			cdoy = cdoy-1;
			if(i %% 365 == 0) 
				{ 
				cat("\n =====   ", cyear, " ===== \n"); 
				flush.console(); 
				}
				
			cday = cday - 1;
			if(cday < 1) 
				{ 
				# cday = 1;
				cmonth = cmonth - 1;
				if(cmonth < 1)
					{
					cmonth = 12;
					cyear = cyear - 1;
					cleap = date.calculateLeapDays(cyear, ytype, "integer");
					LENS_[2] = 28 + cleapdays; # 28 or 29 .. 
					cdoy = sum(LENS_);
					}				
				clen = LENS_[cmonth];
				cday = clen;  # last day of given month 
				}
			if(cdoy < 1) { cdoy = sum(LENS_); }  ## why again?

			idx.cday = idx.cday - 1;
			if(idx.cday < 1) { idx.cday = 7;}
			str.cday = DAYS_[idx.cday];
			}		
		}

		cat("\n RESULTS are STORED HERE: \n\n\t", filename, "\n\n");	
	}



















# > getwd();
# [1] "C:/Users/Monte J. Shaffer/Documents"

# A leap year normally occurred every four years, and the leap day was historically inserted by doubling 24 February. However, it is now customary to number the days of February sequentially with no gaps, and 29 February is typically considered the leap day. 

# https://en.wikipedia.org/wiki/List_of_adoption_dates_of_the_Gregorian_calendar_by_country
# https://en.wikipedia.org/wiki/Calendar_(New_Style)_Act_1750

# the date of the spring equinox (which determines the date of Easter) had drifted by about eleven days from its date at the time of the First Council of Nicaea, 21 March
# m21 = subset(df, MM == 3 & DD == 21)
# m21n = subset(df, MM == 3 & DD == 21 & (YYYY > 350 & YYYY < 400))

# https://en.wikipedia.org/wiki/First_Council_of_Nicaea
# The feast of Easter is linked to the Jewish Passover and Feast of Unleavened Bread, as Christians believe that the crucifixion and resurrection of Jesus occurred at the time of those observances.

# Wednesday, 2 September 1752, was followed by Thursday, 14 September 1752.
# 1752	2 Sept	14 Sept	11
# subset(df, MM == 9 & DD == 2 & (YYYY == 1752))
#         IDX YYYY MM DD DOW DOY
# 282459 55546 1752  9  2 Wed 246


########### 
	




MONTHS_ = format(ISOdate(2000, 1:12, 1), "%b");
LENS_ = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
DAYS_ = format(ISOdate(2000, 1, 1:10), "%a")[1:7];

# day 0 ... Thursday, 14 September 1752
ytype = "Gregorian"
cyear = 1752;
cleap = gregorian.isLeap(cyear);
cmonth = 9; # August
clen = LENS_[cmonth];
cday = 14;  
str.cday = "Thu";
idx.cday = which(DAYS_ == str.cday);
doy = 247;  # FROM https://www.timeanddate.com/date/weekday.html

# 9/14/1752
# It is day number 247 of the year, 108 days left.
# It is Thursday number 36 out of 51 in 1752.
# It is Thursday number 1 out of 3 in September 1752.
# Year 1752 has 355 days.
# 9/13/1752
# The entered date does not exist for United States because 11 days were skipped when the Gregorian calendar was adopted
# 9/2/1752
# September 2, 1752 is a Wednesday
# It is day number 246 of the year, 109 days left.


dir = "FORWARD"

### 154,130 days
#n = 222333;
n = 333333;
n = 433333;

#n = 1500;

row = c("IDX", "YYYY", "MM", "DD", "DOW", "DOY");
file = paste0(getwd(), "/", "british-",n,".txt");
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
			cleap = gregorian.isLeap(cyear);
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



# day 0 ... Thursday, 14 September 1752
ytype = "Gregorian"
cyear = 1752;
cleap = gregorian.isLeap(cyear);
cmonth = 9; # August
clen = LENS_[cmonth];
cday = 14;  
str.cday = "Thu";
idx.cday = which(DAYS_ == str.cday);
doy = 247;  # FROM https://www.timeanddate.com/date/weekday.html





dir = "BACKWARDS"

### 154,130 days
#n = -3222333;
n = -3933333;

# n = -1500;

row = c("IDX", "YYYY", "MM", "DD", "DOW", "DOY");
file = paste0(getwd(), "/", "britishR-",n,".txt");
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
			cleap = gregorian.isLeap(cyear);
			if(cleap) { LENS_[2] = 29; } else { LENS_[2] = 28; }
			doy = sum(LENS_);
			}
		
		clen = LENS_[cmonth];
		cday = clen;
		}
	if(doy < 1) { doy = sum(LENS_); }

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




