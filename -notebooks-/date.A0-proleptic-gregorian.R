
# library(humanVerse);
# source( github/humanVerse/-ONE- );
# github.install("humanVerse");  # this is hardcoded as first, HVcpp also
## TODO: add timers 
## setwd("C:/_git_/-SANDBOX-/greg");





## On Friday, 15 October 1582, Pope Gregory XIII
##   https://en.wikipedia.org/wiki/Gregorian_calendar
##   https://en.wikipedia.org/wiki/Inter_gravissimas
## The principal change was to space leap years differently so as 
##   to make the average calendar year 365.2425 days long, 
##   more closely approximating the 365.2422-day 'tropical' 
##   or 'solar' year that is determined by the Earth's revolution around the Sun.
##  365.2425 = 365 + 1/4 - 1/100 + 1/400
##  --------> 15 October 1582, Fri, DOY = 288 <--------
## Julian Thursday, 4 October 1582, being followed by Gregorian Friday, 15 October. 
## DOY is 288, as-if the ytype="gregorian" was always working
## YEAR / DAY differences are "calendar" rules that switch from 
##   one ytype to another
## PAPAL is one "switch" EPOCH, BRITISH is another

# https://en.wikipedia.org/wiki/Calendar_(New_Style)_Act_1750
# NOTE, the START_OF_YEAR (SOY) changed with ACT 1750
#  PROLEPTIC ANCHOR to JANUARY 1 as "modern" start
#  OTHER RULES can be created (CALENDAR means RULES) 
#    to adjust subyear elements (SOY, DAYS_IN_WEEKS, MONTHS, MONTH NAMES)
# # # EPOCH_ANCHOR = "BRITISH";
# # # YTYPE = "gregorian";
# # # CYEAR = 1752;
# # # CMONTH = 9;
# # # CDAY = 14;
# # # STR_CDAY = "Thu";				# Assuming 7 day week
# # # CDOY = 258;					# Day Of Year (assuming January 1 start, leap year)
									# AS-IF this calendar was always in place


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' SETUP 'EPOCH' PARAMETERS
#'
#------------------------------------------------#

								
EPOCH_ANCHOR = "PAPAL";
YTYPE = "gregorian";
CYEAR = 1582;
CMONTH = 10;
CDAY = 15;
STR_CDAY = "Fri";				# Assuming 7 day week
CDOY = 288;						# Day Of Year (assuming January 1 start)
DAYS_BACKWARD = -9914;			# -3933333
DAYS_FORWARD = 9914;			#   433333
TRUNCATE_YEAR_LOWER = 1575;		# -7575
TRUNCATE_YEAR_UPPER = 1625;		#  2525
PATH = getwd();
FILE_TEMPLATE = "{EPOCH}_{n}_{dir}.txt";  	# {n} is DAYS 
											# {dir} is FORWARD or BACKWARD
											# if(n < 0) ---> BACKWARD 
											# if(n > 0) ---> FORWARD
											
FINAL_TEMPLATE = "{EPOCH}_({UPPER},{LOWER}){ytype}";



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' PERFORM THE RUNS 
#'
#------------------------------------------------#

## THE CODE CAN BE RUN IN SEPARATE RGUI INSTANCES,
##   ONE FORWARD, ONE BACKWARD
## ONCE COMPLETE, THEY CAN BE MERGED AND TRUNCATED
## A YEAR HAS ABOUT 365.25 DAYS, so you can 
##   COMPUTE DAYS_BACK / DAYS_FORWARD if you wish

 
# fR = forwardRUN; bR = backwardRUN;
# file.fR = date.generateProlepticGregorian(9914); 	# THESE HAVE DEFAULTS
# file.bR = date.generateProlepticGregorian(-9914); # THESE HAVE DEFAULTS



file.fR = date.generateProlepticGregorian(DAYS_FORWARD,
										path = PATH,
										epochname = EPOCH_ANCHOR,
										filename = FILE_TEMPLATE,
										ytype = YTYPE,
										cyear = CYEAR,
										cmonth = CMONTH,
										cday = CDAY, 
										str.cday = STR_CDAY,
										cdoy = CDOY
										);
										
file.bR = date.generateProlepticGregorian(DAYS_BACKWARD,
										path = PATH,
										epochname = EPOCH_ANCHOR,
										filename = FILE_TEMPLATE,
										ytype = YTYPE,
										cyear = CYEAR,
										cmonth = CMONTH,
										cday = CDAY, 
										str.cday = STR_CDAY,
										cdoy = CDOY
										);









stop(" Runs STAGE complete ");

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' REVIEW AND ADJUST RANGES AS NECESSARY
#'
#------------------------------------------------#

############# ONCE THE RUNS ARE COMPLETE #############
df.fR = readFromPipe(file.fR);
df.bR = readFromPipe(file.bR);

range(df.fR$YYYY);
range(df.bR$YYYY);


stop(" May want to verify the runs and the ranges ");
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' YOU CAN MANUALLY UPDATE RANGES IF NEEDED
#'
#------------------------------------------------#

# TRUNCATE_YEAR_LOWER = 1576;
# TRUNCATE_YEAR_UPPER = 1626;


LOWER = TRUNCATE_YEAR_LOWER;  # we looped on days = 9914
UPPER = TRUNCATE_YEAR_UPPER;  # let's truncate the date to have nice START/END in RANGE


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' TRUNCATE DATA FRAMES BASE ON LOWER, UPPER YEARS
#'
#------------------------------------------------#
df.bRn = subset(df.bR, YYYY >= LOWER); head(df.bRn); tail(df.bRn);
df.fRn = subset(df.fR, YYYY <= UPPER); head(df.fRn); tail(df.fRn);


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' COMBINE DATAFRAMES
#'
#------------------------------------------------#
df.bfRn = rbind(df.bRn, df.fRn);  dim(df.bfRn);
# df.sortBy() may be easier ... 
df.bfRns = df.bfRn[with(df.bfRn, order(-YYYY, -MM, -DD)), ];
	rownames(df.bfRns) = 1:dim(df.bfRns)[1];
	head(df.bfRns); tail(df.bfRns);  


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' EPOCH_ANCHOR is IDX = 0, REMOVE ONLY DUPLICATE
#'
#------------------------------------------------#
# MY DUPLICATE is just one element, has the same IDX of 0
# df = df.bfRns[!duplicated(df.bfRns), ]; # or unique()  # SLOW on large datasets, WHY?
idxs = which(df.bfRns$IDX == 0);
# df.removeRows() may be easier ... 
df = df.bfRns[-c(idxs[1]), ]; dim(df);

df.printHead(df, 8, 1);
df.printHead(df, 8, 999999999);

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' REVIEW ANCHOR ELEMENT
#'
#------------------------------------------------#
search = which( df$YYYY == CYEAR & df$MM == CMONTH & df$DD == CDAY );
df.printHead(df, 8, search[1]);  # should only have one, JIK



filestem = FINAL_TEMPLATE;
	filestem = str.replace("{EPOCH}", toupper(EPOCH_ANCHOR), filestem);
	filestem = str.replace("{UPPER}", UPPER, filestem);
	filestem = str.replace("{LOWER}", LOWER, filestem);
	filestem = str.replace("{ytype}", tolower(substring(YTYPE, 1, 3)), filestem);


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' SAVE TO FILE (.txt) and (.zip)
#'
#------------------------------------------------#


txtfile = paste0( filestem, ".txt" );
zipfile = paste0( filestem, ".zip" );
# rdsfile = paste0( filestem, ".rds" );

writeToPipe(	df, txtfile);
	Sys.sleep(1);
zip(zipfile, txtfile);
	Sys.sleep(1);

# saveRDS(	df, rdsfile);


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' JULIAN PROLEPTIC NUMBER (SIMILAR TO JDN)
#'
#------------------------------------------------#

# do proleptic number checks ... this was JULIAN PROLEPTIC


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' Are the toGPN and fromGPN the same?
#'
#------------------------------------------------#

df$GPN = as.integer( date.toGregorianProlepticNumber(df$YYYY, df$MM, df$DD) );

res = date.fromGregorianProlepticNumber(df$GPN);
df$jyear = res$jyear;  		# how to make this happen as a function 
df$jmonth = res$jmonth;		# names unique in df, or append .1 
df$jday = res$jday; 		# check lengths, return NULL on mismatch

# identical ?
# identical(df$DD, df$jday);
idxs = which( (df$YYYY != df$jyear) & (df$MM != df$jmonth) & (df$DD != df$jday) );


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' idxs should be EMPTY, debugging offsets below
#'
#------------------------------------------------#

## debugging
# df$diff = df$DD - df$jday;
# search = which(df$diff > 0);
# df.printHead(df, 8, search[1]);
## current code has error in FEB 29th ...
## cleap ==> cleapdays

# QED


