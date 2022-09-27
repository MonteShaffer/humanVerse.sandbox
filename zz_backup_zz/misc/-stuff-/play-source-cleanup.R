
## memory



memory.defaults = function() {}

memory.getDefaults
memory.setDefaults




### stats 




getQuantiles = function(x, qs, type=1)
	{
  # quantile(data, probs = c(.37, .53, .87))
	xx = stats::na.omit(x);
	as.numeric(stats::quantile(xx, prob=qs, type=type));
	}




# // names(qs) <- format_perc(probs, digits=digits)
# stats.median = 
# stats.mean = 
# stats.summary




# stats.median
doMedian = function(x, type=1)
  {
  xx = stats::na.omit(x);
  as.numeric(stats::quantile(xx, prob=c(0.5), type=type));
  }

# stats.mean
doMean = function(x)
  {
  xx = stats::na.omit(x);
  m = mean(xx);
  deviationFromMean = abs(xx-m);
  xx[ whichMin(deviationFromMean)[1] ];  # value from data
  }







#
# doStatsSummaryDataFrame = function(df)
# 	{
# 	# pastec?
#
# 	}

#' doStatsSummary
#'
#' @family Stats
#'
#' @param x a numeric vector
#'
#' @return a list of various statistical summaries
#' @export
#'
#' @examples
#' x.norm = doStatsSummary ( rnorm(100,0,1) );
#' x.unif = doStatsSummary ( runif(100,0,1) );
#'
doStatsSummary = function(x)
	{
	# getAttribute("na.action", xx)
	# getAttribute("class", getAttribute("na.action", xx) )
	result = list();
		result$length = length(x);
	xx = stats::na.omit(x);
		result$length.na = length(x) - length(xx);
		result$length.good = length(xx);
	result$mean = mean(xx);
	result$mean.trim.05 = mean(xx, trim=0.05);
	result$mean.trim.20 = mean(xx, trim=0.20);

	result$sorted = sort(xx);

	result$median = stats::median(xx);
	result$MAD = stats::mad(xx);
	result$IQR = stats::IQR(xx);
	result$quartiles = stats::quantile(xx, prob=c(.25,.5,.75), type=1); # tries to use actual data, not averages ...
	# https://stats.stackexchange.com/questions/430391/are-there-99-percentiles-or-100-percentiles-and-are-they-groups-of-numbers-or
	# https://math.stackexchange.com/questions/1419609/are-there-3-or-4-quartiles-99-or-100-percentiles
		probs.deciles = seq(0.1,0.9,by=0.1);
		#probs.deciles = seq(0.1,1.0,by=0.1);
	result$deciles = stats::quantile(xx, prob=probs.deciles, type=1 );
	result$decile.members = cutMe(xx, probs.deciles, lower.equal = TRUE);
	  probs.centiles = seq(0.01,0.99,by=0.1);
	result$centiles = stats::quantile(xx, prob=probs.centiles, type=1 );
	# ?cut   cut(xx, 10) ... will be usefull for a histogram-ish device ...
	# https://stackoverflow.com/questions/11728419/using-cut-and-quartile-to-generate-breaks-in-r-function
		probs.niniles = (1:8)/9;
	result$niniles = stats::quantile(xx, prob=probs.niniles, type=1 );
	result$niniles.members = cutMe(xx, probs.niniles, lower.equal = TRUE);

#
	result$median.weighted = matrixStats::weightedMad(xx);
	result$MAD.weighted = matrixStats::weightedMedian(xx);

	# value from data
	result$myMedian = doMedian(xx,1);
	result$myMean   = doMean(xx);

	result$max = max(xx);
	result$min = min(xx);
	result$range = result$max - result$min;
	result$xlim = range(xx);

	result$max.idx = whichMax(x);
	result$min.idx = whichMin(x);

	result$mode = result$freq.max = doMode(x);  # elements with highest frequency
	result$which.min.freq = doModeOpposite(x);

	result$ylim = c( findFrequencyMinimumInVector(xx), findFrequencyMaximumInVector(xx) );

	# you could later get indexes of each mode(freq.max)/freq.min using findAllIndexesWithValueInVector

	result$sd = stats::sd(xx);
	result$var = stats::var(xx);

	result$se.mean = result$sd / sqrt(result$length.good);
	result$IDR.3 = doIDR(xx, 1/3);

	result$var.naive = doSampleVariance(x,"naive");
	result$var.2step = doSampleVariance(x,"2step");

	result$outliers.z = findOutliersUsingZscores(x);
	result$outliers.IQR = findOutliersUsingIQR(x);

	# result$z = calculateZscores(x); # works same as scale ...
	# append "attributes" ...

	result;
	}



#' zCutOverlay
#'
#' @param z.table
#' @param steps.z
#' @param verbose
#' @param myColor
#' @param ...
#'
#' @return
#' @export
zCutOverlay = function(z.table, steps.z = 1/2, verbose = FALSE, myColor = "blue", ...)
	{

	# z = calculateZscores(x);
	  # range.z = c(-3,3);
	  # steps.z = 1/2;
	# z.cut = cutZ(z, range.z, steps.z, verbose=TRUE);
	# z.table = table(z.cut$member);

	# how to deal with -Inf, +Inf ... put them one step above/below the range.z values ... maybe two steps ...
	# currently, I think they will grpah, just at Inf

	keys = as.numeric(names(z.table));
	vals = as.numeric(z.table); # these are the counts as height

	xleft = keys ;
	xright = keys + steps.z;
	ybottom = 0 * keys;
		vals.sum = standardizeToSum(vals);
		vals.max = standardizeToMax(vals.sum);
		vals.normmax = standardizeToFactor(vals.max, 0.4);
	ytop = vals.normmax

	graphics::rect(xleft, ybottom, xright, ytop, col=myColor);
	}



#' cutZ
#'
#' @param z
#' @param range.z
#' @param steps.z
#' @param verbose
#'
#' @return
#' @export
cutZ = function(z, range.z = c(-3,3), steps.z = 1/2, verbose = FALSE)
	{
	# allows overlay of rectangles on normal graph
	zz = z;
	nz = length(zz);

	zmin = range.z[1];
	zmax = range.z[2];

	df = as.data.frame(zz);
		df$member = 0 * zz;

	i = 1;
	buckets = c();
	breaks = c();
		which.z = which(zz < zmin);
			buckets[[i]] = length(which.z);
			breaks[[i]] = -Inf;
		if(length(which.z) > 0)
			{
			df$member[which.z] = -Inf;
			zz[which.z] = NA; # set to NA so won't count any more
			}

	i = i + 1;



	zlower = zmin;
		if(verbose)
			{
			cat("\n", "zmin: ", zmin, "  ...  ", "zmax: ", zmax, "\n");
			}
	while(zlower < zmax)
		{
		if(verbose)
			{
			cat("\n", " == WHILE == ", "zlower: ", zlower, "  <  ", "zmax: ", zmax, "\n");
			}

		zupper = zlower +  steps.z;

		which.z = which(zz < zupper);
			buckets[[i]] = length(which.z);
			breaks[[i]] = zlower;
		if(length(which.z) > 0)
			{
			df$member[which.z] = zlower;
			zz[which.z] = NA; # set to NA so won't count any more
			}

		i = i + 1;
		zlower = zupper;
		}

	which.z = which(!is.na(zz));
		buckets[[i]] = length(which.z);
		breaks[[i]] = Inf;
	if(length(which.z) > 0)
			{
			df$member[which.z] = Inf;
			}

	df;
	}




#' cutMe
#'
#' @param x
#' @param qs
#' @param type
#' @param lower.equal
#'
#' @return
#' @export
cutMe = function(x, qs, type=1, lower.equal=TRUE)
	{
  	# freq.df = as.data.frame( cbind( breaks, buckets ) );
	#	colnames(freq.df) = c("break", "count");

	# df = setAttribute("cuts", q.cuts, df);  # set KEY to VAL in OBJ

	# I can get freq.df by just calling table(df)
	# colnames(df) = c("z", "member");



# # create a 2 by 5 matrix
# x <- 1:10
# attr(x,"dim") <- c(2, 5)
# https://stackoverflow.com/questions/27546901/how-to-set-attributes-for-a-variable-in-r



	# xx = na.omit(x);
	xx = x;
	df = as.data.frame(xx);
		df$yy = 0 * xx;
	nx = length(xx);
	q.cuts = getQuantiles(xx, qs, type);
		# attributes(df)[["cuts"]] = q.cuts;
		df = setAttribute("cuts", q.cuts, df);  # set KEY to VAL in OBJ
		# getAttribute("cuts", df);

	# buckets = length(qs) + 1;
	# yy = list();
	for(b in 1:length(qs) )
		{
		idxb = which(xx <= q.cuts[b]);
		df$yy[idxb] = b;
		xx[idxb] = NA;
		# yy[[b]] = idxb;
		}
		idxb = which(!is.na(xx)); # leftovers
	# yy[[b+1]] = idxb;
		df$yy[idxb] = b+1;
	colnames(df) = c("x", "member");
	df;

	}


#' cutN
#'
#' @param x
#' @param ...
#' @param n
#'
#' @return
#' @export
cutN = function(..., n=2)
	{
	more = unlist(list(...));
	x = c(x, more);

	# create a list of elements with "n"
	out = list();
	i = 0;
	while(length(x) > 0)
		{
		i = 1 + i;
		sub = x[1:n];
			out[[i]] = sub;
		x = x[-c(1:n)];
		}
	out;
	}


#' getQuantiles
#'
#' @param x
#' @param qs
#' @param type
#'
#' @return
#' @export
getQuantiles = function(x, qs, type=1)
	{
  # quantile(data, probs = c(.37, .53, .87))
	xx = stats::na.omit(x);
	as.numeric(stats::quantile(xx, prob=qs, type=type));
	}


#' doIDR
#'
#' @param x
#' @param lower
#' @param upper
#' @param type
#'
#' @return
#' @export
# should this be IXR for variable range?  custom sort range ... custom quantile range ... CQR 
doIDR = function(x, lower = 0.25, upper = 1 - lower, type=1)
	{
	xx = stats::na.omit(x);
	q.lower = getQuantiles(xx, lower, type);
	q.upper = getQuantiles(xx, upper, type);
	q.lim = c(q.lower, q.upper);
	q.range = abs(q.upper - q.lower); # in case they enter them backwards

	list("call" = list("lower" = lower, "upper" = upper, "type" = type),
		"IDR" = list("lower" = q.lower, "upper" = q.upper, "xlim" = q.lim, "range" = q.range)
		);
	}

#' doSampleVariance
#'
#' Computes the sample variance with (n-1) ...
#'
#' @family Stats
#'
#' @param x numeric vector
#' @param method "two-pass" prevents "naive" floating-point issues
#'
#' @return list (x.bar, s.var, s.sd)
#' @export
#'
#' @examples
#' doSampleVariance( c(1) ); # returns null
#' doSampleVariance( 1:2 );
#'
# stats.variance
doSampleVariance = function(x, method="two-pass")
	{
	x = stats::na.omit(x);
	if(method=="naive")
		{
		n = 0;
		sum = 0;
		sum2 = 0;

		for(i in 1:length(x))  ## stats::na.omit(x)
			{
			n = n + 1;
			sum = sum + x[i];
			sum2 = sum2 + x[i]*x[i];
			}

		if(n < 2) { return(NULL);} #
			x.bar = sum/n;
			s.var = (sum2 - (sum*sum)/n)/(n-1);

		} else	{
				# two-pass algorithm # testing
				n = sum = sum2 = 0;
				## first pass
				for(i in 1:length(x))  ## stats::na.omit(x)
					{
					n = n + 1;
					sum = sum + x[i];
					}
		if(n < 2) { return(NULL);} #
				x.bar = sum/n;
				## second pass
				for(i in 1:length(x))  ## stats::na.omit(x)
					{
					deviation = x[i] - x.bar;
					sum2 = sum2 + deviation * deviation;
					}
				s.var = sum2/(n-1);
				}

		s.sd = sqrt(s.var);
	list("x.bar"=x.bar,"s.var"=s.var,"s.sd"=s.sd);
	}


#' calculateZscores
#'
#' Calculate z-scores using formula:  (x - x.bar) / s.hat;
#'
#'
#' @family Stats
#'
#' @param x numeric vector
#' @param x.bar default NULL, you can pass in the parameter
#' @param s.hat default NULL, you can pass in the parameter
#'
#' @return z (numeric vector)
#' @export
#'
#' @examples
#' calculateZscores( 1:5 );
#' calculateZscores( 1:9 );
#'
#' calculateZscores( 1:5, x.bar=3, s.hat=1 );
#' calculateZscores( 1:9, x.bar=3, s.hat=1 );
#'
calculateZscores = function(x, x.bar=NULL, s.hat=NULL)
	{
  if(is.numeric(x.bar) && is.numeric(s.hat)) { return ((x - x.bar) / s.hat);}
  # maybe throw a warning if one is null, but not the other
  if( (is.null(x.bar) + is.null(s.hat)) == 1)
      {
      warning("Only one value was entered for x.bar / s.hat ... Computing these values instead.")
      }


	dsv = doSampleVariance(x);

	x.bar = dsv$x.bar;
	s.hat = dsv$s.sd;

	if(is.null(s.hat)) { return (NULL); }  # we take care of division by zero in our custom sampleVarianceFunction

	(x - x.bar) / s.hat;
	}


#' findOutliersUsingZscores
#'
#'
#' [assuming normality.  Is that a good assumption?]
#' \url{https://statisticsbyjim.com/basics/outliers/}
#'
#' @family Stats
#'
#'
#' @param x numeric vector
#' @param zmin what is the lower-bound cutoff to label an outlier
#' @param zmax what is the upper-bound cutoff to label an outlier
#'
#' @return list of various features of the outliers
#' @export
#'
#' @examples
#' findOutliersUsingZscores( c(-5, rep(1:3,9), 9) );
#' findOutliersUsingZscores( c(-5,-4, rep(1:3,9), 8,9) );
#' findOutliersUsingZscores( c(-5, rep(1:3,9), 9), -2, 2);
#' findOutliersUsingZscores( c(-5,-4, rep(1:3,9), 8,9), -2, 2 );
#'
findOutliersUsingZscores = function(x, zmin=-3, zmax=3)
	{
  result = list();
  result$z = z = calculateZscores(x);
  result$z.min = zmin;
  result$z.max = zmax;

	outliers = x[z < zmin | z > zmax];
	  outliers.lower.which = which(z < zmin);
	  outliers.lower = x[outliers.lower.which];
			v.lower = rep("lower", length(outliers.lower));
		outliers.upper.which = which(z > zmax);
		outliers.upper = x[outliers.upper.which];
			v.upper = rep("upper", length(outliers.upper));

	df.lower = cbind(outliers.lower, v.lower);
	df.upper = cbind(outliers.upper, v.upper);

	df = as.data.frame(rbind(df.lower,df.upper));
		colnames(df) = c("value","direction");

	result$df = df;

	result$z.lower = outliers.lower.which;  # indexes
	result$z.upper = outliers.upper.which;  # indexes

	result;
	}


#' findOutliersUsingIQR
#'
#' [assuming nothing about the data]
#' \url{https://statisticsbyjim.com/basics/outliers/}
#'
#' @family Stats
#'
#'
#' @param x numeric vector
#' @param innerFenceFactor typically 1.5 * IQR
#' @param outerFenceFactor typically 3.0 * IQR
#'
#' @return list of various features of the outliers
#'
#' @export
#'
#' @examples
#' findOutliersUsingIQR( c(-5, rep(1:3,9), 9) );
#' findOutliersUsingIQR( c(-5,-4, rep(1:3,9), 8,9) );
#' findOutliersUsingIQR( c(-5, rep(1:3,9), 9), 1, 2);  # unchanging
#' findOutliersUsingIQR( c(-5,-4, rep(1:3,9), 8,9), 1, 2 );  # unchanging
#' findOutliersUsingIQR( c(-5, rep(1:3,9), 9), 2, 4);  # unchanging
#' findOutliersUsingIQR( c(-5,-4, rep(1:3,9), 8,9), 2, 4 );  # unchanging
#'
findOutliersUsingIQR = function(x, innerFenceFactor=1.5, outerFenceFactor=3)
	{
  result = list();

	result$IQR = myIQR = stats::IQR(x, na.rm=TRUE, show.warning=TRUE);
	result$Quartiles = myQuartiles = as.numeric( stats::quantile(x, na.rm=TRUE, prob=c(.25,.5,.75)) );

	result$inner.fence = innerFence = myIQR * innerFenceFactor;
	result$outer.fence = outerFence = myIQR * outerFenceFactor;

	result$Q1.inner = Q1.inner = myQuartiles[1] - innerFence;
	result$Q1.outer = Q1.outer = myQuartiles[1] - outerFence;

	result$Q3.inner = Q3.inner = myQuartiles[3] + innerFence;
	result$Q3.outer = Q3.outer = myQuartiles[3] + outerFence;


	# values that fall inside the two inner fences are not outliers ...
	result$inner.which = inner.which = which(x < Q1.inner | x > Q3.inner);
	          inner.which.lower = which(x < Q1.inner);
	          inner.which.upper = which(x > Q3.inner);
	  result$inner = inner = x[inner.which];	# circles
	result$outer.which = outer.which = which(x < Q1.outer | x > Q3.outer);
	          outer.which.lower = which(x < Q1.outer);
	          outer.which.upper = which(x > Q3.outer);
	  result$outer = outer = x[outer.which];	# * in boxplot
	# outer and inner may have duplicates, let's remove from inner so they are disjoint ...
	result$inner.u = inner = setdiff(inner,outer);
	# I could separate into lower and upper for later manipulation

	v.inner = rep("inner", length(inner));
		inner.lower = inner[inner < Q1.inner];
			v.inner.lower = rep("lower", length(inner.lower));
		inner.upper = inner[inner > Q3.inner];
			v.inner.upper = rep("upper", length(inner.upper));
	v.outer = rep("outer", length(outer));
		outer.lower = outer[outer < Q1.outer];
			v.outer.lower = rep("lower", length(outer.lower));
		outer.upper = outer[outer > Q3.outer];
			v.outer.upper = rep("upper", length(outer.upper));

	df.inner = cbind( c(inner.lower, inner.upper), v.inner, c(v.inner.lower, v.inner.upper) );

	df.outer = cbind( c(outer.lower, outer.upper), v.outer, c(v.outer.lower, v.outer.upper) );

	df = as.data.frame(rbind(df.inner,df.outer));
		colnames(df) = c("value","fence","direction");

	df$value = as.numeric(df$value);

	result$df = df;

    # indexes
	result$inner.lower = setdiff(inner.which.lower,outer.which.lower);
	result$inner.upper = setdiff(inner.which.upper,outer.which.upper);

	result$outer.lower = outer.which.lower;
	result$outer.upper = outer.which.upper;

	#list("df" = df, "inner" = c(i.lower,i.upper), "outer" = c(o.lower,o.upper) ); ;
	result;
	}




#' doMode
#'
#' Returns \code{mode} of a numeric vector x
#'
#' \code{mode} is the most frequent value(s) in a set of data
#'
#'
#' @family Stats
#'
#' @param x numeric vector
#'
#' @return numeric vector that contains _all_ values that are modal (could be bimodal)
#' @export
#'
#' @examples
#' doMode( c(1:9) );
#' doMode( c(1, 1:9, 9) );
#' doMode( c(1, 1:9, 9, 9) );
#'
# stats.mode
doMode = function(x) # alias ?
	{
	whichMaxFreq(x);
	}


# stats.median
doMedian = function(x, type=1)
  {
  xx = stats::na.omit(x);
  as.numeric(stats::quantile(xx, prob=c(0.5), type=type));
  }

# stats.mean
doMean = function(x)
  {
  xx = stats::na.omit(x);
  m = mean(xx);
  deviationFromMean = abs(xx-m);
  xx[ whichMin(deviationFromMean)[1] ];  # value from data
  }

#' doModeOpposite
#'
#' Returns \code{!mode} of a numeric vector x
#'
#' \code{!mode} is the least frequent value(s) in a set of data
#'
#' @family Stats
#'
#' @param x numeric vector
#'
#' @return numeric vector that contains _all_ values that are least modal
#' @export
#'
#' @examples
#' doModeOpposite( c(1:9) );
#' doModeOpposite( c(1, 1:9, 9) );
#' doModeOpposite( c(1, 1:9, 9, 9) );
#'
# stats.notMode
doModeOpposite = function(x)  # alias ?
	{
	whichMinFreq(x);
	}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###



# > is.function(md5_)
# [1] TRUE
# > is.function("md5_")
# [1] FALSE
# exists("md5_")








has.color <- function() 
	{
	# this is large function in crayon
	 
	# DEBIAN ... 
	# Sys.getenv("COLORTERM"); 
	# Sys.getenv("LS_COLORS"); 
	# Sys.getenv("TERM");
	#  
	# TERM                    xterm-256color
	# what if I pass a #F9A3EE into xterm-256, will it work ?
	# have a flag to store in the "humanVerse" ... disable / enable ...
	#
	
	

	}

has.emacs = function()
	{
	Sys.getenv("EMACS") != "" || Sys.getenv("INSIDE_EMACS") != ""	
	}
# property.get("EMAC*", NULL, "system")

has.rstudio = function()
	{
	!(Sys.getenv("RSTUDIO", "") == "")
	}
# property.get("RSTUDIO*", NULL, "system")

# requireNamespace("rstudioapi", quietly = TRUE) &&
#    rstudioapi::isAvailable() &&
#    rstudioapi::hasFun("getConsoleHasColor")





is.list.element = function(element, list) {}







### str 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' charVector
#'
#' @param strvec
#' @param sep
#'
#' @return
#' @export
#'
#' @examples
str.toCharVector = function(strvec, sep="")
	{
	n = length(strvec);
	res = list();
	for(i in 1:n)
		{
		res[[i]] = strsplit(strvec[i], sep ,fixed=TRUE)[[1]];
		}
	list.return(res);
	}


######################### cat #################

# 
# cat.checkColorCapability




ansi_regex <- paste0("(?:(?:\\x{001b}\\[)|\\x{009b})",
                     "(?:(?:[0-9]{1,3})?(?:(?:;[0-9]{0,3})*)?[A-M|f-m])",
                     "|\\x{001b}[A-M]");
					 
					 
has_style <- function(string) {
  grepl(ansi_regex, string, perl = TRUE)
}

strip_style <- function(string) {
  gsub(ansi_regex, "", string, perl = TRUE, useBytes = TRUE)
}





##################### DATE PROLEPTIC ########################


# http://www.webexhibits.org/daylightsaving/

# cyear = c(100 * 16:21)
date.isLeapYear = function(cyear, calendar="gregorian")
	{
	# cyear is 1AD => 1, 1BC => 0, 2BC => -1
	cale = prep.arg(calendar, 4);
	# calendar="julian"
	if(cale == "juli")
		{
		i = (cyear %%4 == 0); 	
		names(i) = cyear;
		return(i);
		}
	if(cale == "xela")
		{
		stop("what, TODO::");
		}
	
	
	# gregorian is default
	k = (cyear %%400 == 0);		# if(k) { return(TRUE); }
	j = (cyear %%100 == 0);		# if(j) { return(FALSE); }
	i = (cyear %%4 == 0);	
		# multivariate, as a set 
		r = i - j + k;
		r = as.logical(r);
		names(r) = cyear;
	r;
	}
	

date.mktime = function (sec = 0, min = 0, hour = 12, 
						month = 10, day = 15, year = 1582, tz="")
	{
	ISOdatetime(year, month, day, hour, min, sec, tz="");
	}

date.ISOdate = function(year, month, day, 
						hour = 12, min = 0, 
						sec = 0, tz = "GMT"
						)

https://adodb.org/dokuwiki/doku.php?id=v5:datetime:datetime_index
adodb_mktime(0,0,0,10,15,1582)

ISOdatetime(year, month, day, hour, min, sec, tz = "")
ISOdate(year, month, day, hour = 12, min = 0, sec = 0, tz = "GMT")

date.secondsBeforeOrigin

# how does adobe::date deal with NEGATIVE 

# fCalendar 
# new years (US, CH) ... easter (W, E) 
# fathers/mothers ... labor/memorial ... 
# "holiday" wrapper function 
# map from ISO and POSIX 
# ?strptime

#' date.formatTime();
#' date.formatTime("%Y-%m-%d");
#' date.formatTime("%Y-%m-%d %H:%M:%S");
#' date.formatTime("%Y-%V");  # week ISO 8601 [may vary on system] ?
#'
#' date.formatTime("%Y-%m-%d", strptime("23mar1973", "%d%b%Y") );
date.formatTime = function(out="%Y-%m-%d %H:%M:%S", time = Sys.time() )
	{
	# multivariate on both allowed, recycles
	format(time, out);
	}


x3 = date.mktime(0,0,0,8,5,1600);
date.formatTime("%Y-%V-%A", x3);



# If you want to extract specific aspects of a time (such as the day of the week) just convert it to class "POSIXlt" and extract the relevant component(s) of the list, or if you want a character representation (such as a named day of the week) use the format method.


# here are two POSIX date/time classes, which differ in the way that the values are stored internally. The POSIXct class stores date/time values as the number of seconds since January 1, 1970, while the POSIXlt class stores them as a list with elements for second, minute, hour, day, month, and year, among others.

# ISOdate(1977,7,13)
# date.toPOSIXlt = function(
# ISOdate(-200,7,13)
# NEGATIVE VALUES DON'T WORK 

# write functions to deal with negative YEARS
# with NEGATIVE YEARS, we could set ORIGIN ... Julian APR 5, -7 (8BC) and [5] leap year rule 

# https://stackoverflow.com/questions/39526527/is-there-a-reliable-way-to-detect-posixlt-objects-representing-a-time-which-does
# The value of as.POSIXct(test) seems to be platform dependent



# mydate = as.POSIXlt('2005-4-19 7:01:00')
# doesn't have attributes




# https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/now
# The static Date.now() method returns the number of milliseconds elapsed since January 1, 1970 00:00:00 UTC.
# The MySQL function NOW() gives the dateTime value in this format: 'YYYY-MM-DD HH:MM:SS' . 
# PHP ... $date =  date("Y-m-d H:i:s");
# 

date.now = function(format="MySQL", tz="")
	{
	now = Sys.time();
	now;
	}












#' asDateTime
#'
#' @param strvec
#' @param from
#' @param to
#' @param num
#'
#' @return
#' @export
#'
#' @examples
asDateTime = function(strvec, from="%Y-%m-%d %H:%M:%S", to="", num=TRUE)
	{
	# ?as.Date
	# https://www.r-bloggers.com/2013/08/date-formats-in-r/
	p.obj = strptime(strvec, format=from );
	res = p.obj;
	if(to != "") { o.obj = strftime(p.obj,  format=to); res = o.obj; }
	if(num) { res = as.numeric(res);}

	res;
	}







 
	> now = Sys.time()
> now 
[1] "2022-08-11 07:37:36 EDT"
> str(now)
 POSIXct[1:1], format: "2022-08-11 07:37:36"
> class(now)
[1] "POSIXct" "POSIXt"

out <- if (!is.POSIXct(time)) as.POSIXct(time) else time
  attr(out, "tzone") <- tzone
  if (is.POSIXlt(time)) {
    out <- as.POSIXlt(out)
  }
  out
  
#  origin = structure(0, class = c("POSIXt", "POSIXct"))




	
	
	
	
	
	
	
	

					  




# http://www.webexhibits.org/calendars/calendar-christian.html




	
# strftime('%Y-%m-%d', time, 'UTC', 'local')

# strftime(date.getNow(), format='%Y-%m-%d', tz='UTC')
# string format time ... not POSIX 
# strptime ... string POSIX time 
## NOT a string input ... 

# stri_timezone_list ... with UTC offsets .. 
# stri_datetime_create(5775, 8, 1, locale='@@calendar=hebrew') # 1 Nisan 5775 -> 2015-03-21
# https://unicode-org.github.io/icu-docs/apidoc/dev/icu4c/calendar_8h_source.html
#include <unicode/calendar.h>
#include <unicode/gregocal.h>
# https://unicode-org.github.io/icu-docs/apidoc/dev/icu4c/classicu_1_1GregorianCalendar.html


## HEBREW 
# Month names
# Biblical references to the pre-exilic calendar include ten of the twelve months identified by number rather than by name. Prior to the Babylonian captivity, the names of only four months are referred to in the Tanakh:

# Aviv – first month[4]
# Ziv – second month[5]
# Ethanim – seventh month[6] and
# Bul – eighth month.[7] ... Bul - shitta ... 
	
	
	
# https://stackoverflow.com/questions/10699511/difference-between-as-posixct-as-posixlt-and-strptime-for-converting-character-v
# First, there are two internal implementations of date/time: POSIXct, which stores seconds since UNIX epoch (+some other data), and POSIXlt, which stores a list of day, month, year, hour, minute, second, etc.
# strptime is a function to directly convert character vectors (of a variety of formats) to POSIXlt format.



# my_date_unformatted <- "10-07-2023 11:38:55"    # Create date with different format
# my_date_3 <- strptime(my_date_unformatted,      # Convert to POSIXlt class
  #                    format = "%m-%d-%Y %H:%M:%S")
					  



# off by a bit ... date.fromJulianDay( date.toJulianDay(0,0,0) )
#  year month   day 
#    0     1    27 
# https://www.sciencedirect.com/topics/engineering/julian-day-numbe


## MAYBE correct ?
## 
## unlist( date.fromJulianDay( date.toJulianDay(0,0,0) ) )
## unlist( date.fromJulianDay( date.toJulianDay(-1,2,30) ) )
## unlist( date.fromJulianDay( date.toJulianDay(-1,1,1) ) )
## unlist( date.fromJulianDay( date.toJulianDay(-1,1,1) ) )
## CONVERGED ... 
## Notice that there is no year zero in the Julian or Gregorian calendars. The day that precedes January 1, 1 A.D. is December 31, 1 B.C. 
## November 16, 1858 == 2,400,000  
## unlist( date.fromJulianDay( date.toJulianDay(1858,11,16) ) )
	
	
	
# MJD 0 thus started on 17 Nov 1858 (Gregorian) at 00:00:00 UTC.
# The Lilian day number is similar to the Julian day number, except that Lilian day number 1 started at midnight on the first day of the Gregorian calendar, that is, 15 October 1582.
	

# https://github.com/derickr/timelib
## this is PHP library for detecting ... lots of REGEX
## https://github.com/derickr/timelib/blob/master/parse_date.re
## 


## in.sol package
		# declination(year,month,day,hour=12,minute=0,sec=0)
		# 	valid 1901 to 2099
		# hour = hour + minute/60 + sec/3600
		# jd = 367*year - (7*(year+(month+9)%/%12))%/%4 + (275*month)%/%9+day+1721013.5 + hour/24
		# if (inverse){ return(as.POSIXct((x-2440587.5)*86400,origin=ISOdate(1970,01,01,0,0,0),format="%Y-%m-%d %H:%M:%S" ))
		# else { return(as.numeric(x)/86400 + 2440587.5)




	
# https://www.r-bloggers.com/2018/07/a-tour-of-timezones-troubles-in-r/
# In any programming tool, dates, times, and timezones are hard. Deceptively hard. They’ve been shaped by politics and whimsy for hundreds of years: timezones can shift with minimal notice, countries have skipped or repeated certain days, some are offset by weird increments, some observe Daylight Saving Time, leap years, leap seconds, the list goes on. Luckily, we rarely need to worry about most of those details because other teams of very smart people have spent a lot of time providing nice abstractions for us that handle most of the weird edge cases
# OlsonName()	














# month names ... 
# Kimberleigh, Karey, Kathy, Khristine, Kandie, Korrie
# Karsen, Katelyn, Karen, Kimberly, Vera, Natalya, 
# XELA is out of time




# https://docs.oracle.com/cd/E17952_01/mysql-8.0-en/date-and-time-type-syntax.html#:~:text=MySQL%20displays%20DATETIME%20values%20in,using%20either%20strings%20or%20numbers.
# A date and time combination. The supported range is '1000-01-01 00:00:00.000000' to '9999-12-31 23:59:59.999999'. MySQL displays DATETIME values in 'YYYY-MM-DD hh:mm:ss[.fraction]'

# https://en.wikipedia.org/wiki/ISO_8601
# ISO 8601:2004 fixes a reference calendar date to the Gregorian calendar of 20 May 1875 as the date the Convention du Mètre (Metre Convention) was signed in Paris

# + 1 month ... "calendarMonth [anchored to modern calendar FEB 28], averageMonth, monthLengthStart [previous, current, next], monthLengthEnd [previous, current, next], 4 'weeks'?
# what does PHP do with + 1 month
# how does strftaim in PHP work?


# holiday ... "as text"
# first day of SPRING in NORTH ==> EQUINOX, first day of MAY, first Monday of September
# full moon something (CHINESE NEW YEAR), Persian Nasraw ... NISSAN (NEW YEAR)
# https://pear.php.net/package/Date_Holidays
# https://stackoverflow.com/questions/6780536/failed-to-download-pear-http-request2-within-preferred-state-stable
# https://en.wikipedia.org/wiki/Public_holidays_in_Argentina
# https://ru.wikipedia.org/wiki/%D0%9F%D1%80%D0%B0%D0%B7%D0%B4%D0%BD%D0%B8%D0%BA%D0%B8_%D0%A0%D0%BE%D1%81%D1%81%D0%B8%D0%B8
# https://en.wikipedia.org/wiki/Public_holidays_in_Russia
            

# date = Sys.time()
# structure(0, class = c("POSIXt", "POSIXct"))
# seq(date, length = 2, by = "day")[2]
# seq(date, length = 2, by = "-1 day")[2]
 
date.findFormat

# input, output ... R, PHP, MYSQL, JavaScript 
# ... output "equivalent" to another language 

# March 21 is the 80th day of the year (81st in leap years) in the Gregorian calendar; 285 days remain until the end of the year.
# universal days ... XELA days 
# anchor March 21, 2000 (at 12:00 noon at GIZA)
# UPDATE March 21, 1973 (at 12:00 noon at GIZA)
## UPDATE March 21, 1972 [CUPS] ... sets Chinese to RAT 
##  UTC ==>   XTC (GiZA)
## XTC:  March 21, 1973 (at solar noon at GIZA) ... FINAL ANSWER?
# call the calendar XELA ... out-of-time (not months) are XELA days
# tz = "" a function of location [NOT based on GOVT rules]
# set.hours = 24; or 36
# set.mins = 60; or 40 (HEBREW?)
# set.seconds = 60 ... blinks of eye or 1/3 of minute (HEBREW)
# set.week = 5 or 10
# set.month = lunar, 30, arbitrary rules 
# new.year = January 1 ... or moon rules 
# set.YEAR0 = 1973 GREG ... -8 JULIAN 
# YEARS start at 0
# offsetYEAR0 = -1980 ... this will set as YEAR 0, (5,25, 500, 5000) rule applies to THAT offsetYEAR0


# For example, the Julian day number for the day starting at 12:00 UT (noon) on January 1, 2000, was 2 451 545
# https://en.wikipedia.org/wiki/Universal_Time#Adoption_in_various_countries
# UTC.modern.long = 0 ... historically these also changed 
# UTC.diff (long from GIZA)
# 29.987, 31.2118
# 29° 59′ 13.2″ N, 31° 12′ 42.48″ E
# https://stackoverflow.com/questions/21414847/convert-a-date-vector-into-julian-day-in-r

# JDN = (1461 * (Y + 4800 + (M - 14)/12))/4 +(367 * (M - 2 - 12 × ((M - 14)/12)))/12 - (3 * ((Y + 4900 + (M - 14)/12)/100))/4 + D - 32075 // julian day number algorithm
# TOD = JDN + ((H-12)/24) + (MN/1440) + (S/86400) // JDN to JD algorithm
# https://www.sciencedirect.com/topics/engineering/julian-day-number#:~:text=The%20Julian%20day%20count%20is,from%20that%20of%20the%20other.
# The astronomical Julian day number is such a system. We will define it as the day count starting with the number 2,400,000 corresponding to November 16, 1858.
# The day that precedes January 1, 1 A.D. is December 31, 1 B.C.


# https://en.wikipedia.org/wiki/Julian_day
# 


# changeover in 1582 or 1752
x1 = date.mktime(0,0,0,10,15,1582);
x2 = date.mktime(0,0,0,10,4,1582);
# windows says 11 days 
#  x1 - x2

x1 = date.mktime(0,0,0,9,2,1752);
x2 = date.mktime(0,0,0,9,14,1752);

# use.calendar = "G/J" ... gregorian or julian
# greg.adoption = 10,4,1582 ... 10,15,1582
#		... OR ... 9,2,1752 ... 9,14,1752
#      ...  OR ... arbitrary (different countries)
# DST daylight savings is not worth it ...

# apply julian / gregorian rules backward in time indefinitely
# use AD/BC to convert to a universal time 

# https://eclipse.gsfc.nasa.gov/SEhelp/dates.html
# https://www.hermetic.ch/cal_stud/cal_art.html
# https://web.archive.org/web/20141130225140/http://astro.nmsu.edu/~lhuber/leaphist.html

# This is illustrated by the adoption of the birth of Christ as the initial epoch of the Christian calendar. This epoch was established by the sixth-century scholar Dionysius Exiguus, who was compiling a table of dates of Easter. An existing table covered the nineteen-year period denoted 228-247, where years were counted from the beginning of the reign of the Roman emperor Diocletian. Dionysius continued the table for a nineteen-year period, which he designated Anni Domini Nostri Jesu Christi 532-550. Thus, Dionysius' Anno Domini 532 is equivalent to Anno Diocletian 248. In this way a correspondence was established between the new Christian Era and an existing system associated with historical records. What Dionysius did not do is establish an accurate date for the birth of Christ. Although scholars generally believe that Christ was born some years before A.D. 1, the historical evidence is too sketchy to allow a definitive dating.

# https://en.wikipedia.org/wiki/Chinese_calendar#Solar_calendars

# One version of the solar calendar is the five-elements calendar (五行曆; 五行历), which derives from the Wu Xing. A 365-day year was divided into five phases of 73 days, with each phase corresponding to a Day 1 Wu Xing element. A phase began with a governing-element day (行御), followed by six 12-day weeks. Each phase consisted of two three-week months, making each year ten months long. Years began on a jiǎzǐ (甲子) day (and a 72-day wood phase), followed by a bǐngzǐ day (丙子) and a 72-day fire phase; a wùzǐ (戊子) day and a 72-day earth phase; a gēngzǐ (庚子) day and a 72-day metal phase, and a rénzǐ day (壬子) followed by a water phase.[1] Other days were tracked using the Yellow River Map (He Tu).

# Another version is a four-quarters calendar (四時八節曆; 四时八节历; 'four sections, eight seasons calendar', or 四分曆; 四分历). The weeks were ten days long, with one month consisting of three weeks. A year had 12 months, with a ten-day week intercalated in summer as needed to keep up with the tropical year. The 10 Heavenly Stems and 12 Earthly Branches were used to mark days.[2]
#  第40篇四時 [Chapter 40: Four Sections]. 管子 [Guanzi] (in Chinese).


# A third version is the balanced calendar (調曆; 调历). A year was 365.25 days, and a month was 29.5 days. After every 16th month, a half-month was intercalated. According to oracle bone records, the Shang dynasty calendar (c. 1600 – c. 1046 BCE) was a balanced calendar with 12 to 14 months in a year; the month after the winter solstice was Zhēngyuè.[3]

# https://eclipse.gsfc.nasa.gov/SEhelp/calendar.html
# The Julian calendar is used for all dates up to 1582 Oct 04. After that date, the Gregorian calendar is used. Due to the Gregorian Calendar reform, the day after 1582 Oct 04 (Julian calendar) is 1582 Oct 15 (Gregorian calendar). Note that Great Britain did not adopt the Gregorian calendar until 1752. For more information, see Calendars.

# The Julian calendar does not include the year 0, so the year 1 BCE[1] is followed by the year 1 CE. This is awkward for arithmetic calculations. All pages in this web site employ the astronomical numbering system for dates (they use the year 0). Years prior to the year 0 are represented by a negative sign. Historians should note that there is a difference of one year between astronomical dates and BCE dates. Thus, the astronomical year 0 corresponds to 1 BCE, and year -100 corresponds to 101 BCE, etc.. (See: Year Dating Conventions )

# There is some historical uncertainty as to which years from 43 BCE to 8 CE were counted as leap years. For the purposes of this web site, we assume that all Julian years divisible by 4 are be counted as leap years.


# date.daysSince(

# September 2, 1752 was followed by September 14, 1752



# Pope Gregory shortened October of A.D. 1582 by ten days. Thursday, October 4, 1582 (Julian) was followed immediately by Friday, October 15, 1582 (Gregorian).
# https://libguides.ctstatelibrary.org/hg/colonialresearch/calendar
# n 45 B.C., Julius Caesar ordered a calendar consisting of twelve months based on a solar year.  This calendar employed a cycle of three years of 365 days, followed by a year of 366 days (leap year).  When first implemented, the "Julian Calendar" also moved the beginning of the year from March 1 to January 1.  However, following the fall of the Roman Empire in the fifth century, the new year was gradually realigned to coincide with Christian festivals until by the seventh century, Christmas Day marked the beginning of the new year in many countries. 

# Because the year began in March, records referring to the "first month" pertain to March; to the second month pertain to April, etc., so that "the 19th of the 12th month" would be February 19.  In fact, in Latin, September means seventh month, October means eighth month, November means ninth month, and December means tenth month.  Use of numbers, rather than names, of months was especially prevalent in Quaker records.

# 1752 MARCH -> JANUARY ...
# The changeover involved a series of steps:

#December 31, 1750 was followed by January 1, 1750 (under the "Old Style" calendar, December was the 10th month and January the 11th)
#March 24, 1750 was followed by March 25, 1751 (March 25 was the first day of the "Old Style" year)
#December 31, 1751 was followed by January 1, 1752 (the switch from March 25 to January 1 as the first day of the year)
#September 2, 1752 was followed by September 14, 1752 (drop of 11 days to conform to the Gregorian calendar)

# ANCHOR to JANUARY 1, 2000 
# count backwards from then, using -days ... (fractions of days could be converted to hours, etc.)









		#  -587094
		# Ruthven offset ...  -4237058 
		# Julian ... +2305675 FROM Ruthven ... -1931383
		# MOD ... Julian - 2400000.5 ...  -4331383.5
		# LILIAN 
		# CUSTOM ... 
# https://www.slideshare.net/chenshuo/datetime-julian-date SLIDE 8

# why is mod this date, BRITAIN was a different date 
# date.toJulianDayNumber(1858, 11, 17-12, "modified"); 	# GREG 
# date.toJulianDayNumber(1600, 8, 5, "ruthven");
# date.toJulianDayNumber(-4712, 1, 1, "julian");		# anchor to special alignment of 3 cycles
# date.toJulianDayNumber(1582, 10, 15-12, "lilian"); 		# GREG
# date.toJulianDayNumber(-7, 3, 21, "equ");

# RUTHVEN-julian
# PAPAL-gregorian
# BRITISH-gregorian






##########################  file #############################



 # as.is to BYPASS the "factors" issue 
  
	# get comments and append as attribute
	# search comments for typeof elements ... 
	# call type of ... if not that type already
df;
  }






####################### more file ###################




##########
# dir.createDirectoryRecursive
# dir.getDirectoryPath
# dir.deleteLocalCacheFolder
# dir.getSourceLocation


# file.readFrom  RDS, PIPE, CSV, BIN, STR (lines), RDS(remote)
# file.writeTo
# file.getDirectoryName # dirname
# file.move  # file.rename(from, to)
# file.writeLine

# is this url.download() not file?
file.download = function() {} 
	# Note that you cannot use devtools::install_github() because it uses curl ;)
	# install.packages("https://github.com/jeroen/curl/archive/master.tar.gz", repos = NULL)
	# https://jeroen.cran.dev/curl/

# curl::curl_version()
# libcurlVersion()
## https://github.com/jeroen/curl/issues/276


# /*
# library(curl)

# repro <- function(n) {
  # urls <- paste0("https://httpbingo.org/get?q=", 1:n)

  # make_handle <- function(url) new_handle(url=url)

  # pool <- new_pool()

  # fail <- function(msg) cat("failed connection:", msg, "\n")

  # done <- function(data) cat("status:", data$status_code, "\n")

  # for(u in urls)
	# multi_add(make_handle(u), done=done, fail=fail, pool=pool)

  # stat <- multi_run(timeout=10, pool=pool)

  # cat("remaining:",  stat$pending, "\n")
# }
# */
































































#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' createDirectoryRecursive
#'
#' @param folder the folder to be created
#'
#' @return
#' @export
#'
#' @examples
#' # createDirectoryRecursive("R:/monte/says/hi/");
createDirectoryRecursive = function(folder, verbose=TRUE)
  {
	msg = list();
	msg[["EXISTS"]] = paste0(" DIRECTORY ", "\n\t\t\t", folder, "\n\n\t", "already exists", "\n\n");
	msg[["ATTEMPT"]] = paste0(" ATTEMPTING TO CREATE DIRECTORY ", "\n\t\t\t", folder, "\n\n");
	msg[["SUCCESS"]] = paste0(" SUCCESS ", "\n\t\t\t", folder, "\n\n");
	msg[["FAILURE"]] = paste0(" FAILURE ", "\n\t\t\t", folder, "\n\n");

			
  if(dir.exists(folder))
	{
	if(verbose) { print(msg$EXISTS); }
	}
	else
		{
		if(verbose) { print(msg$ATTEMPT); }

		dir.create(folder, recursive=TRUE);

		if(dir.exists(folder))
			{
			if(verbose) { print(msg$SUCCESS); }
			} else {
					if(verbose) { print(msg$FAILURE); }
					}
		}
	
	}












#' writeLine
#'
#' This function writes a single character string to a file.
#' Very useful for simulations and building data one line at a time.
#' [ encoding is slow ]
#'
#' @param str The character string to be written
#' @param append If TRUE, will append to the end of the file, otherwise it will overwrite an existing file
#' @param end EOL character to finish the line; the line separator
#' @param file The file to store the (str) line
#'
#' @return
#' @export
#'
#' @examples
#' # writeLine("hello there", file="R:/monte/says/hi/again/my.log", append=FALSE);
#' # writeLine("hi again", file="R:/monte/says/hi/again/my.log");
#  'append' is missing in base::writeLines
writeLine = function(str, file, append=TRUE, end="\n")
  {
  cat( paste(str, end, sep=""),
		file=file,
		sep="", append=append );
  }


#' @rdname file.writeLine
#' @export
file.writeLine = writeLine;




#' storeToFile
#'
#' Store a string to a file (e.g., an HTML page downloaded).
#'
#' @param str The string to store
#' @param file The file to store the string (it will override).
#'
#' @return
#' @export
storeToFile = function (str, file, method="cat", ...)
	{
	mmm = prep.arg(method, 3);
	if(mmm == "cha") { writeChar(str, file, ... ); }
	if(mmm == "cat") { cat(str, file=file, append=FALSE); }	
	}


#' @rdname file.storeToFile
#' @export
file.storeToFile = storeToFile;




# readChar(con, nchars, useBytes = FALSE)

# writeChar(object, con, nchars = nchar(object, type = "chars"),	   eos = "", useBytes = FALSE)



#' readRDS.url
#'
#' This wraps 'url' and 'readRDS' so they are webfriendly ...
#'
#' @param file The file is likely a URL in this function
#'
#' @return a data object, likely a dataframe or a list
#' @export
#'
readRDS.url = function(file)
	{
	# update this function based on functions-url.R 
	file = cleanup.url(file);
	# kudos to antonio
	readRDS( url(file) );
	# https://stackoverflow.com/questions/19890633/
	# readRDS( RCurl::getURL(file, ssl.verifypeer=0L, followlocation=1L));
	}













#' file.readLines
#'
#' @param file
#' @param n
#' @param skip
#'
#' @return
#' @export
#'
#' @examples
file.readLines = function(file, n=-1, skip=NULL)
	{
	# why base::readLines doesn't have skip ?!?
	# where did the fopen/fread stuff go ... that would enable skip
	content = readLines(file, n=n);
	# fopen is "open" connection, I can rework this ...
	nlen = length(content); # how many lines ?
	if(!is.null(skip))
		{
		if(skip < nlen)
			{
			content = content[skip:nlen];
			}
		}
	paste(content, collapse="\n");
	}






isForceDownload = function(args)
	{
	force.download = FALSE;
	if(exists("args"))
			{
			if(exists(".dots.keys.", where=args))
				{
				if(is.element("force.download", args$.dots.keys.))
					{
					# idx = which(args$.dots.keys. == "force.download");
					force.download = args$.dots.vals.$force.download;
					}
				}
			}
	force.download;
	}






#' readStringFromFile
#'
#' @param myFile
#' @param n
#' @param method
#' @param source
#'
#' @return
#' @export
#'
#' @examples
readStringFromFile = function(myFile, n = NULL, method ="readChar", source = "local")
	{
	# methods are "readChar" or "readLines"
	# readChar is one long string; readLines is a vector broken on "\n"
	if(source == "remote")
		{
		myFile = cleanup.url(myFile);
		if(is.null(n)) { n = if(method == "readLines") { n = -1; } else { n = (2^31 - 1); } }
		} else {
				if(is.null(n)) { n = if(method == "readLines") { n = -1; } else { n = file.info(myFile)$size; } }
				}

	if(method == "readLines")
		{
		readLines(myFile, n);
		} else 	{
				readChar(myFile, n);
				}
	}








#' getSourceLocation
#'
#' @param tmp.folder
#' @param create
#'
#' @return
#' @export
#'
#' @examples
getSourceLocation = function(tmp.subfolder = "/humanVerse/cache/", create=FALSE)
  {
  my.tmp = Sys.getenv("HUMANVERSE_CACHE");
	if(trimMe(my.tmp) == "") { my.tmp = Sys.getenv("TMP"); }
	if(trimMe(my.tmp) == "") { my.tmp = Sys.getenv("TEMP"); }
	if(trimMe(my.tmp) == "")
		{
		message.stop ("Function: *getSourceLocation* requires \n\t a HUMANVERSE_CACHE or TMP or TEMP folder \n\t in your 'Sys.getenv()' \n   Maybe run 'Sys.setenv(\"HUMANVERSE_CACHE\" = \"/path/to/CACHE\")' \n\t and make certain the directory is made and writeable \n\t as in 'mkdir /path/to/CACHE' ");
		}


  tmp = gsub("\\", "/", paste0(my.tmp,"/") , fixed=TRUE); # windoze?
  mypath = paste0(tmp, tmp.subfolder);
  mypath = cleanup.local(mypath);
	if(create) { createDirectoryRecursive(mypath); }

  mypath;
  }


#' getDirectoryPath
#'
#' @param file
#' @param trailing
#'
#' @return
#' @export
#'
#' @examples
getDirectoryPath = function(file, trailing=TRUE)
	{
	dn = dirname(file);
	dn = str_replace("\\", "/", dn); # windoze issues
	if(trailing)
		{
		paste0(dirname(file), "/");
		} else 	{
				dirname(file);
				}
	}





#' getRemoteAndCache
#'
#' @param remote
#' @param local.file
#' @param tmp.folder
#' @param force.download
#' @param verbose
#' @param md5.hash
#' @param append
#'
#' @return
#' @export
#'
#' @examples
getRemoteAndCache = function(remote, local.file = NULL, local.pre = "TMP",
	tmp.folder = "/humanVerse/cache/", force.download = FALSE,
	verbose = FALSE, md5.hash = FALSE, append = "")
  {
  remote = cleanup.url(remote);
  useTEMP = FALSE;
  trailingSlash = ( lastChar(remote) == "/");
  if(verbose)
	{
	cat("\n", "remote ... ", remote, "\n\n");
	cat("\n", "force.download ... ", force.download, "\n\n");
	}
  if(!is.null(local.file))
	{
	localpath = paste0( dirname(local.file), "/", local.pre, "/" );  # project subfolder
		createDirectoryRecursive(localpath);
	if(!dir.exists(localpath)) { useTEMP = TRUE; }
	} else { useTEMP = TRUE; }

if(verbose)
	{
	cat("\n", "useTEMP ... ", useTEMP, "\n\n");
	}

  if(useTEMP)
	{
	subfolder = if(trailingSlash) {  folderizeURL(remote); } else { folderizeURL(dirname(remote)); }
	filestem  = if(trailingSlash) {  "index.html" } else { basename(remote); }

	# if(!isFALSE(append.me)) { filestem = cleanup.local(filestem); } # this will append ".html" if necessary ...

	filestem = cleanup.local(filestem, append=append);

	if(md5.hash) { filestem = md5(filestem); }

	mypath = getSourceLocation(subfolder);
		createDirectoryRecursive(mypath);
	myfile = paste0(mypath,"/",filestem);
	} else {
			mypath 		= dirname(local.file);
				createDirectoryRecursive(mypath);
			filestem 	= basename(local.file);
			myfile 		= local.file;
			}


	myfile 		= cleanup.local(myfile);
	mypath 		= cleanup.local(mypath);
	filestem 	= cleanup.local(filestem);

	myfile = setAttribute("path", 		mypath, 	myfile);
	myfile = setAttribute("filestem", 	filestem, 	myfile);

  if(verbose)
	{
	cat("\n", "myfile ... ", myfile, "\n\n");
	}

  # cat("\n", "mypath ... ", mypath, "\n\n");

  if(force.download)
	{
	if(file.exists(myfile))
	  {
		mypath.b = paste0(mypath, "/.backup/");  createDirectoryRecursive(mypath.b);
		myfile.b = paste0(mypath.b, "/", filestem, "-", as.integer(Sys.time()) );

	  # file.copy(myfile, myfile.b);  # this is not file.move, doesn't exist
	  # unlink(myfile);

		moveFile(myfile, myfile.b);
	  }
	}
  if(!file.exists(myfile))
	{
	downloadFile(remote, myfile, cacheOK = !force.download);
	}
  myfile;
  }


#' moveFile
#'
#' @param src
#' @param dest
#' @param delete.src
#'
#' @return
#' @export
#'
#' @examples
moveFile = function(src, dest, delete.src=TRUE)
	{
		src 	= as.character(src);
		dest 	= as.character(dest);
	file.copy(src, dest);  # there is no file.move ???
	if(delete.src) { unlink(src); }
	}


#' deleteLocalCacheFolder
#'
#' @param folder
#'
#' @return
#' @export
#'
#' @examples
deleteLocalCacheFolder = function(folder)
  {
# TODO # unlink("tmp", recursive = TRUE)
  }


#' downloadFile
#'
#' @param remote
#' @param myfile
#' @param n
#' @param quiet
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
downloadFile = function(remote, myfile, n=(2^31 - 1), quiet = TRUE, mode="wb", ...)  # n could be 2^31 - 1
  {
  if(isTRUE(capabilities("libcurl")))
	{
	utils::download.file(remote, myfile, quiet = quiet, mode=mode, ...);
	} else {
			# this approach is not working ... maybe readChar
			raw.binary = readBin(remote, "raw", n);
			# what if I don't have stringi ???   ... encoding = "UTF-8"
			url.encoding = "UTF-8";
			if( isTRUE(requireNamespace("stringi", quietly = TRUE)) )
				{
				url.encoding = stringi::stri_enc_detect(raw.binary)[[1]]$Encoding[1];
				}
			raw.out = iconv( readBin(raw.binary, character()), from = url.encoding, to = "UTF-8");
			writeChar(raw.out, myfile);
			}
  }










#########################   date     ##########################



# date.time = function(datePOSIX) 
# date.




#  date.calculateLeapDays(100*15:18, "julian", "integer");
#  date.calculateLeapDays(100*15:18);




# date.getTimeZoneOffset(OlsonNames()[123])
date.getTimeZoneOffset = function(tz = "UTC")
	{
	info = stringi::stri_timezone_info(tz);
	info$RawOffset;
	}
	

date.setOrigin = function(origin)
	{
	memory.init();
	memory.set("tz.origin", "DATE", origin);
	invisible(origin);
	}
	
date.getOrigin = function()
	{
	memory.init();
	x = memory.get("tz.origin", "DATE");
	if(is.null(x))
		{
		# x = structure(0, class = c("POSIXt", "POSIXct") ); # list and compact
		x = structure(0, class = "POSIXct" ); # compact only
		memory.set("tz.origin", "DATE", x);
		}
	x;
	}
	
date.init = function()
	{
	memory.init();
	
	# ?OlsonNames ... interesting
	# As from R 3.5.0, when a time zone location is first found in a session, its value is cached in object .sys.timezone in the base environment.
	# https://data.iana.org/time-zones/tz-link.html#tzdb
	# https://time.is/ [your clock is 0.6 seconds behind ... latency?]
	
	memory.set("tz.names", 	"DATE", OlsonNames() );  # doesn't have map to offsets?  stringi
	memory.set("tz.out", 	"DATE", "UTC");
		s.tz = Sys.timezone();
	memory.set("tz.in", "DATE", s.tz);
	memory.set("tz.local", "DATE", s.tz);
	memory.set("tz.local", "DATE", date.getTimeZoneOffset(s.tz) );
	# 1970 EPOCH 
	memory.set("tz.origin", "DATE", structure(0, class = c("POSIXt", "POSIXct") ) );
	
		i.tz = stringi::stri_timezone_info();
	memory.set("tzi.local", "DATE", i.tz);
	memory.set("tzi.local.offset", "DATE", i.tz$RawOffset);
	
	# x = stri_timezone_list(); y = OlsonNames(); stopifnot(identical(x,y));
	# stri_timezone_list(offset=5.5)
	# stri_timezone_list(region='US', offset=-10)
	
		cat("\n", " DATE INIT:  unless otherwise specified, all dates ",
					" will be outputed to [UTC] time.  Your local timezone is: ",
					Sys.timezone(), " \n\n\t ", "You can change these default ",
					" paramaters any time with `memory.set` and access them ",
					" with `memory.get`.  Please look inside this function ",
					" for more details. ", "\n");
	
	}
	

date.getVar = function(key)
	{
	ke = prep.arg(key, 2, keep=".");
	if(key == "l.t")  # local.tz
		{
		
		}
	
	}



# ISO 8601 date format
# https://en.wikipedia.org/wiki/Proleptic_Gregorian_calendar
# https://en.wikipedia.org/wiki/Gregorian_calendar
# Meeus # https://articles.adsabs.harvard.edu/pdf/1992JBAA..102...40M
# Frank 

# G = 365.2425
# J = 365.25
# X = 365.2422 ?
# The mean tropical year is 365.24219 days long
date.getDaysInTropicalYear = function()
	{
	# Meeus shows the fluctuations ... this is the mean 
	days = 365.24219;
	days;
	}


## everything to "UTC" than back to your format 
## sometimes UTC is tz.in, sometimes tz.out 

date.getTimeZone = function(which.tz="in")
	{
	wt = prep.arg(which.tz, 2);
	# if from INIT, we have other parameters, use them 
	
	## [lo]cal ... 
	
	# CURRENTLY
	if(wt == "ou") { return("UTC"); }
	Sys.timezone();
	}
		
date.checkTimeZone = function() {} 		
date.checkTimeZone = function(tz)
	{
	tz %in% OlsonNames();
	}
	
	
date.convertFromUTC = function(datePOSIX.UTC, out.tz="local")
	{
	if(out.tz == "local") { out.tz = 
	if(!is.null(out.tz)) 
		{ 
		res = 	as.POSIXct(res, tz = out.tz, origin=origin); 
		}
	
	}


date.searchForTimeZone = function(tzw = "*Cairo*")
	{	
	memory.init();
	timezones = memory.get("timezones");
	res = regex.wildcardSearch(timezones, tzw);
	if(length(res) < 1) { return(NULL); }
	
	out = timezones[res];
	names(out) = res;
	out;
	}


date.checkPOSIXct = function(datePOSIX, in.tz, origin, out.tz)
	{
	# recasting, or actually check elementwise?
	# if(multivariate) ... could do which but doesn't seem to work on a partial vector ... 
	res = datePOSIX;
	if(!is.POSIXct(datePOSIX[1]))
		{	
		res = as.POSIXct(datePOSIX, tz = in.tz,  origin=origin);
		}
	if(!is.null(out.tz)) 
		{ 
		res = 	as.POSIXct(res, tz = out.tz, origin=origin); 
		}
		
	res;
	}
	
	
		
	
# args = list(.dot.keys. = c("in.tz","out.tz", "origin"), .dot.vals. = list("monte", "natalya", "alex"));
	
date.defaults = function(dots)
	{
	# cat("\n\n ==== DEFAULTS ===== \n\n");
	# dput(dots);  
	# cat("\n\n ==== DEFAULTS ===== \n\n");
	
	
	in.tz = if("in.tz" %in% dots) { dots$in.tz; } else { date.getTimeZone("in"); }
	out.tz = if("out.tz" %in% dots) { dots$out.tz; } else { date.getTimeZone("out"); }
	origin = if("origin" %in% dots) { dots$origin; } else { date.getOrigin(); }
	
	pf = parent.frame(1);
	assign("in.tz", in.tz, envir=pf);
	assign("out.tz", out.tz, envir=pf);
	assign("origin", origin, envir=pf);
	}
				
### TODO ... date.defaults() ... list extract ... set like str.MD5
					
date.toUnix = function(datePOSIX = date.now(), ...)
	{
	dots = functions.getParameterInfo("dots");
	# date.defaults will select dots info, fall back to a default ... 
	date.defaults(dots); # assign in date.defaults return here ... 
	
	res = date.checkPOSIXct(datePOSIX, in.tz, origin, out.tz);
	as.numeric(res);
	}
	
# https://www.neonscience.org/resources/learning-hub/tutorials/dc-convert-date-time-posix-r
# eg.lt = as.POSIXlt("2015-01-19 10:15"); str(eg.lt); str(unclass(eg.lt));
# eg.ct = as.POSIXct("2015-01-19 10:15"); str(eg.ct); str(unclass(eg.ct));	
date.fromUnix = function(unixNumeric, ..., return="lt")
	{
	args = functions.getParameterInfo();  # why is this not directly built in ??? JS 1995
	# date.defaults will choose the default, or value from dots ... 
	date.defaults(args$dots); # assign in date.defaults return here ... 
	
	re = prep.arg(return, 2);  # "lt" or "ct"
	
	if(re == "ct") 
		{ 
		res = as.POSIXlt(unixNumeric, tz=in.tz, origin=origin);
		# rollback to in.tz / out.tz [REVERSE?]
		return(res);
		}
	
	res = as.POSIXct(numvec, tz=in.tz, origin=origin);
	
	if(which != "ct")
		{
		as.POSIXlt(numvec, origin=origin);
		} else	{
				as.POSIXct(numvec, origin=origin);
				}
	}
	
	
	
	

## accessors 

date.get = function() {}	
date.get = function(datePOSIX = date.now(), 
							in.tz  = Sys.timezone(), 
							origin = date.getOrigin(),
							out.tz = NULL,

						use.numeric = FALSE,
						format = "%Y-%m-%d", 
						formatter="R")
	{
	res = date.checkPOSIXct(datePOSIX, in.tz, origin, out.tz);
	res = format(res, format = format);
	if(use.numeric) { res = as.numeric(res); }
	res;
	}



					  
# YYYYMMDDHHMMSS
date.getYMDHMS = function() {}
date.getYMDHMS = function(datePOSIX = date.now(), 
							in.tz  = Sys.timezone(), 
							origin = date.getOrigin(),
							out.tz = NULL,

						use.numeric = FALSE,
						format = "%Y%m%d%H%M%S",
						formatter="R")
	{
	res = date.checkPOSIXct(datePOSIX, in.tz, origin, out.tz);
	res = format(res, format = format);
	if(use.numeric) { res = as.numeric(res); }
	res;
	}

date.getYear = function() {}	
date.getYear = function(datePOSIX = date.now(), 
							in.tz  = Sys.timezone(), 
							origin = date.getOrigin(),
							out.tz = NULL,

						use.numeric = TRUE,
						format = "%Y",
						formatter="R")
	{
	res = date.checkPOSIXct(datePOSIX, in.tz, origin, out.tz);
	res = format(res, format = format);
	if(use.numeric) { res = as.numeric(res); }
	res;
	}





	
date.getMonth = function(datePOSIX = date.now(), 
							in.tz  = Sys.timezone(), 
							origin = date.getOrigin(),
							out.tz = NULL,

						use.numeric = TRUE,
						format = "%m",
						formatter="R")
	{
	res = date.checkPOSIXct(datePOSIX, in.tz, origin, out.tz);
	res = format(res, format = format);
	if(use.numeric) { res = as.numeric(res); }
	res;
	}
	
date.getDay = function(datePOSIX = date.now(), 
							in.tz  = Sys.timezone(), 
							origin = date.getOrigin(),
							out.tz = NULL,

						use.numeric = TRUE,
						format = "%d",
						formatter="R")
	{
	res = date.checkPOSIXct(datePOSIX, in.tz, origin, out.tz);
	res = format(res, format = format);
	if(use.numeric) { res = as.numeric(res); }
	res;
	}
	

# date.getDaysInTropicalYear()

# http://www.webexhibits.org/calendars/calendar-christian.html
# modified Julian day number (MJD)
# MJD 0 thus started on 17 Nov 1858 (Gregorian) at 00:00:00 UTC.
# The Lilian day number is similar to the Julian day number, except that Lilian day number 1 started at midnight on the first day of the Gregorian calendar, that is, 15 October 1582.
## different OFFSETS ...

# https://articles.adsabs.harvard.edu/pdf/1992JBAA..102...40M
# refers to EPOCH 1900 for NASA?
## Frank ... https://www.mreclipse.com/Special/quotes2.html
## SOLAR eclipses, history

# this builds a propleptic calendar 
date.daysFromEpoch = function() {}
date.daysFromEpoch = function(datePOSIX, 
									epoch.key="G",	# [G]regorian, [J]ulian, [M]odified Julian, [L]ilian,  [X]elian 
									epoch.start, 	# 1572, 46 BC (when was 4 cycle), 3/21/1973
									epoch.offset	# 1752, ???, -1980
							)	
	{
	# https://www.sciencedirect.com/topics/engineering/julian-day-numbe
	# The Julian day number is the number of days since noon UT on January 1, 4713 BCE
	# Notice that there is no year zero in the Julian or Gregorian calendars. The day that precedes January 1, 1 A.D. is December 31, 1 B.C. If you use the formula above to determine a Julian day number of a B.C. date, you must convert to negative year numbers, as, for instance, 10 B.C. which must be entered as -9
	# http://www.webexhibits.org/calendars/week.html
	# http://www.webexhibits.org/calendars/calendar-christian.html
	
	# from.calendar="[G]"  to.days=[J]
	# Try this one (the divisions are integer divisions, in which remainders are discarded):

	month = date.getMonth(datePOSIX)

	a = as.integer( (14-month)/12 );
	y = year+4800-a
	m = month + 12*a - 3
	
	

	
	
	
	}








#In Roman times, the hour was defined as 1/12 of the time period between sunrise and sunset. Since this interval varies with seasons, the “hour” was longer in the summer than in the winter.

#At the latitude of Rome, about N, 1 hour would last anywhere between 44.7 modern minutes (in the end of December) and 75.3 (in mid-June). 





#' getDate
#'
#' Wrapper for 'format' function
#'
#' @param how See options using ?as.Date [?strptime for codes]
#' @param when You can pass in a time, or let it grab current time
#'
#' @return date string
#' @export
#'
#' @examples
#' getDate();
#' getDate("%Y-%m-%d");
#' getDate("%Y-%m-%d %H:%M:%S");
#' getDate("%Y-%V");  # week ISO 8601
#'
#' getDate("%Y-%m-%d", strptime("23mar1973", "%d%b%Y") );
# get.date
# date.get

getDate = function(how="%Y-%m", when = Sys.time())
	{
  # php date syntax is a bit different
  # doesn't allow for microtime
  # this needs to be updated to a modern version of the ISO standard (R::base)
	format(when, how);
	}





#' convertDateStringToFormat
#'
#' basically wraps strptime and strftime into this single call
#'
#' I have this vector of dates in string format;
#' I want to convert it to this format (numeric),
#' and currently, they are of this format.
#'
#' @family Date-Time
#'
#' @param strvec one or more strings, such as \code{"3/24/2010 18:33"}
#' @param to how you want to return the date \code{"\%Y"} is just Year
#' @param to.name name(s) given to the \code{to} column(s)
#' @param from format of input, default \code{"\%Y-\%m-\%d \%H:\%M:\%S"}
#' @param num if TRUE (default), will return numeric form.
#'
#' @return dataframe same length as strvec, with one or more columns
#' @export
#' @examples
#'
#' date.strings = c("3/24/2010 18:33", "9/3/2009 17:28", "10/14/2009 11:40",
#'                  "7/3/2015 11:16", "11/18/2010 1:29", "4/23/2011 0:08",
#'                  "10/6/2010 11:13", "7/26/2009 13:23","4/9/2008 13:40",
#'                  "8/20/2008 11:32");
#'
#' years = convertDateStringToFormat(date.strings,
#'                              "%Y", "years",
#'                                                            "%m/%d/%Y %H:%M");
#'
#' weeks = convertDateStringToFormat(date.strings,
#'                              "%W","weeks",
#'                                                             "%m/%d/%Y %H:%M");
#'
#' days = convertDateStringToFormat(date.strings,"
#'                               %j", "days",
#'                                                            "%m/%d/%Y %H:%M");
#'
#' ywd = convertDateStringToFormat( date.strings,
#'                              c("%Y","%W","%j"), c("year","week","day"),
#'                                                            "%m/%d/%Y %H:%M");
#'
#'
#' Ymd = convertDateStringToFormat(date.strings,
#'                               "%Y-%m-%d","ISO8601",
#'                                                  "%m/%d/%Y %H:%M",num=FALSE);
#'
convertDateStringToFormat = function (strvec,to="%Y",to.name="years",from="%Y-%m-%d %H:%M:%S",num=TRUE)
	{
  nt = length(to);
  result = NULL;
  for(i in 1:nt)
    {
	  p.obj = strptime(strvec, format=from );
	  o.obj = strftime(p.obj,  format=to[i] );
	  n.obj = if(num) { as.numeric(o.obj); } else { o.obj; }
	  result = cbind(result, n.obj);
    }
    colnames(result) = to.name;
	as.data.frame(result);
	}




################# need to think about how to rework colors ### TODO ####



# https://blog.revolutionanalytics.com/2009/01/multicolor-text-in-r.html
# same text, phantom on some of it, overlay in correct order ... black/red/blue
# used in plotting

# fonts also in plotting
# text ASCII art of V from Vendette ... use 3.14159 on circle, and primes on V ... 
# load in via magic, create matrix? manually adjust
# add to messages, right side ... 
# add to headers of READ pipe ... 

# https://github.com/r-lib/testthat/blob/717b02164def5c1f027d3a20b889dae35428b6d7/R/colour-text.r
# R: cat of many colors ... shows hacked version of solution
# https://stackoverflow.com/questions/24967412/r-a-cat-of-many-colors

# ASCII from readPNG / readJPG, ... not magick ...
# https://github.com/ctaggart878/asciiart/blob/master/asciiArt.R
 
# https://w7.pngwing.com/pngs/114/326/png-transparent-carmageddon-youtube-call-of-duty-modern-warfare-2-fortnite-game-others-game-heart-xbox-one.png
	
color.init = function()
	{
	memory.init();
	
	invisible( color.buildColorTable() );
	
	# MSG INIT ... welcome to the humanVerse ... 
	
	
# ?convertColor 
# ?colorConverter
# Conversion algorithms from http://www.brucelindbloom.com.
# ?col2rgb

# hue, hsl, cmyk ... hsv 
	
	}


color.buildColorTable = function()
	{
debug = FALSE;
	x = memory.get("table", "COLORS");
	if(!is.null(x)) { return(x); }
	
	# local in package, change HVcpp to humanVerse 
	y = suppressError( system.file("inst/R/colors.rds", 
									package = "HVcpp", mustWork = TRUE),
					show.notice=debug, msg="debug: color.buildColorTable" );
					
	if(!is.error(y)) 
		{ 
		z= readRDS(y);
		memory.set("table", "COLORS", z);
		return(z);
		}
		
	# github ?
	# https://github.com/MonteShaffer/humanVerse/raw/main/humanVerse/inst/R/colors.rds
	# download and readRDS 
	
	## otherwise, rebuild from 
	# https://github.com/MonteShaffer/humanVerse/tree/main/-data-/-colors-
	
	
	}





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' dechex
#'
#' Converts a decimal to hexadecimal
#'
#' @param intdec vector of one or more integer values
#' @param ... vector of one or more integer values
#' @param n Should we prepend some zeroes, if so how many?
#' @param hash Should we pre..pre-pend the "#" for colors?
#'
#' @return vector of one or more hex values as a string
#' @export
#'
#' @examples
#' dechex(c(123,255,50), n=2, pre="0x");
#' dechex(123,255,50, n=2, pre="#");
#' dechex(c(16581375,12581375,50), n=6, pre="#");
#' dechex(c(16581375,12581375,50), pre="0x");
#' dechex(c(255,133,50));
dechex = function(intdec, ..., n=NULL, pre=NULL, use.names.if.available=TRUE)
	{
	intdec = dots.addTo(intdec, ...);
	res = vector.useNames(intdec);
	if(use.names.if.available && !is.null(res)) { return(res); }
	
	res = toupper( as.character( as.hexmode( as.integer( intdec ) ) ) );
	# if the vector already has two-character mode ... dechex( 0:255);  ... n is not necessary
	if(!is.null(n)) { res = str.pad( res, n, "0", "LEFT"); 	}
	if(!is.null(pre)) { res = paste0(pre,res); }
	names(res) = as.character(intdec);
	res;
	}


#' @rdname dec2hex
#' @export
dec2hex = dechex;


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' hexdec
#'
#' Converts a hexadecimal to decimal
#'
#' @param hexstr vector of one or more hex values as a string
#' @param ... vector of one or more hex values as a string
#'
#' @return vector of one or more integer values
#' @export
#' @alias hex2dec
#'
#' @examples
#' hexdec("FF");
#' hexdec("0xFFFF");
#' hexdec("0xFFFF", "#FF");
#' hexdec(c("0xFFFF", "FF"), "#DDEECC");
#' x = c("0xFFFF", "#aF", "#dDEeCc");
#' y = hexdec(x);
#' z = dechex(y); 
#' y2 = hexdec(z); stopifnot(identical(y,y2));
hexdec = function(hexstr, ..., use.names.if.available=TRUE)
	{ 
	hexstr = dots.addTo(hexstr, ...);
	o.hexstr = hexstr;
	res = vector.useNames(hexstr);
	if(use.names.if.available && !is.null(res)) { return(res); }
	
	# if it has "color" pre-pend, remove it ...	
	hexstr = str.replace("#", "", hexstr);
	
	# rather than checking, let's remove and add leading "0x"
	hexstr = paste0("0x", str.replace("0x", "", str.trim(tolower(hexstr))) );
	
	res = str.toInteger(hexstr, TRUE);
	names(res) = o.hexstr;
	res;
	}
	
	str.toInteger( paste0("0x",str.trim(toupper(str.replace(c("#","0x"), "", hexstr)))), TRUE);



#' @rdname hex2dec
#' @export
hex2dec = hexdec;



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' color.col2rgb 
#'
#' Extends the built-in grDevices::col2rgb function
#' [ See grDevices::convertColor or grDevices::make.rgb ]
#'
#' @param x vector of colors, with ... as lazy input
#' @param ...  Allows for lazy input of colors 
#' @param alpha FALSE (RGB matrix); TRUE (RGBa matrix)
#' 
#' @return matrix of colors in RGB matrix format
#' @export
#'
#' @examples
#' color.col2rgb(c("red","#FF9944",2));  # colors() = "red", HEX, colors()[2]
#' color.col2rgb("red","#FF9944",2);
#' 
#' colors = c("red","#FF9944",2);
#' cat("\n HVcolor \n");
#' x = color.col2rgb(colors); y = color.rgb2col(x);  stopifnot(identical(y, colors));
#' cat("\n BASE \n");
#' x = grDevices::col2rgb(colors); y = rgb2col(x); stopifnot(identical(y, colors));
color.col2rgb = function(colors, ..., alpha=FALSE)
	{
	colors = dots.addTo(colors, ...);
	res = grDevices::col2rgb(colors, alpha=alpha); 
	colnames(res) = colors;  # original, so inverse will know
	res;
	}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#'
#' color.rgb2col (as rgb2col)
#'
#' Reverse the built-in grDevices::col2rgb function
#' [ See grDevices::convertColor or grDevices::make.rgb ]
#'
#' @param matrixRGB matrix of RGB or RGBa colors (with names)
#' @param force.hex TRUE (returns HEX)
#'					FALSE (names if exist; HEX otherwise)
#'
#'
#' @return CharacterVector of names (if exist), hex colors if not 
#' @export
#'
#' @examples
#'
color.rgb2col = function(matrixRGB, force.hex=FALSE)
	{
	res = grDevices::rgb( t(matrixRGB)/255, names=colnames(matrixRGB));
	if(force.hex){ return(res); }
	if(!is.null(names(res))) { names(res); } else { res; }
	}	
	  
#' @rdname rgb2col
#' @export
rgb2col = color.rgb2col; 	# also works as an inverse for the base




color.forceHEX = function(colors, ..., method="base")
	{
	colors = dots.addTo(colors, ...);
	x = suppressError( grDevices::col2rgb(colors) );
	if(!
	
	}


if(FALSE)
	{
	#check-tables ...
	#if not local, RDS

	#go grab from web ... 

	#setwd("C:/_git_/github/MonteShaffer/humanVerse/-data-/-colors-")

	files = list.files(".", "*.txt");
	# https://github.com/MonteShaffer/humanVerse/tree/main/-data-/-colors-/
	df = readFromPipe("X11-141.txt");
	for(file in files)
		{
		if(file != "X11-141.txt")
			{
			tmp = readFromPipe(file); tmp$class = "";
			df = rbind(df, tmp );
			}
		}
		
	# system(colors.rds)... web(colors.rds) ... build from files on web
	# can I do github.listFiles(dir, wildcard);


	# http://c.mshaffer.com/js/colorpicker/colorpicker.colors.js
	# other color tables 
	}




color.col2hex = function(colors, ..., alpha=FALSE, use.names.if.available=TRUE)
	{
	colors = dots.addTo(colors, ...);
	res = vector.useNames(colors);
	if(use.names.if.available && !is.null(res)) { return(res); }	
	
	matrixRGB = color.col2rgb(colors, alpha=alpha);	
	res = grDevices::rgb( t(matrixRGB)/255, names=colnames(matrixRGB));
	return(res);
	}

	
color.hex2col = function(hexstr, ..., use.names.if.available=TRUE)
	{
	hexstr = dots.addTo(colors, ...);
	res = vector.useNames(hexstr);
	if(use.names.if.available && !is.null(res)) { return(res); }
	
	## are they in our lists?
	
	## should we do a deep search, replace with nearest neighbor?
	
	matrixRGB = color.col2rgb(colors, alpha=alpha); 
	
	res = grDevices::rgb( t(matrixRGB)/255, names=colnames(matrixRGB));
	return(res);
	}




# will map names and colors?
color.buildSet = function(setname="base", alpha=TRUE, colors=NULL)
	{
	memory.init();
	
	if(exists(setname, .GlobalEnv$.humanVerse[["colors"]][["tables"]]))
		{
		return(.GlobalEnv$.humanVerse[["colors"]][["tables"]][[setname]]);
		}
		else {
			if(setname == "base") 
				{ 
				colors = grDevices::colors()[1:10]; 
				x = col2rgb(colors);
				hexcolors = rgb2col(x); 
				
				xt = as.data.frame( t(x) );
				#if(!is.set(xt$alpha)) { xt$alpha = 255; }
				if(is.null(xt$alpha)) { xt$alpha = 255; }
				xt$color = colors;
				xt$hex.color = hexcolors;
				
				## rownames(xt) = NULL;  # DOESN'T WORK
				## xt = property.set("row.names", xt, NULL, as.null=TRUE); ## ALSO not working, 8/10/22 one of those days
				
				
				cols = c("hex.color", "color", "red", "green", "blue", "alpha");
				xt = df.setColumnOrder(xt, cols);
				
				}
			## TODO, custom palette names 
			if(is.null(colors)) 
				{ 
				stop("You need colors");
				}			
			}
	
	}

# extends basic function by allowing
	# as.names=NULL, force.nearest=TRUE, ...
	# if(is.null(as.names)) { return(res); }
	# by default, look in colors() palette, but we could look IN any named list
	# if(force.nearest), map to a NAME for each color ... is the method of search (e.g., cosine similarity)
# x = color.col2rgb("red","#FF9944",2,"#ff994466", alpha=TRUE);
color.rgb2col = function(x, 
							as.names=TRUE, 		# if TRUE, map to names
							force.nearest=TRUE, # if TRUE, force the map (return all names)
							set = "base",		# where are we looking for names?
							...					# "base" is grDevices::colors()
						)
	{
	hexstr = rgb2col(x);  # "#FF0000" "#FF9944" "#DF536B" "#FF9944"
	search = color.buildSet(set);
	
	}


if(!is.null(key))
			{
			if(exists(key, .GlobalEnv$.humanVerse[["colors"]][["lists"]]))
				{
				colvec = .GlobalEnv$.humanVerse[["colors"]][["lists"]][[key]];
				}
			}



if(exists("monte", .GlobalEnv$.humanVerse[["colors"]][["lists"]]))
				{
				print("hello");
				}
				










































































#' .color.average
#'
#'
#' This is univariate
#'
#' @param a.hex The first color 'a' in hexcolor form
#' @param b.hex The second color 'b' in hexcolor form
#'
#' @return An "average" color in hexcolor form
#' @export
#'
#' @examples
#' .color.average("#abcdef", "#123456");
# maybe color.stats(hexstr, "mean/median/mode/dist/cosine")
.color.average = function(a.hex, b.hex)
  {
  # maybe write generic color "function", FUN = mean
  a.rgb = checkRGB(a.hex);
  b.rgb = checkRGB(b.hex);

  n.rgb = (a.rgb + b.rgb) / 2 ;
  rgb2hex(n.rgb);
  }



#' color.setOpacity
#'
#' @param hexvec color in hexform
#' @param opacity opacity level from 0 to 100
#'
#' @return a hexform in RGBa format
#' @export
#'
#' @examples
#' color.setOpacity("#abcdef", 0);
#' color.setOpacity("#abcdef", 50);
#' color.setOpacity("#abcdef", 100);

##  strPadLeft = function(str, len, pad) { str.pad(str,len,pad,"LEFT"); }

color.setOpacity = function(hexvec, opacity=100)
	{
	hexvec = checkHEX(hexvec);  # this allows "color.names"
	alpha = ( dechex(255 * opacity/100, n=2) );
	#unname( paste0(hexvec,alpha) );
	( paste0(hexvec,alpha) );
  }


#' color.buildTable
#'
#' @param colvec This is the colors we wish to include in our table
#' (a subset of the function ?colors)
#' @param key This is where we cache the listname tied to colvec
#' @param save.key This is where we cache the output dataframe generated
#'
#' @return dataframe with color (name), hex.color, r, g, b
#' @export
#'
#' @examples
#' # color.buildTable();
color.buildTable = function(colvec=NULL, key=NULL, save.key=NULL)
	{
	args = getFunctionParameters(TRUE);
	# print(args);
  # we need to cache this in "last" memory
	# colvec is cached as "key"  ... auto-caching
	# the output of this function is cached as "save.key" ... must be specified to cache
	if(is.null(colvec))
		{
		if(!is.null(key))
			{
			if(exists(key, .GlobalEnv$.humanVerse[["colors"]][["lists"]]))
				{
				colvec = .GlobalEnv$.humanVerse[["colors"]][["lists"]][[key]];
				}
			}
		if(is.null(colvec)) { key = "ALL"; colvec = grDevices::colors(TRUE); }  # not found, so let's set to default 'ALL'
		}

	if(is.null(key)) { key = md5.object( colvec ); } # make a "hash-table" of an un-named list


  if(!is.character(save.key)) { save.key = md5.object(args); }
		  cat("\n", "save.key:", save.key, "\n\n");

	if(exists(save.key, .GlobalEnv$.humanVerse[["colors"]][["dataframes"]]))
			{
			return( .GlobalEnv$.humanVerse[["colors"]][["dataframes"]][[save.key]] );
	    }

	n = length(colvec);
	hvec = character(n);
	r = g = b = numeric(n);

	for(i in 1:n)
		{
		color = colvec[i];
		rgb = cleanupRGB( grDevices::col2rgb(color) );
		hvec[i] = rgb2hex( rgb );
		r[i] = rgb[1]; g[i] = rgb[2]; b[i] = rgb[3];
		}

	df = as.data.frame(cbind(colvec, hvec, r, g, b));
		colnames(df) = c("color", "hex.color", "r", "g", "b");

	df = assignColumnsTypeInDataFrame(c("r","g","b"), "numeric", df);

	df = setAttribute("key", key, df);
	df = setAttribute("save.key", save.key, df);

	.GlobalEnv$.humanVerse[["colors"]][["dataframes"]][[key]] = df; # stored in memory
  .GlobalEnv$.humanVerse[["colors"]][["dataframes"]][[save.key]] = df;


	df;
	}








#' color.nameSearch
#'
#' @param skey
#' @param colors.df
#' @param col.name
#' @param ...
#'
#' @return
#' @export
color.nameSearch = function(skey, colors.df = NULL, col.name="color", ...)
	{
	# color "search" by wildcard
	# cache color tables (such as wheels)
	# this caching is WEIRD
	if(is.null(colors.df))
		{
		colors.df = color.buildTable();
		} else {
				# key in color memory
				if(is.character(colors.df))
					{
					if(exists(colors.df, .GlobalEnv$.humanVerse[["colors"]][["dataframes"]]))
						{
						colors.df = .GlobalEnv$.humanVerse[["colors"]][["dataframes"]][[colors.df]];
						}
					}
				}
	if(!is.data.frame(colors.df)) { stop("humanVerse::color.nameSearch ... 'colors.df' not correctly specified"); }

	res = wildcardSearch(skey, col.name, ..., df=colors.df);
	res;
	}










#' color.findNearestName
#'
#' @param hex
#' @param how.many
#' @param scale.me
#' @param how
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
color.findNearestName = function(hex, how.many = 1, scale.me = TRUE, how="cosine", ...)
	{
	# ALMOST WORKING CORRECTLY ...
	# color.findNearestName("#8B8378", how.many = 12, scale.me = TRUE);

	hex = checkHEX(hex);
	  if(is.null(hex)) { return (NULL); }

	# we need a caching mechanism on this ...
	df = color.buildTable();
#################  SEE if HEX is in the TABLE  #################
	## let's just look for it ...
		color.idx = which(df$hex.color == hex);
	if(length(color.idx) > 0)
		{
		res = df$color[color.idx];
		x = res;
			x = setAttribute("match", "exact", x);
			x = setAttribute("distance", 0, x);
			x = setAttribute("hex", rgb2hex(grDevices::col2rgb( res )), x);
		return (x);
		} # should just be one, but maybe more

#################  USE DISTANCE/COSINE SIMILARITY  #################
	rgb = cleanupRGB( hex2rgb(hex) );
		r = rgb[1];
		g = rgb[2];
		b = rgb[3];
	row = c("===search===", hex, r, g, b);

	df = rbind(row, df);
	df = assignColumnsTypeInDataFrame(c("r","g","b"), "numeric", df);


	Xs = ( as.matrix( cbind(df$r, df$g, df$b) ) );
		if(scale.me) { Xs = scale(Xs); }

	if(how == "difference")
		{
		# is this not "manhattan"
		vsearch = Xs[1,];
		vcolors = Xs[-c(1),];

		vdiff = abs(vcolors - vsearch);
		vdiff.rowS = rowSums(vdiff);

		names(vdiff.rowS) = df$color[-c(1)];

		one = sort(vdiff.rowS);

		res = one[1:how.many];
		x = names(res);
			x = setAttribute("match", "difference", x);
			x = setAttribute("distance", as.numeric( res ), x);
			x = setAttribute("hex", rgb2col(grDevices::col2rgb( names(res) )), x);
		return (x);
		}
	if(how == "distance")
		{
		X.d = stats::dist( Xs, method="euclidean");
		X.m = round( as.matrix(X.d), 2);
		X.df = as.data.frame( X.m );
			colnames(X.df) = rownames(X.df) = df$color;

		one = X.df[,1]; names(one) = df$color;
		one = one[-c(1)];  # first is "self"
		one = sort(one);

		res = one[1:how.many];
		x = names(res);
			x = setAttribute("match", "distance", x);
			x = setAttribute("distance", as.numeric( res ), x);
			x = setAttribute("hex", rgb2col(grDevices::col2rgb( names(res) )), x);
		return (x);
		}
	if(how == "cosine")
		{
	  vsearch = Xs[1,];
	      vsearch = setAttribute("name", df$color[1], vsearch);
	      # names(vsearch) = df$color[1];
		vcolors = Xs[-c(1),];
		    names(vcolors) = rownames(vcolors) = df$color[-c(1)];

		vCosine = computeCosineSimilarity(vsearch, vcolors);
		  vCosine$name = df$color;
		  rownames(vCosine) = df$color;
		vCosine = assignColumnsTypeInDataFrame(c("cosine.similarity"), "numeric", vCosine);

		# vsearch = Xs[1,];
		# vcolors = Xs[-c(1),];

	  # https://stackoverflow.com/questions/1746501/
	  # a = c(2,1,0,2,0,1,1,1)
	  # b = c(2,1,1,1,1,0,1,1)
	  # d = (a %*% b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))

	  ## OR

	  # e = crossprod(a, b) / (sqrt(crossprod(a, a)) * sqrt(crossprod(b, b)))


		# v.cos = cosine(vsearch, vcolors); # raw no distance or SVD

		# https://stackoverflow.com/questions/18946966/
		# X.d = stats::dist( Xs, method="euclidean");
		# X.m = round( as.matrix(X.d), 2);
		# X.df = as.data.frame( X.m );
		# 	colnames(X.df) = rownames(X.df) = df$color;




		## library(lsa); # very slow, replace
		#X.cos = as.data.frame( 1 - round( cosine(X.m),4 ) );
		#	colnames(X.df) = rownames(X.df) = df$color;

		#one = X.cos[,1]; names(one) = df$color;


		## similarity is closeness, distance is farness
		one = 1 - vCosine$cosine.similarity;  names(one) = df$color;
		one = one[-c(1)];   # first is "self"

		one = sort(one);  # what if I have negative values ?

		res = one[1:how.many];
		x = names(res);
			x = setAttribute("match", "cosine", x);
			x = setAttribute("distance", as.numeric( res ), x);
			x = setAttribute("hex", rgb2col(grDevices::col2rgb( names(res) )), x);
		return (x);
		}
	}













#' color.chromatics
#'
#' @param rgb
#' @param n
#' @param save.key
#'
#' @return
#' @export
#'
#' @examples
color.chromatics = function() {}
color.chromatics = function(rgb, n = 12, save.key = NULL) # mono steps of monochronic ... half on "white" / half on "black"
	{
	args = getFunctionParameters(TRUE);
	# print(args);
	# we need to cache this in "last" memory
	# accessor can get elements without having to rebuild
	if(!is.null(save.key))
		{ 
		if(!is.character(save.key)) { save.key = md5.object(args); }
		cat("\n", "save.key:", save.key, "\n\n");
		if(exists(save.key, .GlobalEnv$.humanVerse[["colors"]][["dataframes"]]))
			{
			return( .GlobalEnv$.humanVerse[["colors"]][["dataframes"]][[save.key]] );
			}
		}

	if(length(rgb) == 1) { rgb = hex2rgb(rgb); } # they can pass in "hex"
	hex = rgb2hex(rgb);

	n2 = ceiling(n/2);  # 11 will do 13 ... original doesn't count ...
	res = c( color.colorsInGradient(n2+1, c("#FFFFFF", hex)),
	            hex,
	         color.colorsInGradient(n2+1, c(hex, "#000000"))
	        );
	res = unique(res);

	which.hex = which(res == hex)[1];
	idx = -1*( 1:length(res) - which.hex );

	df = as.data.frame(cbind(idx, res));
		colnames(df) = c("index", "hex.color");
		df = setAttribute("hex", hex, df);
		df = setAttribute("rgb", unlist(rgb), df);

		df = setAttribute("save.key", save.key, df);
	# cache it ...
	if(!is.null(save.key))
		{
		.GlobalEnv$.humanVerse[["colors"]][["dataframes"]][[save.key]] = df;
		}


	df;
	}



#' color.buildWheel
#'
#' @param rgb
#' @param wheel.steps
#' @param find.names
#'
#' @return
#' @export
#'
#' @examples 
color.buildWheel = function(rgb, wheel.steps = 12, find.names=FALSE)  # wheel steps needs to be divisible by 360?
	{
	## http://c.mshaffer.com/js/colorpicker/functions.colors.js
	# we need to cache this in "last" memory
	# accessor can get elements without having to rebuild
	# - complement, split, split-complement, triad, square, rectangle, and so on ...

	if(length(rgb) == 1) { rgb = hex2rgb(rgb); } # they can pass in "hex"
	hex = rgb2hex(rgb);

	hsl = rgb2hsl(rgb);
	hsl = as.numeric( unlist( hsl ) );
		h = hsl[1];
		s = hsl[2];
		l = hsl[3];

	res = character(wheel.steps);
	names = character(wheel.steps);
	myh = numeric(wheel.steps);
	deg = numeric(wheel.steps);
	one.step = 360 / wheel.steps;
	degrees = 0;
	  my.hex = rgb2hex(rgb); # starting color ... this is also the ending color, but fine
	  if(find.names)
	    {
	    my.name = color.findNearestName(my.hex);
	    if(getAttribute("match",my.name) != "exact") { my.name = paste0("~",my.name); }
	    } else { my.name = my.hex; }
	res[1] = my.hex;
	names[1] = my.name;
	myh[1] = h;
	deg[1] = degrees;
	for(i in 2:wheel.steps)
		{
		degrees = degrees + one.step;
		h2 = as.integer( round( one.step + h ) );
			h2 = h2 %% 360;
			# update hsl
			hsl[1] = h2;

		my.hex = rgb2hex( hsl2rgb(hsl) );
		if(find.names)
	    {
	    my.name = color.findNearestName(my.hex);
	    if(getAttribute("match",my.name) != "exact") { my.name = paste0("~",my.name); }
	    } else { my.name = my.hex; }
		my.name = color.findNearestName(my.hex);
	    if(getAttribute("match",my.name) != "exact") { my.name = paste0("~",my.name); }
		res[i] = my.hex;
	  names[i] = my.name;
		deg[i] = degrees;
		h = h2;
		myh[i] = h;
		}

	df = as.data.frame(cbind(deg, res, names, myh));
		colnames(df) = c("degrees", "hex.color", "color.names", "wheel");

		df = setAttribute("hex", hex, df);
		df = setAttribute("rgb", unlist(rgb), df);


	df;
	}



color.doMod = function(x)
	{
	xMod = x %% 51;
	if(xMod <= 25) { floor(x/51) * 51; } else { ceiling(x/51) * 51; }
	}
		

#' color.webSafeHEX
#'
#' @param rgb
#'
#' @return
#' @export
#'
#' @examples
color.webSafeHEX = function(rgb)
	{
	rgb = checkRGB(rgb);
	r = rgb[1];
	g = rgb[2];
	b = rgb[3];

	

	rgb = list(
			"r" = color.doMod(r),
			"g" = color.doMod(g),
			"b" = color.doMod(b)
			);

	rgb2hex(rgb);
	}

#' color.randomHEX
#'
#' @param n
#' @param my.seed
#' @param key
#'
#' @return
#' @export
#'
#' @examples
color.randomHEX = function(n=1, my.seed=NULL, key=NULL)
	{
	if(!is.null(key))
		{
		if(exists(key, .GlobalEnv$.humanVerse[["colors"]][["random"]]))
			{
			my.seed = .GlobalEnv$.humanVerse[["colors"]][["random"]][[key]];
			}
		} else { key = "last"; }

	if(is.null(my.seed))
		{
		setSeed(NULL,"color"); my.seed = getSeed("color");
		}
	.GlobalEnv$.humanVerse[["colors"]][["random"]][[key]] = my.seed;
	setSeed(my.seed,"color"); my.numbers = rand(2^1 - 1, (2^8)^3 - 1, n);

	my.hexs = paste0("#",dechex(my.numbers, n=6));
	my.hexs; 
	}


#' color.randomRGB
#'
#' @param n
#' @param my.seed
#' @param key
#'
#' @return
#' @export
#'
#' @examples
color.randomRGB = function(n=1, my.seed=NULL, key=NULL)
	{

	my.hexs = color.randomHEX(n=n, my.seed=my.seed, key=key);
	rgb = hex2rgb( my.hexs );
	rgb = cleanupRGB(rgb);

	rgb;
	}


























#' color.roundHEX
#'
#' @param rgb
#' @param n
#' @param full
#'
#' @return
#' @export
#'
#' @examples
color.roundHEX = function(rgb, n=3, full=FALSE)
	{

	# this round "FA" to "FC", "FD" to "FF"
	rgb = checkRGB(rgb);
	if(full)  # this rounds at the whole "FF" value, not just the last element ...
		{
		r = rgb[1];
		g = rgb[2];
		b = rgb[3];

		doMod = function(x)
			{
			xMod = x %% n;
			floor(x/n) * n;
			}

		rgb = list(
				"r" = doMod(r),
				"g" = doMod(g),
				"b" = doMod(b)
				);

		rgb2hex(rgb);
		} else {
				# round at just the last hex value
				hex = rgb2hex(rgb);
				hex = str_replace("#", "", hex);
				hexV = charVector(hex);
				decV = hexdec(hexV);
					decV[2] = doMod(decV[2]);
					decV[4] = doMod(decV[4]);
					decV[6] = doMod(decV[6]);
				hexV[2] = dechex(decV[2], n=1);
				hexV[4] = dechex(decV[4], n=1);
				hexV[6] = dechex(decV[6], n=1);

				hex = paste0("#", paste(hexV, collapse=""));
				hex;
				}
	}


#' color.plotWheel
#'
#' @param df
#' @param harmony
#'
#' @return
#' @export
#'
#' @examples
color.plotWheel = function() {}  
color.plotWheel = function(df = color.buildWheel("red"), harmony="all")
  {

  df = assignColumnsTypeInDataFrame(c("degrees","wheel"), "numeric", df);
  # subset df based on elements that fit with harmony value
    # - complement ... 180 degrees from original
    # - split-complement ... 150/210 degrees from original
    # - analagous  ... +30/-30 degrees from original
    #               (split ... inverse of split-complement)
    # - triad ... +120/-120 degrees from original
    # - square ... +90/-90/+180
    # - rectangle ... complement, +30 and it's complement

  # blank canvas
  graphics::plot.new( );
  graphics::plot.window(
              xlim=c(-1.5,1.5), # unit circle is 1
              ylim=c(-1.5,1.5),
              log="",
              graphics::par(mar=c(0.25, 0.25, 0.25, 0.25)) # outer margins
            );

  # maybe put a marker like a clock on 12 ("up")
  # figure out the aspect ratio
  radius = 1;
      x0 = 0;
      y0 = 0; # center of circle

  plotrix::draw.circle(x0,y0, radius, col="gray");

    original = df[1,];
      x = x0 + radius * sin( deg2rad( original$wheel ) );
      y = y0 + radius * cos( deg2rad( original$wheel ) );
        plotrix::draw.circle(x,y, radius/3, col=original$hex.color);
        graphics::text(x,y, adj=c(0.5,0.5), cex=1, labels=original$hex.color);
            # maybe add names to wheel.table
            # maybe write function "best contrast" to determine
            # foreground color

    remaining = df[-c(1),];
    nr = dim(remaining)[1];
    for(i in 1:nr)
      {
      x = x0 + radius * sin( deg2rad( remaining$wheel[i] ) );
      y = y0 + radius * cos( deg2rad( remaining$wheel[i] ) );
        plotrix::draw.circle(x,y, radius/6, col=remaining$hex.color[i]);
        graphics::text(x,y, adj=c(0.5,0.5), cex=1/2, labels=remaining$hex.color[i]);
      }

  }


#' color.displayColorOptions
#'
#' @param my.colors
#' @param showHEX
#' @param alpha
#' @param xlim
#' @param ylim
#' @param cex
#' @param ncol
#' @param nrow
#'
#' @return
#' @export
#'
#' @examples
color.displayColorOptions = function(my.colors = grDevices::colors(),
                              showHEX = FALSE,
                              alpha = TRUE, # works with showHEX = TRUE
                              xlim=c(0,10),
                              ylim=c(0,10),
                              cex = 0.75,
                              ncol=2, nrow=10)
  {
  # http://www.sthda.com/english/wiki/colors-in-r

  ## build a collection of graphs, each two columsn ...
  ## limit the total number per row of a column ... pagination ...
  ## how to let them copy it?  identify? clipboard?
  nc = length(my.colors);
  per.page = ncol*nrow;
  pages = ceiling(nc / per.page);

  i = 1;
  page = 1;

  xunit = diff(xlim) / ncol;
  yunit = diff(ylim) / nrow;

  while(page <= pages)
    {
    xstart = xlim[1];
    ystart = ylim[2];
    graphics::plot.new( );
    graphics::plot.window(
                xlim=xlim,
                ylim=ylim,
                log="",
                graphics::par(mar=c(0.25, 0.25, 0.25, 0.25))

              );

    for(c in 1: ncol)
      {
      xleft = xstart + (c-1) * xunit;
      ytop  = ystart;
      for(r in 1: nrow)
        {
        mycolor = as.character(my.colors[i]);
        if(is.na(mycolor)) { break; break; }
        mycolor.name = names(my.colors)[i];

        hexcolor = rgb2col( grDevices::col2rgb(mycolor, alpha=alpha)  );

        if(is.null(mycolor.name)) { mycolor.name = mycolor;}

        if(!showHEX)
          {
          mycolor.label = paste0(mycolor.name, "  ...  ", i);
          } else {
                  mycolor.label = paste0(mycolor.name, "  .  ", i ,
                          "  .  ", hexcolor   );
                  }




        xright  = xleft + xunit;
        ybottom = ytop - yunit;

        graphics::rect(xleft, ybottom, xright, ytop, col=hexcolor);  # hexcolor is safer

          top.y = mean(c(ytop,ytop,ytop,ybottom));
        graphics::text(xleft, top.y, label=mycolor.label,
                              cex=cex, pos=4, col="black");
          bottom.y = mean(c(ytop,ybottom,ybottom,ybottom));
        graphics::text(xleft, bottom.y, label=mycolor.label,
                              cex=cex, pos=4, col="white");

        i = 1 + i;
        ytop = ybottom;
        }
      ytop  = ystart;
      }
    page = 1 + page;
    }
  }





################### HELPER FUNCTIONS ###################


#' color.colorsInGradient
#'
#' @param n Number of colors to return
#' @param colvec Vector of color names "red" or RGB "#FF0000" or RGBa "#4169E1FF"
#' @param alpha Pass transparency filter "alpha" as TRUE or FALSE
#'
#' @return vector of colors in RGB or RGBa form (depending on alpha)
#' @export
#'
#' @examples
#'
#' color.colorsInGradient(4, c("red", "royalblue"));
#' color.colorsInGradient(4, c("#FF000000", "#FF0000FF"), TRUE);  # red through alphas
#' color.colorsInGradient(4, c("#FF000000", "#4169E1FF"), TRUE);
#'
color.colorsInGradient = function(n, colvec=c("red","royalblue"), alpha=FALSE)
  {
  # alpha doesn't seem to work as expected ... unless I pass in RGBa?
  grDevices::colorRampPalette(colvec, alpha=alpha)(n);
  }














#' stringToInteger
#'
#' @param strvec Vector of strings
#' @param isHEX Are we looking at hexadecimal values
#'
#' @return vector of integers
#' @export
#'
#' @examples
#' stringToInteger( c("0xff", "077", "123"), FALSE );
#' stringToInteger( c("0xff", "077", "123"), TRUE );
stringToInteger = function(strvec, isHEX = FALSE)
	{
  # as.hexmode(333)
  # base::strtoi
  # strtoi(c("0xff", "077", "123"))  # string to integer
  # For decimal strings as.integer is equally useful.
	if(isHEX)
		{
		strtoi( tolower(strvec) );  # we could have other "base 10"?
		} else {
				as.integer(strvec);
				}
	}













# source('C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-colors.R')
# source('C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-str.R')
# hex2rgb("monte");
# hex2rgb("red");
# hex2rgb( "#abcdef" );
# as.numeric( unlist( hex2rgb( "abcdef" ) ) );
# unlist( hex2rgb( "ABC" ) );


#' hex2rgb
#'
#' Converts a hexform of color to rgb form
#' Input can be of RGB or RRGGBB hexadecimal form
#' Input can also be a named color
#'
#' @param hexvec vector of one or more hexforms of color
#' @param ... vector of one or more hexforms of color
#'
#' @return list of one or more rgb forms of color
#' @export
#'
#' @examples
#' hex2rgb("#FF0000");
#' hex2rgb(c("#FF0000", "#00FF00"));
#' hex2rgb("#FF0000", "#FFFFFF");
#'
#'
#' hex2rgb("red");
#' hex2rgb(c("red", "green"));
#' hex2rgb("red", "white");
#'
#' hex2rgb("alex");
#'
#' as.numeric( unlist( hex2rgb( "abcdef" ) ) );
#' unlist( hex2rgb( "ABC" ) );
#'
hex2rgb = function(hexvec, ...)
	{
	more = unlist(list(...));
	hexvec = c(hexvec, more);

	m = length(hexvec);
	result = list();

	for(i in 1:m)
		{
		hex = hexvec[i];
		hex = checkHEX(hex);
		if(is.null(hex))
		  {
		  res = NA;
		  } else {

          		hex = str_replace("#", "", trimMe(toupper(hex)));
          			hexV = charVector(hex);
          		if(length(hexV) == 3)
          			{
          			#  'FFF' => 'FFFFFF'
          			hexV = c(hexV[1], hexV[1], hexV[2], hexV[2], hexV[3], hexV[3]);
          			}

          			res = list(
          					"r" = hexdec( paste(hexV[1:2], collapse="") ),
          					"g" = hexdec( paste(hexV[3:4], collapse="") ),
          					"b" = hexdec( paste(hexV[5:6], collapse="") )
          					);
		          }

		result[[i]] = res;
		}

	if(m > 1) { result; } else { result[[1]]; }
	}




#' checkHEX
#'
#' This checks a hexadecimal input
#'
#' @param hex the hex form of the color
#'
#' @return cleansed hex form of the color
#' @export
#'
#' @examples
#' checkHEX("ABC");  # returns an updated 3-digit form, unless override??
#' checkHEX("ABCDEF");
#' checkHEX("123456");
#'
#' checkHEX("red"); # looks this up, colorlibrary?
#' checkHEX("alex"); # can't find anything in the lookup
#'
checkHEX = function(hex)
	{
  # this check if the hex was past; if it was "color", it sees if it can look it up
  # this is a vectorized function ...
	hex = str_replace("#", "", trimMe(tolower(hex)));

	## CHECK if [hex] is [colorname] ##
	    # insert "color" check here ... if it is "red", let's get "hex" from that ...
	    # "^[A-Fa-f0-9]{6}|[A-Fa-f0-9]{3}$"
	    # "^#?(([0-9a-fA-F]{2}){3}|([0-9a-fA-F]){3})$"
	#### ALPHA SEARCH FIRST
	search.alpha = grep("^#?(([0-9a-fA-F]{2}){4}|([0-9a-fA-F]){4})$", hex);
	if(length(search.alpha) > 0)
	  {
	  return ( str_replace("##", "#", toupper( paste0("#", hex) ) ) );
	  }
	search = grep("^#?(([0-9a-fA-F]{2}){3}|([0-9a-fA-F]){3})$", hex);
	if(length(search) < 1)
		{
		hasFound = FALSE;
		# look in base for now
		# color.idx = which( colors(TRUE) == hex );
		color.idx = match(hex, grDevices::colors(TRUE));  # %in% is currently defined as "%in%" <- function(x, table) match(x, table, nomatch = 0) > 0
		if(is.na(color.idx)) { color.idx = NULL;}
		if(length(color.idx) > 0)
			{
			hasFound = TRUE;
			hex = rgb2hex(grDevices::col2rgb(hex));
			hasFound = TRUE;
			}
		if(!hasFound)
		    {
		    warning( paste0(" Bad input in function [hex2rgb] : ", hex, "\n") );
		    return (NULL);
		    }
		}

	return ( str_replace("##", "#", toupper( paste0("#", hex) ) ) );
	}



#' checkRGB
#'
#' This checks a rgb input and replaces a bad "hex"
#'
#'
#' @param rgb the rgb input
#'
#' @return cleansed rgb
#' @export
#'
#' @examples
#'
#' checkRGB("red");
#' checkRGB(hex2rgb("red"));
#' checkRGB(hex2rgb("#F00"));
#'
checkRGB = function(rgb)
	{
  # this checks if the rgb was past, if it was hex, it proceeds ...

	if(is.null( dim(rgb) ) )
		{
		# my.names = sort(unique(names(rgb)));  	# if I did "unlist"
		# my.length = length(rgb);				# if I did as.numeric on "unlist"
												# my.length %% 3
		if(is.character(rgb))
			{
			rgb = hex2rgb(rgb);
			}
		} # they can pass in "hex"
	cleanupRGB(rgb);
	}


#' cleanupRGB
#'
#' @param rgb matrix of rgb values
#'
#' @return matrix of rgb values on 255 scale
#' @export
#'
#' @examples
#'
#' rgbs = hex2rgb("#F00", "pink");
#' cleanupRGB(rgbs);
cleanupRGB = function(rgb)
	{

	rgb = as.numeric( unlist( rgb ) ); # just in case ...
		rgb = matrix(rgb, nrow=3);

	if( max(rgb) <= 1 ) { rgb = 255 * rgb; } # we should be in 255 as "1"

	rgb;
	}


#' rgb2hex
#'
#' @param rgb matrix of RGB values
#' @param pre do we want to pre-pend the hash "#" ?
#'
#' @return vector of hexform colors
#' @export
#'
#' @examples
#' rgbs = hex2rgb("#F00", "pink");
#' rgb2hex(rgbs);#'
rgb2hex = function(rgb, pre="#")
	{

	rgb = cleanupRGB(rgb);
	# print(rgb);
	paste0(pre, dechex(rgb[1,], n=2), dechex(rgb[2,], n=2), dechex(rgb[3,], n=2) );
	}






#' hue2rgb
#'
#' Converts hue color format to rgb color format
#'
#' @param hues matrix of hues
#'
#' @return matrix of rgbs
#' @export
hue2rgb = function(hues)
	{
  # TODO: where is rgb2hue ???
	hues = as.numeric( unlist( hues ) ); # just in case ...
	hues = matrix(hues, nrow=3);

	m = ncol(hues);
	res = numeric(m);
	for(i in 1:m)
		{
		hue = hues[,i];

		v1 = hue[1];
		v2 = hue[2];
		vH = hue[3];

		if(vH < 0) { vH = vH + 1;}
		if(vH > 1) { vH = vH - 1;}

		once = v1;
		useOnce = FALSE;

		if ( (( 6 * vH ) < 1) && !useOnce ) {once = ( v1 + ( v2 - v1 ) * 6 * vH ); useOnce = TRUE;}
		if ( (( 2 * vH ) < 1) && !useOnce ) {once= ( v2 ); useOnce = TRUE;}
		if ( (( 3 * vH ) < 2) && !useOnce ) {once =( v1 + ( v2 - v1 ) * ( ( 2 / 3 ) - vH ) * 6 );}

		res[i] = 255 * once;
		}
	res;
	}


#' hsl2rgb
#'
#' @param hsls
#'
#' @return
#' @export

hsl2rgb = function(hsls)
	{
	hsls = as.numeric( unlist( hsls ) ); # just in case ...
	hsls = matrix(hsls, nrow=3);

	m = ncol(hsls);
	res = list();
	for(i in 1:m)
		{
		hsl = hsls[,i];

		h = hsl[1] / 360;  # fraction of circle in degrees
		s = hsl[2];
		l = hsl[3];

		if(s == 0)
			{
			r = g = b = l * 255; # gray
			} else 	{
					v2	=	if(l < 0.5) {l * (1+s); } else {(l + s) - (s*l); }
					v1	=	2*l-v2;

					r	= hue2rgb(c(v1,v2,h+1/3));
					g	= hue2rgb(c(v1,v2,h));
					b	= hue2rgb(c(v1,v2,h-1/3));
					}

		res[[i]] = list(
						"r" = as.integer( r ),
						"g" = as.integer( g ),
						"b" = as.integer( b )
						);
		}

	if(m > 1) { res; } else { res[[1]]; }
	}





#' rgb2hsl
#'
#' @param rgb
#'
#' @return
#' @export
rgb2hsl = function(rgb)
	{
  # rgb = hex2rgb("#abcdef"); unlist(rgb);
# rgb2hex(rgb);
# hsl = rgb2hsl(rgb);  unlist(hsl);
# unlist( hsl2rgb(hsl) );

	rgb = checkRGB(rgb);
	rgb = rgb / 255; # we want it on the [0,1] scale

	m = ncol(rgb);
	res = list();
	rgbs = rgb;

	for(i in 1:m)
		{
		rgb = rgbs[,i];

			myMin = min(rgb);
			v = myMax = max(rgb);
			myRange = myMax - myMin;
			l = myMid = (myMax + myMin) / 2;
		r = rgb[1];
		g = rgb[2];
		b = rgb[3];

		if(myRange == 0)  ## gray
			{
			s = 0;

			h = 0;
			} else 	{
					# this line is only difference with rgb2hsv
					# l is myMid, v = myMax
					s = if(myMid < 0.5) { myRange / (myMax + myMin)} else {myRange / (2-myMax-myMin); }

					useOnce = FALSE;
						deltaR = ((myMax - r / 6) + (myRange /2))/myRange;
						deltaG = ((myMax - g / 6) + (myRange /2))/myRange;
						deltaB = ((myMax - b / 6) + (myRange /2))/myRange;
					if(myMax == r && !useOnce) { h = deltaB - deltaG; useOnce = TRUE; }
					if(myMax == g && !useOnce) { h = (1/3) + deltaR - deltaB; useOnce = TRUE; }
					if(myMax == b && !useOnce) { h = (2/3) + deltaG - deltaR; useOnce = TRUE; }
					if(h < 0) { h = h + 1;}
					if(h > 1) { h = h - 1;}
					}

			l = myMid;
			h = 360*h; # 360 degrees in a circle

		res[[i]] = list(
					"h" = h,
					"s" = s,
					"l" = l
					);
		}

	if(m > 1) { res; } else { res[[1]]; }
	}






#' hsv2rgb
#'
#' @param hsvs
#'
#' @return
#' @export
#'
#' @examples
hsv2rgb = function(hsvs)
	{
  # rgb = hex2rgb("#abcdef"); unlist(rgb);
# rgb2hex(rgb);
# hsv = rgb2hsv(rgb);  unlist(hsv);
# unlist( hsv2rgb(hsv) );


# wsu.crimson = "#981e32";
# (rgb = unlist ( hex2rgb( wsu.crimson ) ) );
# cat("\n", "===============", "\n\n");
# (hsv = unlist(rgb2hsv(rgb) ) );
# cat("\n", "===============", "\n\n");
# unlist ( hsv2rgb( hsv ) );
### SEEMS TO BE A BUG on "blue"

	hsvs = as.numeric( unlist( hsvs ) ); # just in case ...
	hsvs = matrix(hsvs, nrow=3);

	m = ncol(hsvs);
	res = list();
	for(i in 1:m)
		{
		hsv = hsvs[,i];

		h = hsv[1];
		s = hsv[2];
		v = hsv[3];

		if(s == 0)
			{
			r = g = b = v; # gray
			} else 	{
					vH	=	h * 6;
					# mod 6
					vH = if(vH == 6) { 0; } else { vH; }
					vI = as.integer(vH);

					v1	=	v * (1-s);
					v2	=	v * (1-s*(vH-vI));
					v3	=	v * (1-s*(1-(vH-vI)));

					if( vI == 0 )
						{
						r = v;
						g = v3;
						b = v1;
						} else if( vI == 1 )
							{
							r = v2;
							g = v;
							b = v1;
							}  else if( vI == 2 )
								{
								r = v1;
								g = v;
								b = v3;
								} else if( vI == 3 )
									{
									r = v1;
									g = v2;
									b = v;
									} else if( vI == 4 )
										{
										r = v3;
										g = v1;
										b = v;
										} else 	{
												# default case "5"
												r = v;
												g = v1;
												b = v2;
												}

					}

				res[[i]] = list(
					"r" = as.integer( 255*r ),
					"g" = as.integer( 255*g ),
					"b" = as.integer( 255*b )
					);
		}

	if(m > 1) { res; } else { res[[1]]; }
	}



#' rgb2hsv
#'
#' @param rgb
#'
#' @return
#' @export
# THIS FUNCTION EXISTS in base ... 
# http://www.brucelindbloom.com/
# Conversion algorithms from http://www.brucelindbloom.com.
rgb2hsv = function(rgb)
	{
  # package:grDevices
  # ? grDevices::rgb2hsv (r,g,b)
	rgb = checkRGB(rgb);
	rgb = rgb / 255; # we want it on the [0,1] scale

	m = ncol(rgb);
	res = list();
	rgbs = rgb;

	for(i in 1:m)
		{
		rgb = rgbs[,i];
				myMin = min(rgb);
				v = myMax = max(rgb);
				myRange = myMax - myMin;
				l = myMid = (myMax + myMin) / 2;
			r = rgb[1];
			g = rgb[2];
			b = rgb[3];

			if(myRange == 0)  ## gray
				{
				s = 0;

				h = 0;
				} else 	{
						# this line is only difference with rgb2hsl
						# l is myMid, v = myMax
						s = myRange / myMax;

						useOnce = FALSE;
							deltaR = ((myMax - r / 6) + (myRange /2))/myRange;
							deltaG = ((myMax - g / 6) + (myRange /2))/myRange;
							deltaB = ((myMax - b / 6) + (myRange /2))/myRange;
						if(myMax == r && !useOnce) { h = deltaB - deltaG; useOnce = TRUE; }
						if(myMax == g && !useOnce) { h = (1/3) + deltaR - deltaB; useOnce = TRUE; }
						if(myMax == b && !useOnce) { h = (2/3) + deltaG - deltaR; useOnce = TRUE; }
						if(h < 0) { h = h + 1;}
						if(h > 1) { h = h - 1;}
						}

				l = myMid;
				h = 360*h; # 360 degrees in a circle

			res[[i]] = list(
						"h" = h,
						"s" = s,
						"v" = v
						);


		}

	if(m > 1) { res; } else { res[[1]]; }
	}









#' cmyk2rgb
#'
#' @param cmyks
#' @param ...
#'
#' @return
#' @export
cmyk2rgb = function(cmyks, ...)
	{
  # rgb = hex2rgb("#abcdef"); unlist(rgb);
# rgb2hex(rgb);
# cmyk = rgb2cmyk(rgb);  unlist(cmyk);
# unlist( cmyk2rgb(cmyk) );
	more = unlist(list(...));
	cmyks = c(cmyks, more);

	cmyks = as.numeric( unlist( cmyks ) ); # just in case ...
	cmyks = matrix(cmyks, nrow=4);

	m = ncol(cmyks);
	res = list();
	for(i in 1:m)
		{
		cmyk = cmyks[,i];


			c = cmyk[1];
			m = cmyk[2];
			y = cmyk[3];
			k = cmyk[4];

			# cmyk -> CMY
			C = ( c * ( 1 - k ) + k );
			M = ( m * ( 1 - k ) + k );
			Y = ( y * ( 1 - k ) + k );


			# CMY -> RGB
			r = ( 1 - C );
			g = ( 1 - M );
			b = ( 1 - Y );

		res[[i]] = 	list(
						"r" = as.integer( 255*r ),
						"g" = as.integer( 255*g ),
						"b" = as.integer( 255*b )
						);
		}

	if(m > 1) { res; } else { res[[1]]; }

	}

#' rgb2cmyk
#'
#' @param rgb
#'
#' @return
#' @export
rgb2cmyk = function(rgb)
	{
	rgb = checkRGB(rgb);

	m = ncol(rgb);
	res = list();
	rgbs = rgb;

	for(i in 1:m)
		{
		rgb = rgbs[,i];

				myMin = min(rgb);
				v = myMax = max(rgb);
				myRange = myMax - myMin;
				l = myMid = (myMax + myMin) / 2;
			r = rgb[1];
			g = rgb[2];
			b = rgb[3];

			# RGB -> CMY
				C = 1 - ( r / 255 );
				M = 1 - ( g / 255 );
				Y = 1 - ( b / 255 );

			# CMY -> cmyk

				vK = 1;

				# min ?
				if ( C < vK )   {vK = C;}
				if ( M < vK )   {vK = M;}
				if ( Y < vK )   {vK = Y;}
				if ( vK == 1 )
					{
					# Black
					c = 0;
					m = 0;
					y = 0;
					}
					else
						{
						c = ( C - vK ) / ( 1 - vK );
						m = ( M - vK ) / ( 1 - vK );
						y = ( Y - vK ) / ( 1 - vK );
						}
				k = vK;



		# these are  [0,1]
		res[[i]] = list(
						"c" = c ,
						"m" = m ,
						"y" = y ,
						"k" = k
						);


		}

	if(m > 1) { res; } else { res[[1]]; }


	}

















	# cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~", "\n");
	# one
		
	print(one, col.names=FALSE, ...);
	cat("\n");
	cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~", "\n");	
	
	
	