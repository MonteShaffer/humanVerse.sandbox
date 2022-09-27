
##################################################
#'
#' is.library
#'
#'
#' @param str (what is the character string to be searched)
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples
is.library = function(str = "stringi", suggestion=TRUE)
	{
	res = isTRUE(requireNamespace( str , quietly = TRUE));
	if(!res && suggestion)
		{
		pkg = paste0( "install.packages(\"",str,"\", dependencies=TRUE ); ")
		msg = paste0("\n\n", str.commentWrapper("LIBRARY is not found!"), "\n\n",
					"You could try installing the package: ", "\n\n",
					str.commentWrapper( pkg, r.tag = "-", s.pad=15), "\n");
		warning(msg);
		}
	res;
	}




##################################################
#'
#' is.substring
#'
#'
#' @param needle (substring is UNI-VARIATE)
#' @param haystack (is MULTI-VARIATE)
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples
is.substring = function(needle, haystack)
	{
	grepl(needle, haystack, fixed = TRUE);
	}



#' @rdname str_contains
#' @export
str_contains = is.substring;

#' @rdname str.contains
#' @export
str.contains = is.substring;






### THESE FUNCTIONS SEEM TO BE "mono-nuclear"
#' @rdname is.true
#' @export
is.true = isTRUE;


#' @rdname is.false
#' @export
is.false = isFALSE;



# isset — Determine if a variable is declared and is different than null

##################################################
#'
#' is.set
#'
#'
#' @param object
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples
is.set = function(obj)
	{
	obj.str = deparse(substitute(obj));
	my.obj = obj.fromString(obj.str);
		# monte = list("1", NA_character_, jk = c(1, 243)); monte$hi = NULL;
		# # monte$hi4980328 is also NULL
		# # monte$nj038 is also NULL ... anything on $ because monte is list?
		# monte = list("1", NA_character_, jk = c(1, 243), zeroes = c(0, 0, 0, 0, 0), myNA = c(NA, NA, NA), trim = c("", "", ""), mNAN = c(NaN, NaN), mbool = c(FALSE, FALSE, FALSE)); monte$hi = NULL;
	if(isFALSE(my.obj[1])) 
		{
		e = property.get("ERROR", my.obj);
		if(e == "ERROR") { return(FALSE); }
		}
	return(TRUE);
	}


# https://www.php.net/manual/en/function.empty.php
# Determine whether a variable is empty
# Determine whether a variable is considered to be empty. A variable is considered empty if it does not exist or if its value equals false. empty() does not generate a warning if the variable does not exist
# 

##################################################
#'
#' is.empty
#'
#'
#' @param object
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples
is.empty = function(obj)
	{
	# TEST-1 #
	obj.str = deparse(substitute(obj));
	my.obj = obj.fromString(obj.str);
		# monte = list("1", NA_character_, jk = c(1, 243)); monte$hi = NULL;
		# # monte$hi4980328 is also NULL
		# # monte$nj038 is also NULL ... anything on $ because monte is list?
		# monte = list("1", NA_character_, jk = c(1, 243), zeroes = c(0, 0, 0, 0, 0), myNA = c(NA, NA, NA), trim = c("", "", ""), mNAN = c(NaN, NaN), mbool = c(FALSE, FALSE, FALSE)); monte$hi = NULL;
	if(is.null(my.obj)) { return(TRUE); }  
	if(isFALSE(my.obj[1])) 
		{
		e = property.get("ERROR", my.obj);
		if(e != "") { return(TRUE); }
		}

	n.len = length(my.obj);
	n.type = typeof(my.obj);

	if(n.type == "list" && n.len > 0) { return(FALSE); }


	## check for all-zeros, all-str.trim "", all-na, all-nan

	if(is.numeric(my.obj) && stats.sum(my.obj) > 0) { return(FALSE); }
	if(is.logical(my.obj) && stats.sum(my.obj) > 0) { return(FALSE); }

	# all(is.na(c(NA, NaN)))

	if(is.character(my.obj))
		{
		my.obj[is.na(my.obj)] = "";
		s1 = (my.obj == ""); # empty
		if(all(s1)) { return(TRUE); }
		}


	if(!is.null( typeof(obj) ) ) { return (FALSE); }

	return (TRUE);  # unknown typeof ?
	}



















access <- `$$` <- function(str)
	{
	E = unlist( strsplit(as.character(str),"[@]") );
		k = length(E);
		if(k==1)
			{
			eval(parse(text=str));
			} else {
				# k = 2
				nstr = paste("attributes(",E[1],")",sep="");
				nstr = paste(nstr,'$',E[2],sep="");

				if(k>2) {
					for(i in 3:k)
						{
						nstr = paste("attributes(",nstr,")",sep="");
						nstr = paste(nstr,'$',E[i],sep="");
						}
					}
				access(nstr);
				}
	}










is.empty = function(obj)
	{
	# TEST-1 #
	obj.str = deparse(substitute(obj));
	my.obj = obj.fromString(obj.str);
		# monte = list("1", NA_character_, jk = c(1, 243)); monte$hi = NULL;
		# # monte$hi4980328 is also NULL
		# # monte$nj038 is also NULL ... anything on $ because monte is list?
	if(is.null(my.obj)) { return(TRUE); }  
	if(isFALSE(my.obj[1])) 
		{
		e = property.get("ERROR");
		print(e);
		}

			# obj.fromString = function(obj.str)






print(obj.info);







	aa = tryCatch( access(obj.str), error = identity);
	
	tt = tryCatch( eval(parse(text = obj.str)), error = identity);

	# monte = list("1", NA_character_, jk = 243);
	# monte[[3334]]
	if(is.null(tt)) { return (TRUE); }

	print(obj.str);

	at = attributes(tt);

print(typeof(tt));

	if(is.list(tt) && exists("class", at))
		{
		if(at$class[2] == "error") { return (TRUE); }
		}

	print(tt);
print(at);
	str(tt);
	return(tt);

	if(!exists(obj.str)) { return (TRUE); }

	# TEST-4 #
	if(is.null(obj)) { return (TRUE); }
	# TEST-5 #
	if(length(obj) == 0 ) { return (TRUE); }

	
	n.len = length(obj);

	# TEST-6 #
	if(is.character(obj))
		{
		n.trim = str.trim(obj);
		n.es = sum(n.trim == "");
		if(n.es == n.len) { return (TRUE); }
		}

	## ALL "zero"
	
	
	# TEST-6 #
	n.nas = sum(is.na(obj));
	if(n.nas == n.len) { return (TRUE); }

	if(!is.null( typeof(obj) ) ) { return (FALSE); }

	return (FALSE);
	}
















#' is.wholeNumber
#'
#' `is.integer` doesn't operate as you would expect, this does
#'
#' @param x number (and vector) to evaluate
#' @param tol tolerance of "zero"
#'
#' @return TRUE OR FALSE
#' @export
#'
#' @examples
#'
#' is.wholeNumber(1);
#' is.wholeNumber(1.1);
#'
#' is.wholeNumber(0);
#' is.wholeNumber(0.1);
#'
#' is.wholeNumber(-1);
#' is.wholeNumber(-1.1);
#'
#' is.wholeNumber(rnorm(5));
#' is.wholeNumber(rpois(5,1));
#'
# is.whole.number
is.wholeNumber = function(..., tol = sqrt(.Machine$double.eps), part="Re")
  {
  # See ?is.integer
  more = unlist(list(...)); x = c(x, more); 
  x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
  abs(x - round(x)) < tol;
  }




is.even = function(..., part="Re")
	{
	more = unlist(list(...)); x = c(x, more); 
	x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
	x = as.integer(x);
	
	( (x %% 2) == 0 );  # this implies it is a whole number ... https://stackoverflow.com/questions/6102948/why-does-modulus-division-only-work-with-integers
	}

is.odd = function(..., part="Re")
	{
	more = unlist(list(...)); x = c(x, more); 
	x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
	x = as.integer(x);
	
	( (x %% 2) == 1 );  # this implies it is a whole number
	}



#' is.positive
#'
#' @param x number (and vector) to evaluate
#' @param tol tolerance of "zero"
#'
#' @return TRUE OR FALSE
#' @export
#'
#' @examples
#'
#' is.positive(1);
#' is.positive(0);
#' is.positive(-1);
#' is.positive( c(-1*1:5,-sin(pi), 0,0, sin(pi), 1:5) );
#'
is.positive = function(..., tol = sqrt(.Machine$double.eps), part="Re")
  {
  more = unlist(list(...)); x = c(x, more);
  x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
  x > tol;
  }

#' is.negative
#'
#' @param x number (and vector) to evaluate
#' @param tol tolerance of "zero"
#'
#' @return TRUE OR FALSE
#' @export
#'
#' @examples
#'
#' is.negative(1);
#' is.negative(0);
#' is.negative(-1);
#' is.negative( c(-1*1:5,-sin(pi), 0,0, sin(pi), 1:5, NA, NA) );
#'
is.negative = function(..., tol = sqrt(.Machine$double.eps), part="Re")
  {
  more = unlist(list(...)); x = c(x, more);
  x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
  x < ( -1 * tol );
  }

# x = c(NULL,-1*1:5,0+2i,1+2i,-1+2i,-sin(pi), 0,0, sin(pi), 0-2i,1-2i,-1-2i, 1:5, NA, NA, NULL, NA, NULL);
# y = c( -1*1:5,0+2i,1+2i,-1+2i,-sin(pi), 0,0, sin(pi), 0-2i,1-2i,-1-2i, 1:5, NA, NA, NA);
# is.zero( c(NULL, -1*1:5,0+2i,1+2i,-1+2i,-sin(pi), 0,0, sin(pi), 0-2i,1-2i,-1-2i, 1:5, NA, NA, NULL) );
is.zero = function(..., tol = sqrt(.Machine$double.eps), part="Re")
	{
	more = unlist(list(...)); x = c(x, more);
	x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
	
	#x < tol || x > -1 * tol;	
	# !is.negative(x) && !is.positive(x);	
	
	x.pos = x < tol;
	x.neg = x > -1 * tol;
	
	( (x.pos + x.neg) > 1);
	}






























getAttribute = function(myAttributes, myObj)
	{
	# maybe alias 'getAttributes'
	res = list();
	i = 0;
	for(myAttribute in myAttributes)
		{
		i = 1 + i;
		res[[i]] = attributes(myObj)[[myAttribute]];
		}

	returnList(res);
	}



















# Sys.setenv("HUMANVERSE_CACHE" = "R:/R-humanVerse-cache");
# Sys.setenv("HUMANVERSE_DATA" = "R:/R-humanVerse-cache");
# maybe just have a path.set("key") ... path.get("key") ... path.print("key")
# path can be local or HTTP


##################################################
#'
#' memory.init;
#'
#' @param purge.memory
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
memory.init = function(purge.memory = FALSE, verbose = TRUE)
  {
  if(!exists(".humanVerse") || purge.memory)
    {
    if(verbose)
      {
      cat("humanVerse::memory.init; ... initializing HIDDEN list '.humanVerse'", "\n");
      }

    .GlobalEnv$.humanVerse = list();
	}
  }


#' @rdname initMemory
#' @export
initMemory = memory.init;



memory.clear = function(purge.ls=TRUE, all.names=TRUE, purge.gc=TRUE, detach.packages=TRUE)
	{
	# does this unregister loaded libraries?
	if(purge.ls) { rm(list=ls(all.names=all.names)); }
	if(purge.gc) { gc(); }
	if(detach.packages) { }
	}


# Clear console                      Ctrl+L                     Ctrl+L
# cat("\014")  is the code to send CTRL+L to the console and therefore will clear the screen.
# You can also type cat("\f")


# https://stackoverflow.com/questions/14260340/function-to-clear-the-console-in-r-and-rstudio
# clc <- function() cat(rep("\n", 50))  ## only one that works?
# shell("cls")
# shell("clear")


# https://stackoverflow.com/questions/6979917/how-to-unload-a-package-without-restarting-r
# sessionInfo()
# pryr::mem_used()
# memory.size (max = F)
# htop


# packages.list  ... # search()
# packages.restoreDefault ... getOption("defaultPackages")
# unattach, detach all others that are of type=package and not element of defaultPackages

















# Sys.setenv("HUMANVERSE_CACHE" = "R:/R-humanVerse-cache");


# maybe just one global container called ".humanVerse"
# I can init() and store to Global Space
# I can register() something like "random.seed" or another element
# "functions" which keeps track of getParameters externally
# "color" caches wheel,  md5(stringify(params)) as key?


# ## utils::globalVariables(c(".github.humanVerse.raw", ".github.humanVerse.view"));

## utils::globalVariables(c(".random.seed.memory"));

# utils::globalVariables(c(".humanVerse"));

## maybe create a generic set of functions ... get/set Memory


#' initMemory
#'
#' @param purge.memory
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
initMemory = function(purge.memory = FALSE, verbose = TRUE, variable='.humanVerse')
  {
  if(!exists(".humanVerse") || purge.memory)
    {
    if(verbose)
      {
      cat("humanVerse::initMemory ... initializing list '", variable, "'", "\n");
      }

	# assign(".humanVerse", list(), envir = .GlobalEnv);  # we could assign to a different variable name (variadic), but why?

    .GlobalEnv$.humanVerse = list();
		initPathMemory();
		initInflationMemory();
		initColorMemory();
		initSQLMemory();
		initFunctionMemory();
		initSystemMemory();
		initSeedMemory();
		initTimerMemory();

	}
  }


#' initInflationMemory
#'
#' @param purge.memory
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
initInflationMemory = function(purge.memory = FALSE, verbose = FALSE)
	{
	if(!exists(".humanVerse")) { initMemory(); }


	if(!exists("inflation", .GlobalEnv$.humanVerse) || purge.memory)
		{
		if(verbose)
		  {
		  cat("humanVerse::initInflationMemory ... initializing list '.humanVerse[[\"inflation\"]]'", "\n");
		  }
		.GlobalEnv$.humanVerse[["inflation"]] = list();
		}
	}


#' initFunctionMemory
#'
#' @param purge.memory
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
initFunctionMemory = function(purge.memory = FALSE, verbose = FALSE)
	{
	if(!exists(".humanVerse")) { initMemory(); }

	if(!exists("functions", .GlobalEnv$.humanVerse) || purge.memory)
		{
		if(verbose)
		  {
		  cat("humanVerse::initFunctionMemory ... initializing list '.humanVerse[[\"functions\"]]'", "\n");
		  }
		# this is a list of "included" or "sourced" functions ...
		.GlobalEnv$.humanVerse[["functions"]] = list();
			.GlobalEnv$.humanVerse[["functions"]][["local"]] = list();
			.GlobalEnv$.humanVerse[["functions"]][["local-search"]] = list();
		# this is a list of functions referenced in stack using getFunctionParameters(TRUE)
		.GlobalEnv$.humanVerse[["stack"]] = list();
		.GlobalEnv$.humanVerse[["stack-order"]] = list();
		}
	}

#' initSQLMemory
#'
#' @param purge.memory
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
initSQLMemory = function(purge.memory = FALSE, verbose = FALSE)
	{
	if(!exists(".humanVerse")) { initMemory(); }

	if(!exists("sql", .GlobalEnv$.humanVerse) || purge.memory)
		{
		if(verbose)
		  {
		  cat("humanVerse::initSQLMemory ... initializing list '.humanVerse[[\"sql\"]]'", "\n");
		  }
		.GlobalEnv$.humanVerse[["sql"]] = list();
		}
	}

#' initSystemMemory
#'
#' @param purge.memory
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
initSystemMemory = function(purge.memory = FALSE, verbose = FALSE)
	{
	if(!exists(".humanVerse")) { initMemory(); }

	if(!exists("system", .GlobalEnv$.humanVerse) || purge.memory)
		{
		if(verbose)
		  {
		  cat("humanVerse::initSystemMemory ... initializing list '.humanVerse[[\"system\"]]'", "\n");
		  }
		.GlobalEnv$.humanVerse[["system"]] = list(
												"nine" = list(), # cache for functions-nines.R
												"stack-length" = 10,
												"max-print" = 225  # useful for reviewing global .humanVerse
												);

		# setOption("max.print", .GlobalEnv$.humanVerse[["system"]][["max-print"]]);
		# options(max.print = .GlobalEnv$.humanVerse[["system"]][["max-print"]]);
		}
	}

#' initPathMemory
#'
#' @param purge.memory
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
initPathMemory = function(purge.memory = FALSE, verbose = FALSE)
	{
	if(!exists(".humanVerse")) { initMemory(); }

	if(!exists("path", .GlobalEnv$.humanVerse) || purge.memory)
		{
		if(verbose)
		  {
		  cat("humanVerse::initPathMemory ... initializing list '.humanVerse[[\"path\"]]'", "\n");
		  }
		.GlobalEnv$.humanVerse[["path"]] = list(
												"CACHE" = getSourceLocation(create=FALSE),
												"github" = list( "main" = "https://github.com/MonteShaffer/humanVerse/",
																 "raw"  = "https://raw.githubusercontent.com/MonteShaffer/humanVerse/"
																 )
												);
		}
	}




#' initColorMemory
#'
#' @param purge.memory
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
initColorMemory = function(purge.memory = FALSE, verbose = FALSE)
  {
  if(!exists(".humanVerse")) { initMemory(); }



  if(!exists("color", .GlobalEnv$.humanVerse) || purge.memory)
    {
    if(verbose)
      {
	    cat("humanVerse::initColorMemory ... initializing list '.humanVerse[[\"color\"]]'", "\n");
      }
    .GlobalEnv$.humanVerse[["colors"]] = list();
		.GlobalEnv$.humanVerse[["colors"]][["random"]] = list();  			# captures get/set seed
		.GlobalEnv$.humanVerse[["colors"]][["lists"]] = list();				# keyed lists of hex with "alpha" maybe
		.GlobalEnv$.humanVerse[["colors"]][["dataframes"]] = list();		# cached tables
		.GlobalEnv$.humanVerse[["colors"]][["nearest"]] = list();			# cached "nearest-color" index
		.GlobalEnv$.humanVerse[["colors"]][["search"]] = list();			# cached "search" history
    }
  }


#' initSeedMemory
#'
#' @param purge.memory If TRUE, this memory is erased and reset.
#'
#' @return NULL (nothing)
#' @export
#'
#' @examples
#' # initSeedMemory();
#' # setSeed(); getSeed(); initSeedMemory(purge.memory = TRUE); getSeed();
initSeedMemory = function(purge.memory = FALSE, verbose = FALSE)
  {
  if(!exists(".humanVerse")) { initMemory(); }

  if(!exists("seed", .GlobalEnv$.humanVerse) || purge.memory)
    {
    if(verbose)
      {
	    cat("humanVerse::initSeedMemory ... initializing list '.humanVerse[[\"seed\"]]'", "\n");
      }
    .GlobalEnv$.humanVerse[["seed"]] = list();
    }
  }


#' initTimerMemory
#'
#' @param purge.memory If TRUE, this memory is erased and reset.
#'
#' @return NULL (nothing)
#' @export
#'
#' @examples
#' # initTimerMemory();
#' # setTimer(); getTimer(); initTimerMemory(purge.memory = TRUE); getTimer();
initTimerMemory = function(purge.memory = FALSE, verbose = FALSE)
  {
  if(!exists(".humanVerse")) { initMemory(); }

  if(!exists("timer", .GlobalEnv$.humanVerse) || purge.memory)
    {
    if(verbose)
      {
	    cat("humanVerse::initTimerMemory ... initializing list '.humanVerse[[\"timer\"]]'", "\n");
      }
    .GlobalEnv$.humanVerse[["timer"]] = list();
    }
  }










# Clear console                      Ctrl+L                     Ctrl+L
# cat("\014")  is the code to send CTRL+L to the console and therefore will clear the screen.
# You can also type cat("\f")


# https://stackoverflow.com/questions/14260340/function-to-clear-the-console-in-r-and-rstudio
# clc <- function() cat(rep("\n", 50))  ## only one that works?
# shell("cls")
# shell("clear")


# https://stackoverflow.com/questions/6979917/how-to-unload-a-package-without-restarting-r
# sessionInfo()
# pryr::mem_used()
# memory.size (max = F)
# htop


# packages.list  ... # search()
# packages.restoreDefault ... getOption("defaultPackages")
# unattach, detach all others that are of type=package and not element of defaultPackages

















# Sys.setenv("HUMANVERSE_CACHE" = "R:/R-humanVerse-cache");


# maybe just one global container called ".humanVerse"
# I can init() and store to Global Space
# I can register() something like "random.seed" or another element
# "functions" which keeps track of getParameters externally
# "color" caches wheel,  md5(stringify(params)) as key?


# ## utils::globalVariables(c(".github.humanVerse.raw", ".github.humanVerse.view"));

## utils::globalVariables(c(".random.seed.memory"));

# utils::globalVariables(c(".humanVerse"));

## maybe create a generic set of functions ... get/set Memory


#' initMemory
#'
#' @param purge.memory
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
initMemory = function(purge.memory = FALSE, verbose = TRUE, variable='.humanVerse')
  {
  if(!exists(".humanVerse") || purge.memory)
    {
    if(verbose)
      {
      cat("humanVerse::initMemory ... initializing list '", variable, "'", "\n");
      }

	# assign(".humanVerse", list(), envir = .GlobalEnv);  # we could assign to a different variable name (variadic), but why?

    .GlobalEnv$.humanVerse = list();
		initPathMemory();
		initInflationMemory();
		initColorMemory();
		initSQLMemory();
		initFunctionMemory();
		initSystemMemory();
		initSeedMemory();
		initTimerMemory();

	}
  }


#' initInflationMemory
#'
#' @param purge.memory
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
initInflationMemory = function(purge.memory = FALSE, verbose = FALSE)
	{
	if(!exists(".humanVerse")) { initMemory(); }


	if(!exists("inflation", .GlobalEnv$.humanVerse) || purge.memory)
		{
		if(verbose)
		  {
		  cat("humanVerse::initInflationMemory ... initializing list '.humanVerse[[\"inflation\"]]'", "\n");
		  }
		.GlobalEnv$.humanVerse[["inflation"]] = list();
		}
	}


#' initFunctionMemory
#'
#' @param purge.memory
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
initFunctionMemory = function(purge.memory = FALSE, verbose = FALSE)
	{
	if(!exists(".humanVerse")) { initMemory(); }

	if(!exists("functions", .GlobalEnv$.humanVerse) || purge.memory)
		{
		if(verbose)
		  {
		  cat("humanVerse::initFunctionMemory ... initializing list '.humanVerse[[\"functions\"]]'", "\n");
		  }
		# this is a list of "included" or "sourced" functions ...
		.GlobalEnv$.humanVerse[["functions"]] = list();
			.GlobalEnv$.humanVerse[["functions"]][["local"]] = list();
			.GlobalEnv$.humanVerse[["functions"]][["local-search"]] = list();
		# this is a list of functions referenced in stack using getFunctionParameters(TRUE)
		.GlobalEnv$.humanVerse[["stack"]] = list();
		.GlobalEnv$.humanVerse[["stack-order"]] = list();
		}
	}

#' initSQLMemory
#'
#' @param purge.memory
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
initSQLMemory = function(purge.memory = FALSE, verbose = FALSE)
	{
	if(!exists(".humanVerse")) { initMemory(); }

	if(!exists("sql", .GlobalEnv$.humanVerse) || purge.memory)
		{
		if(verbose)
		  {
		  cat("humanVerse::initSQLMemory ... initializing list '.humanVerse[[\"sql\"]]'", "\n");
		  }
		.GlobalEnv$.humanVerse[["sql"]] = list();
		}
	}

#' initSystemMemory
#'
#' @param purge.memory
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
initSystemMemory = function(purge.memory = FALSE, verbose = FALSE)
	{
	if(!exists(".humanVerse")) { initMemory(); }

	if(!exists("system", .GlobalEnv$.humanVerse) || purge.memory)
		{
		if(verbose)
		  {
		  cat("humanVerse::initSystemMemory ... initializing list '.humanVerse[[\"system\"]]'", "\n");
		  }
		.GlobalEnv$.humanVerse[["system"]] = list(
												"nine" = list(), # cache for functions-nines.R
												"stack-length" = 10,
												"max-print" = 225  # useful for reviewing global .humanVerse
												);

		# setOption("max.print", .GlobalEnv$.humanVerse[["system"]][["max-print"]]);
		# options(max.print = .GlobalEnv$.humanVerse[["system"]][["max-print"]]);
		}
	}

#' initPathMemory
#'
#' @param purge.memory
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
initPathMemory = function(purge.memory = FALSE, verbose = FALSE)
	{
	if(!exists(".humanVerse")) { initMemory(); }

	if(!exists("path", .GlobalEnv$.humanVerse) || purge.memory)
		{
		if(verbose)
		  {
		  cat("humanVerse::initPathMemory ... initializing list '.humanVerse[[\"path\"]]'", "\n");
		  }
		.GlobalEnv$.humanVerse[["path"]] = list(
												"CACHE" = getSourceLocation(create=FALSE),
												"github" = list( "main" = "https://github.com/MonteShaffer/humanVerse/",
																 "raw"  = "https://raw.githubusercontent.com/MonteShaffer/humanVerse/"
																 )
												);
		}
	}




#' initColorMemory
#'
#' @param purge.memory
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
initColorMemory = function(purge.memory = FALSE, verbose = FALSE)
  {
  if(!exists(".humanVerse")) { initMemory(); }



  if(!exists("color", .GlobalEnv$.humanVerse) || purge.memory)
    {
    if(verbose)
      {
	    cat("humanVerse::initColorMemory ... initializing list '.humanVerse[[\"color\"]]'", "\n");
      }
    .GlobalEnv$.humanVerse[["colors"]] = list();
		.GlobalEnv$.humanVerse[["colors"]][["random"]] = list();  			# captures get/set seed
		.GlobalEnv$.humanVerse[["colors"]][["lists"]] = list();				# keyed lists of hex with "alpha" maybe
		.GlobalEnv$.humanVerse[["colors"]][["dataframes"]] = list();		# cached tables
		.GlobalEnv$.humanVerse[["colors"]][["nearest"]] = list();			# cached "nearest-color" index
		.GlobalEnv$.humanVerse[["colors"]][["search"]] = list();			# cached "search" history
    }
  }


#' initSeedMemory
#'
#' @param purge.memory If TRUE, this memory is erased and reset.
#'
#' @return NULL (nothing)
#' @export
#'
#' @examples
#' # initSeedMemory();
#' # setSeed(); getSeed(); initSeedMemory(purge.memory = TRUE); getSeed();
initSeedMemory = function(purge.memory = FALSE, verbose = FALSE)
  {
  if(!exists(".humanVerse")) { initMemory(); }

  if(!exists("seed", .GlobalEnv$.humanVerse) || purge.memory)
    {
    if(verbose)
      {
	    cat("humanVerse::initSeedMemory ... initializing list '.humanVerse[[\"seed\"]]'", "\n");
      }
    .GlobalEnv$.humanVerse[["seed"]] = list();
    }
  }


#' initTimerMemory
#'
#' @param purge.memory If TRUE, this memory is erased and reset.
#'
#' @return NULL (nothing)
#' @export
#'
#' @examples
#' # initTimerMemory();
#' # setTimer(); getTimer(); initTimerMemory(purge.memory = TRUE); getTimer();
initTimerMemory = function(purge.memory = FALSE, verbose = FALSE)
  {
  if(!exists(".humanVerse")) { initMemory(); }

  if(!exists("timer", .GlobalEnv$.humanVerse) || purge.memory)
    {
    if(verbose)
      {
	    cat("humanVerse::initTimerMemory ... initializing list '.humanVerse[[\"timer\"]]'", "\n");
      }
    .GlobalEnv$.humanVerse[["timer"]] = list();
    }
  }





# convert from bits to integer and reverse 
# https://www.geeksforgeeks.org/convert-an-integer-to-bits-in-r-programming-inttobits-function/#:~:text=Convert%20an%20Integer%20to%20Bits%20in%20R%20Programming%20%E2%80%93%20intToBits()%20Function&text=intToBits()%20function%20in%20R,the%20length%20of%20integer%20vector.
# base::intToBits ... type = raw ... 
# https://stackoverflow.com/questions/6614283/converting-decimal-to-binary-in-r




  # http://php.net/manual/en/function.hexdec.php
  # http://php.net/manual/en/function.dechex.php
  
  # https://www.mathworks.com/help/matlab/ref/bin2dec.html
  # bin2dec is matlab
  # bindec is PHP 
  
  # Convert text representation of binary integer to double value
  

bindec = function(binstr)
	{
	n = strlen(binstr);
	res = 0; power = 0;
	for(i in n:1)
		{
		bit = as.integer(charAt(binstr,i));
		add = 0;
		if(bit == 1) { add = 2^power; }
		
		res = res + add;
		power = 1 + power;
		}
	res;
	}



decbin = function(decnum) 
	{
	bvect = rep(0, 1 + floor(log(decnum, 2))); # pre-populate with zeroes
	while (decnum >= 2) 
		{
		power = floor(log(decnum, 2));
		bin_vect[1 + power] = 1;
		decnum = decnum - 2^power;
		} 
	bvect[1] = decnum %% 2;
	paste(rev(bvect), collapse = ""); # convert to a string
	} 



# DDEECC -> rounds to dcedcb
# hexadecimal to decimal
# hexdec("FF");
# alias hex2dec


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
hexdec = function(hexstr, ...)
	{
  # http://php.net/manual/en/function.hexdec.php
  # http://php.net/manual/en/function.dechex.php
  # java conversions:: http://www.cs.rit.edu/~ncs/color/t_convert.html
  #	http://www.easyrgb.com/math.php?MATH=M19#text19

	more = unlist(list(...));
	hexstr = c(hexstr, more);

	# if it has "color" pre-pend, remove it ...
	hexstr = str_replace("#", "", hexstr);
	# rather than checking, let's remove and add leading "0x"
	hexstr = paste0("0x", str_replace("0x", "", trimMe(tolower(hexstr))) );
	stringToInteger(hexstr, TRUE);
	}




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
#' @alias dec2hex
#'
#' @examples
#' dechex(123,255,50, n=2, hash=FALSE);
#' dechex(16581375,12581375,50, n=6, hash=TRUE);
#' dechex(16581375,12581375,50, hash=FALSE);
#' dechex(255,133,50, hash=FALSE);
dechex = function(intdec, ..., n=NULL, hash=FALSE)
	{
	more = unlist(list(...));
	intdec = c(intdec, more);

	res = toupper( as.character( as.hexmode( as.integer( round(intdec) ) ) ) );
	# if the vector already has two-character mode ... dechex( 0:255);  ... n is not necessary
	if(!is.null(n)) { res = strPadLeft( res, n, "0"); 	}
	if(hash) { res = paste0("#",res); }
	res;
	}



#' deg2rad
#'
#' Convert angles from degrees to radians.
#' Similar to pracma::deg2rad however is vectorized (multivariate).
#'
#' @param degs One or more angles in degrees
#' @param ...  One or more angles in degrees
#'
#' @return One or more angles in radians.
#' @export
#'
#' @examples
#' deg2rad(c(1,3,34));
#' deg2rad(1,3,34);
#' deg2rad(1,3,"alex");
#'
deg2rad = function(degs, ...)
	{
	more = unlist(list(...));
	degs = c(degs, more);

	res = list();
	i = 0;
	for(deg in degs)
		{
		i = 1 + i;
		ndeg = suppressWarnings(as.numeric(deg));
		rad = NaN;
		if( !is.na(ndeg) )  { rad = (pi/180) * ndeg; }
		res[[i]] = rad;
		}
	returnList(res);
	}

#' rad2deg
#'
#' Convert angles from radians to degrees.
#' Similar to pracma::rad2deg however is vectorized (multivariate).
#'
#' @param degs One or more angles in radians.
#' @param ...  One or more angles in radians.
#'
#' @return One or more angles in degrees.
#' @export
#'
#' @examples
#' rad2deg(c(1,3,34));
#' rad2deg(1,3,34);
#' rad2deg(1,3,"alex");
#'
rad2deg = function(rads, ...)
	{
	more = unlist(list(...));
	rads = c(rads, more);

	res = list();
	i = 0;
	for(rad in rads)
		{
		nrad = suppressWarnings(as.numeric(rad));
		i = 1 + i;
		deg = NaN;
		if( !is.na(nrad) )  { deg = (180/pi) * nrad; }
		res[[i]] = deg;
		}
	returnList(res);
	}
	
	
	
	
	
	
	
	
	


#' charAt
#'
#' Get the character of a string at position [idx]
#'
#' @param str String
#' @param idx position to get character
#'
#' @return single character
#' @export
#'
#' @examples
#'
#' charAt("Alex", 2);
#' charAt(c("Hello","there","Alex"), 2);
#' charAt("Alex", 8);
#' charAt("Alexander", 8);
#'
charAt = function(str,idx)
  {
  substr(str,idx,idx);
  }

#' lastChar
#'
#' Get the last character of a string
#'
#' @param str String
#' @param trim should the string be trimmed first
#'
#' @return single character
#' @export
#'
#' @examples
#'
#' lastChar("Alex");
#' lastChar(c("Hello","there","Alex"));
#' lastChar("Sasha");
#' lastChar("Alexander");
#'
lastChar = function(str, trim=TRUE)
	{
	# this also works:: ... # .substr(str, -1)
	if(trim){ str = trimMe(str); }
	s.len = strlen(str);
	charAt(str, s.len);
	}


#' charCodeAt
#'
#' Get the ASCII character code of a string at position [idx]
#'
#' @param str String
#' @param idx position to get character
#'
#' @return
#' @export
#'
#' @examples
#'
#' charCodeAt("Alex", 2);
#' charCodeAt(c("Hello","there","Alex"), 2);
#' charCodeAt("Alex", 8);
#' charCodeAt("Alexander", 8);
#'
charCodeAt = function(str,idx)
  {
  charCode ( charAt(str,idx) ); #  as.numeric( iconv( charAt(str,idx), from="ASCII", to="unicodeFFFE", toRaw=TRUE)[[1]][2] );
  }


#' charCode
#'
#' @param svec A vector of characters
#'
#' @return ASCII character code for each character
#' @export
#'
#' @examples
#'
#' s = "Alexander"; svec = strsplit(s,"",fixed=TRUE)[[1]];
#' charCode(svec);
#'
charCode = function(svec)
  {
	v1 = iconv( svec, from="ASCII", to="unicodeFFFE", toRaw=TRUE);
	v2 = as.integer( unlist(v1) )
	v2[v2 > 0];

  # s = "monte";
  # svec = strsplit(s,"",fixed=TRUE)[[1]];
  r = c();
  for(s in svec)
    {
    r = c(r, as.numeric( iconv( s, from="ASCII", to="unicodeFFFE", toRaw=TRUE)[[1]][2] ) );
    }
  return(r);
	# https://coolbutuseless.github.io/2021/12/04/base64-encoding/decoding-in-plain-r/

	unname(vapply(as.character(svec), utf8ToInt, integer(1)))
  }


as.numeric ( unlist( iconv( svec, from="ASCII", to="unicodeFFFE", toRaw=TRUE) ) )









path.openInExplorer = function(key = "wd")
	{
	path = path.get(key);
	utils::browseURL(path);
	if(is.windows())
			{
			# cmd = paste("explorer",  gsub('/', '\\\\', path, fixed=TRUE ) );
			cmd = paste("explorer",  gsub('/', '\\', path, fixed=TRUE ) );
			shell(cmd);
			suppressWarnings( shell( cmd ) );
			}
	}















##################################################
#'
#' is.url
#'
#'
#' @param file (what is the character string to be searched)
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples

is.url = function(files)
	{
	files = str.trim(files);
	fil = prep.arg(files, 3);
	x = (fil == "htt"); y = (fil == "ftp");  # multivariate, truth tables
	return ( (x+y > 0) );
	}


















## WORKS well
## https://regex101.com/r/jFn9Yw/1
## needs post-processing, but doable
## ((?<protocol>https?|ftp)\:\/\/)?((?<user>[a-z0-9+!*(),;?&=$_.-]+)(\:(?<pass>[a-z0-9+!*(),;?&=$_.-]+))?@)?(?<host>[a-z0-9-.]*)(\:(?<port>[0-9]{2,5}))?(?<page>\/([a-z0-9+$_-]\.?)+)*\/?(?<get>\?[a-z+&$_.-][a-z0-9;:@&%=+\/$_.-]*)?(?<anchor>#[a-z_.-][a-z0-9+$_.-]*)?'

## HACKS don't work
## "((?<protocol>https?|ftp)://)?((?<user>[a-z0-9+!*(),;?&=$_.-]+)(:(?<pass>[a-z0-9+!*(),;?&=$_.-]+))?@)?(?<host>[a-z0-9-.]*)(:(?<port>[0-9]{2,5}))?(?<page>/([a-z0-9+$_-].?)+)*/?(?<get>?[a-z+&$_.-][a-z0-9;:@&%=+/$_.-]*)?(?<anchor>#[a-z_.-][a-z0-9+$_.-]*)?";

pattern = "((?<protocol>https?|ftp)\:\/\/)?((?<user>[a-z0-9+!*(),;?&=$_.-]+)(\:(?<pass>[a-z0-9+!*(),;?&=$_.-]+))?@)?(?<host>[a-z0-9-.]*)(\:(?<port>[0-9]{2,5}))?(?<page>\/([a-z0-9+$_-]\.?)+)*\/?(?<get>\?[a-z+&$_.-][a-z0-9;:@&%=+\/$_.-]*)?(?<anchor>#[a-z_.-][a-z0-9+$_.-]*)?";



regex.url = function(pattern = "", subject, perl=TRUE)
	{
	if(pattern == "")
		{
		## HADLEY :: url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"

# (?<protocol>(https?|ftp):\/\/)
		# https://www.regular-expressions.info/pcre2.html
		#  R 4.0.0 also moved its grep and related functions from PCRE to PCRE2. Its sub # and gsub functions continue to use R’s own replacement text syntax.
		## Starting with R 4.0.0, passing perl=TRUE makes R use the PCRE2 library.
		## https://www.regular-expressions.info/rlanguage.html

		## ^(?<protocol>.+?//)(?<username>.+?):(?<password>.+?)@(?<address>.+)$
		## https://stackoverflow.com/questions/9472466/regex-for-http-url-with-basic-authentication

	pattern = "^(?<protocol>.+?//)(?<username>.+?):(?<password>.+?)@(?<address>.+)$";
/*
> test = "((http[s]?|ftp?):\/\/)?"
Error: '\/' is an unrecognized escape in character string starting ""((http[s]?|ftp?):\/"
> test = "((http[s]?|ftp?):\\/\\/)?"
*/

		## maybe a %.=% function 
		# https://www.php.net/manual/en/function.preg-match.php#93824
		## SCHEME
		pattern = 		"((http[s]?|ftp)://)?"; 
		## User and Pass
		
		pattern = str.push.back(pattern, "([a-z0-9+!*(),;?&=$_.-]+(:[a-z0-9+!*(),;?&=$_.-]+)?@)?"  );
		## Host or IP
		pattern = str.push.back(pattern, "([a-z0-9-.]*).([a-z]{2,3})"  );
		## Port
		pattern = str.push.back(pattern, "(:[0-9]{2,5})?"  );  
		## Path
		pattern = str.push.back(pattern, "(/([a-z0-9+$_-].?)+)*/?"  );  
		## GET Query
		pattern = str.push.back(pattern, "(?[a-z+&$_.-][a-z0-9;:@&%=+/$_.-]*)?"  ); 
		## Anchor
		pattern = str.push.back(pattern, "(#[a-z_.-][a-z0-9+$_.-]*)?"  );  
		}
	}




# see preg_match at PHP
regex.match = function(pattern, subject, perl=TRUE)
	{
	pattern = "(^[^:]+):(.+)";

# https://bookdown.org/rdpeng/rprogdatascience/regular-expressions.html#regexpr
	## rc = regexec(pattern, subject, perl=perl);
	## sc = regmatches(subject, rc);

	## rr = regexpr(pattern, subject, perl=perl);
	## sr = regmatches(subject, rr);

	r = gregexpr(pattern, subject, perl=perl);
	s = regmatches(subject, r);

	# r = regexec(pattern, subject, perl=TRUE) ### THIS WORKS

	# s = stringi::stri_extract_all_regex(subject, pattern, omit_no_match = TRUE);

	slen = length(s);

	if(slen == 0) { return(FALSE); }
	if(slen == 1 && length(s[[1]]) == 0 ) { return(FALSE); }

	}


 


/*
x <- c("A and B", "A, B and C", "A, B, C and D", "foobar")
pattern <- "[[:space:]]*(,|and)[[:space:]]"
## Match data from regexpr()
m <- regexpr(pattern, x)
regmatches(x, m)
regmatches(x, m, invert = TRUE)

n <- regexec(pattern, x)
regmatches(x, n)
regmatches(x, n, invert = TRUE)

x <- c("A and B", "A, B and C", "A, B, C and D", "foobar")
> pattern <- "[[:space:]]*(,|and)[[:space:]]"
> ## Match data from regexpr()
> m <- regexpr(pattern, x)
> regmatches(x, m)
[1] " and " ", "    ", "   
> regmatches(x, m, invert = TRUE)
[[1]]
[1] "A" "B"

[[2]]
[1] "A"       "B and C"

[[3]]
[1] "A"          "B, C and D"

[[4]]
[1] "foobar"

> 
x <- c("A and B", "A, B and C", "A, B, C and D", "foobar")
> pattern <- "[[:space:]]*(,|and)[[:space:]]"
> n <- regexec(pattern, x)
> regmatches(x, n)
[[1]]
[1] " and " "and"  

[[2]]
[1] ", " "," 

[[3]]
[1] ", " "," 

[[4]]
character(0)

> regmatches(x, n, invert = TRUE)
Error in (function (u, so, ml)  : 
  need non-overlapping matches for ‘invert = TRUE’
*/


preg_match('/(foo)(bar)(baz)/', 'foobarbaz', $matches, PREG_OFFSET_CAPTURE);

pattern = '/(foo)(bar)(baz)/';
subject = 'foobarbaz';

pattern = "(^[^:]+):(.+)";
r = regexec(pattern, row);
(s = regmatches(row, r) );


preg_match(
    string $pattern,
    string $subject,
    array &$matches = null,
    int $flags = 0,
    int $offset = 0
): int|false























#' @rdname is.dir
#' @export
is.dir = dir.exists;

#' @rdname is.file
#' @export
is.file = file.exists;


##################################################
#'
#' is.windows
#'
#'
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples
is.windows = function()
	{
	str.contains("win", str.tolower(.Platform[["OS.type"]]) );
	}


##################################################
#'
#' is.url
#'
#'
#' @param file (what is the character string to be searched)
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples

is.url = function(files)
	{
	files = str.trim(files);
	fil = prep.arg(files, 3);
	x = (fil == "htt"); y = (fil == "ftp");  # multivariate, truth tables
	return ( (x+y > 0) );
	}


##################################################
#'
#' is.library
#'
#'
#' @param str (what is the character string to be searched)
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples
is.library = function(str = "stringi", suggestion=TRUE)
	{
	res = isTRUE(requireNamespace( str , quietly = TRUE));
	if(!res && suggestion)
		{
		pkg = paste0( "install.packages(\"",str,"\", dependencies=TRUE ); ")
		msg = paste0("\n\n", str.commentWrapper("LIBRARY is not found!"), "\n\n",
					"You could try installing the package: ", "\n\n",
					str.commentWrapper( pkg, r.tag = "-", s.pad=15), "\n");
		warning(msg);
		}
	res;
	}




##################################################
#'
#' is.substring
#'
#'
#' @param needle (substring is UNI-VARIATE)
#' @param haystack (is MULTI-VARIATE)
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples
is.substring = function(needle, haystack)
	{
	grepl(needle, haystack, fixed = TRUE);
	}



#' @rdname str_contains
#' @export
str_contains = is.substring;

#' @rdname str.contains
#' @export
str.contains = is.substring;



### THESE FUNCTIONS SEEM TO BE "mono-nuclear"
#' @rdname is.true
#' @export
is.true = isTRUE;


#' @rdname is.false
#' @export
is.false = isFALSE;



##################################################
#'
#' is.set
#'
#' isset — Determine if a variable is declared and is different than null
#' https://www.php.net/manual/en/function.isset.php
#'
#' @param object
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples
is.set = function(obj, allow.NULL=FALSE)
	{
	obj.str = deparse(substitute(obj));
	my.obj = obj.fromString(obj.str);
		# monte = list("1", NA_character_, jk = c(1, 243)); monte$hi = NULL;
		# # monte$hi4980328 is also NULL
		# # monte$nj038 is also NULL ... anything on $ because monte is list?
		# monte = list("1", NA_character_, jk = c(1, 243), zeroes = c(0, 0, 0, 0, 0), myNA = c(NA, NA, NA), trim = c("", "", ""), mNAN = c(NaN, NaN), mbool = c(FALSE, FALSE, FALSE)); monte$hi = NULL;
	if(isFALSE(my.obj[1])) 
		{
		e = property.get( my.obj, "ERROR" );
		if(!is.null(e)) { return(FALSE); }
		}
	# extend functionality, we can check  is.set(obj, TRUE) ... returns true if exists REGARDLESS of NULL ... default behavior is like php::isset
	if(!allow.NULL && is.null(my.obj)) { return(FALSE); }
	return(TRUE);
	}


##################################################
#'
#' is.empty
#'
#' https://www.php.net/manual/en/function.empty.php
#' Determine whether a variable is empty
#' Determine whether a variable is considered to be empty. A variable is considered empty if it does not exist or if its value equals false. empty() does not generate a warning if the variable does not exist
#'
#' @param object
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples
is.empty = function(obj)
	{
	obj.str = deparse(substitute(obj));
	my.obj = obj.fromString(obj.str);
	if(is.null(my.obj)) { return(TRUE); }  
	if(isFALSE(my.obj[1])) 
		{
		e = property.get( my.obj, "ERROR" );
		if(!is.null(e)) { return(TRUE); }
		}

	n.len = length(my.obj);
	n.type = typeof(my.obj);

	if(n.type == "list" && n.len > 0) { return(FALSE); }

	## check for all-zeros, all-str.trim "", all-na, all-nan

	if(is.numeric(my.obj) && stats.sum(my.obj) > 0) { return(FALSE); }
	if(is.logical(my.obj) && stats.sum(my.obj) > 0) { return(FALSE); }

	# all(is.na(c(NA, NaN)))

	if(is.character(my.obj))
		{
		my.obj[is.na(my.obj)] = "";
		s1 = (my.obj == ""); # empty
		if(all(s1)) { return(TRUE); }
		}

	if(!is.null( typeof(obj) ) ) { return (FALSE); }

	return (TRUE);  # unknown typeof ?
	}




##################################################
#'
#' is.wholeNumber
#'
#' `is.integer` doesn't operate as you would expect, this does
#'
#' @param x number (and vector) to evaluate
#' @param tol tolerance of "zero"
#'
#' @return TRUE OR FALSE
#' @export
#'
#' @examples
#'
#' is.wholeNumber(1);
#' is.wholeNumber(1.1);
#'
#' is.wholeNumber(0);
#' is.wholeNumber(0.1);
#'
#' is.wholeNumber(-1);
#' is.wholeNumber(-1.1);
#'
#' is.wholeNumber(rnorm(5));
#' is.wholeNumber(rpois(5,1));
#'
is.wholeNumber = function(..., tol = sqrt(.Machine$double.eps), part="Re")
  {
  # See ?is.integer
  more = unlist(list(...)); x = c(x, more); 
  x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
  abs(x - round(x)) < tol;
  }



##################################################
#'
#' is.even
#'
#'
#'
#' @param x number (and vector) to evaluate
#' @param part By default, the "Re"al part
#'
#' @return TRUE OR FALSE
#' @export
#'
#' @examples
#'
is.even = function(..., part="Re")
	{
	more = unlist(list(...)); x = c(x, more); 
	x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
	x = as.integer(x);	
	( (x %% 2) == 0 );  
	}

##################################################
#'
#' is.odd
#'
#'
#'
#' @param x number (and vector) to evaluate
#' @param part By default, the "Re"al part
#'
#' @return TRUE OR FALSE
#' @export
#'
#' @examples
#'
is.odd = function(..., part="Re")
	{
	more = unlist(list(...)); x = c(x, more); 
	x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
	x = as.integer(x);	
	( (x %% 2) == 1 );  
	}

##################################################
#'
#' is.positive
#'
#' @param x number (and vector) to evaluate
#' @param tol tolerance of "zero"
#'
#' @return TRUE OR FALSE
#' @export
#'
#' @examples
#'
#' is.positive(1);
#' is.positive(0);
#' is.positive(-1);
#' is.positive( c(-1*1:5,-sin(pi), 0,0, sin(pi), 1:5) );
#'
is.positive = function(..., tol = sqrt(.Machine$double.eps), part="Re")
  {
  more = unlist(list(...)); x = c(x, more);
  x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
  x > tol;
  }

##################################################
#'
#' is.negative
#'
#' @param x number (and vector) to evaluate
#' @param tol tolerance of "zero"
#'
#' @return TRUE OR FALSE
#' @export
#'
#' @examples
#'
#' is.negative(1);
#' is.negative(0);
#' is.negative(-1);
#' is.negative( c(-1*1:5,-sin(pi), 0,0,0, sin(pi), 1:5, NA, NA) );
#'
is.negative = function(..., tol = sqrt(.Machine$double.eps), part="Re")
  {
  more = unlist(list(...)); x = c(x, more);
  x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
  x < ( -1 * tol );
  }


##################################################
#'
#' is.zero
#'
#' @param x number (and vector) to evaluate
#' @param tol tolerance of "zero" [floating point]
#'
#' @return TRUE OR FALSE
#' @export
#'
#' @examples
#'
#' is.zero(1);
#' is.zero(0);
#' is.zero(-1);
#' is.zero( c(-1*1:5,-sin(pi), 0,0,0, sin(pi), 1:5, NA, NA) );
#'
is.zero = function(..., tol = sqrt(.Machine$double.eps), part="Re")
	{
	more = unlist(list(...)); x = c(x, more);
	x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
	
	x.pos = x < tol;
	x.neg = x > -1 * tol;
	
	( (x.pos + x.neg) > 1);
	}



































  # history :: # https://en.cppreference.com/w/c/string/byte/strlen
  # http://www.cplusplus.com/reference/cstring/
  # https://en.wikipedia.org/wiki/C99
  # https://www.programiz.com/c-programming/library-function/string.h/strlen
  # vectorized ... already
### Error in str.len(str) : could not find function "str.len" ... strlen, str_len, str.length, str_length
  nchar( as.character(str), type="chars");
  



# sep.http = "__{PROTOCOL}__"; 


	info2 = str.explode("_B64__", list.getElements(info, 2) );
		b64_ = list.getElements(info2, 1);
		
		# NOT stringi ... nor base ... could update base with str.explode,
		# b64_ = list.getElements(info, 2);
		# b64_ = str.trim(b64_, side="right", method="cpp" pattern="_B64__");
	
	info2 = str.explode("__GET_", list.getElements(info2, 1)































































// need for locale for string functions 
#include <unicode/unistr.h>
#include <unicode/ustream.h>
#include <unicode/locid.h>

#include <Rcpp.h>
using namespace Rcpp;








//' Repeat a String s
//'
//' @param String to 'repeat'
//' @param Integer 'times' to 'repeat'
//' @return updated String repeated 'times'	
// [[Rcpp::export]]
std::string s_str_repeat(std::string s, int times)
	{
	std::string out = "";
	if(times < 1) { return out; }
	for(int i = 0; i < times; i++)
			{
			out += s;
			}
	return out; 
	}
	

//' Repeat a String s
//'
//' @param String to 'repeat'
//' @param Integer 'times' to 'repeat'
//' @return updated String repeated 'times'	
// [[Rcpp::export]]
CharacterVector cpp_str_repeat(const std::vector<std::string> str, int times)
{
	CharacterVector r{};
	for (auto& element : str) 
		{
		std::string res = s_str_repeat(element, times);
		r.push_back(res);
		}
	return r;
}














// https://stackoverflow.com/questions/216823/how-to-trim-a-stdstring


//' String Right Trim
//'
//' @param s String to be trimmed
//' @param t Trimming elements
//' @return Updated String s, right trimmed
// [[Rcpp::export]]
std::string s_rtrim(std::string s, std::string t = " \t\n\r\f\v")
	{
	s.erase(s.find_last_not_of(t) + 1);
	return s;
	}
	
//' String Right Trim
//'
//' @param s String to be trimmed
//' @param t Trimming elements
//' @return Updated String s, right trimmed
// [[Rcpp::export]]
CharacterVector cpp_rtrim(const std::vector<std::string> str, std::string t = " \t\n\r\f\v")
{
	CharacterVector r{};
	for (auto& element : str) 
		{
		std::string res = s_rtrim(element, t);
		r.push_back(res);
		}
	return r;
}

//' String Left Trim
//'
//' @param s String to be trimmed
//' @param t Trimming elements
//' @return Updated String s, left trimmed
// [[Rcpp::export]]
std::string s_ltrim(std::string s, std::string t = " \t\n\r\f\v")
	{
	s.erase(0, s.find_first_not_of(t));
	return s;
	}

//' String Left Trim
//'
//' @param s String to be trimmed
//' @param t Trimming elements
//' @return Updated String s, left trimmed
// [[Rcpp::export]]
CharacterVector cpp_ltrim(const std::vector<std::string> str, std::string t = " \t\n\r\f\v")
{
	CharacterVector r{};
	for (auto& element : str) 
		{
		std::string res = s_ltrim(element, t);
		r.push_back(res);
		}
	return r;
}


//' String Trim
//'
//' @param s String to be trimmed
//' @param t Trimming elements
//' @return Updated String s, trimmed (both left and right)
// [[Rcpp::export]]
std::string s_trim(std::string s, std::string t = " \t\n\r\f\v")
	{
	return s_ltrim(s_rtrim(s, t), t);
	}
	
//' String Trim
//'
//' @param s String to be trimmed
//' @param t Trimming elements
//' @return Updated String s, trimmed (both left and right)
// [[Rcpp::export]]
CharacterVector cpp_trim(const std::vector<std::string> str, std::string t = " \t\n\r\f\v")
{
	CharacterVector r{};
	for (auto& element : str) 
		{
		std::string res = s_trim(element, t);
		r.push_back(res);
		}
	return r;
}


//' String to lower case
//'
//' @param s String to be transformed
//' @return Updated String s, lower cased
// [[Rcpp::export]]
std::string s_tolower(std::string s, std::string locale="en_US.UTF-8")
	{
	// maybe locale.in ... locale.out 
	// NONTRIVIAL
	// https://stackoverflow.com/questions/313970/how-to-convert-an-instance-of-stdstring-to-lower-case
	std::transform(s.begin(), s.end(), s.begin(), ::tolower);
	return s;
	/*
	// boost::to_upper was a bottle-neck ... DNA (not international)
	// https://stackoverflow.com/questions/313970/how-to-convert-an-instance-of-stdstring-to-lower-case
	// https://stackoverflow.com/questions/10688831/fastest-way-to-capitalize-words
	std::transform(s.begin(), s.end(), s.begin(), ::tolower);
	return s;
	*/
	/*
	// https://www.geeksforgeeks.org/conversion-whole-string-uppercase-lowercase-using-stl-c/
	std::for_each(s.begin(), s.end(), [](char & c)
			{
				c = ::tolower(c);
				});
		return s ;
	*/
	}

//' String to lower case
//'
//' @param s String to be transformed
//' @return Updated String s, lower cased
// [[Rcpp::export]]
CharacterVector cpp_strtolower(const std::vector<std::string> str, std::string locale="en_US.UTF-8")
{
	CharacterVector r{};
	for (auto& element : str) 
		{
		std::string res = s_tolower(element, locale);
		r.push_back(res);
		}
	return r;
}

//' String to upper case
//'
//' @param s String to be transformed
//' @return Updated String s, upper cased
// [[Rcpp::export]]
std::string s_toupper(std::string s, std::string locale="en_US.UTF-8")
	{
	std::transform(s.begin(), s.end(), s.begin(), ::toupper);
	return s;
	/*
	// https://www.geeksforgeeks.org/conversion-whole-string-uppercase-lowercase-using-stl-c/
	std::for_each(s.begin(), s.end(), [](char & c)
			{
				c = ::toupper(c);
				});
		return s;
	*/
	}
	
//' String to upper case
//'
//' @param s String to be transformed
//' @return Updated String s, upper cased
// [[Rcpp::export]]
CharacterVector cpp_strtoupper(const std::vector<std::string> str, std::string locale="en_US.UTF-8")
{
	CharacterVector r{};
	for (auto& element : str) 
		{
		std::string res = s_toupper(element, locale);
		r.push_back(res);
		}
	return r;
}

//' Get String Length
//'
//' @param s String to be sized
//' @return integer length
// [[Rcpp::export]]
long long unsigned int s_strlen(std::string s)
	{
	return s.length();
	}
//' Get String Length
//'
//' @param s String to be sized
//' @return integer length
// [[Rcpp::export]]
NumericVector cpp_strlen(const std::vector<std::string> str)
{
	NumericVector r{};
	for (auto& element : str) 
		{
		long long unsigned int res = s_strlen(element);
		r.push_back(res);
		}
	return r;
}


/*
// [[Rcpp::export]]
inline char cpp_charAt(std::string s, long long int w)
	{
	// R indexes at [1], C++ indexes at [0]
	if(w > s.length()) return ('\0');
	if(w < 1 ) return ('\0');
	return s[w-1];
	}
*/



//' Explode string into array based on separator
//'
//' @param sep String to determine how split
//' @param str String to be exploded (split)
//' @return vector of strings (List array as CharacterVector)
// [[Rcpp::export]]
std::vector<std::string> s_explode(std::string sep, std::string s)
	{
	std::vector<std::string> r{};
	if(sep == "")
			{
			std::string m;
			for(long long int unsigned i = 0; i < s.length(); i++)
					{
					m = s[i];
					r.push_back( m );				 
					}
			return r;	 
			}

	size_t pos = 0;
		std::string token;
		while ((pos = s.find(sep)) != std::string::npos) 
				{
				token = s.substr(0, pos);
				r.push_back(token);
				s.erase(0, pos + sep.length());
				}
		if(s.size() > 0) { r.push_back(s);}
	return r;
	}


//' Explode string into array based on separator
//'
//' @param sep String to determine how split
//' @param str String to be exploded (split)
//' @return vector of strings (List array as CharacterVector)
// [[Rcpp::export]]
List cpp_explode(std::string sep, const std::vector<std::string> str)
{
	List r{};
	for (auto& element : str) 
		{
		std::vector<std::string> res = s_explode(sep, element);
		r.push_back(res);
		}
	return r;
}	



//' Implode string array into string based on separator
//'
//' @param vector of strings (array as CharacterVector)
//' @param sep String to determine how join
//' @return joined String
// [[Rcpp::export]]
std::string s_implode(std::string sep, std::vector<std::string> r)
	{
	std::string s = "";
	int n = r.size();
	int i = 0;
	for (auto& element : r) 
			{
			s += element;
			++i; 
			if(i != n) { s += sep; } // no separator on last element
			}
	return s;
	}
	
//' Implode string array into string based on separator
//'
//' @param vector of strings (array as CharacterVector)
//' @param sep String to determine how join
//' @return joined String
// [[Rcpp::export]]
CharacterVector cpp_implode(std::string sep, Rcpp::List str)
{
	CharacterVector r{};
	for (auto& element : str) 
		{
		std::string res = s_implode(sep, element);
		r.push_back(res);
		}
	return r;
}	



//' Search/Replace a String Subject
//'
//' @param String to 'search'
//' @param String to 'replace'
//' @param String 'subject'
//' @return updated 'subject' String appropriate replaced ... (no REGEX here)
// [[Rcpp::export]]
std::string s_str_replace(const std::vector<std::string> search, const std::vector<std::string> replace, std::string subject)
	{
	long long unsigned int slen = search.size();		
	long long unsigned int rlen = replace.size();
	long long unsigned int mlen = std::max(slen, rlen);
	std::string res = subject;
		///*
		std::cout << slen;
		std::cout << "\n";
		std::cout << rlen;
		std::cout << "\n";
		std::cout << mlen;
		std::cout << "\n";
		std::cout << res;
		std::cout << "\n";
		//*/
	// std::replace( s.begin(), s.end(), 'x', 'y');
	
	for(long long unsigned int i = 0; i < mlen; i++)
		{
		std::string mysearch = (slen == 1) ? search[0] : ( (i < slen) ? search[i] : "" );
		std::string myreplace = (rlen == 1) ? replace[0] : ( (i < rlen) ? replace[i] : "" );
		
		std::cout << mysearch;
		std::cout << "\n";
		std::cout << myreplace;
		std::cout << "\n";
		
		std::vector<std::string> tmp = s_explode(mysearch, res);
		res = s_implode(myreplace, tmp);
		}
		///*
		std::cout << res;
		std::cout << "\n";
		//*/
		
	return res;
	}
	
//' Search/Replace a String Subject
//'
//' @param String to 'search'
//' @param String to 'replace'
//' @param String 'subject'
//' @return updated 'subject' String appropriate replaced ... (no REGEX here)
// [[Rcpp::export]]
CharacterVector cpp_str_replace(const std::vector<std::string> search, const std::vector<std::string> replace, std::vector<std::string> subject)
{
	CharacterVector r{};
	
	int slen = search.size();
	int rlen = replace.size();
	int nlen = subject.size();
	if(nlen == 1) 
		{ 
		if(slen == 1 && rlen == 1)
		{} else {
				int mlen = std::max(rlen, slen);
				
				std::vector<std::string> nsubject{};
				std::string res = subject[0];
				for(int i=0; i<mlen; i++)
					{					
					nsubject.push_back(res);
					}
				subject = nsubject;
				}
		}
	/*
	// max(slen, rlen)
	if(slen != rlen)
		{
		if(slen == 1 && nlen <= rlen)
			{
			//# repeat slen until rlen in size 
			for(int i = 2; i <= rlen; i++)
				{
				std::string mysearch = search[0];		
				search.push_back( mysearch );
				}
			for(int i = 2; i <= rlen; i++)
				{
				std::string mysubject = subject[0];
				search.push_back( mysubject );
				}	
			}
			
		}
	*/
	
	for (auto& element : subject) 
		{
		std::string res = s_str_replace(search, replace, element);
		r.push_back(res);
		}
	return r;
}





// https://stackoverflow.com/questions/667183/padding-stl-strings-in-c
// no need to do str_pad
// https://stackoverflow.com/questions/26241085/rcpp-function-check-if-missing-value
// TODO ... how to implement missing values as INPUTS
// str.trim( c(1, NA), method="s"); str.trim( c(1, NA), method="c");




























			str = gsub(search[1], replace[j], str, fixed=TRUE);
			
			for(i=0; i<slen; i++)
				{
				std::string mysearch = search[i];
				std::string myreplace = replace[0];
				str = s_str_replace(mysearch, myreplace, str);
				}
			r.push_back(str);
			}
		return r;
		}


	if(slen == rlen)  ## pairwise over EACH subject
		{
		res = character(nlen);
		for(j = 1:nlen)
			{
			str = subject[j];
			for(i in 1:slen)
				{
				str = gsub(search[i], replace[i], str, fixed=TRUE);
				}	
			res[j] = str;
			}
		return (res);
		}

	# str.replace(c("{monte}", "{for}"), "MONTE", c("Here is {monte} template", "Here is another {for} sure template {monte}!") );
	if(rlen == 1)
		{
		res = character(nlen);
		for(j = 1:nlen)
			{
			str = subject[j];
			for(i in 1:slen)
				{
				str = gsub(search[i], replace[1], str, fixed=TRUE);
				}	
			res[j] = str;
			}
		return (res);
		}

	# str.replace(c("{monte}"), c("MONTE","FOR"), c("Here is {monte} template", "Here is another {for} sure template {monte}!") );
	if(slen == 1 && rlen > nlen)
		{
		res = character(rlen);
		si = 1;
		for(j = 1:rlen)
			{
			str = subject[si]; 
			str = gsub(search[1], replace[j], str, fixed=TRUE);
			res[j] = str;
			si = 1 + si;  if(si > nlen) { si = 1; }  # loop over s, end, back to beginning
			}
		return (res);
		}

	# DEFAULT ... all replaces over all subjects
	res = character(nlen);
	for(j in 1:nlen)
		{
		str = subject[j];
		mlen = max(rlen, slen);
		si = ri = 1;
		for(i in 1:mlen)
			{
			mysearch = search[si];
			myreplace = replace[ri];
			str = gsub(mysearch, myreplace, str, fixed=TRUE);
			si = 1 + si;  if(si > slen) { si = 1; }  # loop over s, end, back to beginning
			ri = 1 + ri;  if(ri > rlen) { ri = 1; }  # loop over s, end, back to beginning
			}
		res[j] = str;			
		}
	return(res);
	
	
	
	if(nlen == 1) 
		{ 
		if(slen == 1 && rlen == 1)
		{} else {
				int mlen = std::max(rlen, slen);
				
				std::vector<std::string> nsubject{};
				std::string res = subject[0];
				for(int i=0; i<mlen; i++)
					{					
					nsubject.push_back(res);
					}
				subject = nsubject;
				}
		}
	/*
	// max(slen, rlen)
	if(slen != rlen)
		{
		if(slen == 1 && nlen <= rlen)
			{
			//# repeat slen until rlen in size 
			for(int i = 2; i <= rlen; i++)
				{
				std::string mysearch = search[0];		
				search.push_back( mysearch );
				}
			for(int i = 2; i <= rlen; i++)
				{
				std::string mysubject = subject[0];
				search.push_back( mysubject );
				}	
			}
			
		}
	*/
	
	for (auto& element : subject) 
		{
		std::string res = s_str_replace(search, replace, element);
		r.push_back(res);
		}
	return r;










listToString = function(mylist,sep1="-",sep2="_")
  {
  str = c();
  mynames = names(mylist);
  for(myname in mynames)
    {
    val = mylist[[myname]];
    str = c(str, paste0(myname,sep1,val));
    }
  paste0(str,collapse=sep2);
  }



























































































str.substr = function(str, offset = -1, length=NULL, PHP.offset=TRUE)
	{
	n = as.integer(offset);
		if(!PHP.offset) { n = n - 1; } # PHP indexes at "0"

	if(!is.null(length))
		{
		length = as.integer(length);
		if(!PHP.offset) { length = length - 1; } # PHP indexes at "0"
		}

	str.len = strlen(str);
		if(is.negative(n))
			{
			print(" CASE 1 ");
			str.tmp = substr(str, start=1+(str.len + n), stop=str.len );
				if(is.null(length)) { return (str.tmp); }
				if(length == 0) 	{ return (str.tmp); }
			if(is.positive(length))
				{
				print(" CASE 2a ");
				str.final = substr(str.tmp, start=1, stop = length);
				} else {
						print(" CASE 2b ");
						str.len.tmp = strlen(str.tmp);
						str.final = substr(str.tmp, start=1, stop = str.len.tmp + length);
						}
			return ( str.final );
			} else {
					print(" CASE 3 ");
					# PHP allows n = 0 ... first element ...
					# does this need if PHPOFFSET logic?
					str.tmp = substr(str, start=1+n, stop=str.len );
						if(is.null(length)) { return (str.tmp); }
						if(length == 0) 	{ return (str.tmp); }


					if(is.positive(length))
						{
						print(" CASE 4a ");
						str.final = substr(str.tmp, start=1, stop = length);
						} else {
								print(" CASE 4b ");
								str.len.tmp = strlen(str.tmp);
								str.final = substr(str.tmp, start=1, stop = str.len.tmp + length);
								}
					return ( str.final );
					}
	stop("humanVerse::.substr ... how did you get here?!?");
}



















































#' returnList
#'
#' If list only has one element return it.
#' By default unlist so it is just a vector ... (redundant with above?)
#'
#' @param res
#' @param unlist
#'
#' @return
#' @export
returnList = function(res, unlist=TRUE)
	{
	nr = length(res);
		if(nr == 0) { return (NULL); }
		if(nr == 1) { return (res[[1]]); }
	if(unlist) { unlist(res); } else { res; }
	}




#' is.empty
#'
#' @param x
#'
#' @return
#' @export
is.empty = function(x)
  {

  # # source('C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-file.R')

# No warning is generated if the variable does not exist. That means empty() is essentially the concise equivalent to !isset($var) || $var == false.
# https://www.php.net/manual/en/function.empty.php
## the variable has to be passed in isolation, so it is not "substitute/parsed"
## my.tmp = Sys.getenv("TMP"); is.empty(my.tmp); 	# works as expected
## is.empty(Sys.getenv("TMP"));						# does **NOT** work as expected


  # if(!obj.exists(x)) { return (TRUE); }
  # below is the function obj.exists, but it has to run at this level, not a passthru
  x.str = deparse(substitute(x));
  if(!exists(x.str)) { return (TRUE); }

  if(trimMe(x) == "") { return (TRUE); } # nothing inside (except maybe white space)




  if(is.null(x)) { return (TRUE); }
  if(length(x) == 0 ) { return (TRUE); }
  if(is.na(x)) { return (TRUE); }

# can I be certain ???
  return (FALSE);
  }


#' obj.exists
#'
#' @param x
#'
#' @return
#' @export
obj.exists = function(x)
	{
  # similar to `dir.exists` or `file.exists`
	x.str = deparse(substitute(x));
	exists(x.str);
	}


#' is.set
#'
#' @param x
#'
#' @return
#' @export
is.set = function(x)
  {
# maybe use my `$$` accessor function for exists ...


# https://www.php.net/manual/en/function.isset.php
  }


#' list.element.exists
#'
#' Traps error in tryCatch for non-existent list which `exists` rejects
#'
#' @param listvar Variable Name of list
#' @param element Element in list to check ... can be number or string
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples
#'
#' list.element.exists(some.random.nonexistent.element, "base");
#'
#' my.list = list("a"=42, "b"=NULL);
#'   list.element.exists(my.list, "a");  # technically [["a"]] and [[1]] works in R
#'   list.element.exists(my.list, "b");  # null, but exists by name
#'   list.element.exists(my.list, "c");
#'   list.element.exists(my.list, 1);    # technically [["a"]] and [[1]] works in R
#'   list.element.exists(my.list, 2);   # it is technically NULL, but exists by name
#'   list.element.exists(my.list, 99);  # you shouldn't be mixing numeric / string ?
#'
#' my.list = list(); my.list[[1]] = "hi"; my.list[[3]] = "alex";
#'   list.element.exists(my.list, 1);
#'   list.element.exists(my.list, 2);  # technically R may set this equal to NULL
#'   list.element.exists(my.list, 3);
#'   list.element.exists(my.list, 99);
#'
list.element.exists = function(mylist, myelement)
  {
  # does list element exist?
  # could be a numeric or a key
  # https://stackoverflow.com/questions/7719741/how-to-test-if-list-element-exists
  tryCatch(
            {
            len = length(mylist); # error means doesn't exist
            if(len == 0) { return(FALSE); } # sub-element can't exist
            elen = length(mylist[[myelement]]);
            # print(elen);
            if(elen > 0) { return(TRUE); } # has data

            mynames = names(mylist);
            if(is.element(myelement,mynames)) { return(TRUE); }

            if(is.numeric(myelement))
              {
              myname = mynames[myelement];
              if(!is.na(myname))
                {
                if(is.element(myname,mynames)) { return(TRUE); }
                }
              }


            # is.null(listvar[[element]]);
            # could exist but be NULL

            # res = exists(element, where = listvar);
            # if(length(listvar[[element]]) > -1) { return(TRUE);}
            }, error = function(e)
                {
                return(FALSE);
                }
          );
  return(FALSE); # no errors?  ... may be NULL
  }



#' extractList
#'
#' This is not recurive, gets "names" of lists and extracts them to the environment as accessible variables using assign.
#'
#' @param myList the list to be extracted
#' @param envir the environment inwhich to extract it
#'
#' @return updates and assigns the values
#' @export
#'
#' @examples
#'
#' mylist = list("a" = 1, "b" = 2, "c" = 3);
#'         extractList(mylist);
#'         print(a); print(b); print(c);
#'
#'
# list.extract 
extractList = function(myList, envir = .GlobalEnv)
    {
    n.myList = length(myList);  # maybe create an extract function ...
                                  # parent.env() ...
    if(n.myList > 0)
      {
      for(i in 1:n.myList)
        {
        assign(names(myList)[i], myList[[i]], envir = .GlobalEnv);
        }
      }
    }



extractList = function(myList, envir = parent.env(environment()) )
    {
	# .GlobalEnv
    n.myList = length(myList);  # maybe create an extract function ...
                                  # parent.env() ...
    if(n.myList > 0)
      {
      for(i in 1:n.myList)
        {
        assign(names(myList)[i], myList[[i]], envir = envir);
        }
      }
    }

# https://stackoverflow.com/questions/8771942/how-can-i-reference-the-local-environment-within-a-function-in-r
f <- function() {
  function() list(curEnv=environment(), parent=parent.env(environment()), 
          grandParent=parent.env(parent.env(environment())), callStack=sys.frames(), 
          callStackDepth=sys.nframe())
}
g <- function(f, n=2) if (n>2) g(f, n-1) else f()

floc <- f() # generate a local function
g(floc, 3) # call it


# "%in%" <- function(x, table) match(x, table, nomatch = 0) > 0
# is.element for an S-compatible equivalent of %in%.





