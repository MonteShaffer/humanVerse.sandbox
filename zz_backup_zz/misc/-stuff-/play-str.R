







































str.trimFixed = function(str, trim="#", side="both")
	{
	s = prep.arg(side, 1);
	str.pos = str.getPositions(str, trim);

	n.str = length(str);
	res = character(n.str);
	for(i in 1:n.str)
		{
		str.df = str.pos[[i]];
		n.df = nrow(str.df);
		df.first = str.df[1,]$string;
		df.last = str.df[n.df,]$string;

		if(df.last == trim && df.first == trim && s == "b")
			{
			# str.df[n.df, ] = NULL;
			str.df = str.df[-c(1,n.df), ];
			}
		if(df.last == trim && s == "r")
			{
			str.df = str.df[-c(n.df), ];
			}
		if(df.first == trim && s == "l")
			{
			str.df = str.df[-c(1), ];
			}
		 
		res[i] = paste0(str.df$string, collapse="");
		}
	res;	
	}



str.explodeMap = function(chars, str, search)
	{
	slen = strlen(search);
	res = NULL;
	nlen = length(chars);
	nchars = strlen(chars);
	clen = sum(nchars);
	tlen = clen + nlen;  # this should be original string length
	n = nlen;
	idx = 1;
	for(i in 1:n)
		{
		cslen = strlen(chars[i]);
		if(cslen == 0) 
			{ 
			# row = c("", NA, NA);
			row = NULL;
			} else { 
					row = c(chars[i], idx, idx + cslen - 1); 
					}
		idx = idx + cslen;
		res = rbind(res, row);

		# if(i == nlen) { break; }  # don't append last n (n - 1)
		if((idx + slen - 1) >= tlen) { break; }
		row = c(search, idx, idx + slen - 1);
		res = rbind(res, row);		
		idx = idx + slen;
		}
	res = data.frame(res);
	colnames(res) = c("string", "start", "stop");
	res$start = as.integer(res$start);
	res$stop = as.integer(res$stop);

print(str);
cat("\n", "--- ", strlen(str), " ---", "\n");
print(chars);
print(res);
	res;

	}



# gregexpr("B", x);
# https://statisticsglobe.com/find-position-of-character-in-string-in-r
# gregexpr(search, str);
# stringr::str_locate_all(pattern = "B", x)
# stringr::str_locate_all(pattern = search, str)
# https://github.com/tidyverse/design/issues/13
# recycling examples 
# df = data.frame(matrix(  vector(), 0, 5, dimnames=list(c(), c("C1","C2","C3","C4","C5"))), stringsAsFactors=F)

str.getPositions = function(str, search)
	{
	slen = strlen(search);
	n.list = length(str);
	
	info = str.explode(search, str);  	# exploded char vectors
										# lengths of exploded char vectors


	res = list();
	for(i in 1:n.list)
		{
		res[[i]] = str.explodeMap(info[[1]], str[1], search);
		}
	res;
	# ss = as.relistable( str.len(info) ); # a list of lengths
		# u.info = unlist(ss);	# unlist
		# u.info = u.info + slen;  	# add	
	# ee = relist(u.info);			# relist
	# list("start" = ss, "end" = ee);
	}






#' implodeMe
#'
#' Similar to javascript.join and php.implode
#'
#' @param delimiter character(s) to unsplit with
#' @param strvec a character string to be unsplit
#'
#' @return a character string
#' @export
#'
#' @examples
#' implodeMe();
#'
#'
#' str = removeWhiteSpace("    four   scores    and  seven      years     ", "[s]");
#' strvec = explodeMe("[s]", str);
#' implodeMe(",", strvec);
#'
implodeMe = function(delimiter=" ", strvec = c("hello","friend") )
  {
  paste0(strvec, collapse = delimiter);
  }







#' ascii.line
#'
#' @param strs
#' @param out.length
#' @param left
#' @param right
#' @param sep
#' @param justification
#'
#' @return
#' @export
#'
#' @examples
ascii.line = function(strs, out.length=66, left = "## ", right = " ##", sep=" ", justification="center")
	{
	res = list();
	n = length(strs);
	for(i in 1:n)
		{
		str = strs[i];

		sep = charAt(sep,1); # we only allow a 1-element separator

		len.s = strlen(str);
		len.l = strlen(left);
		len.r = strlen(right);

		if(justification == "center")
			{
			out.left  = out.right = floor( (out.length - len.l - len.s - len.r )/2 );
				# offset = out.length - len.l - out.left - len.s - out.right - len.r;
			line = paste0(left, str_repeat(sep,out.left), str, str_repeat(sep, out.right));

			remaining = out.length - strlen(line) - len.r;
			if(remaining > 0)
				{
				line = paste0(line, str_repeat(sep, remaining));
				}
			line = paste0(line, right);
			} else {
					# left
					line = paste0(left, str);
					remaining = out.length - strlen(line) - len.r;
					if(remaining > 0)
						{
						line = paste0(line, str_repeat(sep, remaining), right);
						}
					}

		res[[i]] = line;
		}

	if(n > 1) { res; } else { res[[1]]; }
	}
	
	
	





## strpos ... strrpos ... reverse

# Rcpp::sourceCpp("C:\\_git_\\github\\MonteShaffer\\humanVerse\\HVcpp\\src\\str.cpp");
# rm(list=ls())
# rm(list=ls(all.names=TRUE)); gc();







#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' 
#'
#'
#' catMe
#'
#' @param lines
#' @param pre
#' @param post
#' @param file
#' @param append
#'
#' @return
#' @export
#'
#' @examples
catMe = function(lines, pre="\n", post="", file="", append=TRUE)
	{
	lines = getElementsInList(lines, 1);
	n = length(lines);
	for(line in lines)
		{
		cat(pre, line, post, file=file, append=append);
		}
	}





# e.g., strpos in PHP




#' str_repeat
#'
#' @param str
#' @param times
#'
#' @return
#' @export
#'
#' @examples
str_repeat = function(str, times=1)
	{
  # https://www.php.net/manual/en/function.str-repeat.php
	paste( rep(str, times), collapse="");
	}


#' str_replace
#'
#' @param find
#' @param replace
#' @param str
#' @param method
#'
#' @return
#' @export
#'
#' @examples
str_replace = function(find, replace, str, method="base")
  {
  # this is "fixed" find and replace # str = gsub(find[i], replace[i], str, fixed=TRUE);
  # method = base, method = stringi
  # stringi is not performing well on this:  "{wsu.crimson}" with fixed


  # if find/replace are longer ... if one is length one, repeat the other one
  n.find = length(find);
  n.replace = length(replace);
  n.max = max(n.find, n.replace);

  if(n.find == 1 && n.replace == 1)
    {
    if(n.find == 1)
      {
      find = rep(find, n.replace);
      } else {
              if(n.replace == 1)
                {
                find = rep(replace, n.find);
                } else {
                        stop("find and replace mismatch");
                        }
              }
    }
  ### let's loop and replace ...
  for(i in 1:n.max)
    {
    if( isTRUE(requireNamespace("stringi", quietly = TRUE)) && method=="stringi" )
      {
      # I need to verify this is the correct function ...
      str = stringi::stri_replace_first_fixed(str, find[i], replace[i]);
      } else {
              str = gsub(find[i], replace[i], str, fixed=TRUE);
              }
    }
  str;
  }

# str_replace_all(
























#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#'
#' str.substr
#'
#' @param str
#' @param n
#' @param length
#' @param PHP.offset  # if true, the EXAMPLES on PHP.NET work exactly
#'
#' @return
#' @export
#'
#' @examples
# if offset and length are multivariate, they must EQUAL EACH OTHER and str
# FOR NOW == NO ... multivariate str, univariate offset/length
# if(is.negative(n)) ... multivariate is an issue 
str.substr = function(str, offset = -1, length=NULL)
	{
	n = as.integer(offset);   # R indexes at "1"
							  # PHP indexes at "0"
	if(!is.negative(n)) { n = 1 + n; }

	if(!is.null(length))
		{
		length = as.integer(length);
		if(!PHP.offset && is.negative(n)) { length = length - 1; } # PHP indexes at "0"
		}

	str.len = strlen(str);  ## this is multivariate ?
	# cat("\n", "n = ", n, "\t\t", "length = ", length, "\n");
	# print(str.len);
		if(is.negative(n))
			{
			# print(" CASE 1 ");
			str.tmp = substr(str, start=1+(str.len + n), stop=str.len );
				if(is.null(length)) { return (str.tmp); }
				if(length == 0) 	{ return (str.tmp); }
			if(is.positive(length))
				{
				# print(" CASE 2a ");
				str.final = substr(str.tmp, start=1, stop = length);
				} else {
						# print(" CASE 2b ");
						str.len.tmp = strlen(str.tmp);
						str.final = substr(str.tmp, start=1, stop = str.len.tmp + length);
						}
			return ( str.final );
			} else {
					# print(" CASE 3 ");
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




#' @rdname .substr
#' @export
.substr = str.substr;

#' @rdname substr.neg
#' @export
substr.neg = str.substr;

#' @rdname str.substring
#' @export
str.substring = str.substr;





str.substrPHP = function(str, offset = -1, length=NULL)
	{
	n = as.integer(offset); # PHP indexes at "0"

	if(!is.null(length))
		{
		length = as.integer(length);
		}

	str.len = strlen(str);  ## this is multivariate ?
	# cat("\n", "n = ", n, "\t\t", "length = ", length, "\n");
	# print(str.len);
		if(is.negative(n))
			{
			# print(" CASE 1 ");
			str.tmp = substr(str, start=1+(str.len + n), stop=str.len );
				if(is.null(length)) { return (str.tmp); }
				if(length == 0) 	{ return (str.tmp); }
			if(is.positive(length))
				{
				# print(" CASE 2a ");
				str.final = substr(str.tmp, start=1, stop = length);
				} else {
						# print(" CASE 2b ");
						str.len.tmp = strlen(str.tmp);
						str.final = substr(str.tmp, start=1, stop = str.len.tmp + length);
						}
			return ( str.final );
			} else {
					# print(" CASE 3 ");
					# PHP allows n = 0 ... first element ...
					# does this need if PHPOFFSET logic?
					str.tmp = substr(str, start=1+n, stop=str.len );
						if(is.null(length)) { return (str.tmp); }
						if(length == 0) 	{ return (str.tmp); }


					if(is.positive(length))
						{
						# print(" CASE 4a ");
						str.final = substr(str.tmp, start=1, stop = length);
						} else {
								# print(" CASE 4b ");
								str.len.tmp = strlen(str.tmp);
								str.final = substr(str.tmp, start=1, stop = str.len.tmp + length);
								}
					return ( str.final );
					}
	stop("humanVerse::.substr ... how did you get here?!?");
}





















































#' strtolower
#'
#' @param str
#'
#' @return
#' @export
#'
#' @examples
strtolower = function(str)
	{
	tolower(str);
	}

#' strtoupper
#'
#' @param str
#'
#' @return
#' @export
#'
#' @examples
strtoupper = function(str)
	{
	toupper(str);
	}


#
#' trimMe
#'
#'
#' library(stringi);
#'
#' @param str character string to be "trimmed"
#'
#' @return updated trimmed string
#' @export
#'
#' @examples
#'
#' trimMe( c(" Monte", " is ", "Alexander's ", "  daddy!") );
#'
#' trimMe("    four   scores    and  seven      years     ");
#' trimMe("    four   scores    and  seven      years     ", "left");
#' trimMe("    four   scores    and  seven      years     ", "riGht");
#' trimMe("    four   scores    and  seven      years     ", "both");
#' trimMe("    four   scores    and  seven      years     ", "albjdskj")
#'
#' trimMe("\r\n    four   scores    and  seven      years   \t\t  ");
#'
trimMe = function(str, side="both", method="stringi")
  {
  # sides = c("both", "left", "right")
  # methods = c("cpp", "stringi", "base")
  side = tolower(side);
  ###m = substr(tolower(method),1,1);  # is this slowing me down?
  # stringr::str_trim(str);
  # if(!is.element(side,sides)) { stop("option for 'side' must be one of:  both, left, right"); }
  # set default to both

  # dump stringr and go to stringi
  # dump tidyverse altogether
  # review httr and build appropriate functions
  # use base whenever possible, then the best packages whenever possible
  # define best by "least associated with new grammar tactics"
  # new grammar is arbitrary and not c-based
  # human-readable functions with variadic inputs is not new grammar
# lsf.str("package:stringi")
# ls("package:stringi")

	if(method == "cpp" && exists("cpp_trim"))
			{
			res = switch(side,
							  "left"  = cpp_ltrim(str),
							  "right" = cpp_rtrim(str),
							  "both"  = cpp_trim(str),
					cpp_trim(str)
					);
			return (res);
			}
		
		
  if( method == "stringi" && isTRUE(requireNamespace("stringi", quietly = TRUE)) )
    {
    res = switch(side,
						  "left"  = stringi::stri_trim_left(str),
						  "right" = stringi::stri_trim_right(str),
						  "both"  = stringi::stri_trim_both(str),
				stringi::stri_trim_both(str)
				);
	return (res);
	}
	
	
		
    res = switch(side,
						  "left"  = gsub("^\\s+", "", str),
						  "right" = gsub("\\s+$", "", str),
						  "both"  = gsub("^\\s+|\\s+$", "", str),
                  gsub("^\\s+|\\s+$", "", str)
                  );
    return (res);
  }


#' explodeMe
#'
#' Similar to javascript.split and php.explode
#'
#' @param delimiter character(s) to delimit the split
#' @param str a character string to be split
#'
#' @return a character vector
#' @export
#'
#' @examples
#' str = removeWhiteSpace("    four   scores    and  seven      years     ", "[s]");
#' strvec = explodeMe("[s]", str);
#' strvec[3];
explodeMe = function(delimiter = " ", str = "hello friend", n = 1)
  {
  res = strsplit(str, delimiter, fixed=TRUE);
  if(is.null(n)) { return (res); } # ALL OF THEM
  res[[n]]; # single element
  }



#' implodeMe
#'
#' Similar to javascript.join and php.implode
#'
#' @param delimiter character(s) to unsplit with
#' @param strvec a character string to be unsplit
#'
#' @return a character string
#' @export
#'
#' @examples
#' implodeMe();
#'
#'
#' str = removeWhiteSpace("    four   scores    and  seven      years     ", "[s]");
#' strvec = explodeMe("[s]", str);
#' implodeMe(",", strvec);
#'
implodeMe = function(delimiter=" ", strvec = c("hello","friend") )
  {
  paste0(strvec, collapse = delimiter);
  }



#' removeWhiteSpace
#'
#' @param str character string to be adjusted
#' @param replace what will we replace the white space with
#' @param n number of spaces to find and replace
#' @param pre.trim if TRUE, trims the string before removing white space within
#' @param post.trim if TRUE, trims the string after removing white space within
#'
#' @return updated adjusted string
#' @export
#'
#' @examples
#' removeWhiteSpace("    four   scores    and  seven      years     ");
#' removeWhiteSpace("\r\n    four   scores    and  seven      years   \t\t  ");
#'
#' removeWhiteSpace("    four   scores    and  seven      years     ", "");
#' removeWhiteSpace("    four   scores    and  seven      years     ", "[s]");
#'
#' removeWhiteSpace("The quick brown fox jumps over the lazy dog", ""); # default is 2
#' removeWhiteSpace("The quick brown fox jumps over the lazy dog", "", n=1);
removeWhiteSpace = function( str, replace=" ", n = 2,
                              pre.trim = TRUE, post.trim = TRUE, trim.method = "stringi" )
  {
  # ?regex
  # $string = preg_replace('/\s+/', '', $string);
  if(pre.trim) { str = trimMe(str, method = trim.method); }
    regex.s = paste0("[[:space:]]{",n,",}");
  str = gsub( regex.s, replace, str );
  # str = gsub("[[:space:]]", remain, str); # ... call it twice ?
  if(post.trim) { str = trimMe(str, method = trim.method); }
  str;
  }






#' removeFunnyCharacters
#'
#' @param str
#' @param which
#' @param ASCII
#'
#' @return
#' @export
#'
#' @examples
removeFunnyCharacters = function(str, which="alpha-numeric", ASCII=TRUE, trim=TRUE)
	{
  method = tolower(which);  # str.toLowerCase()
	# https://stackoverflow.com/questions/10294284/remove-all-special-characters-from-a-string-in-r
	# str = "Ábcdêãçoàúü"; str = iconv(str, from = 'UTF-8', to = 'ASCII//TRANSLIT'); str;
	# str = "Ábcdêãçoàúü"; str = iconv(str, from = '', to = 'ASCII//TRANSLIT'); str;
	if(ASCII) { str = iconv(str, from = '', to = 'ASCII//TRANSLIT'); }

	# ?regex
  # switch ...
  str = switch(method,
            "alpha-numeric"    = gsub("[^[:alnum:]]", " ", str),
            "alphanumeric"    = gsub("[^[:alnum:]]", " ", str),

            "alpha"    = gsub("[^[:alpha:]]", " ", str),

            "numeric"    = gsub("[^[:digit:]]", " ", str),
            "num"    = gsub("[^[:digit:]]", " ", str),
            "number"    = gsub("[^[:digit:]]", " ", str),

           str # default case of switch
          );

  if(trim) { str = trimMe(str); }

	str; # maybe just ascii move
	}

#' strPadLeft
#'
#' When caching pages of content, useful for organization.
#'  (e.g., page1.html becomes page_001.html)
#'
#' @param str The 'string' (can be a number)
#' @param final.str.len How long the final str is to be
#' @param padding Fill with, default is "0" (zero)
#'
#' @return string
#' @export
#'
#' @aliases numberPadLeft str_pad_left
#'
#' @examples
#' strPadLeft(33,1);
#' strPadLeft(33,2);
#' strPadLeft(33,3);
#' strPadLeft(33,4);
strPadLeft = function(str, final.str.len, padding="0", method="stringi")
  {
  if( isTRUE(requireNamespace("stringi", quietly = TRUE)) && method=="stringi" )
    {
    stringi::stri_pad_left(str, final.str.len, pad = padding);
    } else {
            n = strlen(str);
            r = final.str.len - n;
            if(r < 0) { stop("strPadLeft is too short!"); }
            paste0(paste(rep(padding,r),collapse=""),str);

            }
  }

#' strPadRight
#'
#'
#'
#' @param str The 'string' (can be a number)
#' @param final.str.len How long the final str is to be
#' @param padding Fill with, default is "0" (zero)
#'
#' @return string
#' @export
#'
#' @aliases numberPadRight str_pad_right
#'
#' @examples
#' strPadRight("33.01",5);
#' strPadRight("33.01",6);
#' strPadRight("33.01",7);
#' strPadRight("33.01",8);
strPadRight = function(str, final.str.len, padding="0", method="stringi")
  {
  if( isTRUE(requireNamespace("stringi", quietly = TRUE)) && method=="stringi" )
    {
    stringi::stri_pad_right(str, final.str.len, pad = padding);
    } else {
            n = strlen(str);
            r = final.str.len - n;
            if(r < 0) { stop("strPadRight is too short!"); }
            paste0(str, paste(rep(padding,r),collapse=""));
            }
  }


#' strlen
#'
#' @param str the character string
#'
#' @return the numeric length of said string
#' @export
#'
#' @examples
#' strlen("3.1415926535897932384626");
#' strlen( pi );
#' strvec = c("hi","how","are","you"); strlen(strvec);
strlen = function(str)
  {
  # history :: # https://en.cppreference.com/w/c/string/byte/strlen
  # http://www.cplusplus.com/reference/cstring/
  # https://en.wikipedia.org/wiki/C99
  # https://www.programiz.com/c-programming/library-function/string.h/strlen
  # vectorized ... already
### Error in str.len(str) : could not find function "str.len" ... strlen, str_len, str.length, str_length
  nchar( as.character(str), type="chars");
  }




#' .substr
#'
#' @param str
#' @param n
#' @param length
#' @param PHP.offset
#'
#' @return
#' @export
#'
#' @examples
.substr = substr.neg = function(str, n = -1, length=NULL, PHP.offset=TRUE)
	{
  # https://stackoverflow.com/questions/2681786/how-to-get-the-last-char-of-a-string-in-php
  # .substr = function(str,  # maybe write a PHP wrapper
  # by default, this will return the last character of a string ...
  # .substr("abcdef", 4, -4);  // returns false ... returns EMPTY ""
  ## earlier called 'substr.neg'
  # PHP wrapper ... https://www.php.net/manual/en/function.substr.php

	n = as.integer(n);
		if(!PHP.offset) { n = n - 1; } # PHP indexes at "0"

	if(!is.null(length))
		{
		length = as.integer(length);
		if(!PHP.offset) { length = length - 1; } # PHP indexes at "0"
		}

	str.len = strlen(str);
		if(is.negative(n))
			{
			str.tmp = substr(str, start=1+(str.len + n), stop=str.len );
				if(is.null(length)) { return (str.tmp); }
				if(length == 0) 	{ return (str.tmp); }
			if(is.positive(length))
				{
				str.final = substr(str.tmp, start=1, stop = length);
				} else {
						str.len.tmp = strlen(str.tmp);
						str.final = substr(str.tmp, start=1, stop = str.len.tmp + length);
						}
			return ( str.final );
			} else {
					# PHP allows n = 0 ... first element ...
					str.tmp = substr(str, start=1+n, stop=str.len );
						if(is.null(length)) { return (str.tmp); }
						if(length == 0) 	{ return (str.tmp); }


					if(is.positive(length))
						{
						str.final = substr(str.tmp, start=1, stop = length);
						} else {
								str.len.tmp = strlen(str.tmp);
								str.final = substr(str.tmp, start=1, stop = str.len.tmp + length);
								}
					return ( str.final );
					}
	stop("humanVerse::.substr ... how did you get here?!?");
}






#' .substring
#'
#' @param strvec
#' @param n
#' @param length
#' @param PHP.offset
#'
#' @return
#' @export
#'
#' @examples
.substring = function(strvec, n = -1, length=NULL, PHP.offset=TRUE)
	{
  # this is a vectorized form of .substr ... not analagous to base::substring at all?
  # x <- c("asfef", "qwerty", "yuiop[", "b", "stuff.blah.yech")
  # ? substr
  # .substring(x, 2, 5, FALSE) ... is equivalent to ... substr(x, 2, 5) ... but why?  If you want the PHP negative offsetting, just use its indexing.

	m = length(strvec);
	res = character(m);
	for(i in 1:m)
		{
		str = strvec[i];
		res[i] = .substr(strvec[i], n=n, length=length, PHP.offset=PHP.offset);
		}
	res;
	}

































#' char.vec
#'
#' @param str
#' @param sep
#'
#' @return
#' @export
#'
#' @examples
char.vec = function(str, sep="")
	{
  # splits a string into a vector ...
	strsplit(str, sep, fixed=TRUE)[[1]];
	}






#' str_replace
#'
#' @param find
#' @param replace
#' @param str
#' @param method
#'
#' @return
#' @export
#'
#' @examples
str_replace = function(find, replace, str, method="base")
  {
  # this is "fixed" find and replace # str = gsub(find[i], replace[i], str, fixed=TRUE);
  # method = base, method = stringi
  # stringi is not performing well on this:  "{wsu.crimson}" with fixed


  # if find/replace are longer ... if one is length one, repeat the other one
  n.find = length(find);
  n.replace = length(replace);
  n.max = max(n.find, n.replace);

  if(n.find == 1 && n.replace == 1)
    {
    if(n.find == 1)
      {
      find = rep(find, n.replace);
      } else {
              if(n.replace == 1)
                {
                find = rep(replace, n.find);
                } else {
                        stop("find and replace mismatch");
                        }
              }
    }
  ### let's loop and replace ...
  for(i in 1:n.max)
    {
    if( isTRUE(requireNamespace("stringi", quietly = TRUE)) && method=="stringi" )
      {
      # I need to verify this is the correct function ...
      str = stringi::stri_replace_first_fixed(str, find[i], replace[i]);
      } else {
              str = gsub(find[i], replace[i], str, fixed=TRUE);
              }
    }
  str;
  }

# str_replace_all(











#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' str.contains
#'
#' See PHP str_contains
#'
#' @param haystack Can be MULTI-VARIATE
#' @param needle is UNI-VARIATE
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples
str.contains = function(haystack = "hello friend", needle = " ", ...)
	{
	grepl(needle, haystack, fixed = TRUE);
	# /*
	# tmp = str.split(needle, haystack);
	# n = length(tmp);
	# res = logical(n);
	# for(i in 1:n)
		# {
		# res[i] = if( length(tmp[[i]]) > 1 ) { TRUE } else { FALSE }
		# }
	# res;
	# */
	}






































i = 3;
while(i < n.sqrt)
  {
  if(i %% 1000 == 0) { cat("\n", "THREES::  i ", i, " of ", n.sqrt, " ==> ", round(100*i/n.sqrt, 2), "\n");  flush.console(); }
  if(bits.prime[i] == TRUE)
            {
            k = i * 2;
            j = i * i;
            while(j < n)
              {
              if(j %% 1000 == 0) { cat("\n", "INNER::  j ", j, " of ", n, " ==> ", round(100*j/n, 2), "\n");  flush.console(); }
              bits.prime[j] = FALSE;
              j = k + j;
              }
            }
  i = 2+i;
  }

