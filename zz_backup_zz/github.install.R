# auto-populate functions by running *** initMemory(TRUE); pseudoCompile('C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/', 'C:/_git_/github/MonteShaffer/humanVerse/github.install.R') ***

##### .INCLUDE #####
## C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-github.R
github.buildPath = function(github.user="", github.repo="", which="http")
	{
	method = tolower(which);
	
	str = switch(method,
            "raw"    = paste0("https://raw.githubusercontent.com/", github.user, "/", github.repo, "/"),
			
            "https"    = paste0("https://github.com/", github.user, "/", github.repo, "/"),
			"http"    = paste0("https://github.com/", github.user, "/", github.repo, "/"),
			
			"legacy"    = paste0("https://codeload.github.com/", github.user, "/", github.repo, "/legacy.tar.gz/main"),
			"tar.gz"    = paste0("https://codeload.github.com/", github.user, "/", github.repo, "/legacy.tar.gz/main"),
			
			

           paste0("https://github.com/", github.user, "/", github.repo, "/") # default case of switch
          );
		  
		  
	cleanup.url(str);
	}
## C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-file.R
getRemoteAndCache = function(remote, local.file = NULL,
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
    localpath = dirname(local.file);
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
	
	filestem = cleanup.local(filestem, append=append)

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
## C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-parse.R
cleanup.local = function(myfile, append="")
	{
	myfile = str_replace("//", "/",   myfile);
	myfile = str_replace("?", "^-QUESTION-^",   myfile);
	myfile = str_replace("&", "^-AND-^",   myfile);
	myfile = str_replace("=", "^-EQUAL-^",   myfile);

	if(append != "")
	  {
	  # append .html
	  s = strlen(append);
	  last.s = .substr(str, -1*s);
	  if(last.s != append)
	    {
	    myfile = paste0(myfile, append);
	    }
	  }
	myfile;
	}
## C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-file.R
createDirectoryRecursive = function(folder)
  {
  if(!dir.exists(folder))
    {
    dir.create(folder, recursive=TRUE);
    }
  }
## C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-parse.R
cleanup.url = function(url)
	{
	url = str_replace("///", "/",   url);
	url = str_replace("//", "/",   url);
	url = str_replace(":/", "://", url); # https://
	url;
	}
## C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-str.R
lastChar = function(str, trim=TRUE)
	{
	# this also works:: ... # .substr(str, -1)
	if(trim){ str = trimMe(str); }
	s.len = strlen(str);
	charAt(str, s.len);
	}
## C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-parse.R
folderizeURL = function(url = "https://en.wikipedia.org/wiki/Columbia_Falls,_Montana")
  {
  # TODO: update to explodeMe and str_replace
  str = strsplit(url, "//", fixed=TRUE)[[1]][2];
    find = c("/",".",",");
    replace = c("-","^","+");
    n.find = length(find);
  for(i in 1:n.find)
    {
    str = gsub(find[i],replace[i],str,fixed=TRUE);
    }
  str;
  }
## C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-encryption.R
md5 = function(strvec, times=1)
  {
  # https://cran.r-project.org/web/packages/openssl/openssl.pdf
  # md5(serialize)?
  # digest is not vector driven ... # `apply` could work ...
  if (requireNamespace("openssl", quietly = TRUE))
    {
    nstrvec = openssl::md5(strvec);
    if(times > 1)
        {
        for(j in 2:times)
          {
          nstrvec = openssl::md5(nstrvec);
          }
        }
    return( nstrvec );
    } else {
            if (requireNamespace("digest", quietly = TRUE))
              {
              return( md5.digest(strvec,times) );
              } else {
                      # do INTERNAL .md5 very slow
          					  n = length(strvec);
          					  nstrvec = character(n);
          					  for(i in 1:n)
          						{
          						nstrvec[i] = .md5(strvec[i]);
          						for(j in 2:times)
          						  {
          						  nstrvec[i] = .md5(nstrvec[i]);
          						  }
          						}
          					  return( nstrvec );
                      }

            }
	# should never get here with SLOW INTERNAL
  stop("`openssl` or `digest` must be installed to use `md5()`");
  }





#' .md5
#'
#' This is a univariate function ...
#'
#' @param s a string as an input
#'
#' @return an md5-string output
#' @export
#'
#' @examples
#' .md5("The quick brown fox jumps over the lazy dog");
#' .md5("alex 22");
.md5 = function(s)
  {
  # http://md5.mshaffer.com/         ... circa 2005 ???
  # http://md5.mshaffer.com/md5.js
  # https://tools.ietf.org/html/rfc1321 ... ported from c -> javascript -> R

  s = s[1]; # this is not vectorized ... very slow
  w = 8 * nchar( as.character(s), type="chars");
  hex = "0123456789abcdef";
  # w is length, so >>> should be >>
  L = bitShiftL( bitShiftR(w+64,9, TRUE), 4) + 15;
  x = numeric(L+15);
  i = 1; j = 1;
  while(i < w)
    {
    idx = bitShiftR(i,5) + 1;
    # print(idx);
    mychar = bitShiftL( bitwAnd( charCodeAt(s,j), 255), ((i-1) %% 32));
    nx = bitwOr(x[idx], mychar); # print(nx);
    x[idx] = nx;
    i = 8 + i;
    j = 1 + j;
    }


  idx = bitShiftR(w,5)+1;
  # x[w>>5] |= 0x80 << ((w)%32);
  # nx = bitwOr( x[idx], bitShiftL( 0x80, (w %% 32)) );
  nx = bitOr( x[idx], bitShiftL( 0x80, (w %% 32), unsigned=TRUE) );  # prevent some overflow


  x[idx] = nx;
  x[L] = w;

############### .INTERNAL FUNCTIONS ###############
# Shifting is done assuming the values represent unsigned integers.
  X = function (xx,yy)
    {
    l = bitwAnd(xx, 0xFFFF) + bitwAnd(yy, 0xFFFF);
    m = bitShiftR(xx,16) + bitShiftR(yy,16) + bitShiftR(l,16);
    bitwOr( bitShiftL(m,16),  bitwAnd(l, 0xFFFF) ); ## will this overflow?
    # bitOr
    }
  Y = function (qi,aa,bb,xi,si,ti)
    {
    X(Z(X(X(aa,qi),X(xi,ti)),si),bb);
    }
  Z = function (ni,ci)
    {
    # print(ni);
    # print(ci);
    bitwOr( bitShiftL(ni,ci), bitShiftR(ni,32-ci,TRUE) );
    }


  A = function (aa,bb,cc,dd,xi,si,ti)
    {
    Y( (bitwOr( bitwAnd(bb,cc), bitwAnd(bitwNot(bb),dd) )),
        aa,bb,xi,si,ti);
    }
  B = function (aa,bb,cc,dd,xi,si,ti)
    {
    Y( (bitwOr( bitwAnd(bb,dd), bitwAnd(cc,bitwNot(dd)) )),
        aa,bb,xi,si,ti);
    }
	C = function (aa,bb,cc,dd,xi,si,ti){
	  Y( (bitwXor(bb,bitwXor(cc,dd))),
	      aa,bb,xi,si,ti);
	  }
	D = function (aa,bb,cc,dd,xi,si,ti)
	  {
	  Y( (bitwXor(cc, (bitwOr(bb,bitwNot(dd))))),
	      aa,bb,xi,si,ti);
	  }
############### INTERNAL FUNCTIONS. ###############


############### DIGEST ###############
  a=1732584193; b=-271733879; c=-1732584194; d=271733878;
  i = 1;

  while(i < (1+L))
    {
    oa = a; ob = b; oc = c; od = d;

      a= A(a,b,c,d,x[i],    7, -680876936);
      d= A(d,a,b,c,x[i+1], 12, -389564586);
      c= A(c,d,a,b,x[i+2], 17,  606105819);
      b= A(b,c,d,a,x[i+3], 22, -1044525330);

    a=A(a,b,c,d,x[i+4],    7, -176418897);
    d=A(d,a,b,c,x[i+5],   12,  1200080426);
    c=A(c,d,a,b,x[i+6],   17, -1473231341);
    b=A(b,c,d,a,x[i+7],   22, -45705983);

      a=A(a,b,c,d,x[i+8],  7,  1770035416);
      d=A(d,a,b,c,x[i+9], 12, -1958414417);

      c=A(c,d,a,b,x[i+10],17, -42063);
      b=A(b,c,d,a,x[i+11],22, -1990404162);

    a=A(a,b,c,d,x[i+12],   7,  1804603682);
    d=A(d,a,b,c,x[i+13],  12, -40341101);
    c=A(c,d,a,b,x[i+14],  17, -1502002290);
    b=A(b,c,d,a,x[i+15],  22,  1236535329);

      a=B(a,b,c,d,x[i+1],  5, -165796510);
      d=B(d,a,b,c,x[i+6],  9, -1069501632);
      c=B(c,d,a,b,x[i+11],14,  643717713);
      b=B(b,c,d,a,x[i],   20, -373897302);

    a=B(a,b,c,d,x[i+5],    5, -701558691);
    d=B(d,a,b,c,x[i+10],   9,  38016083);
    c=B(c,d,a,b,x[i+15],  14, -660478335);
    b=B(b,c,d,a,x[i+4],   20, -405537848);

      a=B(a,b,c,d,x[i+9],  5,  568446438);
      d=B(d,a,b,c,x[i+14], 9, -1019803690);
      c=B(c,d,a,b,x[i+3], 14, -187363961);
      b=B(b,c,d,a,x[i+8], 20,  1163531501);

    a=B(a,b,c,d,x[i+13],   5, -1444681467);
    d=B(d,a,b,c,x[i+2],    9, -51403784);
    c=B(c,d,a,b,x[i+7],   14,  1735328473);
    b=B(b,c,d,a,x[i+12],  20, -1926607734);

      a=C(a,b,c,d,x[i+5],  4, -378558);
      d=C(d,a,b,c,x[i+8], 11, -2022574463);
      c=C(c,d,a,b,x[i+11],16,  1839030562);
      b=C(b,c,d,a,x[i+14],23, -35309556);

    a=C(a,b,c,d,x[i+1],    4, -1530992060);
    d=C(d,a,b,c,x[i+4],   11,  1272893353);
    c=C(c,d,a,b,x[i+7],   16, -155497632);
    b=C(b,c,d,a,x[i+10],  23, -1094730640);

      a=C(a,b,c,d,x[i+13], 4,  681279174);
      d=C(d,a,b,c,x[i],   11, -358537222);
      c=C(c,d,a,b,x[i+3], 16, -722521979);
      b=C(b,c,d,a,x[i+6], 23,  76029189);

    a=C(a,b,c,d,x[i+9],    4, -640364487);
    d=C(d,a,b,c,x[i+12],  11, -421815835);
    c=C(c,d,a,b,x[i+15],  16,  530742520);
    b=C(b,c,d,a,x[i+2],   23, -995338651);

      a=D(a,b,c,d,x[i],    6, -198630844);
      d=D(d,a,b,c,x[i+7], 10,  1126891415);
      c=D(c,d,a,b,x[i+14],15, -1416354905);
      b=D(b,c,d,a,x[i+5], 21, -57434055);

    a=D(a,b,c,d,x[i+12],   6,  1700485571);
    d=D(d,a,b,c,x[i+3],   10, -1894986606);
    c=D(c,d,a,b,x[i+10],  15, -1051523);
    b=D(b,c,d,a,x[i+1],   21, -2054922799);

      a= D(a,b,c,d,x[i+8],  6,  1873313359);
      d=D(d,a,b,c,x[i+15],10, -30611744);
      c=D(c,d,a,b,x[i+6], 15, -1560198380);
      b=D(b,c,d,a,x[i+13],21,  1309151649);

    a=D(a,b,c,d,x[i+4],    6, -145523070);
    d=D(d,a,b,c,x[i+11],  10, -1120210379);
    c=D(c,d,a,b,x[i+2],   15,  718787259);
    b=D(b,c,d,a,x[i+9],   21, -343485551);

      a=X(a,oa);
  		b=X(b,ob);
  		c=X(c,oc);
  		d=X(d,od);

    i = 16 + i;
    }
############### CONVERT TO HEXADECIMAL ###############
	xb= c(a,b,c,d);
  o = "";
  for(i in 0:15)
    {
    idx = 1 + bitwAnd( bitwShiftR( xb[ (bitwShiftR(i,2) + 1)] ,  ((i%%4)*8+4)), 0xF);
      o = paste0(o, charAt(hex,idx) );
    idx = 1 + bitwAnd( bitwShiftR( xb[ (bitwShiftR(i,2) + 1)] ,  ((i%%4)*8)), 0xF);
      o = paste0(o, charAt(hex,idx) );
    }
	o = setAttribute("xb", xb, o);
	o;
  }
## C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-file.R
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
## C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-get-set.R
setAttribute = function(myAttribute, myValue, myObj)
	{
	attributes(myObj)[[myAttribute]] = myValue;
	myObj;  # no object referencing, so I must return
	}
## C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-file.R
moveFile = function(src, dest, unlink=TRUE)
	{
		src 	= as.character(src);
		dest 	= as.character(dest);
	file.copy(src, dest);  # there is no file.move ???
	unlink(src);
	}
## C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-file.R
downloadFile = function(remote, myfile, n=(2^31 - 1), quiet = TRUE, ...)  # n could be 2^31 - 1
  {
  if(isTRUE(capabilities("libcurl")))
    {
    utils::download.file(remote, myfile, quiet = quiet, ...);
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
## C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-str.R
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
## C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-str.R
strlen = function(str)
  {
  # history :: # https://en.cppreference.com/w/c/string/byte/strlen
  # http://www.cplusplus.com/reference/cstring/
  # https://en.wikipedia.org/wiki/C99
  # https://www.programiz.com/c-programming/library-function/string.h/strlen
  # vectorized ... already
  nchar( as.character(str), type="chars");
  }
## C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-str.R
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
## C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-mysql.R
        warning = function(w)
            {
              warning(paste0("fetchAllSQL throws a warning",w));
              res; # let's still return the value ...
            },
## C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-str.R
trimMe = function(str, side="both", method="stringi")
  {
  # sides = c("both", "left", "right")
  side = tolower(side);
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

  if( isTRUE(requireNamespace("stringi", quietly = TRUE)) && method=="stringi" )
    {
    switch(side,
          "left"  = stringi::stri_trim_left(str),
          "right" = stringi::stri_trim_right(str),
          "both"  = stringi::stri_trim_both(str),
          stringi::stri_trim_both(str)
          );
    } else {
            switch(side,
                  "left"  = gsub("^\\s+", "", str),
                  "right" = gsub("\\s+$", "", str),
                  "both"  = gsub("^\\s+|\\s+$", "", str),
                  gsub("^\\s+|\\s+$", "", str)
                  );
            }
  }
## C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-str.R
charAt = function(str,idx)
  {
  substr(str,idx,idx);
  }
## C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-encryption.R
md5.digest = function(strvec, times=1, serialize=FALSE)
  {
  if (requireNamespace("digest", quietly = TRUE))
    {
	  ####################### digest is not vector driven ... # `apply` could work ...
	  n = length(strvec);
	  nvec = c();
	  for(i in 1:n)
		{
		myval = digest::digest(strvec[i], algo="md5", serialize=serialize);
		if(times > 1)
		  {
		  for(j in 2:times)
			{
			myval = digest::digest(myval, algo="md5", serialize=serialize);
			}
		  }
		nvec[i] = myval;
		}
	  nvec;
	} else { return (NULL); }
  }
## C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-encryption.R
.md5 = function(s)
  {
  # http://md5.mshaffer.com/         ... circa 2005 ???
  # http://md5.mshaffer.com/md5.js
  # https://tools.ietf.org/html/rfc1321 ... ported from c -> javascript -> R

  s = s[1]; # this is not vectorized ... very slow
  w = 8 * nchar( as.character(s), type="chars");
  hex = "0123456789abcdef";
  # w is length, so >>> should be >>
  L = bitShiftL( bitShiftR(w+64,9, TRUE), 4) + 15;
  x = numeric(L+15);
  i = 1; j = 1;
  while(i < w)
    {
    idx = bitShiftR(i,5) + 1;
    # print(idx);
    mychar = bitShiftL( bitwAnd( charCodeAt(s,j), 255), ((i-1) %% 32));
    nx = bitwOr(x[idx], mychar); # print(nx);
    x[idx] = nx;
    i = 8 + i;
    j = 1 + j;
    }


  idx = bitShiftR(w,5)+1;
  # x[w>>5] |= 0x80 << ((w)%32);
  # nx = bitwOr( x[idx], bitShiftL( 0x80, (w %% 32)) );
  nx = bitOr( x[idx], bitShiftL( 0x80, (w %% 32), unsigned=TRUE) );  # prevent some overflow


  x[idx] = nx;
  x[L] = w;

############### .INTERNAL FUNCTIONS ###############
# Shifting is done assuming the values represent unsigned integers.
  X = function (xx,yy)
    {
    l = bitwAnd(xx, 0xFFFF) + bitwAnd(yy, 0xFFFF);
    m = bitShiftR(xx,16) + bitShiftR(yy,16) + bitShiftR(l,16);
    bitwOr( bitShiftL(m,16),  bitwAnd(l, 0xFFFF) ); ## will this overflow?
    # bitOr
    }
  Y = function (qi,aa,bb,xi,si,ti)
    {
    X(Z(X(X(aa,qi),X(xi,ti)),si),bb);
    }
  Z = function (ni,ci)
    {
    # print(ni);
    # print(ci);
    bitwOr( bitShiftL(ni,ci), bitShiftR(ni,32-ci,TRUE) );
    }


  A = function (aa,bb,cc,dd,xi,si,ti)
    {
    Y( (bitwOr( bitwAnd(bb,cc), bitwAnd(bitwNot(bb),dd) )),
        aa,bb,xi,si,ti);
    }
  B = function (aa,bb,cc,dd,xi,si,ti)
    {
    Y( (bitwOr( bitwAnd(bb,dd), bitwAnd(cc,bitwNot(dd)) )),
        aa,bb,xi,si,ti);
    }
	C = function (aa,bb,cc,dd,xi,si,ti){
	  Y( (bitwXor(bb,bitwXor(cc,dd))),
	      aa,bb,xi,si,ti);
	  }
	D = function (aa,bb,cc,dd,xi,si,ti)
	  {
	  Y( (bitwXor(cc, (bitwOr(bb,bitwNot(dd))))),
	      aa,bb,xi,si,ti);
	  }
############### INTERNAL FUNCTIONS. ###############


############### DIGEST ###############
  a=1732584193; b=-271733879; c=-1732584194; d=271733878;
  i = 1;

  while(i < (1+L))
    {
    oa = a; ob = b; oc = c; od = d;

      a= A(a,b,c,d,x[i],    7, -680876936);
      d= A(d,a,b,c,x[i+1], 12, -389564586);
      c= A(c,d,a,b,x[i+2], 17,  606105819);
      b= A(b,c,d,a,x[i+3], 22, -1044525330);

    a=A(a,b,c,d,x[i+4],    7, -176418897);
    d=A(d,a,b,c,x[i+5],   12,  1200080426);
    c=A(c,d,a,b,x[i+6],   17, -1473231341);
    b=A(b,c,d,a,x[i+7],   22, -45705983);

      a=A(a,b,c,d,x[i+8],  7,  1770035416);
      d=A(d,a,b,c,x[i+9], 12, -1958414417);

      c=A(c,d,a,b,x[i+10],17, -42063);
      b=A(b,c,d,a,x[i+11],22, -1990404162);

    a=A(a,b,c,d,x[i+12],   7,  1804603682);
    d=A(d,a,b,c,x[i+13],  12, -40341101);
    c=A(c,d,a,b,x[i+14],  17, -1502002290);
    b=A(b,c,d,a,x[i+15],  22,  1236535329);

      a=B(a,b,c,d,x[i+1],  5, -165796510);
      d=B(d,a,b,c,x[i+6],  9, -1069501632);
      c=B(c,d,a,b,x[i+11],14,  643717713);
      b=B(b,c,d,a,x[i],   20, -373897302);

    a=B(a,b,c,d,x[i+5],    5, -701558691);
    d=B(d,a,b,c,x[i+10],   9,  38016083);
    c=B(c,d,a,b,x[i+15],  14, -660478335);
    b=B(b,c,d,a,x[i+4],   20, -405537848);

      a=B(a,b,c,d,x[i+9],  5,  568446438);
      d=B(d,a,b,c,x[i+14], 9, -1019803690);
      c=B(c,d,a,b,x[i+3], 14, -187363961);
      b=B(b,c,d,a,x[i+8], 20,  1163531501);

    a=B(a,b,c,d,x[i+13],   5, -1444681467);
    d=B(d,a,b,c,x[i+2],    9, -51403784);
    c=B(c,d,a,b,x[i+7],   14,  1735328473);
    b=B(b,c,d,a,x[i+12],  20, -1926607734);

      a=C(a,b,c,d,x[i+5],  4, -378558);
      d=C(d,a,b,c,x[i+8], 11, -2022574463);
      c=C(c,d,a,b,x[i+11],16,  1839030562);
      b=C(b,c,d,a,x[i+14],23, -35309556);

    a=C(a,b,c,d,x[i+1],    4, -1530992060);
    d=C(d,a,b,c,x[i+4],   11,  1272893353);
    c=C(c,d,a,b,x[i+7],   16, -155497632);
    b=C(b,c,d,a,x[i+10],  23, -1094730640);

      a=C(a,b,c,d,x[i+13], 4,  681279174);
      d=C(d,a,b,c,x[i],   11, -358537222);
      c=C(c,d,a,b,x[i+3], 16, -722521979);
      b=C(b,c,d,a,x[i+6], 23,  76029189);

    a=C(a,b,c,d,x[i+9],    4, -640364487);
    d=C(d,a,b,c,x[i+12],  11, -421815835);
    c=C(c,d,a,b,x[i+15],  16,  530742520);
    b=C(b,c,d,a,x[i+2],   23, -995338651);

      a=D(a,b,c,d,x[i],    6, -198630844);
      d=D(d,a,b,c,x[i+7], 10,  1126891415);
      c=D(c,d,a,b,x[i+14],15, -1416354905);
      b=D(b,c,d,a,x[i+5], 21, -57434055);

    a=D(a,b,c,d,x[i+12],   6,  1700485571);
    d=D(d,a,b,c,x[i+3],   10, -1894986606);
    c=D(c,d,a,b,x[i+10],  15, -1051523);
    b=D(b,c,d,a,x[i+1],   21, -2054922799);

      a= D(a,b,c,d,x[i+8],  6,  1873313359);
      d=D(d,a,b,c,x[i+15],10, -30611744);
      c=D(c,d,a,b,x[i+6], 15, -1560198380);
      b=D(b,c,d,a,x[i+13],21,  1309151649);

    a=D(a,b,c,d,x[i+4],    6, -145523070);
    d=D(d,a,b,c,x[i+11],  10, -1120210379);
    c=D(c,d,a,b,x[i+2],   15,  718787259);
    b=D(b,c,d,a,x[i+9],   21, -343485551);

      a=X(a,oa);
  		b=X(b,ob);
  		c=X(c,oc);
  		d=X(d,od);

    i = 16 + i;
    }
############### CONVERT TO HEXADECIMAL ###############
	xb= c(a,b,c,d);
  o = "";
  for(i in 0:15)
    {
    idx = 1 + bitwAnd( bitwShiftR( xb[ (bitwShiftR(i,2) + 1)] ,  ((i%%4)*8+4)), 0xF);
      o = paste0(o, charAt(hex,idx) );
    idx = 1 + bitwAnd( bitwShiftR( xb[ (bitwShiftR(i,2) + 1)] ,  ((i%%4)*8)), 0xF);
      o = paste0(o, charAt(hex,idx) );
    }
	o = setAttribute("xb", xb, o);
	o;
  }
## C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-message.R
message.stop = function(... , collapse = NULL, recycle0 = FALSE, pre=TRUE)
  {
  str.pre = str.post = "";
  if(pre)
	{
	str.pre  = paste0("\n", "====================== ERROR ======================", "\n");
	str.post = paste0("\n", "===================================================", "\n");
	}
  stop( paste0(str.pre, ..., str.post, collapse = collapse, recycle0 = recycle0), call. = FALSE );
  }
## C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-number.R
is.negative = function(x, ..., tol = sqrt(.Machine$double.eps))
  {
  more = unlist(list(...)); x = c(x, more);
  x < ( -1 * tol );
  }
## C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-number.R
is.positive = function(x, ..., tol = sqrt(.Machine$double.eps))
  {
  more = unlist(list(...)); x = c(x, more);
  x > tol;
  }
## C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-bits.R
bitShiftL = function(x, bits, unsigned=FALSE)
  {
  if(!is.negative(x) | unsigned)
    {
    tmp = suppressWarnings( bitwShiftL(x,bits) );                # <<<
    if(is.na(tmp)) { tmp = -2^31; }  # 0x80 << 24
    return( tmp );
    }
  tmp = suppressWarnings( -bitwShiftL(-x,bits) ); # - 1;                  # <<<
  if(is.na(tmp))
    {
    tmp = 2^31;
    if(is.negative(x)) { tmp = -1 * tmp; }
    }
  tmp;
  }
## C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-bits.R
bitShiftR = function(x, bits, unsigned=FALSE)
  {
  if(!is.negative(x) | unsigned) { return( bitwShiftR(x,bits) ); }
  -bitwShiftR(-x,bits) - 1; #  - 1;                  # >>>
  }
## C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-str.R
charCodeAt = function(str,idx)
  {
  charCode ( charAt(str,idx) ); #  as.numeric( iconv( charAt(str,idx), from="ASCII", to="unicodeFFFE", toRaw=TRUE)[[1]][2] );
  }
## C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-bits.R
bitOr = function(a, b)
  {
  if(!is.negative(a) && ( b <= -1 * 2^31) )
    {
    return (a + b);
    }
  if(!is.negative(b) && ( a <= -1 * 2^31) )
    {
    return (a + b);
    }
  bitwOr(a,b);
  }
## C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-str.R
charCode = function(svec)
  {
  # s = "monte";
  # svec = strsplit(s,"",fixed=TRUE)[[1]];
  r = c();
  for(s in svec)
    {
    r = c(r, as.numeric( iconv( s, from="ASCII", to="unicodeFFFE", toRaw=TRUE)[[1]][2] ) );
    }
  r;
  }

##### INCLUDE. #####

github.buildFromRepo(github.user="MonteShaffer", github.repo="humanVerse");

# https://boards.greenhouse.io/firaxis/jobs/4536156003