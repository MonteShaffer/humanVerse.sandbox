























# https://www.gutenberg.org/cache/epub/feeds/
# https://stackoverflow.com/questions/24853/what-is-the-difference-between-i-and-i#:~:text=The%20only%20difference%20is%20the,the%20value%20the%20operator%20returns.&text=So%20basically%20%2B%2Bi%20returns,will%20have%20its%20value%20incremented.



encode.base64 = function(obj)
	{ 
	x = serialize(obj);
	
	}

url.toBase64 = function(obj)
	{
	
	raw = as.raw(obj)

	openssl::base64_encode
	}

url.fromBase64 = function(str)
	{

	}

url.fromList = function(str)
	{

	}

url.toList = function(url = c("https://en.wikipedia.org/wiki/Columbia_Falls,_Montana"), ...)
	{
	# url = "
	# manually parse ... https://www.mshaffer.com/hello/there.html?j=3&id=3309480
# https://stackoverflow.com/questions/695438/what-are-the-safe-characters-for-making-urls

	info = str.explode("://", url);
	http_ = list.getElements(info, 1);
	
	more = list.getElements(info, 2);
		find = c("/",".",",");
    replace = c("-","^","+");



	}


# https://coolbutuseless.github.io/2021/12/04/base64-encoding/decoding-in-plain-r/

## https://coolbutuseless.github.io/


RFC 3548 suggests not only to replace the / character. The URL and Filename safe Alphabet replaces:

the 63:nd / character with the underscore _
the 62:nd + character with the minus -.







# find = c("/",".",",");
    replace = c("-","^","+");
url.folderize = function(){}
url.folderize = function(url, 	w.chars = c("://","/","?",":", "*",'"'), 
								f.chars = c("@", "^^", "++", "^", "+", "__" ) )
	{
	# this could get spurious if they are not equal
	str.replace(w.chars, f.chars, url); # this needs to be pair-wise
	}

url.defolderize = function(url, w.chars = c("://","/","?",":", "*",'"'), 
								f.chars = c("@", "^^", "++", "^", "+", "__" ) )
	{
	# this could get spurious if they are not equal
	## order matters ^^ before ^ on replacement
	str.replace(f.chars, w.chars, url); # this needs to be pair-wise
	}

## https://en.wikipedia.org/wiki/Columbia_Falls,_Montana?monte=says&hi=TRUE#Demographics
## url ="https://en.wikipedia.org/wiki/Columbia_Falls,_Montana?monte=says&hi=TRUE#Demographics"
## folderize
url.stringify = function(url, showGET = TRUE,
						sep.dir = "^DIR^",						
						sep.get = "__GET_{DATA}_GET__",
						sep.base64 = "__B64_{DATA}_B64__"
						)
	{
	# using REGEX to deFolderize ... 
	sep.http = "__{PROTOCOL}__";		
										# pattern = "(_{2}){1}([^_]+){1}(_{2}){1}";
										# info = regex.match(pattern, path_);

	info = str.explode("://", url);
	http_ = str.replace("{PROTOCOL}", list.getElements(info, 1), sep.http);
	more = list.getElements(info, 2);
	 
	info = str.explode("?", more);
	path_ = dir.cleanupPath( list.getElements(info, 1), file.sep = sep.dir );
	more = list.getElements(info, 2);

	get_ = NULL;
	if(showGET)
		{
		info = str.explode("#", more);
		get_ = str.replace("{DATA}", list.getElements(info, 1), sep.get) ;
		## DONE here 
			get_[is.na(get_)] = "";
		more = list.getElements(info, 2);
		}
	# lot's of funny symbols possible in ANCHOR
	more.na = is.na(more);
	b64_ = str.replace("{DATA}", base64.enc(more), sep.base64);
	b64_[more.na] = "";
	

	folder = paste0(http_, path_, get_, b64_ );
				### flen = str.len(folder);
		# long filenames may create a PROBLEM ... calculate at FULL PATH
		# maybe in folder one-level UP store a hash-table 
		# MD5 -> TRUE KEY
		# maybe multiple files if pooling ... each pooling element writes to its file ... 
		# names(folder) = md5.digest(folder);  
	folder;
	}


# ALIASE:: url.toPath


url.deStringify = function(folder, showGET = TRUE,
								sep.dir 	= "^DIR^",
								sep.get 	= "__GET_{DATA}_GET__",
								sep.base64 	= "__B64_{DATA}_B64__"
						)
	{
	sep.http = "__{PROTOCOL}__";  # hardcoded b/c of REGEX

	get_ = NULL;
	if(showGET)
		{
		s.get = str.explode("{DATA}", sep.get);
			get_ = str.between(folder, s.get);
				na.get = is.na(get_);
				get_ = paste0("?", get_); 
				get_[na.get] = "";
		}

	s.base64 = str.explode("{DATA}", sep.base64);
		b64_ = base64.dec( str.between(folder, s.base64) );
			na.b64 = is.na(b64_);
				# if !showGET, the TRUE anchor tag <#> is in the base64
				# anchor is WORTHLESS? storing anyway
				sep.anchor = "#"; if(!showGET) { sep.anchor = "?"; }
			b64_ = paste0(sep.anchor, b64_); 
			b64_[na.b64] = "";


	info = str.explode(s.get[1], folder);
		more = list.getElements(info, 1);
		path_ = str.replace(sep.dir, "/", more);

	## THIS WILL BREAK if you change the 
	# https://regex101.com/r/dYJHqo/1
	pattern = "(_{2}){1}([^_]+){1}(_{2}){1}";
	info = regex.match(pattern, path_);
		http_ = list.getElements(info, 3);

		http__ = paste0("__",http_,"__");

		final_ = str.replace(http__, paste0(http_,"://"), path_);

	url = paste0(final_, get_, b64_ );

	}

# ALIASE:: url.fromPath

# https://www.mshaffer.com/hello/there.html?j=3&id=3309480

urls = c("https://en.wikipedia.org/wiki/Columbia_Falls,_Montana?monte=says&hi=TRUE#Demographics", "https://www.mshaffer.com/hello/there.html?j=3&id=3309480", "http://google.com");

folderizeURL = function(url = "https://en.wikipedia.org/wiki/Columbia_Falls,_Montana")
  {
	# could be multivariate?
  # TODO: update to explodeMe and str_replace
  info = strsplit(url, "//", fixed=TRUE);
	str = info[[1]][2];
	prot = str.replace(":","", info[[1]][1]);

    #find = c("/",".",",");
    #replace = c("_-_","^","+");
	
	find = c("/",",");
    replace = c("^","+");
    n.find = length(find);
  for(i in 1:n.find)
    {
    str = gsub(find[i],replace[i],str,fixed=TRUE);
    }
	str = paste0("__",prot,"__", str);
  str;
  }



url.download = function(url, local) {} 


url.trapOpenConnection = function(url, ...)
	{
	## # just give it a whirl 
	## https://stackoverflow.com/questions/52911812/check-if-url-exists-in-r
	# ## sees if the URL registers as valid with libcurl
	conn = url.trap(url);
		## connection mode ... ?open.connection
		if(!is.set(cmode)) 		{ cmode = "rt"; } # read-text  
		if(!is.set(timeout)) 	{ timeout = 2; }  # seconds
	
	if(isFALSE(conn))
		{
		return(conn); # it has some append attributed
		} else {
				conn.info = tryCatch	(

										{
										info = open.connection(conn, open=cmode, timeout=timeout, ...);
										},

										warning = function(w)
											{
											warning(paste0("### WARNING ###  throws a warning","\n\n",w));
											info; # let's still return the value 	.
											},
					
										error = function(e)
											{
											# warning(paste0("### ERROR ###  throws an error","\n\n",e));
											res = FALSE;
											res = property.set("ERROR", res, e);
											return (res);
											},

										finally =
											{
								
											}
										);
				return(conn.info);
				}
	return(FALSE);
	}


## call this function url.trap (url) instead of url (url)
url.trap = function(url)
	{
	url.info = tryCatch	(

						{
						info = url ( url );
						},

						warning = function(w)
							{
							warning(paste0("### WARNING ###  throws a warning","\n\n",w));
							info; # let's still return the value 	.
							},
	
						error = function(e)
							{
							# warning(paste0("### ERROR ###  throws an error","\n\n",e));
							res = FALSE;
							res = property.set(res, "ERROR", e);
							return (res);
							},

						finally =
							{
				
							}
						);
	url.info;
	}





# https://stackoverflow.com/questions/52911812/check-if-url-exists-in-r

# urls = c("http://google.com/", "http://amazon.com/")
# stati = c(200, 300, 404);
url.test = function(urls, stati = NULL, ... )
	{
	n.urls = length(urls);
	res = logical(n.urls);
	for(i in 1:n.urls)
		{
		url = urls[i];		
		if(isTRUE(capabilities("libcurl")))
			{
			h = http.headers(url);
			s = http.status(h);
			## is the plural of status ... statuses or stati ?
			if(is.null(stati))
				{
				res[i] = !is.empty(s);
				} else {
						res[i] = if(s %in% stati) { TRUE } else { FALSE }
						}
			} else {				
					check = url.trapOpenConnection(url);	
					res[i] = if(is.null(check)) { FALSE } else { TRUE }
					}		
		}
	res;
	}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
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
# format or test connection 
is.validURL = function(urls, deep=FALSE)
	{
	urls = str.trim(urls);
	if(!deep)
		{
		fil = prep.arg(urls, 3);
		x = (fil == "htt"); y = (fil == "ftp");  # multivariate, truth tables
		return ( (x+y > 0) );
		}
	# is valid ???
	stop("TODO: REGEX to filter valid URL");
	}


















 
dots.addTo = function(key, ..., by="column")
	{
	BY = prep.arg(by, n=3);

# dput(deparse(substitute(key)));

	a = is.atomic(key);  	# what type is the key ...
	v = is.vector(key);		# maybe list append
	l = is.list(key); 		# maybe cbind or rbind
	m = is.matrix(key);
	d = is.data.frame(key);
	
# cat("\n\n a: ", a, "\t v: ", v,  "\t l: ", l,  "\t m: ", m,  "\t d: ", d, "\n\n");	
# dput(list(...));
	# dots = as.list(...); 
	dots = match.call(expand.dots = FALSE)$...
# dput(dots);
# print(str(key));
# ct.KEY = check.type(key);
# dput(ct.KEY);
		names = as.character(dots);
		
	nd = length(dots);
	ndlen = list.getLengths(dots);
	ndmax = 0;
	if(!is.null(ndlen)) { ndmax = max(ndlen); }
	dnames = names(dots);
	kname = names(key);
	
# cat("\n\n nd: ", nd, "\t ndlen: ", ndlen,  "\t ndmax: ", ndmax,  "\t dnames: ", dnames,  "\t kname: ", kname, "\n\n");	

	if((a || v) && (ndmax > 1)) 
		{
		# make lists  
		res = check.list(key);
		names(res) = names(key);
		for(i in 1:nd)
			{
			dot = dots[[i]];
			dname = dnames[i];
			res[[dname]] = dot;
			}
		return(res);
		}
		
	if((a || v) && !m) 
		{
		more = unlist(dots);
		res = c(key, more);
		return(res);
		}
	if(m || d)
		{
		dm = dim(key);  # 3 x 1 ... match or transpose more 
		if(nd > 0)
			{
			for(i in 1:nd)
				{
				dot = dots[[i]];
				di = dim(dot);
				if(all.equal(di,dm))
					{
					# 3x1 and 3x1 ... 3x2 or 6x1
					if(BY == "col") { key = cbind(key, dot); }
					if(BY == "row") { key = rbind(key, dot); }
					} else {
							if(di[1] == dm[1])
								{
								# 3x2 and 3x1 ... rbind 
								key = rbind(key, dot);
								}
							if(di[2] == dm[2])
								{
								# 3x2 and 1x2 ... cbind  
								key = cbind(key, dot);
								}							
							}
				}
			}
		return(key);
		}
	
cat("\n key \n");
dput(key);
cat("\n list(...) \n");
dput(list(...));
cat("\n unlist(list(...)) \n");
dput(unlist(list(...)));
stop("monte");		
	}

#' @rdname dots.addToKey
#' @export
dots.addToKey = dots.addTo;
