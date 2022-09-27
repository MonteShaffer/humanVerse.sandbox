.GlobalEnv$imdb = list();

imdb.setCurrentPath = function()
	{
	mypath = paste0(.GlobalEnv$imdb$dataPath, .GlobalEnv$imdb$when, "/");
	.GlobalEnv$imdb$currentPath = mypath;
	}
	
imdb.setPath = function(path)
	{
	.GlobalEnv$imdb$dataPath = path;
	}
	
imdb.setWhen = function(when="")
	{
	if(is.empty(when)) { when = Sys.time(); }
	.GlobalEnv$imdb$when = getDate("%Y-%m", when);
	}	
	
imdb.urlTemplates = function()
	{	
	tmp = list();
		tmp[["movie-info"]] 				= "https://www.imdb.com/title/{ttid}/";
		tmp[["movie-crew"]] 				= "https://www.imdb.com/title/{ttid}/fullcredits";
		tmp[["movie-companies"]] 			= "https://www.imdb.com/title/{ttid}/companycredits";
		tmp[["movie-boxoffice"]] 			= "https://www.boxofficemojo.com/title/{ttid}/";
		
		tmp[["movie-boxoffice-release"]]	= "https://www.boxofficemojo.com/release/{rlid}/weekend/";
		tmp[["movie-list-by-year"]] 		= "https://www.imdb.com/search/title/?title_type=feature&year={date-start},{date-stop}&start={start}&sort={sort}";
		
		
		tmp[["actor-info"]] 				= "https://www.imdb.com/name/{nmid}/";
		tmp[["actor-bio"]] 					= "https://www.imdb.com/name/{nmid}/bio";
		tmp[["actor-awards"]] 				= "https://www.imdb.com/name/{nmid}/awards";
		tmp[["actor-films"]] 				= "https://www.imdb.com/filmosearch/?explore=title_type&role={nmid}&ref_=filmo_ref_typ&sort=year,asc&mode=detail&page={page}&title_type=movie"
		
	.GlobalEnv$imdb$urlTemplates = tmp;
	}




###########   FILMOGRAPHY   ########### 
imdb.getTotalMovieCount = function(local)
	{
	html.str = readStringFromFile(local);
	# <div class="desc">
	info = sliceDiceContent(html.str, start='<div class="desc">', end='</div>', strip=TRUE);
		# let's look for word title or titles
	info = removeWhiteSpace(info);
		tmp = explodeMe("title", info);
			first = trimMe(tmp[1]);
		tmp2 = explodeMe(" ", first);
			len = length(tmp2);
	as.numeric(tmp2[len]);	
	}
	


imdb.parseFilmography = function(key=key, verbose = FALSE)
	{
	template = "actor-films";
	local = paste0(.GlobalEnv$imdb$currentPath, template, "/", key, "/");
		createDirectoryRecursive(local);		
		
	cache.file = paste0(local, "films.txt");
	if(file.exists(cache.file))
		{
		return( readFromPipe(cache.file) );		
		}
		
	## we don't have it cached, so let's build it...
	pages = list.files(local, pattern=".html", full.names = TRUE);
	ttids = c();
	for(page in pages)
		{
		html.str = readStringFromFile(page);
		info = sliceDiceContent(html.str, start='<div class="lister-list">', end='<div class="footer filmosearch">', strip=FALSE);
		
		tmp = explodeMe('<a href="/title/', info);
		len = length(tmp);
		for(i in 2:len)
			{
			tmp2 = trimMe( explodeMe('/', tmp[i])[1] );
			tmp3 = trimMe( explodeMe('"', tmp2)[1] );
			ttids = c(ttids, tmp3);
			}		
		}
		
	ttids = unique(ttids);
		nts = length(ttids);
	
	nmids = rep(key, nts);
	
	df = as.data.frame( cbind(nmids, ttids) );
		colnames(df) = c("nmid", "ttid");
		
		writeToPipe(df, cache.file);
		
	df;
	}
	
imdb.downloadFilmography = function(key=key, replaces=NULL, force.download = FALSE, verbose = FALSE)
	{
	template = "actor-films";
	local = paste0(.GlobalEnv$imdb$currentPath, template, "/", key, "/");
		createDirectoryRecursive(local);		
	local.file = paste0(local, "page_{NNN}.html");
	
	if(verbose)
		{
		cat("\n", "local ... ", local.file, "\n\n");
		}
		
	remote = as.character(.GlobalEnv$imdb$urlTemplates[template]);
	nr = length(replaces);
	if(nr > 0)
		{
		for(i in 1:nr)
			{
			k = names(replaces)[i];
			v = replaces[[k]];
			remote = str_replace(paste0("{",k,"}"), v, remote);
			}
		}
		
	if(verbose)
		{
		cat("\n", "remote ... ", remote, "\n\n");
		}
		
	## now we can paginate ... 
	page = 1;
	myPage = strPadLeft(page,3);
	
	current.local 	= local.file; # current version
			current.local 	= str_replace("{NNN}", myPage, current.local);
	current.remote 	= remote;
			current.remote 	= str_replace("{page}", page, current.remote);
	getRemoteAndCache(current.remote, current.local, force.download = force.download, verbose = verbose);	
	
	movie.count = imdb.getTotalMovieCount(current.local);
	pages = ceiling(movie.count/50);
	if(pages > 1)
		{
		for(i in 2:pages)
			{
			page = i;
			myPage = strPadLeft(page,3);
				current.local 	= local.file; # current version
				current.local 	= str_replace("{NNN}", myPage, current.local);
				current.remote 	= remote;
				current.remote 	= str_replace("{page}", page, current.remote);
			getRemoteAndCache(current.remote, current.local, force.download = force.download, verbose = verbose);
			}
		}	
	}



imdb.downloadFile = function(template=template, key=key, replaces=NULL, force.download = FALSE, verbose = FALSE)
	{
	local = paste0(.GlobalEnv$imdb$currentPath, template, "/", key, "/");
		createDirectoryRecursive(local);		
	local.file = paste0(local, "index.html");
	
	if(verbose)
		{
		cat("\n", "local ... ", local.file, "\n\n");
		}
	
	remote = as.character(.GlobalEnv$imdb$urlTemplates[template]);
	nr = length(replaces);
	if(nr > 0)
		{
		for(i in 1:nr)
			{
			k = names(replaces)[i];
			v = replaces[[k]];
			remote = str_replace(paste0("{",k,"}"), v, remote);
			}
		}
		
	if(verbose)
		{
		cat("\n", "remote ... ", remote, "\n\n");
		}
	
	
	getRemoteAndCache(remote, local.file, force.download = force.download, verbose = verbose);	
	}
	
	
	
	
	
	
imdb.parseActorAwards = function(key=key, verbose = FALSE)
	{
	template = "actor-awards";
	local = paste0(.GlobalEnv$imdb$currentPath, template, "/", key, "/");
		createDirectoryRecursive(local);		
		
	cache.file = paste0(local, "awards.txt");
	if(file.exists(cache.file))
		{
		return( readFromPipe(cache.file) );		
		}	
	local.file = paste0(local, "index.html");
	html.str = readStringFromFile(local.file);
		tmp = explodeMe("<h3>", html.str);
	len = length(tmp);
		if(len > 1)
			{
			for(i in 2:len)
				{
				# this is category ...
				
				}
			}
	}
	
	
	
	










































imdb.parsePage= function(template=template, key=key)
	{
	local = paste0(.GlobalEnv$imdb$currentPath, template, "/", key, "/");
		createDirectoryRecursive(local);
	local.file = paste0(local, "index.html");
	
	if(!file.exists(local.file))
		{
		stop( paste0("imdb.parsePage ... local.file NOT FOUND: ", local.file) );
		}
	
	local.file;
	}
	

	
	
	
	
	
	