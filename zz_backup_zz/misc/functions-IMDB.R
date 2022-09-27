
utils::globalVariables(c("imdb.data"));



IMDB.buildSocialNetwork = function(return="all", ttids=NULL, imdb=imdb.data, use.cpp="auto")
	{	
	## need rownames/colnames or the eigenRank won't have good indexes ...
	
	if(is.character(use.cpp)) 
		{ 
		use.cpp = requireNamespace("Rcpp", quietly = TRUE); 
		} else 	{
				if(use.cpp) 
					{ 
					use.cpp = requireNamespace("Rcpp", quietly = TRUE); 
					if(!use.cpp)
						{
						warning("You wanted to use 'Rcpp', but not found, using traditional");
						}
					}
				}
	
	if(use.cpp)
		{
		cpp.file = "https://raw.githubusercontent.com/MonteShaffer/humanVerse/main/humanVerse/inst/cpp/matrix.cpp";
		
		cpp.local = getRemoteAndCache(cpp.file, force.download = TRUE);
		
		cat("\n\n ########### loading matrix.cpp FILE ########### \n\n");
		Rcpp::sourceCpp(cpp.local);		
		
		# A <- matrix(rnorm(10000), 100, 100);
		# B <- matrix(rnorm(10000), 100, 100);
		# library(microbenchmark); microbenchmark(eigenMatTrans(A), matrix.transpose(A), A%*%B, eigenMatMult(A, B), eigenMapMatMult(A, B))
		}
	
	
	work = c();
	if(return == "all") { work = c("AM", "AM.t", "AA", "MM"); } else { work = return; }
	
	
	network = imdb$movies.df$cast; 		
	
	if(is.null(ttids)) { ttids = network.ttids; }  # all of them 
		nids = as.data.frame(cbind(ttids)); colnames(nids) = "ttid";		
		net = merge(network, nids, by="ttid");		
	
		net.ttids = sort( unique(net$ttid) );
		net.nmids = sort( unique(net$nmid) );
		
		n.ttids = length(net.ttids);
		n.nmids = length(net.nmids);
		
		if(n.ttids < 2)
			{
			print(net);
			stop("we don't have enought data");
			}
			
	
	
	# let's cache this ...
	AM.folder = paste0( getSourceLocation(), "IMDB/AM/");
		createDirectoryRecursive(AM.folder);
		
	cat("\n\n", AM.folder, "\n\n");
	
	AM.stem = md5.object( c(net.nmids, net.ttids) );
	AM.file = paste0( AM.folder, AM.stem, ".rds");
	
	res = list();
	
	total = nrow(net);
	dim(AM);
	cat("\n\n We have [ ", total, " ] elements to load into matrix. ", "\n\n");
	
	if(file.exists(AM.file))
		{
		cat("\n\n ########### reading from CACHE ########### \n\n");
		AM = readRDS(AM.file);
		} else 	{
				# AM = actors in row, M in cols
				AM = matrix(0, nrow=n.nmids, ncol=n.ttids);
					rownames(AM) = net.nmids;
					colnames(AM) = net.ttids;
				# this does not use Matrix "sparse" class yet ...	
					
				# dim(AM);
				# AM[1:10,1:5];		
				
				
				# TODO: build functions-timer.R ... 
					## register timers ... start, stop, print for a key
				s.start = as.numeric(Sys.time());
				
				# There is likely a FASTER way, TBD
				for(i in 1:total)
					{
					row = net[i,];		
					if(i %% 5000 == 1) 
						{ print(i); flush.console();
						cat("\n\t\t", i, " ... ", " ttid: ", row$ttid, " => nmid: ", row$nmid, " ... name: ", row$name, "\n\n");
						}
					ttid = row$ttid;
					nmid = row$nmid;
					
					r = which(net.nmids == nmid);
					c = which(net.ttids == ttid);
						AM[r,c] = 1;		
					}
				
				s.end = as.numeric(Sys.time());
				s.time = round((s.end - s.start), 3);
				cat("\n\t\t\t", "MATRIX loaded in ", s.time, " seconds.", "\n\n");
				
				
				density = sum(AM) / (prod(dim(AM)));
				AM = setAttribute("density", paste0( round(100*density, 5), " %"), AM);
				
				cat("\n\n ########### writing to CACHE ########### \n\n");
				writeRDS(AM, AM.file);  # 5 MB compressed
				}
	
	if(is.element("AM", work))
		{
		res$AM = AM;
		}
	
	if(use.cpp)
		{
		AM.t = eigenMatTrans(AM);
		} else 	{
				AM.t = matrix.transpose(AM);
				}
		
	if(is.element("AM.t", work))
		{
			colnames(AM.t) = net.nmids;
			rownames(AM.t) = net.ttids;
					
		res$AM.t = AM.t;
		}
	
	# library(microbenchmark); microbenchmark( eigenMatTrans(AM), matrix.transpose(AM), times=1 );  # both take 13 seconds for a large AM 
	
	
	
	## can CPP use the CSR sparse matrix 
	
	if(is.element("AA", work))
		{
		AA.file = paste0( AM.folder, AM.stem, "-AA.rds");
		
		if(file.exists(AA.file))
			{
			cat("\n\n ########### reading AA from CACHE ########### \n\n");
			AA = readRDS(AA.file);
			} else 	{
					cat("\n\n ########### multiplying AA ########### \n\n");
		
					if(use.cpp)
						{
						AA = eigenMapMatMult(AM, AM.t);
						} else 	{
								AA = AM %*% AM.t;
								}
					
					
					colnames(AA) = rownames(AA) = net.nmids;
					
					cat("\n\n ########### writing AA to CACHE ########### \n\n");
					writeRDS(AA, AA.file);  		
					}
		
		# library(microbenchmark); microbenchmark( eigenMapMatMult(AM, AM.t), AM %*% AM.t, times=1 );  # regular takes [] seconds, eigen takes [] seconds
		# memory is MAXED ... Outise of RStudio for large might be a good idea
		
		res$AA = AA;
		}
	
	
	if(is.element("MM", work))
		{
		MM.file = paste0( AM.folder, AM.stem, "-MM.rds");
		
		if(file.exists(MM.file))
			{
			cat("\n\n ########### reading MM from CACHE ########### \n\n");
			MM = readRDS(MM.file);
			} else 	{
					cat("\n\n ########### multiplying MM ########### \n\n");
		
					if(use.cpp)
						{
						MM = eigenMapMatMult(AM.t, AM);
						} else 	{
								MM = AM.t %*% (AM);
								}
								
					colnames(MM) = rownames(MM) = net.ttids;
					
					cat("\n\n ########### writing MM to CACHE ########### \n\n");
					writeRDS(MM, MM.file);  		
					}
		res$MM = MM;
		}
		
	res;	
	}



IMDB.loadData = function(which="2020-Sept", store.global=TRUE)
	{
	imdb.data = NULL;
	if(which == "2020-Sept")
		{
		main = github.buildPath("DataWar", "imdb");
		raw  = github.buildPath("DataWar", "imdb", "raw");
		imdb.data = data.load("2020-Sept", "imdb", mode="rds", main=main, raw=raw, sub="");
		}
	if(store.global && !is.null(imdb.data)) { .GlobalEnv$imdb.data = imdb.data; }
	}



#' IMDB.getMovieInfoFromActorSearch
#'
#' @param ttid Film identifier `ttid`
#' @param return.cols list of columns you want to display, by default all
#' @param imdb If you want to apply this function to a different dataframe, it's possible
#'
#' @return dataframe of results ... ...
#' @export
IMDB.getMovieInfoFromActorSearch = function(ttid, return.cols=NULL, imdb=imdb.data$all.actors.movies)
  {
  info = stats::na.omit( imdb[imdb$ttid == ttid, ] ) ;
  if(is.null(return.cols)) { info; } else { info[, return.cols]; }
  }


#' IMDB.searchMovieTitle
#'
#' @param str Search string with basic wildcard `*` operator
#' @param return.cols list of columns you want to display, by default all
#'
#' @param ignore.case Defaults to TRUE, matches "mont" and "Mont"
#' @param perl Defaults to FALSE, convert string query to PERL regex?
#' @param imdb If you want to apply this function to a different dataframe, it's possible
#'
#'
#' @return dataframe of results
#' @export
IMDB.searchMovieTitle = function(str, return.cols=NULL, ignore.case=TRUE, perl=FALSE, imdb=imdb.data$all.actors.movies)
  {
  grx = utils::glob2rx(str);  # https://stackoverflow.com/questions/5823503/
  grx.grep = grep(grx,imdb$title, ignore.case=ignore.case, perl=perl);
  rows = imdb[grx.grep, ];
  if(is.null(return.cols)) { rows; } else { rows[, return.cols]; }
  }


#' IMDB.getPersonInfo
#'
#' @param nmid  Person identifier `nmid`
#' @param return.cols list of columns you want to display, by default all
#' @param imdb If you want to apply this function to a different dataframe, it's possible
#'
#' # dim(imdb.data$all.actors.info);  # 71332    10
#'
#' @return dataframe of results
#' @export
IMDB.getPersonInfo = function(nmid, return.cols=NULL, imdb=imdb.data$all.actors.info)
  {
  info = imdb[imdb$nmid == nmid, ];
  if(is.null(return.cols)) { info; } else { info[, return.cols]; }
  }



#' IMDB.getMoviesForPerson
#'
#' @param nmid  Person identifier `nmid`
#' @param return.cols list of columns you want to display, by default all
#' @param imdb If you want to apply this function to a different dataframe, it's possible
#'
#' dim(imdb.data$all.actors.info);     # 71332      10
#' dim(imdb.data$all.actors.movies);   # 282062     11
#' # glue table ::  nmid|ttid|rank ... rank from ActorSearchMovies
#' dim(imdb$all.actors.rank);          # 1842305     3
#'
#' @return dataframe of results
#' @export
IMDB.getMoviesForPerson = function(nmid, return.cols=NULL, imdb=imdb.data)
  {
  info = imdb$all.actors.rank[imdb$all.actors.rank$nmid == nmid, ];
  info.more = merge(info, imdb$all.actors.movies, by="ttid");
  info.more = sortDataFrameByNumericColumns(info.more,"rank", "ASC");

  if(is.null(return.cols)) { info.more; } else { info.more[, return.cols]; }
  }


#' IMDB.getActorsFromMovie
#'
#' @param ttid  Movie identifier `ttid`
#' @param return.cols list of columns you want to display, by default all
#' @param imdb If you want to apply this function to a different dataframe, it's possible
#'
#' dim(imdb.data$all.movies.actors.characters);     # 901324      4
#'
#' @return dataframe of results
#' @export
IMDB.getActorsFromMovie = function(ttid, return.cols=NULL, imdb=imdb.data)
  {
  info = imdb.data$all.movies.actors.characters[imdb.data$all.movies.actors.characters$ttid == ttid, ];
  info.more = info;
  info.more = sortDataFrameByNumericColumns(info.more,"actor.rank", "ASC");
  if(is.null(return.cols)) { info.more; } else { info.more[, return.cols]; }
  }


#' IMDB.searchPersonName
#'
#' @param str Search string with basic wildcard `*` operator
#' @param return.cols list of columns you want to display, by default all
#'
#' @param ignore.case Defaults to TRUE, matches "mont" and "Mont"
#' @param perl Defaults to FALSE, convert string query to PERL regex?
#' @param imdb If you want to apply this function to a different dataframe, it's possible
#'
#'
#' @return dataframe of results
#' @export
IMDB.searchPersonName = function(str, return.cols=NULL, ignore.case=TRUE, perl=FALSE, imdb=imdb.data$all.actors.info)
  {
  grx = utils::glob2rx(str);  # https://stackoverflow.com/questions/5823503/
  grx.grep = grep(grx,imdb$name, ignore.case=ignore.case, perl=perl);
  rows = imdb[grx.grep, ];
  if(is.null(return.cols)) { rows; } else { rows[, return.cols]; }
  }


#' IMDB.genericSearch
#'
#' @param str Search string with basic wildcard `*` operator
#' @param col.name The column to perform search on
#' @param return.cols list of columns you want to display, by default all
#'
#' @param ignore.case Defaults to TRUE, matches "mont" and "Mont"
#' @param perl Defaults to FALSE, convert string query to PERL regex?
#' @param imdb If you want to apply this function to a different dataframe, it's possible

#'
#' @return dataframe of results
#' @export
IMDB.genericSearch = function(str, col.name, return.cols=NULL, ignore.case=TRUE, perl=FALSE, imdb=imdb.data$all.actors.info)
  {
  grx = utils::glob2rx(str);  # https://stackoverflow.com/questions/5823503/
  grx.grep = grep(grx,imdb[col.name][[1]], ignore.case=ignore.case, perl=perl);
  rows = imdb[grx.grep, ];
  if(is.null(return.cols)) { rows; } else { rows[, return.cols]; }
  }



#' IMDB.getUniqueCharactersForPerson
#'
#' This is currently a function of `$all.movies.actors.characters`
#'  and *NOT* `$all.actors.info`
#'
#' @param nmid  Person identifier `nmid`
#' @param imdb If you want to apply this function to a different dataframe, it's possible
#'
#'
#' @return dataframe of results
#' @export
IMDB.getUniqueCharactersForPerson = function(nmid, imdb=imdb.data$all.movies.actors.characters )
  {
  rows = imdb[imdb$nmid==nmid, ];
  characters = as.data.frame( table( stats::na.omit( rows$character ) ) )[,c(2,1)];
      colnames(characters) = c("count","character");
  sortDataFrameByNumericColumns(characters,"count");
  }


