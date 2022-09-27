

# https://stackoverflow.com/questions/24481868/regular-expression-to-find-function-calls-in-a-function-body



# 
# 
# functions.search = function(fstr)
# 	{
# 	f = eval(parse(text=fstr));
# 
# 
# 	call.ignore <-c("[[", "[", "&","&&","|","||","==","!=",
#     "-","+", "*","/", "!", ">","<", ":");
# 
# 	v <- list()
#     if (is(f, "call") && !(deparse(f[[1]]) %in% call.ignore)) {
#         v[[1]] <- deparse(f)
#         if(!descend) return(v[[1]])
#     }
#     v <- append(v, lapply(as.list(f), functions.search))
#     unname(do.call(c, v))
# 
# 
# 	}



# https://stackoverflow.com/questions/39464205/how-to-get-the-list-of-in-built-functions-used-within-a-function

# https://stackoverflow.com/questions/24481868/regular-expression-to-find-function-calls-in-a-function-body
# list.functions?  ls("package:microbenchmark")
#
# find.funcs = function(f, descend=FALSE) {
#
# # we could look at list ?
# call.ignore <-c("[[", "[", "&","&&","|","||","==","!=",
#     "-","+", "*","/", "!", ">","<", ":")
#
# 	# what if "f" is a character ... ???
# 	if(is.character(f))
# 		{
# 		# fn = eval(parse(text=f));
# 		# f = body(fn);
# 		}
#
#
#     if( is.function(f)) {
#         return(find.funcs(body(f), descend=descend))
#     } else if (is(f, "name") | is.atomic(f)) {
#         return(character(0))
#     }
#
#
#     v <- list()
#     if (is(f, "call") && !(deparse(f[[1]]) %in% call.ignore)) {
#         v[[1]] <- deparse(f)
#         if(!descend) return(v[[1]])
#     }
# 	print(as.list(f));
#     v <- append(v, lapply(as.list(f), find.funcs, descend=descend))
#     unname(do.call(c, v))
# }

# scanFunctionsInFile('C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-get-set.R')
# outpath = 'C:/_git_/github/MonteShaffer/humanVerse/';
# buildFunctionsForSetup = function(srcpath, outpath, outfile = "functions-setup.R", append.str="### ALEX ###", fns=c(".onLoad", "mySource", "getSourceLocation", "github.includeFolder", "github.installBinary", "github.buildFromRepo" ), sort=TRUE)
# 	{
# 	# we have to register them all, so we can later match ...
# 	# we do a while loop, for each "error", we traceback, find function, and build into master file
# 	# functions.setup.R ... this file will be auto populated ...
# 	##			.onLoad();
# 	##			mySource();
# 	##			getSourceLocation();
# 	##			includeGithubFolder();
# 	##			github.includeFolder
# 	## 			github.installBinary
# 	## 			github.buildFromRepo
#
# 	myfile 	= paste0(outpath, outfile);
# 	mystr 	= readStringFromFile(myfile);
# 	nstr	= paste0(explodeMe(append.str,mystr)[1], append.str, "\n");
#
# 	# myfns = list();
#
#
# 	## maybe SOURCE EVERYTHING, and FIND ...
# 	for(fn in fns)
# 		{
# 		# let's walk through and find functions, add to myfns$myfn = 1 (count)
# 		# first try function called functions.traceforward ...
# 		# then do a while loop with trace on errors?
# 		}
#
#
#
# 	}
























# R> library(fortunes)
# R> fortune("parse")

# If the answer is parse() you should usually rethink the question.
   # -- Thomas Lumley
      # R-help (February 2005)

# R>




