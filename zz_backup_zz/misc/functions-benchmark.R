
library(microbenchmark);




benchmark.efficiency = function(mb.res)
	{
	mb.names = levels(mb.res$expr);
	
	out = NULL;

	A.name 		= mb.names[1];
	A 			= subset(mb.res, expr==A.name)$time;
	A.median 	= doMedian(A);

	row = c(1,A.name,A.median,0); # BENCHMARK, delta efficiency is 0
	out = rbind(out, row);

	n.names = length(mb.names);
	if(n.names > 1)
		{
		for(i in 2:n.names)
			{
			B.name 		= mb.names[i];
			B 			= subset(mb.res, expr==B.name)$time;
			B.median 	= doMedian(B);
	
			B.eff 		= round(100* (A.median-B.median)/A.median , 2);

			row = c(i,B.name,B.median,B.eff);
			out = rbind(out, row);
			}
		}

	out = data.frame(out);
	colnames(out)=c("Index","Name","Median","Delta.Efficiency");
	# rownames(out)=out$Name;
	rownames(out) = NULL;
		out$Median = as.numeric(out$Median);
		out$Delta.Efficiency = as.numeric(out$Delta.Efficiency);
		#out$Ranking = order(out$Delta.Efficiency, decreasing=TRUE); # not to be confused with 'rank'
		out$Rank = rank(out$Median, ties="first");
	
	out = setAttribute("microbenchmark.unit",.Options$microbenchmark.unit,out);
		
	out;
	}
	
	