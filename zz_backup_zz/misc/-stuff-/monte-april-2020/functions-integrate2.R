




# monte says hi

# source("C:/_git_/github/MonteShaffer/humanVerse/misc/functions-integrate.R");

computeXiFromResolution = function(x.domain, i.lim = c(0,1), 
                                    dxi = 0.01, forceEven = TRUE, oxi = 10)
  {
  # dxi is "eps" like ... resolution per unit of x
  # if dxi = 100, that means 100 eps per unit of x
  # if dxi = .01, that means eps is about 0.01 in x units, and we compute eps from that ... 
  # "eps" is like "steps"
  # x.domain is the larger picture, i.lim is the integral boundaries ...
  # oxi = 10 ... 10 times fewer elements in the x.domain than in the actual limiting area
  # if (oxi = 1), it will be the same dxi resolution 
  
  # crude.lower ... if(x.domain[1] < i.lim[1])
  # fine.integral ... 
  # crude.upper
  
  if(is.null(i.lim)) { i.lim = c(0,1); }
  
  crude.lower = c();
  if(x.domain[1] < i.lim[1])
    {
    x.lower = x.domain[1];
    x.upper = i.lim[1];
    x.range = abs(x.lower - x.upper);
    
    if(dxi < 1)  # a fraction means steps
      {
      crude.lower  = seq( from = x.lower, 
                            to = x.upper, 
                            by = dxi * oxi);
      } else  {
              crude.lower  = seq( from = x.lower, 
                                    to = x.upper, 
                            length.out = x.range * dxi / oxi);
              }  
    }
  
  
  crude.upper = c();
  if(x.domain[2] > i.lim[2])
    {
    x.upper = x.domain[2];
    x.lower = i.lim[2];
    x.range = abs(x.lower - x.upper);
    
    if(dxi < 1)  # a fraction means steps
      {
      crude.upper  = seq( from = x.lower, 
                            to = x.upper, 
                            by = dxi * oxi);
      } else  {
              crude.upper  = seq( from = x.lower, 
                                    to = x.upper, 
                            length.out = x.range * dxi / oxi);
              }  
    }

  
  
  fine.limit = c();
    x.lower = i.lim[1];
    x.upper = i.lim[2];
    x.range = abs(x.lower - x.upper);
    
    if(dxi < 1)  # a fraction means steps
      {
      fine.limit  = seq( from = x.lower, 
                            to = x.upper, 
                            by = dxi);
      } else  {
              fine.limit  = seq( from = x.lower, 
                                    to = x.upper, 
                            length.out = x.range * dxi);
              }  

  if(forceEven == TRUE)
    {
    fine.len = length(fine.limit);
    if(fine.len %% 2 == 1) 
      { 
      fine.len = 1 + fine.len;
      # update it
      fine.limit  = seq( from = x.lower, 
                           to = x.upper, 
                   length.out = fine.len);
      }
    }
  
  
  xi = unique( c( crude.lower, fine.limit, crude.upper ) );
  
  list( "lower"    = (crude.lower),
        "integral" = (fine.limit),
        "upper"    = (crude.upper),
        "xi"       = (xi)
      );
  
  }


parseNumericalFunctionString = function(fstr="normal: -1, 1")
  {
  
  fstr = trimMe(fstr);
  
  ostr = fstr; # original
  
  ########### normal   -3.5 , 3.5
  #s = str_split( fstr ,":")[[1]];
  s = strsplit( fstr ,":", fixed=TRUE)[[1]];
  
    fkey.original = ( trimMe(s[1]) );
	fkey = tolower(fkey.original);
    fkey.3 = substr(fkey,1,3);
  #ss = str_split( trimMe(s[2]),">")[[1]];
  ss = strsplit( trimMe(s[2]),">", fixed=TRUE)[[1]];
  
  #xd = str_split( trimMe(ss[1]),",")[[1]];
  xd = strsplit( trimMe(ss[1]),",", fixed=TRUE)[[1]];
  xd = trimMe(xd);
    fdomain.x = suppressWarnings( as.numeric(xd) );
    
print(xd);
  xp = trimMe(ss[2]);  
  
  myFunction = "";
print(xp);  
  # let's build as function if appropriate ...
  if(!is.na(xp))
	{
	  vals = parseTextToListOfValues(xp);
	  myFunction = xd;
	  for(j in 1:length(vals))
		{
		key = names(vals)[j];
		val = vals[[key]];
		print(key); print(val);
		if(length(val) == 1)
			{
			myFunction = gsub(key, val, myFunction, fixed=TRUE);
			}
		if(key == "x.domain" && length(val) == 2)
			{
			fdomain.x = val;
			}
		}
	}
  
  res =  list( "fkey.3"  = fkey.3, "fkey" = fkey, "fstr" = ostr, "FUN.s" = xd,
        "FUN.n" = myFunction, "fparams" = xp,     "fdomain.x" = fdomain.x);
	print(res);
res;
  }



parseTextToListOfValues = function(xp)
	{
	# eval(parse(text = xp));
	res = list();
		tmp = strsplit(xp, ";", fixed=TRUE)[[1]];
		n.tmp = length(tmp);
	for(i in 1:n.tmp)
		{
		tmp2 = trimMe(strsplit(tmp[i], "=", fixed=TRUE)[[1]]);
		# print(tmp2);
		key = trimMe(tmp2[1]);
		val = trimMe(tmp2[2]);

		res[[key]] = eval( parse(text = val ) );
		}
		
	# let's sort by length, longest first, so find/replace will work correctly
	# https://stackoverflow.com/questions/22325719/
	v = names(res);
	rv = rev(v[order(nchar(v), v)])
	
	nres = list();
	for(r in rv)
		{
		nres[[r]] = res[[r]];
		}
	
		
	nres;
	}

getAttribute = function(myAttribute, myObj)
    {
    attributes(myObj)[[myAttribute]];  
    }

setAttribute = function(myAttribute, myValue, myObj)
    {
    attributes(myObj)[[myAttribute]] = myValue;
    myObj;  # no object referencing, so I must return
    }

buildNumericalDataForIntegral = function(fprep="normal: -1, 1", 
                                         i.lim = c(NA,NA), x.domain = c(-10,10), dxi = 0.01, forceEven = TRUE)
  {
  if(is.character(fprep))
    {
    fstr = fprep;
    fprep = parseNumericalFunctionString(fstr);
    }
  
  if(!is.list(fprep))
    {
    stop("Something is wrong, fprep is not a list");
    }
  
  print(x.domain);
  xp           = fprep$fparams;
  fkey.3       = fprep$fkey.3;
  fdomain.x    = fprep$fdomain.x;
  print(fdomain.x);
  eval(parse(text = xp)); # maybe will update here as well 
  if(anyNA(fdomain.x)) { fdomain.x = x.domain; } # default 
  
 # print(fdomain.x);
 x.domain = fdomain.x;
  
  # print(i.lim);
  # if(is.null(i.lim)) { i.lim = fdomain.x; } # default for i.lim
  if(is.na(i.lim[1])) { i.lim[1] = fdomain.x[1]; }
  if(is.na(i.lim[2])) { i.lim[2] = fdomain.x[2]; }
  
  # xp           = fprep$fparams;
  rev.x = FALSE; # by default ... 
  
  result = list();
	
	result = setAttribute("fprep", fprep, result);
	result = setAttribute("i.lim", i.lim, result);
	result = setAttribute("x.domain", x.domain, result);
	result = setAttribute("dxi", dxi, result);
	result = setAttribute("forceEven", forceEven, result);
  
###############  CASE :: normal  ###############  
  if(fkey.3 == "nor")
    {
    ### default domain ### 
    x.domain = c(-4, 4);
    if(length(fdomain.x) == 2) { x.domain = fdomain.x;}
    # figure out resolution with dxi and forceEven, given x domain  
    # xi = computeLengthFromResolution(x.domain, dxi = dxi, forceEven=forceEven); 
    i.xi = computeXiFromResolution(x.domain, i.lim = i.lim, dxi = dxi, forceEven=forceEven); 
    xi   = i.xi$xi;
    # resolution is at a generic unit scale, not specific to the integral domain
     
    ### default params ### 
    mean = 0; 
    sd = 1; 
    # could be overwritten with eval(parse
    eval(parse(text = xp));
    
    yi = dnorm(xi, mean=mean, sd=sd);
     # plot(xi,yi);
	result$x = xi;
	result$y = yi;
	
    return ( result );
    }

###############  CASE :: t  ###############  
  if(fkey.3 == "t")
    {
    ### default domain ### 
    x.domain = c(-4, 4);
    if(length(fdomain.x) == 2) { x.domain = fdomain.x;}
    # figure out resolution with dxi and forceEven, given x domain  
    # xi = computeLengthFromResolution(x.domain, dxi = dxi, forceEven=forceEven); 
    i.xi = computeXiFromResolution(x.domain, i.lim = i.lim, dxi = dxi, forceEven=forceEven); 
    xi   = i.xi$xi;
    # resolution is at a generic unit scale, not specific to the integral domain
    
    ### default params ### 
    df = 1; 
     # could be overwritten with eval(parse
    eval(parse(text = xp));
    
    yi = dt(xi, df=df);
     # plot(xi,yi);
    result$x = xi;
	result$y = yi;
	
    return ( result );
  }

###############  CASE :: Hotelling's T2 distribution  ###############  
  if(fkey.3 == "t^2" || fkey.3 == "t2" || fkey.3 == "hot")
    {
    ### default domain ### 
    x.domain = c(0.01, 16);
    if(length(fdomain.x) == 2) { x.domain = fdomain.x;}
    if(x.domain <=0 ) { stop("t^2 requires x > 0 for domain"); }
    # figure out resolution with dxi and forceEven, given x domain  
    # xi = computeLengthFromResolution(x.domain, dxi = dxi, forceEven=forceEven); 
    i.xi = computeXiFromResolution(x.domain, i.lim = i.lim, dxi = dxi, forceEven=forceEven); 
    xi   = i.xi$xi;
    # resolution is at a generic unit scale, not specific to the integral domain
    
    ### default params ### 
    p = 4;  # number of features
    n = 50;  # sample size  # library(Hotelling);
    rev.x = FALSE; 
     # could be overwritten with eval(parse
    eval(parse(text = xp));
    
    df1 = p; df2 = n-p+1;
    yi = df(xi, df1=df1, df2=df2);
    if(rev.x == TRUE) { xi = rev(xi); } # switches skew-left to skew-right
     # plot(xi,yi);
    result$x = xi;
	result$y = yi;
	
    return ( result );
  }  
  
###############  CASE :: f  ###############  
  if(fkey.3 == "f")
    {
    ### default domain ### 
    x.domain = c(0.01, 5);
    if(length(fdomain.x) == 2) { x.domain = fdomain.x;}
    if(x.domain[1] <= 0 ) { stop("F requires x > 0 for domain"); }
    # figure out resolution with dxi and forceEven, given x domain  
    # xi = computeLengthFromResolution(x.domain, dxi = dxi, forceEven=forceEven); 
    i.xi = computeXiFromResolution(x.domain, i.lim = i.lim, dxi = dxi, forceEven=forceEven); 
    xi   = i.xi$xi;
    # resolution is at a generic unit scale, not specific to the integral domain
    
    ### default params ### 
    df1 = 5; 
    df2 = 2; 
    rev.x = FALSE; 
     # could be overwritten with eval(parse
    eval(parse(text = xp));
    
    yi = df(xi, df1=df1, df2=df2);
    if(rev.x == TRUE) { xi = rev(xi); } # switches skew-left to skew-right
    # if(rev.x == TRUE) { xi = rev(xi); yi = rev(yi); }
    # plot(xi,yi);
    result$x = xi;
	result$y = yi;
	
    return ( result );
  }    
  
###############  CASE :: chisq  ###############     
  if(fkey.3 == "chi")
    {
    ### default domain ### 
    x.domain = c(0.01, 8);
    if(length(fdomain.x) == 2) { x.domain = fdomain.x;}
    if(x.domain[1] <=0 ) { stop("chi^2 requires x > 0 for domain"); }
    # figure out resolution with dxi and forceEven, given x domain  
    # xi = computeLengthFromResolution(x.domain, dxi = dxi, forceEven=forceEven); 
    i.xi = computeXiFromResolution(x.domain, i.lim = i.lim, dxi = dxi, forceEven=forceEven); 
    xi   = i.xi$xi;
    # resolution is at a generic unit scale, not specific to the integral domain
    
    ### default params ### 
    df = 2; 
    rev.x = FALSE; 
    # could be overwritten with eval(parse
    eval(parse(text = xp));
    
    yi = dchisq(xi, df=df);
    
    if(rev.x == TRUE) { xi = rev(xi); } # switches skew-left to skew-right
    # plot(xi,yi);
    result$x = xi;
	result$y = yi;
	
    return ( result );
    }
 
###############  CASE :: gamma  ###############     
  if(fkey.3 == "gam")
    {
    ### default domain ### 
    x.domain = c(0.01, 16);
    if(length(fdomain.x) == 2) { x.domain = fdomain.x;}
    if(x.domain[1] <=0 ) { stop("gamma requires x > 0 for domain"); }
    # figure out resolution with dxi and forceEven, given x domain  
    # xi = computeLengthFromResolution(x.domain, dxi = dxi, forceEven=forceEven); 
    i.xi = computeXiFromResolution(x.domain, i.lim = i.lim, dxi = dxi, forceEven=forceEven); 
    xi   = i.xi$xi;
    # resolution is at a generic unit scale, not specific to the integral domain
    
    ### default params ### 
    shape = 1; 
    scale = 2; 
    rev.x = FALSE; 
    # could be overwritten with eval(parse
    eval(parse(text = xp));
    
    yi = dgamma(xi, shape=shape, scale=scale);
    
    if(rev.x == TRUE) { xi = rev(xi); } # switches skew-left to skew-right
    # plot(xi,yi);
    result$x = xi;
	result$y = yi;
	
    return ( result );
    }

###############  CASE :: quadratic  ###############     
  if(fkey.3 == "qua" || fkey.3 == "par")
    {
    ### default domain ### 
    x.domain = c(-5, 5);
    if(length(fdomain.x) == 2) { x.domain = fdomain.x;}
    # figure out resolution with dxi and forceEven, given x domain  
    # xi = computeLengthFromResolution(x.domain, dxi = dxi, forceEven=forceEven); 
    i.xi = computeXiFromResolution(x.domain, i.lim = i.lim, dxi = dxi, forceEven=forceEven); 
    xi   = i.xi$xi;
    # resolution is at a generic unit scale, not specific to the integral domain
    
    ### default params ### 
    a = 1;
    h = 0;
    k = 0;
    rev.x = FALSE; 
    # could be overwritten with eval(parse
    eval(parse(text = xp));
    
    yi = a * (xi - h)^2 + k;
    
    if(rev.x == TRUE) { xi = rev(xi); } # switches skew-left to skew-right
    # plot(xi,yi);
    result$x = xi;
	result$y = yi;
	
    return ( result );
    }  

###############  CASE :: exponential  ###############     
  if(fkey.3 == "exp")
    {
    ### default domain ### 
    x.domain = c(-5, 5);
    if(length(fdomain.x) == 2) { x.domain = fdomain.x;}
    # figure out resolution with dxi and forceEven, given x domain  
    # xi = computeLengthFromResolution(x.domain, dxi = dxi, forceEven=forceEven); 
    i.xi = computeXiFromResolution(x.domain, i.lim = i.lim, dxi = dxi, forceEven=forceEven); 
    xi   = i.xi$xi;
    # resolution is at a generic unit scale, not specific to the integral domain
    
    ### default params ### 
    a = 1;
    b = -2;
    h = 0;
    k = 0;
    rev.x = FALSE; 
    # could be overwritten with eval(parse
    eval(parse(text = xp));
    
    yi = a * exp(b*(xi - h)) + k;
    
    if(rev.x == TRUE) { xi = rev(xi); } # switches skew-left to skew-right
    # plot(xi,yi);
    result$x = xi;
	result$y = yi;
	
    return ( result );
    }    
    
  
  
  
  ###############  CASE :: brachistochrone ###############     
  if(fkey.3 == "bra" || fkey.3 == "cos")
  {
    {
      ### default domain ### 
      x.domain = c(-5, 5);
      if(length(fdomain.x) == 2) { x.domain = fdomain.x;}
      # figure out resolution with dxi and forceEven, given x domain  
      # xi = computeLengthFromResolution(x.domain, dxi = dxi, forceEven=forceEven); 
      i.xi = computeXiFromResolution(x.domain, i.lim = i.lim, dxi = dxi, forceEven=forceEven); 
      xi   = i.xi$xi;
      # resolution is at a generic unit scale, not specific to the integral domain
      
      ### default params ### 
      a = 1;
      b = -2;
      h = 0;
      k = 1;
      rev.x = FALSE; 
      # could be overwritten with eval(parse
      eval(parse(text = xp));
      
      yi = a * cosh(b*(xi - h)) + k;
      
      if(rev.x == TRUE) { xi = rev(xi); } # switches skew-left to skew-right
      # plot(xi,yi);
    result$x = xi;
	result$y = yi;
	
    return ( result );
    }    
    
  }
  
  
  ###############  CASE :: power  ###############     
  if(fkey.3 == "pow")
  {
    {
      ### default domain ### 
      x.domain = c(-5, 5);
      if(length(fdomain.x) == 2) { x.domain = fdomain.x;}
      # figure out resolution with dxi and forceEven, given x domain  
      # xi = computeLengthFromResolution(x.domain, dxi = dxi, forceEven=forceEven); 
      i.xi = computeXiFromResolution(x.domain, i.lim = i.lim, dxi = dxi, forceEven=forceEven); 
      xi   = i.xi$xi;
      # resolution is at a generic unit scale, not specific to the integral domain
      
      ### default params ### 
      a = 2;
      h = 0.3;
      p = 3;
      k = 1;
      rev.x = FALSE; 
      # could be overwritten with eval(parse
      eval(parse(text = xp));
      yi= a*(xi+h)^p + k;
      
      if(rev.x == TRUE) { xi = rev(xi); } # switches skew-left to skew-right
      # plot(xi,yi);
    result$x = xi;
	result$y = yi;
	
    return ( result );
    }    
    
  }
  
  
  ###############  CASE :: log  ###############     
  if(fkey.3 == "log")
  {
    {
      ### default domain ### 
      x.domain = c(-5, 5);
      if(length(fdomain.x) == 2) { x.domain = fdomain.x;}
      # figure out resolution with dxi and forceEven, given x domain  
      # xi = computeLengthFromResolution(x.domain, dxi = dxi, forceEven=forceEven); 
      i.xi = computeXiFromResolution(x.domain, i.lim = i.lim, dxi = dxi, forceEven=forceEven); 
      xi   = i.xi$xi;
      # resolution is at a generic unit scale, not specific to the integral domain
      
      ### default params ### 
      a = 2;
      h = 0.3;
      k = 1;
      rev.x = FALSE; 
      # could be overwritten with eval(parse
      eval(parse(text = xp));
      yi=a*log(xi+h) + k;
      
      if(rev.x == TRUE) { xi = rev(xi); } # switches skew-left to skew-right
      # plot(xi,yi);
    result$x = xi;
	result$y = yi;
	
    return ( result );
    }    
    
  }
  
  ###############  CASE :: bump  ###############     
  if(fkey.3 == "bum")
  {
    {
      ### default domain ### 
      x.domain = c(-5, 5);
      if(length(fdomain.x) == 2) { x.domain = fdomain.x;}
      # figure out resolution with dxi and forceEven, given x domain  
      # xi = computeLengthFromResolution(x.domain, dxi = dxi, forceEven=forceEven); 
      i.xi = computeXiFromResolution(x.domain, i.lim = i.lim, dxi = dxi, forceEven=forceEven); 
      xi   = i.xi$xi;
      # resolution is at a generic unit scale, not specific to the integral domain
      
      ### default params ### 
      a = 2;
      b = 1;
      c = 1;
      rev.x = FALSE; 
      # could be overwritten with eval(parse
      eval(parse(text = xp));
      smoothbump=function(x,b=1,c=2){ifelse( (b*x+c)^2>=1,0,exp(1/ (( b*x + c)^2 - 1))  )
      }
      yi=a*smoothbump(xi,b,c);
      
      if(rev.x == TRUE) { xi = rev(xi); } # switches skew-left to skew-right
      # plot(xi,yi);
    result$x = xi;
	result$y = yi;
	
    return ( result );
    }    
    
  }
  
  
  ###############  CASE :: wavelet  ###############     
  if(fkey.3 == "wav")
  {
    {
      ### default domain ### 
      x.domain = c(-5, 5);
      if(length(fdomain.x) == 2) { x.domain = fdomain.x;}
      # figure out resolution with dxi and forceEven, given x domain  
      # xi = computeLengthFromResolution(x.domain, dxi = dxi, forceEven=forceEven); 
      i.xi = computeXiFromResolution(x.domain, i.lim = i.lim, dxi = dxi, forceEven=forceEven); 
      xi   = i.xi$xi;
      # resolution is at a generic unit scale, not specific to the integral domain
      
      ### default params ### 
      A = 2;
      a=1;
      b = 1;
      rev.x = FALSE; 
      # could be overwritten with eval(parse
      eval(parse(text = xp));
      sinc=function(x,a=-7,b=-6){
        
        y=ifelse(x==0,1, (sin( a*x + b ))/(a*x+b) )
        return(y)
      }
      
      yi=A*sinc(xi,a,b);
      
      if(rev.x == TRUE) { xi = rev(xi); } # switches skew-left to skew-right
      # plot(xi,yi);
    result$x = xi;
	result$y = yi;
	
    return ( result );
    }    
    
  }
  
  ###############  CASE :: sinusoid ###############     
  if(fkey.3 == "sin" || fkey.3 == "cos" )
  {
    {
      ### default domain ### 
      x.domain = c(-5, 5);
      if(length(fdomain.x) == 2) { x.domain = fdomain.x;}
      # figure out resolution with dxi and forceEven, given x domain  
      # xi = computeLengthFromResolution(x.domain, dxi = dxi, forceEven=forceEven); 
      i.xi = computeXiFromResolution(x.domain, i.lim = i.lim, dxi = dxi, forceEven=forceEven); 
      xi   = i.xi$xi;
      # resolution is at a generic unit scale, not specific to the integral domain
      
      ### default params ### 
      a=1;
      b = 1;
      c=2;
      k=0;
      rev.x = FALSE; 
      # could be overwritten with eval(parse
      eval(parse(text = xp));
      
      yi=a*sin(b*xi+c)+k;
      
      if(rev.x == TRUE) { xi = rev(xi); } # switches skew-left to skew-right
      # plot(xi,yi);
    result$x = xi;
	result$y = yi;
	
    return ( result );
    }    
    
  }
  
###############  CASE :: uniform  ###############      
  if(fkey.3 == "uni" || fkey.3 == "rec")
    {
    ### default domain ### 
    x.domain = c(0,1);
    if(length(fdomain.x) == 2) { x.domain = fdomain.x;}
    # figure out resolution with dxi and forceEven, given x domain  
    # xi = computeLengthFromResolution(x.domain, dxi = dxi, forceEven=forceEven); 
    i.xi = computeXiFromResolution(x.domain, i.lim = i.lim, dxi = dxi, forceEven=forceEven); 
      xi   = i.xi$xi;
    # resolution is at a generic unit scale, not specific to the integral domain
    
    ### default params ### 
    height = 1;
    eval(parse(text = xp));
    yi = 0*xi + height;
    #plot(xi,yi);
    result$x = xi;
	result$y = yi;
	
    return ( result );
    }
  
  ###############  CASE :: triangle  ###############     
  if(fkey.3 == "tri" )
  {
    ### default domain ### 
    x.domain = c(-5, 5);
    if(length(fdomain.x) == 2) { x.domain = fdomain.x;}
    # figure out resolution with dxi and forceEven, given x domain  
    # xi = computeLengthFromResolution(x.domain, dxi = dxi, forceEven=forceEven); 
    i.xi = computeXiFromResolution(x.domain, i.lim = i.lim, dxi = dxi, forceEven=forceEven); 
    xi   = i.xi$xi;
    # resolution is at a generic unit scale, not specific to the integral domain
     
    ### default params ### 
    height = 1;
    rev.x = FALSE; 
    # could be overwritten with eval(parse
    eval(parse(text = xp));
    
    yi = height*(1-abs(xi));
    
    if(rev.x == TRUE) { xi = rev(xi); } # switches skew-left to skew-right
    # plot(xi,yi);
	result$x = xi;
	result$y = yi;
	
    return ( result );
  }  
  

    # can we do a generic one with x,y
    # y = ax + b ... let's allow any values, no defaults?
  
  ###############  CASE :: function  ###############     
  if(fkey.3 == "fun" || fkey.3 == "f(x" ||  fkey.3 == "g(x")
  {
    ### default domain ### 
    x.domain = c(-5, 5);
    # if(length(fdomain.x) == 2) { x.domain = fdomain.x;}
    # figure out resolution with dxi and forceEven, given x domain  
    # xi = computeLengthFromResolution(x.domain, dxi = dxi, forceEven=forceEven); 
    i.xi = computeXiFromResolution(x.domain, i.lim = i.lim, dxi = dxi, forceEven=forceEven); 
    xi   = i.xi$xi;
    # resolution is at a generic unit scale, not specific to the integral domain
     
    ### default params ### 
    # height = 1;
    # rev.x = FALSE;
    # could be overwritten with eval(parse
    # eval(parse(text = xp));
    # 
    # yi = height*(1-abs(xi));
	# polynomial = as.function( poly_calc(xs, ys) );
	# y = polynomial(x);
	# eval(parse(text = xp));
	# print(fprep);
	#print(x); print(xi);
		# as.function(alist(a = , b = 2, a+b))
		# myFunction = as.function(
		# FUN.n = gsub(" x", " xi", fprep$FUN.n, fixed=TRUE);
	# yi = eval(parse(text = FUN.n ) );
		myF = function(x) { eval(parse(text = fprep$FUN.n)); }
	yi = myF(xi);
    
    if(rev.x == TRUE) { xi = rev(xi); } # switches skew-left to skew-right
    # plot(xi,yi);
    result$x = xi;
	result$y = yi;
	
    return ( result );
  }  
  
##### END OF THE ROAD #####    
  warning("fkey.3 was not found, returning NA");
  return (NA);  
  }





differentSigns = function(a,b)
  {
  if(Re(a) > 0 && Re(b) < 0) { return(TRUE); }
  if(Re(a) < 0 && Re(b) > 0) { return(TRUE); }
  return(FALSE);
  }


# computeXiFromResolution = function(x.domain, i.lim = c(0,1), dxi = 0.01, forceEven = TRUE, oxi = 10)


computeNumericalIntegrationString = function(fstr,    
    start=NULL, # just added ... 
    stop=NULL,
    verbose=FALSE,
	plotMe = TRUE,
	parNew = FALSE,
    showPolygons = TRUE, # requires an active plot
	..., # pass parameters into plot ... 
    polygon.lwd = 1,
	dxi = 0.01,
	x.domain = c(-10,10), # this is overwritten in fstr
	forceEven = TRUE,
    polygon.border = NA,  # set color for each polygon border
    polygon.col.pos = "#4DBF4D", # "green", # positive-area color
    polygon.col.neg = "#BF4D4D" # "red", # negative-area color
    )
	{
	ilim = c(NA, NA);	
	if(!is.null(start))
      {
	  ilim[1] = start;
	  }
	if(!is.null(stop))
      {
	  ilim[2] = stop;
	  }

# print(ilim);	
	info = buildNumericalDataForIntegral(fstr, i.lim = ilim, x.domain = x.domain, dxi = dxi, forceEven = forceEven);  # ox = 10 is hidden, for values out of bounds ... 
	
	
	xdomain = info$x;
    yfinal = info$y;
	
	#print( range(xdomain) );
	#print( range(yfinal) );
	
	xlen = length(xdomain);
	
	# if everything above worked, this should be exact ...
	which.start = 1;
	if(!is.null(start))
      {
	  my.which = which(xdomain == start);
	  if(length(my.which) > 0) { which.start = my.which[1]; }
	  }
	which.stop = xlen;
	if(!is.null(stop))
      {
	  my.which = which(xdomain == stop);
	  if(length(my.which) > 0) { which.stop = my.which[1]; }
	  }
	
	
	
	xdomain = xdomain[which.start:which.stop];
	yfinal = yfinal[which.start:which.stop];
	  
	#print( range(xdomain) );
	#print( range(yfinal) );
	
	if(plotMe)
		{
		if(parNew) 
			{ 
			par(new=TRUE); 
			plot(info, type="l", lwd=3, xlab="", ylab="", axes=FALSE, xlim=range(info$x), ...);
			} else {
					plot(info, type="l", lwd=4, xlab="x", ylab="y", bty="n", xlim=range(info$x), ...);
					abline(h = 0, col="#888888");
					}
		
		
		}
	
	result = list();
		result$info = info;
		result$positive = result$negative = result$absolute = result$total = 0;
	polygons = list();
	
	xlen = length(xdomain);
	# cat("xlen: ", xlen);
	eps.a = c();
	for(i in 1:(xlen-1))
		{
		x = xdomain[i];
			xn = xdomain[i+1]; # next
		y = yfinal[i];
			yn = yfinal[i+1]; # next
			
		# x,y are already built?
		# why do I need getY ... only on support ?
		
		# TRAPEZOID
		eps = abs(xn - x) * (y + yn) / 2;
		eps.a = c(eps.a, eps);
		
		result$total = result$total + eps;
		result$absolute = result$absolute + abs(eps);
		
		# POLYGONS (and plus / minus)
		mycol =  polygon.col.pos; which.col = "pos";
		
		
		if(Re(eps) > 0)
		  {
		  mycol = polygon.col.pos;
		  which.col = "pos";
		  result$positive = result$positive + eps;
		  }
		if(Re(eps) < 0)
		  {
		  mycol = polygon.col.neg;
		  which.col = "neg";
		  result$negative = result$negative + eps;
		  }
    
		px = c(x, x, xn, xn);
		py = c(0, y, yn,  0);
     
		  if(showPolygons)
			{        
			polygon(px, py, col=mycol, border=polygon.border, lty=1, lwd=polygon.lwd);
			}
		polygons[[i]] = list("x" = px, "y" = py, "col" = which.col);		
		}
	# cat("eps.a: ", length(eps.a));
	
	return(list("result"=result, "eps" = eps.a, "polygons"=polygons));	
	}




computeNumericalIntegration = function(info,
    method="string",
    FUN="yi*1",
    tol= (.Machine$double.eps ^ 0.25),
    skip.signs = FALSE,
    fparams=NULL,
    start=NULL, # just added ... 
    stop=NULL,
    verbose=FALSE,
    animatePolygons = NULL, # if not null, this is Sys.sleep(3)
    showPolygons = FALSE, # requires an active plot
    polygon.lwd = 1,
    polygon.border = NA,  # set color for each polygon border
    polygon.col.pos = "#4DBF4D", # "green", # positive-area color
    polygon.col.neg = "#BF4D4D", # "red", # negative-area color
    return="result"
    )
  {
  if(method=="support")
    {
    ysupport = info$data$f.x;
    xdomain = info$i.xi$integral;
      xlen = length(xdomain);
      xskip = length(info$i.xi$lower);
    yfinal = ysupport[(xskip+1): (xskip+xlen)];
    }
  if(method == "string")
    {
    xdomain = info$x;
    yfinal = info$y;
      xlen = length(xdomain);
    # could be reversed
    if(xdomain[1] > xdomain[xlen])
      {
      print("REVERSED");
      xdomain = rev(info$x);
      yfinal = rev(info$y);
      }
	# if we have start and stop ... let's truncate the area ...
	# under - estimate ???
	# alertStart = FALSE; #   alertStop = FALSE;
	which.start = 1;
	if(!is.null(start))
      {
	  my.which = which(xdomain < start);
	  if(length(my.which) > 0) { which.start = 1 + max(my.which); }
	  }
	which.stop = xdomain[xlen];
	if(!is.null(stop))
      {
	  my.which = which(xdomain > stop);
	  if(length(my.which) > 0) { which.stop = min(my.which) - 1; }
	  }
	
	xdomain = xdomain[which.start:which.stop];
	yfinal = yfinal[which.start:which.stop];
	  
	#print(xdomain);
	#print(yfinal);
	
	
	
	
    }
  
  sign.changes = 0;
  ## track exponential order ... when did it meet a tolerance (x)
  ## if not, what was its avergae area (y) for the last (10) observations
  ## push/pop ... https://stackoverflow.com/questions/28687806/a-better-way-to-push-and-pop-to-from-lists-in-r
  
  # http://adv-r.had.co.nz/Environments.html
  # parent.env()
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
  getY = function(xi=1,yi=1,FUN="1",fparams=NULL)
    {
    #y.env = new.env();
    #extractList(fparams, envir = y.env);
    extractList(fparams);
    # FUN "mean(exp((tc*xi)^2*varvec/2)*cos(tc*xi*z))"
    #yi * eval(parse(text = FUN)); 
    
    eval(parse(text = FUN));
    }
##### let's break down eps further if there is a sign change ##### 
  eps.a = c();
  
  if(skip.signs)
  {
  final.x = xdomain;
  final.y = yfinal;
  } else {
  
  final.x = c();
  final.y = c();
  for(i in 1:(xlen-1))
    {
    # maybe loop through once and get the xc,yc for sign changes ... 
    x = xdomain[i];
    xn = xdomain[i+1]; # next
    
    
    
    ## we can do EVAL on FUN here ???
                                      # FUN "mean(exp((tc*xi)^2*varvec/2)*cos(tc*xi*z))"
    y = yfinal[i];                    # eval(parse(text = xp));
    yn = yfinal[i+1]; # next
    
    #print(paste0("y: ",y," --> yn: ",yn));
    
    # we use getY to build "betweeners, but let's not save as such.
    # the double getY was creating the exponential decay.
    
    y.e = getY( xi = x,  yi = yfinal[i],  FUN,fparams);
    yn.e = getY(xi = xn, yi = yfinal[i+1],FUN,fparams);
    
    
    #print(paste0("y: ",y," --> yn: ",yn));
    
    final.x = c(final.x,x);
    final.y = c(final.y,y);
    
    if(differentSigns(y.e,yn.e))
      {
      yc = 0;
      #xc = x + (xn - x) * (yc - y) / (yn - y);
      xc = x + (xn - x) * (yc - y.e) / (yn.e - y.e);
      
      final.x = c(final.x,xc);
      final.y = c(final.y,yc);
      
      sign.changes = 1 + sign.changes;
      }
    }
    final.x = c(final.x,xn);
    final.y = c(final.y,yn);
        }
  
  xlen = length(final.x);  
##### let's loop and calculate numerical area #####    
  result = list();
  result$positive = result$negative = result$absolute = result$total = 0;
  polygons = list();
  alertStart = FALSE;
  alertStop = FALSE;
  for(i in 1:(xlen-1))
    {
    x = final.x[i];
    
    if(!is.null(start))
      {
      if(start > x) 
        { 
        next; 
        } else {
				if(!alertStart)
					{
					if(verbose){ print(paste0("start: ",start, " ... x: ", x));}
					# return (FALSE);
					}
					
				alertStart = TRUE;
                }
      }
    
    if(!is.null(stop))
      {
      if(stop < x) 
        { 
		if(!alertStop)
			{
			if(verbose){ print(paste0("stop: ",stop, " ... x: ", x)); }
			# return (FALSE);
			}
		alertStop = TRUE;
        break; 
        }
      }
     
    xn = final.x[i+1]; # next
    
    
    y = final.y[i];
    yn = final.y[i+1]; # next
    
    #print(paste0("y: ",y," --> yn: ",yn));
    
    ## we can do EVAL on FUN here ???
    y = getY( xi = x,  yi = final.y[i],  FUN,fparams);
    yn = getY(xi = xn, yi = final.y[i+1],FUN,fparams);
    
    
    #print(paste0("y: ",y," --> yn: ",yn));
    
    if(verbose)
    {
    print(paste0(" ########### i = ",i,"  ###########"));
    print(paste0("y: ",y," --> yn: ",yn));
    print(paste0("x: ",x," --> xn: ",xn));
    }
    
    
    eps = abs(xn - x) * (y + yn) / 2;  
    
    eps.a = c(eps.a, eps);
     
    
    
    # if(is.na(eps)){eps=0}
    result$total = result$total + eps;
    result$absolute = result$absolute + abs(eps);
    mycol =  polygon.col.pos; which.col = "pos";
    if(Re(eps) > 0)
      {
      mycol = polygon.col.pos;
      which.col = "pos";
      result$positive = result$positive + eps;
      }
    if(Re(eps) < 0)
      {
      mycol = polygon.col.neg;
      which.col = "neg";
      result$negative = result$negative + eps;
      }
    
    # polygons

      px = c(x, x, xn, xn);
      py = c(0, y, yn,  0);
     
      if(showPolygons)
        {
        
        polygon(px, py, col=mycol, border=polygon.border, lty=1, lwd=polygon.lwd);
        if(length(animatePolygons) != 0)
          {
           # Sys.sleep(animatePolygons); 
          }
        }


    if(verbose)
    {
    print(paste0("x: ",x, "  ==> eps: ",eps, " ... ", result$total));
    }
    
    
    polygons[[i]] = list("x" = px, "y" = py, "col" = which.col);
    
#### end stop ??? ####      
    
      }
      

  result$sign.changes = sign.changes;
  result$eps = eps.a;
  
  # print(paste0("C.t = ",result$total));
  # print(paste0("F.s = ",result$total));
  # print(paste0("Area = ",result$total));
  
  
  if(return=="result") { return(result); }
  if(return=="total") { return(result$total); }
  if(return=="abs" || return == "absolute") { return(result$absolute); }
  
  if(return=="all") { return(list("result"=result,"polygons"=polygons)); }
  
   
  }
