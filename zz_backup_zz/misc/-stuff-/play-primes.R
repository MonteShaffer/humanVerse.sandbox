



## // bit for primes ...
## https://cran.r-project.org/web/packages/bit/vignettes/bit-usage.html
## https://www.quora.com/Which-is-the-fastest-algorithm-to-find-prime-numbers-using-C
	

sfsmisc::primes ## claims to be fast , integer or bits 

# wrap in SIMULATION, run in RGUI::BASE 
timer.start("sfsmisc::primes"); sfsmisc::primes(1000); timer.stop("sfsmisc::primes"); 
timer.start("pracma::primes"); pracma::primes(1000); timer.stop("pracma::primes"); 
timer.start("prime.findPrimes"); prime.findPrimes(1000); timer.stop("prime.findPrimes"); 
# implement my bit-version, # incorporate my cpp version 
timer.start("prime.cpu"); cpp_primes(1000); timer.stop("prime.cpu"); 





### primes.storeBits
### primes.lookupBits
### primes.range

#' prime.extendN
#'
#' @param n
#' @param base
#'
#' @return
#' @export
prime.extendN = function(n, base = 6)
	{
	remainder 	= n %% base;
	difference 	= base - remainder;
	(n + difference);
	}



#' prime.sieve
#'
#' @param from
#' @param to
#' @param p
#'
#' @return
#' @export
prime.sieve = function(from, to, p = c(2,3,5))
	{

  # this is slower than pracma::primes ... builds a matrix in chunks ...
  # optimus prime

# lcd
# gcf


# sieve attributed to erastothenes, he likely read it in a Gangan book
# base 6, undergrad math  ... 6k + 1 or 6k - 1
# https://primes.utm.edu/notes/faq/six.html
# https://math.stackexchange.com/questions/106417/
# https://mathworld.wolfram.com/PrimeGaps.html

	m = matrix( from:to, ncol=6, byrow = TRUE);

	m1 = m[,1]; # 6k + 1
	m5 = m[,5]; # 6k - 1

	mcs = sort( c(m1,m5) ); # candidates
	for(mc in mcs)
		{
		# if is.element is un-necessary
		remainder = mc %% p;
		zeroes = which(remainder == 0);
		if(length(zeroes) > 0) { next; }
		p = c(p, mc);
		## I could "unset" mcs that are multiples in the list ... change to a while loop
		## in this "chunk size", no real advantage past the first few primes as "gap"
		## pracma uses "seq" to unset the future values ...
		}
	p;
	}

#' prime.findFrom
#'
#' @param n
#' @param max.rows
#'
#' @return
#' @export
prime.findFrom = function(n, max.rows=100)
	{
	p = c(2,3,5);  # let's hold this in memory

	# if we have a long list, let's chunkify them into max.rows * 6
	max.len = 6 * max.rows;

	seq.start = 7;
	seq.end = prime.extendN(n - seq.start) + (seq.start - 1);

	seq.len = seq.end - seq.start + 1;

	if(seq.len <= max.len)
		{
		p = prime.sieve( seq.start, seq.end, p);
		} else 	{
				# let's chunkify
				chunks = ceiling(seq.len / max.len);

				seq.start = 7;
				seq.end = max.len + (seq.start - 1);
				for(i in 1:chunks)
					{
					p = prime.sieve( seq.start, seq.end, p);

					seq.start = 1 + seq.end;
					seq.end = max.len + (seq.start - 1);
					}
				}
	# because we are doing this is base-6 batches (*max.rows), we may have stragglers
	p[p < n];
	}



#' prime.findFirst
#'
#' @param n
#' @param max.rows
#'
#' @return
#' @export
prime.findFirst = function(n, max.rows=10000)
	{
  # https://primes.utm.edu/howmany.html
# https://stackoverflow.com/questions/2068372/fastest-way-to-list-all-primes-below-n
# https://stackoverflow.com/questions/4911777/finding-first-n-primes
# https://en.wikipedia.org/wiki/Prime_number_theorem#Approximations_for_the_nth_prime_number
	# Wolf ... https://mathworld.wolfram.com/PrimeGaps.html
	# seq.end = floor( (sqrt(n) * exp(sqrt(n)) ) );
	gn = ceiling( n * log(n) + n * log(log(n)) );

	p = prime.findFrom(gn, max.rows=max.rows);

	p[1:n];
	}



#' prime.pracmaPrimes
#'
#' @param n
#' @param first
#'
#' @return
#' @export
prime.pracmaPrimes = function(n, first=FALSE)
	{
	# this duplicates the primary logic of pracma::primes
	# by computing 'sqrt' one time, it speeds up things 'slightly'
	# it allows for firstN or fromN with first=FLAG
	gn = n;
	if(first) { gn = ceiling( n * log(n) + n * log(log(n)) ); }
	gn.sqrt = floor( sqrt(gn) );  # needs to round down so the "seq by k" doesn't break ...

	p = seq(1, gn, by = 2); # odd numbers
    q = length(p);
    p[1] = 2; 	# replace 1 with 2 (prime)
				# 9 is the first non-prime?
	if(gn >= 9)
		{
		for (k in seq(3, gn.sqrt, by = 2) )
			{
			k.idx = (k + 1)/2;
			if (p[k.idx] != 0)
				{
				# using a squared rule on indexes ?
				k2.idx = (k * k + 1)/2;

				# cat("\n", " k = ", k, "... (k+1/2) = ", k.idx, "... (k * k + 1)/2 = ", k2.idx, "\n");

				p[ seq(k2.idx, q, by = k) ] = 0;
				}
			}
		}

	p = p[p > 0];

	if(first)
		{
		p[1:n];
		} else {
				p[p < n];
				}
	}

#' prime.findPrimes
#'
#' @param n
#' @param first
#'
#' @return
#' @export
#'
#' @examples
prime.findPrimes = function(n, first=FALSE)
	{
	# this duplicates the basic logic of pracma::primes with base-6 limits
	# doesn't use seq but while ... slower than primary function 'prime.findFrom'
	# the pracma function is looping of odds and then seq 3 ...
	gn = n;
	# https://www.khanacademy.org/computing/computer-science/cryptography/comp-number-theory/v/prime-number-theorem-the-density-of-primes
	if(first) { gn = ceiling( n * log(n) + n * log(log(n)) ); }

	# primes
	p = c(2,3);
	# 6k + 1
	p1 = seq(7, gn, by = 6);
	# 6k - 1
	p5 = seq(5, gn, by = 6);

	# candidates
	pc = sort( unique( c(p1,p5) ) ); # unique should be un-necessary


	while(sum(pc) > length(pc))
		{
		nidx = which(pc > 1)[1];
		np = pc[ nidx ]; # next is prime


		w = which(pc %% np == 0);
		if(length(w) > 0)
			{
			pc[w] = 1; # set equal to one for next iteration
			}
		p = c(p, np);
		}

	if(first)
		{
		p[1:n];
		} else {
				p[p < n];
				}
	}


#' prime.isPrime
#'
#' @param x
#'
#' @return
#' @export
# is.prime 
prime.isPrime = function(x)
	{
	factors = prime.factors(x);
	if(length(factors) == 1) { return (TRUE); }
	return (FALSE);
	}


# for(i in 1:100) { cat(i," :: ",is.prime(i), "\n"); }
is.prime = function(x)
	{
	# https://www.programiz.com/cpp-programming/examples/prime-number
	if(x == 0 || x == 1) { return(FALSE);}
	if(x == 2 || x == 3) { return(TRUE); }
	for(i in 2:as.integer(sqrt(x)))
		{
		if (x %% i == 0) { return(FALSE); }
		}
	TRUE;
	}

# https://dirk.eddelbuettel.com/papers/useR2019_rcpp_tutorial.pdf
is.prime.range = function(xmin, xmax)
	{
	p = prime.pracmaPrimes(xmax, first=FALSE);
	
	p = p[(p >= xmin)];
	p = p[(p <= xmax)];
	
	p;	
	}
	
	
# https://onlinegdb.com/qgvpmcpRr
# https://onlinegdb.com/5HTxwqrS6
# C++ variant ... 
gcd.lcm = function(x,y)
	{
	a=x;
	b=y;
	while (b != 0)
        {
		t = b;
		b = a %% b;
		a = t;
        }
	list("gcd"=a, "lcm"=(x*y)/a);
	}
	

#' prime.factors
#'
#' @param x
#'
#' @return
#' @export
prime.factors = function(x)
	{
	x = as.integer(x);
	p = prime.pracmaPrimes ( ceiling(sqrt(x)) );  # squared rule of primes
	remainder = x %% p;
	zeroes = which(remainder == 0);
	if(length(zeroes) == 0)
		{
		x = setAttribute("prime", TRUE, x);
		return(x);
		}

	factors = c();
	for(q in p[zeroes])
		{
		# divide out as many times as possible the qiven prime
		while (x %% q == 0)
			{
			factors = c(factors, q);
			x = x/q;
			}
		}
	# anything left? ... since we truncated using sqrt(x), maybe one prime ...
	if(x > 1) { factors = c(factors, x); }
	factors;
	}



#' prime.lcm
#'
#' @param a
#' @param b
#'
#' @return
#' @export
#'
#' @examples
prime.lcm = function(a, b)
	{
  # https://www.mathkplus.com/I-Math/Practice/Factoring.aspx
  # In Factorization, the Least Common Multiple (LCM) is also known as Least Common Denominator (LCD).
  # these are univariate
  # The following relation is always true:
  # n * m = gcd(n, m) * lcm(n, m) # ?pracma::Lcm
	gcf = as.numeric( prime.gcf(a,b) );

	a * b / gcf;
	}


#' prime.gcf
#'
#' @param a
#' @param b
#'
#' @return
#' @export
#'
#' @examples
prime.gcf = function(a, b)
	{
  # In Factorization, the Greatest Common Factor (GCF) is also known as Greatest Common Divisor (GCD).
  # a = 108; b = 126;
  # prime.gcf(108,126);
	a.factors = prime.factors(a);
	b.factors = prime.factors(b);

	gcfs = prime.intersect(a.factors, b.factors);

	gcf = prod(gcfs);
	gcf = setAttribute("factors", gcfs, gcf);
		# prime.factors(gcf) would also return this

	gcf;
	}



#' prime.intersect
#'
#' @param x
#' @param y
#' @param duplicates
#'
#' @return
#' @export
#'
#' @examples
prime.intersect = function(x, y, duplicates=TRUE)
	{
	xyi = intersect(x,y);
	if(!duplicates) { return (xyi); }

	res = c();
	for(xy in xyi)
		{
		y.xy = which(y == xy);  ny.xy = length(y.xy);
		x.xy = which(x == xy);	nx.xy = length(x.xy);

		min.xy = min(ny.xy, nx.xy);

		res = c(res, rep(xy, min.xy) );
		}

	res;
	}


# library(microbenchmark);
# res = microbenchmark( "pracma^3" = pracma::primes(10^3), "pracma^4" = pracma::primes(10^4), "hv::findFrom^3" = prime.findFrom(10^3),  "hv::findFrom^4" = prime.findFrom(10^4), "hv::findPrimes^3" = prime.findPrimes(10^3),  "hv::findPrimes^4" = prime.findPrimes(10^4), "hv::pracmaPrimes^3" = prime.pracmaPrimes(10^3),  "hv::pracmaPrimes^4" = prime.pracmaPrimes(10^4),  times=50);
# res;
