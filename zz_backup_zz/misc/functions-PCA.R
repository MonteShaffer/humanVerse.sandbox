library(REdaS);
library(psych);
library(Matrix);
library(nFactors);



likert.reverseScore = function(X, reverse, points=7)
	{
  if(!is.integer(reverse)) { reverse = which(names(X) == reverse); }
  for(column in reverse)
    {
    X[,column] = points - X[,column] + 1;
    }
  X;
  }

#' performKMOTest
#'
#' @param X Either the numeric dataframe or the correlation matrix of the dataframe
#' @return list with $KMO and $msg
#' @export
#'
#' @examples
#' library(datasets);
#' data(iris);
#' head(iris);
#' performKMOTest(iris[,1:4]);
#'
#' setosa = subsetDataFrame( iris, "Species", "==", "setosa" );
#' performKMOTest( setosa[,1:4] );
performKMOTest = function(X)
	{
  # 

  myTest = REdaS::KMOS(X);

  my.kmo = myTest$KMO;


if(my.kmo >= 0.90)
  {
  msg = ("marvelous!");
  } else if(my.kmo >= 0.80)
    {
    msg = ("meritorious!");
    }  else if(my.kmo >= 0.70)
        {
        msg = ("middling!");
        } else if(my.kmo >= 0.60)
            {
            msg = ("mediocre!");
            }  else if(my.kmo >= 0.50)
                {
                msg = ("miserable!");
                } else {
                        msg = ("mayhem!");
                        msg = paste0(msg, "\n", "Oh snap!");
                        msg = paste0(msg, "\n", "Kaiser-Meyer-Olkin (KMO) Test is a measure of how suited your data is for Factor Analysis. The test measures sampling adequacy for each variable in the model and for the complete model. The statistic is a measure of the proportion of variance among variables that might be common variance. The lower the proportion, the more suited your data is to Factor Analysis. <https://www.statisticshowto.com/kaiser-meyer-olkin/>");
                        }

  list("KMO" = my.kmo, "msg" = msg);
  }



#' performBartlettSphericityTest
#'
#' @param X Either the numeric dataframe or the correlation matrix of the dataframe
#' @param n.obs If null, I assume X is a dataframe, not a correlation matrix
#' @param alpha Alpha level (0.05) to report findings as $msg
#'
#' @return list with $pvalue and $msg
#' @export
#'
#' @examples
#' library(datasets);
#' data(iris);
#' head(iris);
#' performBartlettSphericityTest(iris[,1:4]);
#' performBartlettSphericityTest(iris[,1:4], alpha=0.01);
#' performBartlettSphericityTest(cor(iris[,1:4]), n.obs = nrow(iris));
performBartlettSphericityTest = function(X, n.obs = NULL, alpha = 0.05)
	{
  # <https://www.ibm.com/support/knowledgecenter/SSLVMB_23.0.0/spss/tutorials/fac_telco_kmo_01.html>
  if(is.null(n.obs))
    {
    # if X is data, it is now in correlation form X
    n.obs = nrow(X);
    X = stats::cor(X);  # square ...
    }
  myTest = psych::cortest.bartlett(X, n = n.obs);

  pvalue = myTest$`p.value`;

  if(pvalue < alpha)
    {
    msg = (paste0("Bartlett's test of sphericity ... pvalue < alpha ... ", pvalue , " < ", alpha, " ... \n CONCLUSION: we believe this data is likely suitable for factor analysis or PCA"));
    } else {
          msg = "Oh snap!";
          msg = paste0(msg,"\n", "To put this in layman's terms, the  variables in our dataset are fairly uncorrelated so a data reduction technique like PCA or factor analysis would have a hard time compressing these variables into linear combinations that are able to capture significant variance present in the data. <https://www.statology.org/bartletts-test-of-sphericity/>");
          }

  list("pvalue" = pvalue, "msg" = msg);
  }


# http://www.mshaffer.com/arizona/pdf/Joireman(2012).pdf
# pg 6, bottom right ...
performMultiCollinearityTest = function(X, min.val = .00001)
  {
  X.corr.det = Matrix::det(cor(X));
  msg = "The determinant of the correlation matrix was {det.cor.X} \n and the recommended cutoff value is {min.val}, \n ";
  msg = str_replace("{det.cor.X}", X.corr.det, msg);
  msg = str_replace("{min.val}",   min.val, msg);
  if(X.corr.det > min.val) 
    {
    msg = paste0(msg, " ruling out multicollinearity.");
    } else  {
            msg = paste0(msg, " suggesting possible multicollinearity.");
            }
  list("X.corr.det" = X.corr.det, "min.val" = min.val, "msg" = msg);
  }



howManyFactorsToSelect = function(Xs, plotMe      = TRUE,
                                    rotation      = "varimax", 
                                    scores        = "regression",
                                    fa.fm         = "ml",
                                    fa.fa         = "fa", 
                                    fa.iter       = 50,
                                    fa.error.bars = FALSE, 
                                    fa.se.bars    = FALSE,
                                    max.factors   = 12,
                                    eigen.cutoff  = 1,
                                    VAF.cutoff    = 80/100,
                                    alpha         = 0.05)
  {
  res = list();
  res$methods = list();
  res$calls = list();
  choices = c();
  
  ## eigenvalues
  Xs.PCA = stats::prcomp(Xs);
  Xs.eigen = Xs.PCA$sdev^2;
    if(plotMe)
      {
      plot(Xs.eigen, type="b");
        graphics::abline(h=1, col="green");
      }
  
    idxs = which(Xs.eigen > eigen.cutoff);
  res$methods$eigen = idxs;
  
  choices = c(choices, idxs);
  
  
  ## cumulative variance accounted for 
  cumVAF = summary(Xs.PCA)$importance[3,];
    idx = which(cumVAF > VAF.cutoff)[1];
    idxs = 1:idx;
  res$methods$VAF = idxs;
    
  choices = c(choices, idxs);

  ## VSS 
  res$calls$vss = psych::vss(X, n = max.factors, rotate=rotate, plot=showPlots);
  
    
  res$calls$vss.dataframe = as.data.frame(cbind(1:max.factors, res$calls$vss$vss.stats[,c(1:3)]) );
      colnames(res$calls$vss.dataframe) = c("Factors","dof","chisq","pvalue");
                              # negative "degrees of freedom meant something went wrong"
  res$calls$vss.dataframe = res$calls$vss.dataframe[res$calls$vss.dataframe$dof > 0, ];

  if(nrow(res$calls$vss.dataframe) > 0)
    {
    res$calls$vss.dataframe$isFactorChoiceValid = performSimpleChiSquaredTest(
                                                          res$calls$vss.dataframe$chisq,
                                                          res$calls$vss.dataframe$dof, 
                                                          alpha=alpha)$truth;
    }

  zeroIsh( res$calls$vss.dataframe );  
    
  res$methods$vss = n.vsss = getIndexOfDataFrameRows(res$calls$vss.dataframe,"isFactorChoiceValid",TRUE);
  if(!anyNA(n.vss)) { choices = c(choices, n.vss); }
  
  res$methods$vss.c1 = which.max(res$calls$vss$vss.stats$cfit.1);
    choices = c(choices, res$methods$vss.c1);
  res$methods$vss.c2 = which.max(res$calls$vss$vss.stats$cfit.2);
    choices = c(choices, res$methods$vss.c2);
  res$methods$vss.map = which.min(res$calls$vss$map);
    choices = c(choices, res$methods$vss.c2);
  res$methods$vss.BIC = which.min(res$calls$vss$vss.stats$BIC);
    choices = c(choices, res$methods$vss.BIC);
  res$methods$vss.SABIC = which.min(res$calls$vss$vss.stats$SABIC);
    choices = c(choices, res$methods$vss.SABIC);
  
  
  if(showPlots)
    {
    nFactors::plotuScree(Xs.eigen);
    graphics::abline(h = 1, col="blue");
    }
  nResults = nFactors::nScree(eig = Xs.eigen,
              aparallel = nFactors::parallel(
                              subject = nrow(Xs),
                              var = ncol(Xs) )$eigen$qevpea);

  nResults;
  
  res$methods$nfactors.noc       = nResults$Components$noc;
  res$methods$nfactors.naf       = nResults$Components$naf;
  res$methods$nfactors.nparallel = nResults$Components$nparallel;
  res$methods$nfactors.nkaiser   = nResults$Components$nkaiser;
  
  choices = c(choices, nResults$Components$noc);       # optimal coordinates
  choices = c(choices, nResults$Components$naf);       # acceleration factor
  choices = c(choices, nResults$Components$nparallel); # parallel analysis
  choices = c(choices, nResults$Components$nkaiser);   # eigenvalues
  
  if(showPlots)
    {
    nFactors::plotnScree(nResults, main="Component Retention Analysis");
    }
  
  
  strong = FALSE;  # strongly recommend
  if(nResults$Components$noc == nResults$Components$nparallel) 
    { 
    strong = nResults$Components$noc; 
    }
  res$strong = strong;
  if(strong)
    {
    # extra two votes
    choices = c(choices, strong);
    choices = c(choices, strong);
    }
  
  
  fa = psych::fa.parallel(X, 
                          fm = fa.fm, 
                          fa = fa.fa, 
                          n.iter = fa.iter,
                          error.bars = fa.error.bars,
                          se.bars=fa.se.bars);

  res$methods$parallel.nfact = fa$nfact;
  res$methods$parallel.ncomp = fa$ncomp;
  choices = c(choices, fa$nfact, fa$ncomp);
  
  choices[choices >= max.factors] = NA;
  choices = na.omit(choices);
  
  
  myTable = as.data.frame( table(choices), row.names=NULL );
    colnames(myTable) = c("Factor", "vote.count");
  myTable;
  
  res$table = myTable;
  
  votes = whichMaxFreq(choices);
  for(i in 1:length(votes))
    {
    print( paste0("A ",votes[i], "-Factor solution has the most votes!") );
    }
  
  res$votes = votes;
  
  if(!isFALSE(strong))
    {
    print("Due to Optimal Coordinantes and Parallel Analysis Agreement,");
    print(  paste0("A ",strong, "-Factor solution is *strongly* recommended!") );
    }
  
  res$choices = choices;
  
  res;
  }


perform.EFA = function(X, numberFactors = NULL, 
                          plotMe        = TRUE, 
                          scaleMe       = TRUE,
                          rotation      = "varimax", 
                          scores        = "regression",
                          fa.fm         = "ml",
                          fa.fa         = "fa", 
                          fa.iter       = 50,
                          fa.error.bars = FALSE, 
                          fa.se.bars    = FALSE,
                          max.factors   = 12,
                          eigen.cutoff  = 1,
                          VAF.cutoff    = 80/100,
                          alpha         = 0.05
  
                      )
  {
  res = list();
    Xs = X; 
    if(scaleMe) { Xs = scale(X); }
  res$KMO = performKMOTest(Xs);
  res$Bartlett = performBartlettSphericityTest(Xs);
  res$multicollinearity = performMultiCollinearityTest(Xs);
  
  res$howManyFactors = numberFactors;
    if(is.null(numberFactors)) 
      { 
      res$howManyFactors = howManyFactorsToSelect(Xs, plotMe = plotMe,
                                                      rotation = rotation, 
                                                      scores = scores,
                                                      fa.fm = fa.fm,
                                                      fa.fa = fa.fa, 
                                                      fa.iter = fa.iter,
                                                      fa.error.bars = fa.error.bars, 
                                                      fa.se.bars = fa.se.bars,
                                                      max.factors = max.factors,
                                                      eigen.cutoff = eigen.cutoff,
                                                      VAF.cutoff = VAF.cutoff,
                                                      alpha = alpha);
      numberFactors = res$howManyFactors$votes;
      }
  res$numberFactors = numberFactors;
  
  
  Xs.EFA = psych::fa(Xs, 
                      numberFactors,
                      rotate = rotate,
                      scores = scores,
                      fm = fa.fm);
  
  res$CFI = 1-((Xs.EFA$STATISTIC - Xs.EFA$dof)/(Xs.EFA$null.chisq - Xs.EFA$null.dof));
  
  Xs.EFA;
  
  res$Xs.EFA = Xs.EFA;
  
  res;
  }

  
  
  
  
performSimpleChiSquaredTest = function(chi, df, alpha=0.05)
  {
  # performSimpleChiSquaredTest(1944.5,36);
  n.chi = length(chi);
  n.df = length(df);
  if(n.chi != n.df)
    {
    warning("Something wrong in performSimpleChiSquaredTest ... chi and df are of different lengths");
    return (NA);
    }

  truth = c();
  pvalues = c();

  for(i in 1:n.chi)
    {
    pvalue = 1 - stats::pchisq(chi[i],df[i]);
    pvalues[i] = pvalue;

    truth[i] = pvalue < alpha;
    }

  list("alpha" = alpha, "pvalue" = pvalue, "truth" = truth);
  }