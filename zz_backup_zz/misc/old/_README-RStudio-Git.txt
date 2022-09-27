

# RStudio and Github ... Windows 10 
# Rmarkdown with long functions ... 
# Learn how to integrate Knit ... Rnotebooks, and output Latex PDF within RStudio
# _git_ ... where code lives ... C:\
# _R-data_ ... where data lives ... R:\


# RGui ... RStudio ... 64-bit ... 
# Git Bash ...

# source ... custom external R files ... functions-file.R ... functions-imdb.R ... functions-matrix.R

# github ... primary email:  monte.shaffer@gmail.com with username:  MonteShaffer

# ssh key ... 
ssh-keygen -t rsa -b 4096 -C "monte.shaffer@gmail.com"		
# (/c/Users/Alexander Nevsky/.ssh/id_rsa)

ssh-agent -s

ssh -vT git@github.com
ssh -T git@github.com

# https://github.com/MonteShaffer/WSU_STATS419_FALL2020.git


git clone https://github.com/MonteShaffer/WSU_STATS419_FALL2020.git

cd WS # ... hit tab to autocomplete

# https://git-scm.com/docs/git-add
# git add .
# git add --all
git add -A

git status

git commit -m "Adding personality-raw dataset"

git push -u origin master

git remote set-url origin git@github.com:MonteShaffer/WSU_STATS419_FALL2020.git

git push -u origin master 

mkdir directornynakl # make directory
touch fjidljasdkl    # create empty file
	
## make changes to the folder ...
	git remote set-url origin git@github.com:MonteShaffer/WSU_STATS419_FALL2020.git
	
	
 git add .
 git status
 git commit -m "video editing updates"
 git push -u origin master 

# Rnotebook - sandbox to play
# Rmarkdown - writeup for submission (final product)

# Latex ... environment ...
# Miktex
	
################################
	
## make a clone of the "instructor's" github and store (read-only) ... you could do a branch off his master to create your first repository, but that is increasing the level of complexity ...
## ABOVE, change the project from  MonteShaffer/WSU_STATS419_FALL2020 to yours.  Below, actually clone the instructors ...

mkdir MonteShaffer
cd MonteShaffer
git clone https://github.com/MonteShaffer/WSU_STATS419_FALL2020.git



#########################
# Updating all R and Latex packages
# https://yihui.org/tinytex/r/#debugging heres a link I found helpful to update all R and Latex files.
# update.packages(ask = FALSE, checkBuilt = TRUE);
# install.packages("MASS", type="source");
# install.packages("mgcv", type="source");
# install.packages("nlme", type="source");
# install.packages("survival", type="source");



library(devtools);
detach(package:humanVerseWSU);
install_github("MonteShaffer/humanVerseWSU/humanVerseWSU"); 
# Choose (3) None to minimize headaches ....
library(humanVerseWSU);
packageVersion("humanVerseWSU");

# install.packages("ps", type="source");
# install.packages("rlang", type="source");
# install.packages("clipr", type="source");
# install.packages("callr", type="source");
# install.packages("psych", type="source");

# unlink("C:/Users/Alexander Nevsky/Documents/R/win-library/4.0/00LOCK-ps", recursive = TRUE)




git clone https://github.com/MonteShaffer/humanVerseWSU.git




git remote set-url origin git@github.com:MonteShaffer/humanVerseWSU.git
 git add .
 git status
 git commit -m "added personality.json files"
 git push -u origin master 
 
 
 
 
 
  git push -u origin HEAD:main
 
 
 https://s3.amazonaws.com/rstudio-ide-build/desktop/windows/RStudio-1.4.781.exe
 
library(devtools);
install_github("MonteShaffer/humanVerseWSU/humanVerseWSU");
library(humanVerseWSU);



personality.raw = read.csv( "C:/_git_/MonteShaffer/humanVerseWSU/humanVerseWSU/inst/extdata/personality-raw.txt", header=TRUE, sep="|");

saveRDS(personality.raw, file="C:/_git_/MonteShaffer/humanVerseWSU/humanVerseWSU/inst/extdata/personality-raw.rds");


git clone https://github.com/4uchris4/STATS419_FALL2020.git



# I have to compile the documentation before the package update will fully take ...

# Open RStudio
# setwd("C:/_git_/MonteShaffer/humanVerseWSU/humanVerseWSU")
# Open Project humanVerseWSU.Rproj
# Build Tab  ... Check ... fix errors

# devtools::document();
# devtools::check();
# devtools::build();  # build source package
# devtools::build(binary = TRUE, args = c('--preclean')); # build binary

# devtools::document(roclets = c('rd', 'collate', 'namespace', 'vignette'))

# devtools::check(document=FALSE);

# Remember DESCRIPTION to add libraries ...
# load_all()

# library(devtools);
# detach(package:humanVerseWSU);
# install_github("MonteShaffer/humanVerseWSU/humanVerseWSU"); 
# Choose (3) None to minimize headaches ....
# library(humanVerseWSU);
https://www.datacamp.com/community/tutorials/pipe-r-tutorial


https://www.thisprogrammingthing.com/2013/fixing-the-this-exceeds-githubs-file-size-limit-of-100-mb-error/
https://stackoverflow.com/questions/38768454/repository-size-limits-for-github-com


library(devtools);
detach(package:humanVerseWSU);
install_github("MonteShaffer/humanVerseWSU/humanVerseWSU"); 
library(humanVerseWSU);
packageVersion("humanVerseWSU");


packageVersion("imdb");

#####
library(devtools);
path.github = "https://raw.githubusercontent.com/MonteShaffer/humanVerseWSU/master/";
source_url( paste0(path.github, "humanVerseWSU/R/functions-dataframe.R") );
#####

https://github.com/MonteShaffer/imdb.git
git remote set-url origin git@github.com:MonteShaffer/WSU_STATS419_FALL2020.git


git clone https://github.com/MonteShaffer/imdb.git


git remote set-url origin git@github.com:MonteShaffer/imdb.git
 git add .
 git status
 git commit -m "loadIMDB data correct package"
 git push -u origin HEAD:main
 
 
 
  git push -u origin master 
 
 https://git-scm.com/book/en/v2/Git-Basics-Viewing-the-Commit-History
 
 git log
 git log -p -2
 
 
 error: failed to push some refs to 'github.com:MonteShaffer/imdb.git'

 https://stackoverflow.com/questions/24114676/git-error-failed-to-push-some-refs-to-remote
 https://stackoverflow.com/questions/4181861/message-src-refspec-master-does-not-match-any-when-pushing-commits-in-git
 
  git show-ref
	git push -u origin HEAD:main

git reset --mixed origin/master



 personality.raw$md5_email = md5(personality.raw$md5_email, 9);
 
 saveRDS(personality.raw, file="C:/_git_/MonteShaffer/humanVerseWSU/humanVerseWSU/inst/extdata/personality-raw.rds");
 
 write.table( personality.raw , file="C:/_git_/MonteShaffer/humanVerseWSU/humanVerseWSU/inst/extdata/personality-raw.txt", quote=FALSE, col.names=TRUE, row.names=FALSE, sep="|");
 
 
 
 https://stackoverflow.com/questions/45481435/installation-of-package-rcurl-had-non-zero-exit-status
 https://github.com/Bioconductor/BiocManager/issues/19
 
 install.packages("curl", type="source");
 install.packages("RCurl");  # can't do type = source ...
 
 https://stackoverflow.com/questions/39450146/cannot-install-rcurl-rcurl-h423-fatal-error-curl-curl-h-no-such-file-or
 
 install.packages("BiocManager");
 library(BiocManager);
 BiocManager::install();
 
 
 
 
 
 https://stackoverflow.com/questions/2287616/controlling-number-of-decimal-digits-in-print-output-in-r
 
 As mentioned by e3bo, you can use multiple-precision floating point numbers using the Rmpfr package.

mpfr("3.141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825")
These are slower and more memory intensive to use than regular (double precision) numeric vectors, but can be useful if you have a poorly conditioned problem or unstable algorithm.
