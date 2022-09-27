




#
# readBin(remote, "raw", 30528)
#
# raw = readBin(remote, "raw", 30999)


# iconv(readBin(content, character()), from = encoding, to = "UTF-8")



# remote = "https://www.mshaffer.com/";
# headers = http.header(remote);
#
# str = readChar("https://www.mshaffer.com/", 30528);


# (libcurlVersion())
# function.exists ...
# capabilities("libcurl")
# url("https://www.mshaffer.com");
#
#  r = httr::GET( htmlurl );
#                         if(r$status_code == 200)
#                           {
#                           raw.binary = httr::content(r, "raw");
#                           url.encoding = stringi::stri_enc_detect(raw.binary)[[1]]$Encoding[1];
#                           raw.html = httr::content(r, "text", encoding = url.encoding);
#                           #raw.binary = httr::content(r, "raw");
#                           #writeBin(raw.binary, file(htmlfile, encoding=encoding) );
#                           return(raw.html);
#                           }
#                         return( httr::http_status(r) );

#
# readStringFromFile = function(file, method="readLines", n=-1L)
#   {
#   # old school... fopen, fread, fclose ...
#   # base::readChar( file(htmlfile, encoding=encoding), nchars=9724129);
#   }

#' grabHTML
#'
#' This grabs an HTML file (or any web TXT file) and caches locally.
#' There needs to be a global variable `local.data.path` assigned so
#' this function understands it is running in a local environment where
#' it can actually save files.  As opposed to `github.data.path` I can't
#' source this file in a remote repository if I can't store files there.
#'
#' @param htmlfile If cached, we can return this file.  If not, we can store
#'  the url contents to a file.  (rvest doesn't collect the original data
#'  source to possibly do offline re-parsing.  bad data provenance.)
#'
#' @param htmlurl Using RCurl, grab the raw contents off the web.
#'
#' @param return.raw If TRUE, will return the raw string
#' @param verbose If TRUE, will provide some verbosity
#' @param encoding by default UTF-8 ... Windows is ANSI_X3.4-1986 or RStudio default is ISO-8859-1
#'
#' @return
#' @export
# grabHTML = function(htmlfile, htmlurl, verbose=TRUE, return.raw=TRUE, encoding="UTF-8")
#   {
#   if( file.exists(htmlfile) )
#       {
#       if(verbose)
#         {
#         print( paste0("grabHTML() ... from cache ... ", htmlfile) );
#         }
#       if(return.raw)
#         {
#         #raw.html = base::readChar( file(htmlfile, encoding=encoding), nchars=9724129);
#
#         raw.html = base::readChar( htmlfile, nchars=9724129);
#
#         # raw.html = base::readLines( file(htmlfile, encoding=encoding) );
#         return(raw.html);
#         }
#       } else  {  # file does not exist
#               if( exists("local.data.path") ) # is this available from the global scope?
# 		            {
#                 # r = GET( "https://en.wikipedia.org/wiki/Ober%C3%A1" );
#                 r = httr::GET( htmlurl );
#                 if(r$status_code == 200)
#                   {
#                   raw.binary = httr::content(r, "raw");
#                   url.encoding = stringi::stri_enc_detect(raw.binary)[[1]]$Encoding[1];
#                   raw.html = httr::content(r, "text", encoding = url.encoding);
#                   #raw.binary = httr::content(r, "raw");
#                   #writeBin(raw.binary, file(htmlfile, encoding=encoding) );
#                   storeToFile(raw.html, htmlfile, encoding=encoding);
#                   if(return.raw)
#                     {
#                     return(raw.html);
#                     }
#                   }
#                 return( httr::http_status(r) );
#                 #encoding.guess = stringi::stri_enc_detect(content(r, "raw"))[[1]]$Encoding[1];
#                 # r$status_code
#                 # storeToFile
#                 # utils::download.file( url(htmlurl, encoding=encoding), htmlfile, method="curl");
#                 } else  {
#                         #raw.html = curl::curl( htmlurl );
#                         r = httr::GET( htmlurl );
#                         if(r$status_code == 200)
#                           {
#                           raw.binary = httr::content(r, "raw");
#                           url.encoding = stringi::stri_enc_detect(raw.binary)[[1]]$Encoding[1];
#                           raw.html = httr::content(r, "text", encoding = url.encoding);
#                           #raw.binary = httr::content(r, "raw");
#                           #writeBin(raw.binary, file(htmlfile, encoding=encoding) );
#                           return(raw.html);
#                           }
#                         return( httr::http_status(r) );
#                         }
#               }
#
#   # rawHTML = RCurl::getURL( htmlurl );
#   # rawHTML = RCurl::getURLContent ( htmlurl );
#   # rawHTML = curl::curl( htmlurl );
#   # storeToFile(rawHTML,htmlfile);
#
#   }
#
#


