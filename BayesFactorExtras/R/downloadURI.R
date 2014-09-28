##' Creates HTML to insert Rdata file into web page
##' 
##' Create HTML to insert Rdata file into web page. This is convenient for use with 
##' \code{brew} or \code{knitr}.
##' @title Download R data files via HTML
##' @param list R objects to put in the Rdata file
##' @param filename A name to give the downloaded file (without extension)
##' @param textHTML text to link in the web page
##' @param fileext an extension to give the file
##' @param envir the environment to search for the objects in \code{list}
##' @return This function returns NULL invisibly.
##' @export
##' @keywords misc
##' @author Richard D. Morey (\email{richarddmorey@@gmail.com})
##' @references See \url{http://bayesfactor.blogspot.nl/2014/09/embedding-rdata-files-in-rmarkdown.html}
createDownloadURI = function(list, filename = stop("'filename' must be specified"), textHTML = "Click here to download the data.", fileext = "RData", envir = parent.frame())
{
  require(base64enc,quietly = TRUE)
  divname = paste(sample(LETTERS),collapse="")
  tf = tempfile(pattern=filename, fileext = fileext)
  save(list = list, file = tf, envir = envir)
  filenameWithExt = paste(filename,fileext,sep=".")
  
  uri = dataURI(file = tf, mime = "application/octet-stream", encoding = "base64")
  
  scriptTemplate = system.file("etc/html/downloadURI.html", package = "BayesFactorExtras")
  scriptCode = readChar(scriptTemplate, file.info(scriptTemplate)$size)
  
  # Substitute in the variables
  scriptCode = gsub("\\$DIVNAME", divname, scriptCode)
  scriptCode = gsub("\\$FILENAME", filenameWithExt, scriptCode)
  scriptCode = gsub("\\$TEXTHTML", textHTML, scriptCode)
  scriptCode = gsub("\\$URITEXT", uri, scriptCode)
  
  cat(scriptCode)
  
  invisible(NULL)
}
