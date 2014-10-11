.onAttach<- function(libname, pkgname){
  setOptions()
}

#'options() for package BayesFactorExtras
#'
#'Options that can be set for the BayesFactorExtras package
#'
#'The BayesFactorExtras package has options that can be set to globally 
#'change the behavior of the functions in the package. These options can be
#'changed using \code{\link[base]{options}}().
#'
#'\describe{
#'\item{\code{BFEknitrDownload}}{If \code{TRUE} (default), a link to download the Bayes factor object is provided. This object contains the analysis and the data.} 
#'
#'@name options-BayesFactorExtras
#'@seealso \code{\link[base]{options}}

NULL

setOptions <- function(){
  
  if(is.null(options()$BFEknitrDownload)) options(BFEknitrDownload = TRUE)

}

