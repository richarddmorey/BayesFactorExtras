#'Prints the version information for the BayesFactorExtras package
#'
#'Prints the version, revision, and date information for the BayesFactorExtras package
#'
#'This function prints the version and revision information for the BayesFactorExtras
#'package.
#'
#'@param print if \code{TRUE}, print version information to the console
#'@return \code{BFInfo} returns a character string containing the version and
#'  revision number of the package..
#'@author Richard D. Morey (\email{richarddmorey@@gmail.com})
#'@keywords misc
#'@export
BFEInfo <- function(print=TRUE)
{
  if(print){
    cat("Package BayesFactorExtras\n")
    cat(packageDescription("BayesFactorExtras")$Version,"\n")
  }
  retStr = paste(packageDescription("BayesFactorExtras")$Version)
  invisible(retStr)
} 
