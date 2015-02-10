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
#'\item{\code{BFEknitrDownload}}{If \code{TRUE} 
#'(default), a link to download the Bayes factor object is 
#'provided. This object contains the analysis and the data.}
#'
#'\item{\code{BFEmcmcPlotType}}{Should be 'svg' (default),
#''png', or 'jpeg'. This controls the type of image that is used
#' when creating mcmc plots using knitr.}
#'\item{\code{BFEmcmcConvergence}}{Calculate and report convergence
#'statistics when creating mcmc plots using knitr?}
#'\item{\code{BFEmcmcPlotDims}}{A vector of length 2 indicating the 
#'width and height (in inches) of mcmc plots printed by knitr in non-interactive plot mode}
#'#'\item{\code{BFEmcmcPlotDimsInteractive}}{A vector of length 2 indicating the 
#'width and height (in pixels) of mcmc plots printed by knitr in interactive mode.}
#'}
#'
#'@name options-BayesFactorExtras
#'@seealso \code{\link[base]{options}}

NULL

setOptions <- function(){
  
  if(is.null(options()$BFEknitrDownload)) options(BFEknitrDownload = TRUE)
  if(is.null(options()$BFEmcmcPlotType)) options(BFEmcmcPlotType = 'svg')
  if(is.null(options()$BFEmcmcConvergence)) options(BFEmcmcConvergence = FALSE)
  if(is.null(options()$BFEmcmcPlotDims)) options(BFEmcmcPlotDims = c(8,5))
  if(is.null(options()$BFEmcmcPlotDimsInteractive)) options(BFEmcmcPlotDimsInteractive = c(800,400))
  
}

