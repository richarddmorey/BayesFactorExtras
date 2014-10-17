# compares whether two BayesFactorBF objects are identical *except* their time and code.
identicalBF <- function(x, y, ...) {
	x@bayesFactor$time <- NA
	x@bayesFactor$code <- NA
	y@bayesFactor$time <- NA
	y@bayesFactor$code <- NA
	identical(x, y, ...)
}