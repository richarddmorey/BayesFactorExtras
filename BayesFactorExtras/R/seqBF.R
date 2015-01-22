#' @title Compute a sequential Bayes factor object
#' @details Compute a sequential Bayes factor object
#' @param BFobject A BayesFactor object
#' @param ... User parameters which override the settings of the original \code{BFobject}
#' @param min.n Smallest number of participants for which the first BF should be computed. In a one-sample t test / paired  test, this is the overall sample size. In two-sample t tests and ANOVAs, this is the minimum size for each group. #
#' @param var.id The column that defines which rows belong to one person/object of observation. This is relevant for repeated measurements, as we want to sample complete observations. If this parameter is left as NULL, the function tries to guess the relevant identifying variable.
#' @param step Step size with which the number of participants is increased
#' @param verbose Print diagnostic information?

## Not implemented yet:
# @param sampling "as.is": Sample consecutively from the data, in the order the data dropped in. This can be done with any BF, but is problematic if for example the first 20 observations are from the experimental group only (then the BF cannot be computed). "balanced": Sample from each level of a factor. Only works for t-tests so far.


#' @author Felix Schönbrodt

#' @export
#' @import BayesFactor
#' @import dplyr

#' @examples
#' # Paired/ one-sample t test
#' ttest.p <- ttestBF(x = sleep$extra[sleep$group==1], y = sleep$extra[sleep$group==2], paired=TRUE)
#' ttest.p.seq <- seqBF(ttest.p, min.n=5)
#' plot(ttest.p.seq)
#' 
#' # AnovaBF
#' anova1 <- anovaBF(extra ~ group + ID, data = sleep, whichRandom = "ID", progress=FALSE) 
#' anova1.seq <- seqBF(anova1, min.n=5)
#' plot(anova1.seq)
#'
#' data(puzzles)
#' (anova2 <- anovaBF(RT ~ shape*color + ID, data = puzzles, whichRandom = "ID", progress=FALSE))
#' (anova2.seq <- seqBF(anova2, min.n=5))
#' plot(anova2.seq)


seqBF <- function(BFobject, min.n=10, step=1, var.id=NULL, verbose=TRUE, ...) {

	dat <- BFobject@data	# stores the full data set
	BFtype <- class(BFobject@numerator[[1]])
	random <- FALSE	# flag whether data has random factors or not.

	# Determine the number of observations if var.id is given by user
	if (!is.null(var.id)) {
		count = dat %>% group_by_(var.id) %>% summarise(n=n())
		max.n = nrow(count)
	}
	
	# var.id is NULL? Try to guess the grouping variable
	if (is.null(var.id)) {
		if (verbose==TRUE) print("Trying to guess the grouping variable ...")
		switch(BFtype,
			
			# one sample t-test: each row is one observation
			BFoneSample = {
				max.n = nrow(dat)
				if (verbose==TRUE) print("No grouping factor detected.")
				},
			BFindepSample = {
				max.n = nrow(dat)
				if (verbose==TRUE) print("No grouping factor detected.")
				},
			BFlinearModel = {
				# differentiate between anovaBF and lmBF: get RHS of denominator formula; ==1: lmBF; !=1: anovaBF
				if (length(all.vars(as.formula(BFobject@denominator@identifier$formula))) == 1) {
					# no random factor / no repeated measurement
					max.n = nrow(dat)
					if (verbose==TRUE) print("No grouping factor detected.")
				} else {
					# repeated measurement
					# TODO: This works if only one random factor is given (e.g., subject ID in a repeated measurement). Generalize to multiple random factors
					var.id <- all.vars(as.formula(BFobject@denominator@identifier$formula))[2]
					count = dat %>% group_by_(var.id) %>% summarise(n=n())
					max.n = nrow(count)
					random = TRUE
					if (verbose==TRUE) print(paste0("Grouping factor detected: ", var.id))
				}
			}
		)
	}

	# at which n's should the BF be calculated?
	ns <- seq(min.n, max.n, by=step)
	if (tail(ns, 1) != max.n) ns <- c(ns, max.n)

	if (verbose==TRUE) pb <- txtProgressBar(min = 0, max = length(ns), style=3)
	resSeq <- data.frame()
	# ---------------------------------------------------------------------
	# Do the sequential calculation of BFs. Each design needs it's own sampling scheme
	for (i in 1:length(ns)) {
		
		if (random == FALSE) {
			BFobject@data <- dat[1:ns[i], , drop=FALSE]
		}
		
		if (random == TRUE) {
			var.id.pos <- NULL	# hack to please CRAN
			dat$var.id.pos <- as.numeric(dat[, var.id])
			BFobject@data <- dat %>% filter(var.id.pos <= ns[i]) %>% select(-var.id.pos) %>% as.data.frame()
		}
		
		
		# Get rid of unused factor levels
		for (v in 1:ncol(BFobject@data)) {
			if (is.factor(BFobject@data[, v])) {
				BFobject@data[, v] <- factor(BFobject@data[, v])
			}
		}
		# Get rid of old analysis information used for increasing precision with same data
		for(v in length(BFobject)){
		  BFobject@numerator[[v]]@analysis = list()
		}
		BFobject@denominator@analysis = list()
		
		# recompute Bayes factor on reduced data set
		res1 <- recompute(BFobject, progress=FALSE)
		resSeq <- rbind(resSeq, data.frame(
			n  = ns[i], 
			Alt = 1:nrow(res1@bayesFactor),
			bf = res1@bayesFactor$bf,			
			error = res1@bayesFactor$error
		))
		if (verbose==TRUE) setTxtProgressBar(pb, i)
	}
	if (verbose==TRUE) close(pb)

	# Define a new final BF-object. If no user args are provided, this is identical to the original BFobject
	BFobject.new <- res1
	
	# define a new S4 class. You have to set the 'where' argument to please R CMD check: https://stat.ethz.ch/pipermail/r-devel/2007-April/045475.html
	setClass("BFBayesFactorSeq", slots = c(bayesFactorSeq="data.frame"), contains = class(BFobject), where=topenv(parent.frame()))

	RES <- new("BFBayesFactorSeq")
	# copy all existing slots into new object.
	for (sl in names(getSlots(class(BFobject)))) {
		slot(RES, sl) <- slot(BFobject.new, sl)
	}

	# add new sequential slot to results object
	RES@bayesFactorSeq <- resSeq
	return(RES)
}



#' @export
plot.BFBayesFactorSeq <- function(x, ...) {
	dat <- x@bayesFactorSeq
	dat <- na.omit(dat)
	if (max(x@bayesFactorSeq$Alt) == 1) {
		p1 <- seqBFplot(n = dat$n, bf = dat$bf, log.it=FALSE)
	} else {
		p1 <- seqBFplot(n = dat$n, bf = dat$bf, linetype=factor(dat$Alt, labels=names(x@numerator)), linetype.label="Alternative", log.it=FALSE)
	}
	return(p1)
}





## ======================================================================
## SPEICHER
## ======================================================================

# switch(BFtype,
# 	BFoneSample = {
# 		max.n = nrow(dat)
# 	},
# 	BFindepSample = {
# 		count = dat %>% count(group)
# 		max.n = max(count$n)
# 	},
# 	BFlinearModel = {
# 		# differentiate between anovaBF and lmBF: get RHS of denominator formula; ==1: lmBF; !=1: anovaBF
# 		if (length(all.vars(as.formula(BFobject@denominator@identifier$formula))) == 1) {
# 			# lmBF
# 			stop("seqBF does not work for lmBF() yet.")
# 		} else {
# 			# anovaBF
# 			n.models <- length(BFobject@numerator)
# 			call <- paste0("group_by(dat, ", paste(names(which(BFobject@numerator[[n.models]]@dataTypes == "fixed")), collapse=", "), ")")
# 			dat_grouped <- eval(parse(text=call))
# 			count = dat_grouped %>% summarise(n=n())
# 			max.n = max(count$n)	# largest cell
# 		}
# 	}
# )




# switch(BFtype,
# 	# one sample t test
# 	BFoneSample = {BFobject@data <- data.frame(y=dat$y[1:ns[i]])},
#
# 	# two sample t test
# 	BFindepSample = {
# 		# sample min.n from each group
# 		BFobject@data <- dat %>% group_by(group) %>% slice(1:ns[i]) %>% ungroup() %>% as.data.frame()
# 		},
# 	BFlinearModel = {
# 		# sample min.n from each cell (also in interaction designs)
# 		BFobject@data <- dat_grouped %>% slice(1:ns[i]) %>% ungroup() %>% as.data.frame()
# 		}
# )




# @export
# show.BFBayesFactorSeq <- function(x, ...) {
# 	print("Sequential BayesFactor object. The final Bayes factor is:\n=========================")
# 	print(x)
# }