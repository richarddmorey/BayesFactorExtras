#' Compute a sequence of Bayes factors
#' 
#' 

#' @title Compute a sequential Bayes factor object
#' @param BFobject A BayesFactor object
#' @param ... User parameters which override the settings of the original \code{BFobject}
#' @param min.n Smallest number of participants for which the first BF should be computed. In a one-sample t test / paired t test, this is the overall sample size. In two-sample t tests and ANOVAs, this is the minimum size for each group.
#' @param step Step size with which the number of participants is increased
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
#' # This does not work fully yet ...
#' a2 <- anovaBF(RT ~ shape + color, data = puzzles, whichRandom = "ID", progress=FALSE)
#' a2.seq <- seqBF(a2)
	
	
#' # Two-sample t test, one-sided test
#' ttest.2 <- ttestBF(x = sleep$extra[sleep$group==1], y = sleep$extra[sleep$group==2], nullInterval=c(-Inf, 0))
#' ttest.2.seq <- seqBF(ttest.2, min.n=5)
#' plot(ttest.2.seq)
#' 


seqBF <- function(BFobject, min.n=10, step=1, ...) {
	# For testing purposes ...
	# BFobject <- ttest.1
	# BFobject <- ttest.1b
	# BFobject <- ttest.2
	# BFobject <- ttest.2b
	# BFobject <- ttest.p
	# BFobject <- anova1
	# BFobject <- lmBF1
	# BFobject <- a2
	
	if (class(BFobject) != "BFBayesFactor") stop(paste0("seqBF does not work yet for objects of class ", class(BFobject)))
	user.args <- list(...)

	dat <- BFobject@data
	BFtype <- class(BFobject@numerator[[1]])
	
	FUN <- NULL
	FUN.name <- ""
	extra.args <- list()
	n.max <- NULL
	switch(BFtype,
		BFoneSample = {
			FUN = ttestBF
			FUN.name = "ttestBF"
			max.n = nrow(dat)
			extra.args = list(
				paired = FALSE, 
				rscale = BFobject@numerator[[1]]@prior$rscale, 
				mu = BFobject@numerator[[1]]@prior$mu)
			if (!is.null(BFobject@numerator[[1]]@prior$nullInterval)) {
				extra.args$nullInterval = BFobject@numerator[[1]]@prior$nullInterval
			}
		},
		BFindepSample = {
			FUN = ttestBF
			FUN.name = "ttestBF"
			counts = dat %>% count(group)
			max.n = max(counts$n)
			extra.args = list(
				formula = as.formula(BFobject@numerator[[1]]@identifier$formula),
				paired = FALSE, 
				rscale = BFobject@numerator[[1]]@prior$rscale)
			if (!is.null(BFobject@numerator[[1]]@prior$nullInterval)) {
				extra.args$nullInterval = BFobject@numerator[[1]]@prior$nullInterval
			}	
		},
		BFlinearModel = {
			# differentiate between anovaBF and lmBF: get RHS of denominator formula; ==1: lmBF; !=1: anovaBF
			if (length(all.vars(as.formula(BFobject@denominator@identifier$formula))) == 1) {
				# lmBF
#				FUN =  lmBF
# 				extra.args = list(
# 					formula = as.formula(BFobject@numerator[[1]]@identifier$formula),
# 					rscaleFixed  = BFobject@numerator[[1]]@prior$rscale$fixed,
# 					rscaleRandom = BFobject@numerator[[1]]@prior$rscale$random,
# 					rscaleCont	 = BFobject@numerator[[1]]@prior$rscale$continuous,
# 					progress = FALSE,
# 					whichRandom = names(which(BFobject@numerator[[1]]@dataTypes == "random")))
			} else {
				# anovaBF
				
				# get maximum group size per cell
				call <- paste0("group_by(dat, ", paste(names(which(BFobject@numerator[[1]]@dataTypes == "fixed")), collapse=", "), ")")
				dat_grouped <- eval(parse(text=call))
				count = dat_grouped %>% summarise(n=n())
				max.n = max(count$n)
				FUN = anovaBF
				FUN.name = "anovaBF"
				extra.args = list(
					formula = as.formula(BFobject@numerator[[1]]@identifier$formula),
					rscaleFixed  = BFobject@numerator[[1]]@prior$rscale$fixed,
					rscaleRandom = BFobject@numerator[[1]]@prior$rscale$random,
					progress = FALSE,
					whichRandom = names(which(BFobject@numerator[[1]]@dataTypes == "random")))
			}
		}
	)
	if (is.null(FUN)) stop(paste0("BayesFactor object of type '", BFtype, "' not recognized (yet)."))

	
	# override original parameters with the ... argument:
	for (a in names(user.args)) {
		extra.args[[a]] <- user.args[[a]]
	}	
	
	# at which n's should the BF be calculated?
	ns <- seq(min.n, max.n, by=step)
	if (tail(ns, 1) != max.n) ns <- c(ns, max.n)

	pb <- txtProgressBar(min = 0, max = length(ns), style=3)
	resSeq <- data.frame()
	
	# ---------------------------------------------------------------------
	# Do the sequential calculation of BFs
	for (i in 1:length(ns)) {
		switch(BFtype,
			BFoneSample = {extra.args$x <- dat[1:ns[i], ]},
			BFindepSample = {
				# sample min.n from each group
				extra.args$data <- dat %>% group_by(group) %>% slice(1:ns[i]) %>% ungroup() %>% as.data.frame()
				},
			BFindepSample = {
				# sample min.n from each group
				extra.args$data <- dat %>% group_by(group) %>% slice(1:ns[i]) %>% ungroup() %>% as.data.frame()
				},
			BFlinearModel = {
				if (FUN.name == "anovaBF") {
					# sample min.n from each cell (also in interaction designs)
					extra.args$data <- dat_grouped %>% slice(1:ns[i]) %>% ungroup() %>% as.data.frame()
				}
				if (FUN.name == "lmBF") {
					# sample min.n from each factor and each continuos variable
					#extra.args$data <- dat %>% group_by(group) %>% slice(1:ns[i]) %>% ungroup() %>% as.data.frame()
				}
				}
		)
		
		res1 <- do.call(FUN, extra.args, envir = parent.frame())	
		resSeq <- rbind(resSeq, data.frame(
			n  = ns[i], 
			Alt = 1:nrow(res1@bayesFactor),
			bf = res1@bayesFactor$bf,			
			error = res1@bayesFactor$error
		))
		setTxtProgressBar(pb, i)
	}
	close(pb)
	
	# Define a new final BF-object. If no user args are provided, this is identical to the original BFobject
	BFobject.new <- res1
	# Validity check: (doesn-ät work for anovaBF, as the BF varies due to sampling)
	# if (length(user.args) == 0 & !identicalBF(BFobject, BFobject.new)) {stop("Error in calculation! (Error code 1)")}
	
	# define a new S4 class:
	setClass("BFBayesFactorSeq", slots = c(bayesFactorSeq="data.frame"), contains = class(BFobject))

	RES <- new("BFBayesFactorSeq")
	
	# copy all existing slots into new object.
	# FIXME: Can this be done more efficiently?
	for (s in names(getSlots("BFBayesFactor"))) {
		slot(RES, s) <- slot(BFobject.new, s)
	}

	# add new sequential slot to results object
	RES@bayesFactorSeq <- resSeq
	return(RES)
}


#' @export
plot.BFBayesFactorSeq <- function(x, ...) {
	p1 <- seqBFplot(n = x@bayesFactorSeq$n, bf = x@bayesFactorSeq$bf, log.it=FALSE)#, ...)
	if (max(x@bayesFactorSeq$Alt) > 1) {
		p1 <- seqBFplot(n = x@bayesFactorSeq$n, bf = x@bayesFactorSeq$bf, linetype=x@bayesFactorSeq$Alt, log.it=FALSE)
		p1 <- p1 + scale_linetype_discrete("Alternative")
	}
	return(p1)
}