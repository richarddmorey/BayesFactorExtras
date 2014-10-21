#' @title Compute a sequential Bayes factor object
#' @details Compute a sequential Bayes factor object
#' @param BFobject A BayesFactor object
#' @param ... User parameters which override the settings of the original \code{BFobject}
#' @param min.n Smallest number of participants for which the first BF should be computed. In a one-sample t test / paired  test, this is the overall sample size. In two-sample t tests and ANOVAs, this is the minimum size for each group.#
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
#' data(puzzles)
#' #a2 <- anovaBF(RT ~ shape + color, data = puzzles, whichRandom = "ID", progress=FALSE)
#' #a2.seq <- seqBF(a2)
#'	
#' # Two-sample t test, one-sided test
#' ttest.2 <- ttestBF(x = sleep$extra[sleep$group==1], y = sleep$extra[sleep$group==2], 
#'					  nullInterval=c(-Inf, 0))
#' ttest.2.seq <- seqBF(ttest.2, min.n=5)
#' plot(ttest.2.seq)
#' 

seqBF <- function(BFobject, min.n=10, step=1, ...) {
	#if (class(BFobject) != "BFBayesFactor") stop(paste0("seqBF does not work yet for objects of class ", class(BFobject)))

	dat <- BFobject@data
	BFtype <- class(BFobject@numerator[[1]])

	n.max <- NULL
	switch(BFtype,
		BFoneSample = {
			max.n = nrow(dat)
		},
		BFindepSample = {
			count = dat %>% count(group)
			max.n = max(count$n)
		},
		BFlinearModel = {
			# differentiate between anovaBF and lmBF: get RHS of denominator formula; ==1: lmBF; !=1: anovaBF
			if (length(all.vars(as.formula(BFobject@denominator@identifier$formula))) == 1) {
				# lmBF
				stop("seqBF does not work for lmBF() yet.")
			} else {
				# anovaBF
				call <- paste0("group_by(dat, ", paste(names(which(BFobject@numerator[[1]]@dataTypes == "fixed")), collapse=", "), ")")
				dat_grouped <- eval(parse(text=call))
				count = dat_grouped %>% summarise(n=n())
				max.n = max(count$n)
			}
		}
	)
	# at which n's should the BF be calculated?
	ns <- seq(min.n, max.n, by=step)
	if (tail(ns, 1) != max.n) ns <- c(ns, max.n)

	pb <- txtProgressBar(min = 0, max = length(ns), style=3)
	resSeq <- data.frame()
	# ---------------------------------------------------------------------
	# Do the sequential calculation of BFs
	for (i in 1:length(ns)) {
		switch(BFtype,
			BFoneSample = {BFobject@data <- data.frame(y=dat$y[1:ns[i]])},
			BFindepSample = {
				# sample min.n from each group
				BFobject@data <- dat %>% group_by(group) %>% slice(1:ns[i]) %>% ungroup() %>% as.data.frame()
				},
			BFindepSample = {
				# sample min.n from each group
				BFobject@data <- dat %>% group_by(group) %>% slice(1:ns[i]) %>% ungroup() %>% as.data.frame()
				},
			BFlinearModel = {
				# sample min.n from each cell (also in interaction designs)
				BFobject@data <- dat_grouped %>% slice(1:ns[i]) %>% ungroup() %>% as.data.frame()
				}
		)
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
		res1 <- recompute(BFobject, progress=FALSE)
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
	if (max(x@bayesFactorSeq$Alt) == 1) {
		p1 <- seqBFplot(n = x@bayesFactorSeq$n, bf = x@bayesFactorSeq$bf, log.it=FALSE)
	} else {
		p1 <- seqBFplot(n = x@bayesFactorSeq$n, bf = x@bayesFactorSeq$bf, linetype=factor(x@bayesFactorSeq$Alt, labels=names(x@numerator)), log.it=FALSE)
		p1 <- p1 + scale_linetype_discrete("Alternative")
	}
	return(p1)
}