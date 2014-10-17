#' Plot a sequence of Bayes factors
#' 
#' This function creates a lineplot of a sequence of log Bayes factors. As a default, the function expects raw Bayes factors (i.e., non-logged Bayes factors). 
#' If you provide Bayes factors that are already logged (e.g., from the output of the \code{\link{ttest.tstat}} function), set \code{log.it} to \code{FALSE}.
#' This function is in particular useful for plotting the trajectory of a sequential Bayes factor test
#' @title Plot a Bayes factor object
#' @param n A vector of numbers for the x axis
#' @param bf A vector of Bayes factors (same length as x)
#' @param xlab Label for x axis
#' @param ylab Label for y axis
#' @param main Main title
#' @param log.it Should the Bayes factor in the \code{bf} parameter be logged?
#' @param forH1 If \code{TRUE}, positive bfs mean evidence in favor of H1 ("H1 over H0" Bayes factor). This is the default in the Bayes factor package.
#'
#' @export
#' @import ggplot2
#' @import BayesFactor
#' @importFrom grid unit
#' @importFrom grid grid.draw
#'
#' @author Felix D. Schönbrodt (\email{felix@@nicebread.de})
#' @examples
#' ## Sleep data from t test example
#' data(sleep)
#' 
#' # Compute accumulating evidence from n1=5 participants to n2=10 participants
#' bf <- c()
#' for (i in 5:10) {
#' 	bf0 <- ttestbf(
#'		x = sleep$extra[sleep$group==1][1:i], 
#'		y = sleep$extra[sleep$group==2][1:i], paired=TRUE)
#' 	bf <- c(bf, as.vector(bf0))
#' }
#' 
#' seqBFplot(5:10, bf)

seqBFplot <- function(n, bf, linetype=NA, xlab="n", ylab="log(bf)", main="", log.it=TRUE, forH1=TRUE) {
	if (length(n) != length(bf)) stop("`n` and `bf` should have the same length")
	if (length(n) < 1) stop("`n`and `bf` must habe length > 1")
		
	if (log.it==TRUE) bf <- log(bf)
		
	df <- data.frame(n, bf, Alt=factor(linetype))
	p1 <- ggplot(df, aes(x=n, y=bf)) + theme_bw() + ylab(ylab) + xlab(xlab)
	
	# more than one data point? Line plot
	if (length(n) > 1) {
		if (is.na(linetype[1])) {
			p1 <- p1 + geom_line()
		} else {
			p1 <- p1 + geom_line(aes(linetype=Alt, group=Alt))
		}
		
	
		# custom labeler: find breaks with pretty numbers, and not more than 5
		# find good divisor
		steps <- c(2, 4, 5, 10, 15, 20, seq(30, 1000, by=10), seq(1100, 50000, by=100))
		i <- 1
		repeat {
			mod <- (max(n)-min(n)+1) %/% steps[i]
			if (mod <= 5) {break} else {i <- i+1}
		}

	    x.breaks <- seq(steps[i], max(n), by=steps[i])
	    names(x.breaks) <- x.breaks
	}
		
	# One data point? Plot a single point
	if (length(n) == 1) {
		p1 <- p1 + geom_point()
		x.breaks <- n
	}
	
	p1 <- p1 + scale_x_continuous(breaks=x.breaks)
	
	
	# All the annotation stuff ...
	p1 <- p1 + geom_hline(yintercept=c(c(-log(c(100, 30, 10, 3)), log(c(3, 10, 30, 100)))), linetype="dotted", color="darkgrey")
	p1 <- p1 + geom_hline(yintercept=log(1), linetype="dashed", color="grey20")

	p1 <- p1 + annotate("text", x=max(n), y=-5.15, label=paste0("~~Extreme~H[", ifelse(forH1==TRUE,0,1), "]"), 
		hjust=1, vjust=.5, size=3.2, color="black", parse=TRUE)
	p1 <- p1 + annotate("text", x=max(n), y=-4.00, label=paste0("~~Very~strong~H[", ifelse(forH1==TRUE,0,1), "]"), 
		hjust=1, vjust=.5, size=3.2, color="black", parse=TRUE)
	p1 <- p1 + annotate("text", x=max(n), y=-2.85, label=paste0("~~Strong~H[", ifelse(forH1==TRUE,0,1), "]"), 
		hjust=1, vjust=.5, size=3.2, color="black", parse=TRUE)
	p1 <- p1 + annotate("text", x=max(n), y=-1.7 , label=paste0("~~Moderate~H[", ifelse(forH1==TRUE,0,1), "]"), 
		hjust=1, vjust=.5, size=3.2, color="black", parse=TRUE)
	p1 <- p1 + annotate("text", x=max(n), y=-.55 , label=paste0("~~Anectodal~H[", ifelse(forH1==TRUE,0,1), "]"), 
		hjust=1, vjust=.5, size=3.2, color="black", parse=TRUE)

	p1 <- p1 + annotate("text", x=max(n), y=5.15, label=paste0("~~Extreme~H[", ifelse(forH1==TRUE,1,0), "]"), 
		hjust=1, vjust=.5, size=3.2, color="black", parse=TRUE)
	p1 <- p1 + annotate("text", x=max(n), y=4.00, label=paste0("~~Very~strong~H[", ifelse(forH1==TRUE,1,0), "]"), 
		hjust=1, vjust=.5, size=3.2, color="black", parse=TRUE)
	p1 <- p1 + annotate("text", x=max(n), y=2.86 , label=paste0("~~Strong~H[", ifelse(forH1==TRUE,1,0), "]"), 
		hjust=1, vjust=.5, size=3.2, color="black", parse=TRUE)
	p1 <- p1 + annotate("text", x=max(n), y=1.7  , label=paste0("~~Moderate~H[", ifelse(forH1==TRUE,1,0), "]"), 
		hjust=1, vjust=.5, size=3.2, color="black", parse=TRUE)
	p1 <- p1 + annotate("text", x=max(n), y=.55  , label=paste0("~~Anectodal~H[", ifelse(forH1==TRUE,1,0), "]"), 
		hjust=1, vjust=.5, vjust=.5, size=3.2, color="black", parse=TRUE)

	# set scale ticks
	p1 <- p1 + scale_y_continuous(breaks=c(c(-log(c(100, 30, 10, 3)), 0, log(c(3, 10, 30, 100)))), labels=c("-log(100)", "-log(30)", "-log(10)", "-log(3)", "log(1)", "log(3)", "log(10)", "log(30)", "log(100)"))
	

	if (main != "") p1 <- p1 + ggtitle(main)

	#p1 <- p1 + theme(plot.margin = grid::unit(c(1,5,1,1), "lines"))

	# TODO: The annotation only works with this work-around; but now no ggplot-object is returned (which would be nice for users, to add their own themes, e.g.). Set x = Inf
	# Code to override clipping, from http://stackoverflow.com/questions/10014187/displaying-text-below-the-plot-generated-by-ggplot2 
	# gt <- ggplot_gtable(ggplot_build(p1))
	# gt$layout$clip[gt$layout$name == "panel"] <- "off"
	# grid::grid.draw(gt)
	
	return(p1)
}

seqBFplot(1:100, cumsum(rnorm(100, 2)))