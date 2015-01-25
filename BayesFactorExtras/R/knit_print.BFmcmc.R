##' knitr print method for BFmcmc objects
##' 
##' knitr print method for BFmcmc objects
##' @title Print interactive HTML/JavaScript posterior samples from Bayes factor analyses
##' @param x a BFmcmc object
##' @param ... further arguments to be passed to or from methods.
##' @return The function code to be inserted into Rmarkdown documents.
##' @rdname knit_print
##' @method knit_print BFmcmc
##' @export
##' @author Richard D. Morey (\email{richarddmorey@@gmail.com})
knit_print.BFmcmc <- function( x, ... )
{
  model = x@model@identifier$formula
  title = paste0( "Samples from model", model )
  if( options()$BFEmcmcPlotType == "interactive"){
    return( interactive_knit_printing_mcmc(x, title = title, ... ) )     
  }else{
    return( noninteractive_knit_printing_mcmc(x, title = title, ... ) )      
  }
}

##' knitr print method for mcmc objects
##' 
##' knitr print method for mcmc objects
##' @title Print interactive HTML/JavaScript posterior samples from mcmc analyses
##' @param x an mcmc object
##' @param ... further arguments to be passed to or from methods.
##' @return The function code to be inserted into Rmarkdown documents.
##' @rdname knit_print
##' @method knit_print mcmc
##' @export
##' @author Richard D. Morey (\email{richarddmorey@@gmail.com})
knit_print.mcmc <- function( x, ... )
{
  title = "MCMC samples"
  if( options()$BFEmcmcPlotType == "interactive"){
    return( interactive_knit_printing_mcmc(x, title = title, ... ) )      
  }else{
    return( noninteractive_knit_printing_mcmc(x, title = title, ... ) )     
  }  
}  

interactive_knit_printing_mcmc <- function( x, title = "", ... )
{
  options( BFEmcmcPlotType = "svg" )
  noninteractive_knit_printing_mcmc( x, title, ... )
}

noninteractive_knit_printing_mcmc <- function( x, title = "", ... )
{
  divname = paste(sample(LETTERS),collapse="")
  plotDims = options()$BFEmcmcPlotDims
  if( (length(plotDims) != 2 ) | any(is.na(plotDims + 0 ) ) ) stop("Invalid BFEmcmcPlotDims option.")
  
  hoverHelpFile = system.file("etc", "html", "hoverhelp.html", package = "BayesFactorExtras")
  hoverHelpCode = readChar(hoverHelpFile, file.info(hoverHelpFile)$size)
    
  mainDiv = tags$div( id = divname, class = "BFmcmc")
  parSelect = tags$select( id = paste0( divname, "_parselect" ), class = "BFmcmc_select" )  
  titleDiv = tags$h2(title, class = "BFmcmc_title" )
  sliderDiv = tags$div( id = paste0( divname, "_modelslider" ), class = "BFmcmc_modelslider" )
  searchDiv = tags$input(id = paste0(divname,"_search"), class = "BFmcmc_search")
  
  plotType = options()$BFEmcmcPlotType
  if(plotType %in% c('svg', 'jpeg', 'png')){
    plotFun = get(plotType)
  }else{
    stop("Invalid plot type ", plotType, " in options(BFEmcmcPlotType).")
  }
  
  allImgs = list()
  
  for(i in 1:ncol(x)){
    parName = colnames(x)[i]
    parCode = paste(sample(LETTERS),collapse="")
    fn = tempfile(fileext = paste0(".", plotType))
    capture.output({
      plotFun(fn, width = plotDims[1], height = plotDims[2])
      plot(mcmc(x[,i]))
      dev.off()
    })
    if( plotType != "svg" ){
      imgContents = base64encode(fn)
    }else{
      imgContents = readChar(fn, file.info(fn)$size)
    }
    imgsrc = paste0(imgTypes[[plotType]], imgContents)
    allImgs = c(allImgs, 
                list(
                  tags$img( src = HTML(imgsrc), 
                            class = "BFmcmc_plot bfhide", 
                            id = paste( divname, "plot", parCode, sep = "_") 
                            )
                  ) 
                )
    parSelect = tagAppendChild( parSelect, tags$option( parName, value=parCode ) )
  }
  
  html = tagAppendChildren(
      tagAppendChildren(mainDiv, titleDiv, searchDiv, parSelect, sliderDiv, tagList(allImgs) ),
      tags$script(HTML(paste0("
        $(document).ready(function () {
          buildBFmcmc('",divname,"');
        });
      "))) 
  )
  
  if(options()$BFEknitrDownload){
    DLtag = createDownloadURI("x", filename = "BayesFactorMCMCObject", textHTML = "Click here to download this BayesFactorMCMC object.", envir = parent.frame(), printHTML = FALSE)
    html = tagList(html, DLtag)
  }
    
  
  # return html
  html <- attachDependencies(
    html,
    BFBayesFactorMCMC_dependencies
  )
  
  knit_print(browsable(html), ... )
}

imgTypes = list(
  "png" = "data:image/png;base64,",
  "jpeg" = "data:image/jpeg;base64,",
  "svg" = "data:image/svg+xml;utf8,"
  )


BFBayesFactorMCMC_dependencies <- list(
  htmltools::htmlDependency(
    name = "BFBayesFactorMCMC",
    version = "1.0",#BFEInfo(FALSE),
    src = system.file("etc", package = "BayesFactorExtras"),
    stylesheet = c("css/BFmcmc.css", 
                   "css/noUISlider/jquery.nouislider.css"),
    script = c("js/utility_mcmc.js",  
               "js/noUiSlider/jquery.nouislider.all.js",
               "js/filterByText.js")
  )
)
