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
  title = paste0( "Samples from model ", model )
  return( knit_printing_mcmc(x, title = title, ... ) )
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
  return( knit_printing_mcmc(x, title = title, ... ) )
}


knit_printing_mcmc <- function( x, title = "", ... )
{
  interactive = getOption('BFEmcmcPlotType', 'svg') == "interactive"
  divname = paste(sample(LETTERS),collapse="")
  if(interactive){
    plotDims = getOption('BFEmcmcPlotDimsInteractive', c(800, 400))
  }else{
    plotDims = getOption('BFEmcmcPlotDims', c(8,5))
  }

  if( (length(plotDims) != 2 ) | any(is.na(plotDims + 0 ) ) ) stop("Invalid plot dimensions option.")

  hoverHelpFile = system.file("etc", "html", "hoverhelp.html", package = "BayesFactorExtras")
  hoverHelpCode = readChar(hoverHelpFile, file.info(hoverHelpFile)$size)

  mainDiv = tags$div( id = divname, class = "BFmcmc")
  titleDiv = tags$h2(title, class = "BFmcmc_title" )
  sliderDiv = tags$div( id = paste0( divname, "_modelslider" ), class = "BFmcmc_modelslider" )
  searchDiv = tags$input(id = paste0(divname,"_search"), class = "BFmcmc_search")

  plotType = getOption('BFEmcmcPlotType', 'svg')
  if( ( plotType %in% c('svg', 'jpeg', 'png') ) & !interactive ){
    plotFun = get(plotType)
  }else if( !interactive ){
    stop("Invalid plot type ", plotType, " in options(BFEmcmcPlotType).")
  }

  if(interactive){
    retList = create_interactive_mcmc(x, divname, plotType, plotFun, plotDims)
  }else{
    retList = create_noninteractive_mcmc(x, divname, plotType, plotFun, plotDims)
  }
  parSelect = retList$parSelect
  allImgs = retList$allImgs

  html = tagAppendChildren(
      tagAppendChildren(mainDiv, titleDiv, searchDiv, parSelect, sliderDiv, allImgs ),
      tags$script(HTML(paste0("
        $(document).ready(function () {
          buildBFmcmc('",divname,"');
        });
      ")))
  )

  if(getOption('BFEknitrDownload', TRUE)){
    DLtag = createDownloadURI("x", filename = "BayesFactorMCMCObject", textHTML = "Click here to download this BayesFactorMCMC object.", envir = parent.frame(), printHTML = FALSE)
    html = tagList(html, DLtag)
  }


  # return html
  if( interactive ){
    deps = BFBayesFactorMCMC_interactive_dependencies
  }else{
    deps = BFBayesFactorMCMC_dependencies
  }

  html <- attachDependencies(
    html,
    deps
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

BFBayesFactorMCMC_interactive_dependencies <- list(
  htmltools::htmlDependency(
    name = "BFBayesFactorMCMC_interactive",
    version = "1.0",#BFEInfo(FALSE),
    src = system.file("etc", package = "BayesFactorExtras"),
    stylesheet = c("css/BFmcmc.css",
                   "css/noUISlider/jquery.nouislider.css"),
    script = c("js/utility_mcmc.js",
               "js/utility_mcmc_interactive.js",
               "js/noUiSlider/jquery.nouislider.all.js",
               "js/filterByText.js",
               "js/jstat.min.js",
               "js/d3.min.js"
               )
  )
)


create_noninteractive_mcmc <- function(x, divname, plotType, plotFun, plotDims)
  {
  parSelect = tags$select( id = paste0( divname, "_parselect" ), class = "BFmcmc_select" )
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

  return( list(allImgs = tagList(allImgs), parSelect = parSelect) )
}

create_interactive_mcmc <- function(x, divname, plotType, plotFun, plotDims)
{
  parSelect = tags$select( id = paste0( divname, "_parselect" ), class = "BFmcmc_select" )
  allImgs = tagAppendChildren(
              tags$div( id = paste0( divname, "_plotcontainer"),
                        class =  "BFmcmc_plotContainer",
                        style = paste0("width: ", plotDims[1], "px;\n height: ", plotDims[2], "px;") ),
              tags$div(HTML("&nbsp;"), id = paste0( divname, "_lineplot"), class = "BFmcmc_lineplot" ),
              tags$div(HTML("&nbsp;"), id = paste0( divname, "_histplot"), class = "BFmcmc_histplot" )
    )


  for(i in 1:ncol(x)){
    parName = colnames(x)[i]
    parCode = paste(sample(LETTERS),collapse="")

    allImgs = tagAppendChild(allImgs,
                  tags$div( toJSON(as.vector(x[,i])), class = "mcmcdata bfhide",
                            id = paste( divname, "mcmcdata", parCode, sep = "_")
                )
    )
    parSelect = tagAppendChild( parSelect, tags$option( parName, value=parCode ) )

  }

  return( list(allImgs = allImgs, parSelect = parSelect) )
}
