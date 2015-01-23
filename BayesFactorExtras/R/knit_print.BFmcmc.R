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
  divname = paste(sample(LETTERS),collapse="")
  
  hoverHelpFile = system.file("etc", "html", "hoverhelp.html", package = "BayesFactorExtras")
  hoverHelpCode = readChar(hoverHelpFile, file.info(hoverHelpFile)$size)
  
  objSetup = BFmcmcGetSetup(x)
  
  groupSelect = tags$select()
  groupOptions = lapply(objSetup$groupNames, function(name){
    tags$option(name, value=name)
  })
  groupSelect = tagAppendChild(groupSelect, tagList(groupOptions))
  
  mainDiv = tags$div( id = divname, class = "BFBayesFactor")
  
  plotType = options()$BFEmcmcPlotType
  if(plotType %in% c('svg', 'jpeg', 'png')){
    plotFun = get(plotType)
  }else{
    stop("Invalid plot type", plotType, " in options(BFEmcmcPlotType).")
  }
  
  allImgs = list()
  
  for(i in 1:ncol(x)){
    fn = tempfile(fileext = paste0(".", plotType))
    capture.output({
      plotFun(fn)
      plot(mcmc(x[,i]))
      dev.off()
    })
    if( plotType != "svg" ){
      imgContents = base64encode(fn)
    }else{
      imgContents = readChar(fn, file.info(fn)$size)
    }
    imgsrc = paste0(imgTypes[[plotType]], imgContents)
    allImgs = c(allImgs, list(tags$img( src = HTML(imgsrc) ) ) )
  }
  
  html = tagAppendChildren(
      tagAppendChildren(mainDiv, tagList(allImgs)),
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

BFmcmcGetSetup <- function(x){
  # Determine grouping of columns
  if( class(x@model) == "BFlinearModel" ){
    model = formula(x@model@identifier$formula)
    terms = attr(terms(model),"term.labels")
    nGroups = length(terms) + 3
    groups = rep( 1, ncol(x) )
    groups[ ncol(x) - length(terms) ] = nGroups - 1
    groups[ 1 + ncol(x) - 1:length(terms)] = nGroups
    groupNames = 1:nGroups
  }else if(class(x@model) %in% c("BFindepSample", "BFoneSample") ){
    nGroups = 2
    groupNames = c("Model","Nuisance")
    groups = rep( 1, ncol(x) )
    groups[ ncol(x) ] = 2
  }else{
    nGroups = 1
    groupNames = c("Parameters")
    groups = rep( 1, ncol(x) )
  }
return(list(
  nGroups = nGroups,
  groupNames = groupNames,
  groups = groups
  ))
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
    stylesheet = "css/BFmcmc.css",
    script = "js/utility_mcmc.js"
  )
)
