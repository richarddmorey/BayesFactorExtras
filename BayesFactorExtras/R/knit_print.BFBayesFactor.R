##' knitr print method for BFBayesFactor objects
##' 
##' knitr print method for BFBayesFactor objects
##' @title Print interactive HTML/JavaScript Bayes factors
##' @param x a BFBayesFactor object
##' @param ... further arguments to be passed to or from methods.
##' @return This function code to be inserted into Rmarkdown documents.
##' @rdname knit_print
##' @method knit_print BFBayesFactor
##' @export
##' @author Richard D. Morey (\email{richarddmorey@@gmail.com})
knit_print.BFBayesFactor <- function( x, ... )
{
  divname = paste(sample(LETTERS),collapse="")
  denom = (1/x[1]) / (1/x[1])
  df = c(denom,x)@bayesFactor
  df = data.frame(df,index = 1:nrow(df) - 1)
  df$row = htmlEscape(rownames(df))
  rownames(df) = NULL
  jsonobject = toJSON(df)
  
  modelType = class(x@denominator)[1]
  analysisType = x@denominator@type
  
  mainDiv = tags$div(id = divname, class = "BFBayesFactor")
  html = tagAppendChildren(mainDiv, 
      tags$div(HTML(jsonobject), id = paste0(divname,"_json"), class = "BFBayesFactor_json bfhide"),
      tags$div(HTML(analysisType), id = paste0(divname,"_analysistype"), class = "BFBayesFactor_analysistype bfhide"),
      tags$div(HTML(modelType), id = paste0(divname,"_modeltype"), class = "BFBayesFactor_modeltype bfhide"),
      tags$div(id = paste0(divname,"_denom"), class = "BFBayesFactor_denom"),
      tags$input(id = paste0(divname,"_search"), class = "BFBayesFactor_search"),
      tags$table(id = paste0(divname,"_bf"), class = "BFBayesFactor_bf"),
      tags$script(HTML(paste0("
        $(document).ready(function () {
          buildBFBayesFactor('",divname,"');
        });
      "))) 
  )
  
  if(options()$BFEknitrDownload){
    DLtag = createDownloadURI("x", filename = "BayesFactorObject", textHTML = "Click here to download this BayesFactor object.", envir = parent.frame(), printHTML = FALSE)
    html = tagList(DLtag, html)
  }
    
  
  # return html
  html <- attachDependencies(
    html,
    BFBayesFactor_dependencies
  )
  
  knit_print(browsable(html), ... )
}


BFBayesFactor_dependencies <- list(
  htmltools::htmlDependency(
    name = "BFBayesFactor",
    version = "1.0",#BFEInfo(FALSE),
    src = system.file("etc", package = "BayesFactorExtras"),
    stylesheet = "css/BFBayesFactor.css",
    script = "js/utility.js"
  )
)
