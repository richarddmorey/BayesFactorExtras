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
  jsonobject = toJSON(x@bayesFactor)
    
  #scriptTemplate = system.file("etc", "html", "BFBayesFactor.html", package = "BayesFactorExtras")
  #res = readChar(scriptTemplate, file.info(scriptTemplate)$size)

  # Substitute in the variables
  #res = gsub("\\$DIVNAME", divname, res)
  #res = gsub("\\$JSONOBJECT", jsonobject, res)
  
  mainDiv = tags$div(id = divname, class = "BFBayesFactor")
  html = tagAppendChildren(mainDiv, 
      tags$div(HTML(jsonobject), id = paste0(divname,"_json"), class = "BFBayesFactor_json"),
      tags$table(id = paste0(divname,"_bf"), class = "BFBayesFactor_bf"),
      tags$script(HTML(paste0("
        $(document).ready(function () {
          buildBFBayesFactor('",divname,"');
        });
      "))) 
  )
  
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
