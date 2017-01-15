##' knitr print method for BFBayesFactor objects
##'
##' knitr print method for BFBayesFactor objects
##' @title Print interactive HTML/JavaScript Bayes factors
##' @param x a BFBayesFactor object
##' @param ... further arguments to be passed to or from methods.
##' @return The function code to be inserted into Rmarkdown documents.
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

  hoverHelpFile = system.file("etc", "html", "hoverhelp.html", package = "BayesFactorExtras")
  hoverHelpCode = readChar(hoverHelpFile, file.info(hoverHelpFile)$size)

  modelType = class(x@denominator)[1]
  analysisType = x@denominator@type

  mainDiv = tags$div(id = divname, class = "BFBayesFactor")
  mainTable = tags$table(id = paste0(divname,"_bf"), class = "BFBayesFactor_bf")
  mainTable = tagAppendChildren(mainTable,
      tagAppendChildren(tags$thead(class = "bfhead"),
            tagAppendChildren(tags$tr(class = "bfhrow"),
                tags$th(class = "bfcolunsorted bfhmodel"),
                tags$th(class = "bfcolunsorted bfhbf"),
                tags$th(class = "bfcolunsorted bfherr")
            ),tagAppendChildren(tags$tr(class = "bfhtitles"),
                                tags$th("...the model below..."),
                                tags$th("...is preferred by..."),
                                tags$th("")
            )
      ),
      tags$tbody(class = "bfbody")
  )
  html = tagAppendChildren(mainDiv,
      tags$div(HTML(jsonobject), id = paste0(divname,"_json"), class = "BFBayesFactor_json bfhide"),
      tags$div(HTML(analysisType), id = paste0(divname,"_analysistype"), class = "BFBayesFactor_analysistype bfhide"),
      tags$div(HTML(modelType), id = paste0(divname,"_modeltype"), class = "BFBayesFactor_modeltype bfhide"),
      tagAppendChildren(tags$div(class="denomccontainer"),
        tags$span("When compared against the model ", class="denomdescription"),
        tags$span(id = paste0(divname,"_denom"), class = "BFBayesFactor_denom"),
        tags$span("...", class="denomdescription")
      ),
      tagAppendChildren(tags$div(class="searchcontainer"),
                        tags$div(HTML(hoverHelpCode), class="BFBayesFactor_hoverhelp bfhide"),
                        tags$span(HTML("&nbsp;"),class="BFBayesFactor_hoverhelpicon"),
                        tags$input(id = paste0(divname,"_search"), class = "BFBayesFactor_search")
      ),
      mainTable,
      tags$script(HTML(paste0("
        $(document).ready(function () {
          buildBFBayesFactor('",divname,"');
        });
      ")))
  )

  if(getOption('BFEknitrDownload', TRUE)){
    DLtag = createDownloadURI("x", filename = "BayesFactorObject", textHTML = "Click here to download this BayesFactor object.", envir = parent.frame(), printHTML = FALSE)
    html = tagList(html, DLtag)
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
