#' @importFrom utils packageDescription
#'
.onAttach <- function(libname, pkgname) {

  desc <- packageDescription("misty")
  d1 <- desc$Version
  nk <- paste0(rep(" ", 17L - nchar(d1)))

  if (isTRUE(is.null(getOption("knitr.in.progress")) && .Platform$GUI == "RStudio")) {

    # Unicode for vertical bar: https://stackoverflow.com/questions/10572627/vertical-bar-unicode-replacement
    packageStartupMessage("", "\U0007C", paste(rep("\U203E", times = 39), collapse = ""), "\U0007C", "\n",
                          "", paste0("\U0007C", "  ", desc$Package, " ", d1," (", substring(desc$Date, first = 1, last = 10), ")"), nk, " ", "\U0007C", "\n" ,
                          "", "\U0007C", "  Miscellaneous Functions T. Yanagida  ", "\U0007C", "\n" ,
                          "", "\U0007C", paste(rep("\u005F", times = 39), collapse = ""), "\U0007C")

    } else {

    packageStartupMessage("|-------------------------------------|\n",
                          paste0("| ", desc$Package, " ", d1," (", substring(desc$Date, first = 1, last = 10), ")"), nk,"|\n" ,
                          "| Miscellaneous Functions T. Yanagida |\n" ,
                          "|-------------------------------------|" )

  }

}
