#' @importFrom utils packageDescription
#'
.onAttach <- function(libname, pkgname) {

  desc <- packageDescription("misty")
  d1 <- desc$Version
  nk <- paste0(rep(" ", 17 - nchar(d1)))

  packageStartupMessage("|-------------------------------------|\n",
                          paste0("| ", desc$Package, " ", d1," (", substring(desc$Date, first = 1, last = 10), ")"), nk,"|\n" ,
                        "| Miscellaneous Functions T. Yanagida |\n" ,
                        "|-------------------------------------|" )

}

version <- function(pkg = "misty") {

  lib <- dirname(system.file(package = pkg))
  desc <- packageDescription(pkg)

  return(paste(desc$Package, desc$Version, desc$Date,lib))

}
