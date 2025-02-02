#' Open new R Script, R Markdown script, or SQL Script in RStudio
#'
#' This function opens a new R script, R markdown script, or SQL script in RStudio.
#
#' @param text     a character vector indicating what text should be inserted in
#'                 the new R script. By default, an empty script is opened.
#' @param type     a character string indicating the type of document to be
#'                 created, i.e., \code{r} (default) for an R script, \code{rmakrdown}
#'                 for an R Markdown file, or \code{sql} for an SQL script.
#' @param position \code{document_position()} function in the \pkg{rstudioapi}
#'                 package indicating the cursor position.
#' @param run      logical: if \code{TRUE}, the code is executed after the document
#'                 is created.
#' @param check    logical: if \code{TRUE} (default), argument specification is
#'                 checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{script.close}}, \code{\link{script.open}}, \code{\link{script.save}},
#' \code{\link{script.copy}}, \code{\link{setsource}}
#'
#' @references
#' Ushey, K., Allaire, J., Wickham, H., & Ritchie, G. (2023). \emph{rstudioapi: Safely
#' access the RStudio API}. R package version 0.15.0
#' https://CRAN.R-project.org/package=rstudioapi
#'
#' @note
#' This function uses the \code{documentNew()} function in the \pkg{rstudioapi}
#' package by Kevin Ushey, JJ Allaire, Hadley Wickham, and Gary Ritchie (2023).
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Example 1: Open new R script file
#' script.new()
#'
#' # Example 2: Open new R script file and run some code
#' script.new("#----------------------------
#' # Example
#'
#' # Generate 100 random numbers
#' rnorm(100)")
#' }
script.new <- function(text = "", type = c("r", "rmarkdown", "sql"),
                       position = rstudioapi::document_position(0, 0),
                       run = FALSE, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = "run", s.character = list(type = c("r", "rmarkdown", "sql")), package = "rstudioapi", envir = environment(), input.check = check)

  # Check input 'text'
  if (isTRUE(check)) { if (isTRUE(!is.character(text))) { stop("Please specify a character vector for the argument 'text'.", call. = FALSE) } }

  #_____________________________________________________________________________
  #
  # Data and Arguments ---------------------------------------------------------

  type <- ifelse (all(c("r", "rmarkdon", "sql") %in% type), "r", type)

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  # Open new R script
  invisible(rstudioapi::documentNew(text = text, type = type, position = position, execute = run))

}

#_______________________________________________________________________________
