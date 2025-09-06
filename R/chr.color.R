#' Colored and Styled Terminal Output Text
#'
#' This function adds color and style to output texts on terminals that support
#' 'ANSI' color and highlight codes that can be printed by using the \code{cat}
#' function.
#'
#' @param x      a character vector.
#' @param color  a character string indicating the text color, e.g., \code{red}
#'               for red and \code{b.red} for bright red text.
#' @param bg     a character string indicating the background color of the text,
#'               e.g., \code{red} for red background.
#' @param style  a character vector indicating the font style, i.e., \code{regular},
#'               (default) for regular text, \code{bold} for bold text, \code{italic},
#'               for italic text, and \code{underline} for underline text. Note
#'               that font styles can be combined, e.g., \code{style = c("bold", "italic")}
#'               provides a bold and italic text.
#' @param check  logical: if \code{TRUE} (default), argument specification is
#'               checked.
#'
#' @author
#' Takuya Yanagida
#'
#' @seealso
#' \code{\link{chr.grep}}, \code{\link{chr.grepl}}, \code{\link{chr.gsub}},
#' \code{\link{chr.omit}},  \code{\link{chr.trim}}, \code{\link{chr.trunc}}
#'
#' @references
#' Csárdi G (2022). \emph{crayon: Colored Terminal Output}. R package version 1.5.2,
#' https://CRAN.R-project.org/package=crayon
#'
#' @return
#' Returns a character vector.
#'
#' @note This function is based on functions provided in the \pkg{crayon} package
#' by Gábor Csárdi.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Example 1:
#' cat(chr.color("Text in red.", color = "red"))
#'
#' # Example 2:
#' cat(chr.color("Text in blue with green background.",
#'               color = "blue", bg = "yellow"))
#'
#' # Example 3a:
#' cat(chr.color("Text in boldface.", style = "bold"))
#'
#' # Example 3b:
#' cat(chr.color("Text in boldface and italic.", style = c("bold", "italic")))
#'
#' }
chr.color <- function(x,
                      color = c("black", "red", "green", "yellow", "blue", "violet", "cyan", "white", "gray1", "gray2", "gray3",
                                "b.red", "b.green", "b.yellow", "b.blue", "b.violet", "b.cyan", "b.white"),
                      bg = c("none", "black", "red", "green", "yellow", "blue", "violet", "cyan", "white"),
                      style = c("regular", "bold", "italic", "underline"),
                      check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'x' is missing or NULL
  if (isTRUE(missing(x) || is.null(x))) { stop("Please specify a numeric vector, character vector or factor for the argument 'x'", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs 'style'
  .check.input(m.character = list(style = c("regular", "bold", "italic", "underline")), envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(check)) {

    # Check input 'color'
    if (isTRUE(!all(color %in% c("black", "red", "green", "yellow", "blue", "violet", "cyan", "white", "gray1", "gray2", "gray3", "b.red", "b.green", "b.yellow", "b.blue", "b.violet", "b.cyan", "b.white")))) { stop("Character string in the argument 'color' does not match with \"black\", \"red\", \"green\", \"yellow\", \"blue\", \"violet\", \"cyan\", \"white\", \"gray1\" etc.", call. = FALSE) }

    # Check input 'bg'
    if (isTRUE(!all(bg %in% c("none", "black", "red", "green", "yellow", "blue", "violet", "cyan", "white")))) { stop("Character string in the argument 'bg' does not match with \"black\", \"red\", \"green\", \"yellow\", \"blue\", \"violet\", \"cyan\", or \"white\".", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## color Argument ####

  if (isTRUE(all(c("black", "red", "green", "yellow", "blue", "violet", "cyan", "white", "gray1", "gray2", "gray3", "b.red", "b.green", "b.yellow", "b.blue", "b.violet", "b.cyan", "b.white") %in% color))) { color <- "black" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## bg Argument ####

  if (isTRUE(all(c("black", "red", "green", "yellow", "blue", "violet", "cyan", "white") %in% bg))) { bg <- "none" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## style Argument ####

  if (isTRUE(all(c("regular", "bold", "italic", "underline") %in% style))) { style <- "regular" }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Color ####

  switch(color,
         black =     { x[!is.na(x)] <- paste0("\033[30m", x[!is.na(x)], "\033[39m") },
         red =       { x[!is.na(x)] <- paste0("\033[31m", x[!is.na(x)], "\033[39m") },
         green =     { x[!is.na(x)] <- paste0("\033[32m", x[!is.na(x)], "\033[39m") },
         yellow =    { x[!is.na(x)] <- paste0("\033[33m", x[!is.na(x)], "\033[39m") },
         blue =      { x[!is.na(x)] <- paste0("\033[34m", x[!is.na(x)], "\033[39m") },
         violet =    { x[!is.na(x)] <- paste0("\033[35m", x[!is.na(x)], "\033[39m") },
         cyan =      { x[!is.na(x)] <- paste0("\033[36m", x[!is.na(x)], "\033[39m") },
         white =     { x[!is.na(x)] <- paste0("\033[37m", x[!is.na(x)], "\033[39m") },
         gray1 =     { x[!is.na(x)] <- paste0("\033[90m", x[!is.na(x)], "\033[39m") },
         gray2 =     { x[!is.na(x)] <- paste0("\033[0;37m", x[!is.na(x)], "\033[0m") },
         gray3 =     { x[!is.na(x)] <- paste0("\033[0;97m", x[!is.na(x)], "\033[0m") },
         b.red =     { x[!is.na(x)] <- paste0("\033[91m", x[!is.na(x)], "\033[39m") },
         b.green =   { x[!is.na(x)] <- paste0("\033[92m", x[!is.na(x)], "\033[39m") },
         b.yellow =  { x[!is.na(x)] <- paste0("\033[93m", x[!is.na(x)], "\033[39m") },
         b.blue =    { x[!is.na(x)] <- paste0("\033[94m", x[!is.na(x)], "\033[39m") },
         b.violet =  { x[!is.na(x)] <- paste0("\033[95m", x[!is.na(x)], "\033[39m") },
         b.cyan =    { x[!is.na(x)] <- paste0("\033[96m", x[!is.na(x)], "\033[39m") },
         b.white =   { x[!is.na(x)] <- paste0("\033[97m", x[!is.na(x)], "\033[39m") })

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Background ####

  if (isTRUE(bg != "none")) {

    switch(bg,
           black =  { x[!is.na(x)] <- paste0("\033[40m", x[!is.na(x)], "\033[49m") },
           red =    { x[!is.na(x)] <- paste0("\033[41m", x[!is.na(x)], "\033[49m") },
           green =  { x[!is.na(x)] <- paste0("\033[42m", x[!is.na(x)], "\033[49m") },
           yellow = { x[!is.na(x)] <- paste0("\033[43m", x[!is.na(x)], "\033[49m") },
           blue =   { x[!is.na(x)] <- paste0("\033[44m", x[!is.na(x)], "\033[49m") },
           violet = { x[!is.na(x)] <- paste0("\033[45m", x[!is.na(x)], "\033[49m") },
           cyan =   { x[!is.na(x)] <- paste0("\033[46m", x[!is.na(x)], "\033[49m") },
           white =  { x[!is.na(x)] <- paste0("\033[47m", x[!is.na(x)], "\033[49m") })

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Style ####

  if (isTRUE("bold" %in% style))      { x[!is.na(x)] <- paste0("\033[1m", x[!is.na(x)], "\033[22m") }
  if (isTRUE("italic" %in% style))    { x[!is.na(x)] <- paste0("\033[3m", x[!is.na(x)], "\033[23m") }
  if (isTRUE("underline" %in% style)) { x[!is.na(x)] <- paste0("\033[4m", x[!is.na(x)], "\033[34m") }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(x)

}

#_______________________________________________________________________________
