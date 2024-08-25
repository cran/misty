#' Effect Sizes for Categorical Variables
#'
#' This function computes effect sizes for one or more than one categorical
#' variable, i.e., (adjusted) phi coefficient, (bias-corrected) Cramer's \emph{V},
#' (bias-corrected) Tschuprow's \emph{T}, (adjusted) Pearson's contingency
#' coefficient, Cohen's \emph{w}), and \emph{Fei}. By default, the function
#' computes \emph{Fei} based on a chi-square goodness-of-fit test for one
#' categorical variable, phi coefficient based on a chi-square test of
#' independence for two dichotomous variables, and Cramer's \emph{V} based on a
#' chi-square test of independence for two variables with at least one polytomous
#' variable.
#'
#' @param ...         a vector, factor, matrix or data frame. Alternatively, an
#'                    expression indicating the variable names in \code{data} e.g.,
#'                    \code{as.na(x1, x2, data = dat)}. When specifying more than
#'                    one variable, the first variable is always the focal variable
#'                    in the Chi-square test of independence which association with
#'                    all other variables is investigated. Note that the operators
#'                    \code{.}, \code{+}, \code{-}, \code{~}, \code{:}, \code{::},
#'                    and \code{!} can also be used to select variables, see
#'                    'Details' in the \code{\link{df.subset}} function.
#' @param data        a data frame when specifying one or more variables in the
#'                    argument \code{...}. Note that the argument is \code{NULL}
#'                    when specifying a vector, factor, matrix, array, data frame,
#'                    or list for the argument \code{...}.
#' @param type        a character string indicating the type of effect size, i.e.,
#'                    \code{phi} for phi coefficient, \code{cramer} for Cramer's
#'                    V, \code{tschuprow} for Tschuprow’s T, \code{cont} for
#'                    Pearson's contingency coefficient, \code{w} for Cohen's w,
#'                    and \code{Fei} for Fei.
#' @param alternative a character string specifying the alternative hypothesis,
#'                    must be one of \code{"two.sided"} (default), \code{"greater"}
#'                    or \code{"less"}.
#' @param conf.level  a numeric value between 0 and 1 indicating the confidence
#'                    level of the interval.
#' @param adjust      logical: if \code{TRUE} (default), phi coefficient and
#'                    Pearson's contingency coefficient are adjusted by relating
#'                    the coefficient to the possible maximum, or Cramer's \emph{V}
#'                    and Tschuprow’s \emph{T} are corrected for small-sample bias.
#' @param indep       logical: if \code{TRUE}, effect size computation is based
#'                    on a chi-square test of independence (default when specifying
#'                    two variable in \code{...}), if \code{FALSE} effect size
#'                    computation is based on a chi-square goodness-of-fit test
#'                    (default when specifying one variable in \code{...}).
#' @param p           a numeric vector specifying the expected proportions in
#'                    each category of the categorical variable when conducting a
#'                    chi-square goodness-of-fit test. By default, the expected
#'                    proportions in each category are assumed to be equal.
#' @param digits      an integer value indicating the number of decimal places
#'                    digits to be used for displaying the results.
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to \code{NA} before
#'                    conducting the analysis.
#' @param write       a character string naming a file for writing the output
#'                    into either a text file with file extension \code{".txt"}
#'                    (e.g., \code{"Output.txt"}) or Excel file with file
#'                    extension \code{".xlsx"}  (e.g., \code{"Output.xlsx"}). If
#'                    the file name does not contain any file extension, an Excel
#'                    file will be written.
#' @param append      logical: if \code{TRUE} (default), output will be appended
#'                    to an existing text file with extension \code{.txt} specified
#'                    in \code{write}, if \code{FALSE} existing text file will be
#'                    overwritten.
#' @param check       logical: if \code{TRUE} (default), argument specification
#'                    is checked.
#' @param output      logical: if \code{TRUE} (default), output is shown on the
#'                    console.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{cor.matrix}}, \code{\link{cohens.d}}
#'
#' @references
#' Bergsma, W. (2013). A bias correction for Cramer's V and Tschuprow's T.
#' \emph{Journal of the Korean Statistical Society, 42}, 323-328.
#' https://doi.org/10.1016/j.jkss.2012.10.002
#'
#' Ben-Shachar M. S., Lüdecke D., Makowski D. (2020). effectsize: Estimation of Effect
#' Size Indices and Standardized Parameters. \emph{Journal of Open Source Software, 5}
#' (56), 2815. https://doi.org/10.21105/joss.02815
#'
#' Ben-Shachar, M. S., Patil, I., Theriault, R., Wiernik, B. M., Lüdecke, D. (2023).
#' Phi, Fei, Fo, Fum: Effect sizes for categorical data that use the chi-squared
#' statistic. \emph{Mathematics, 11}, 1982. https://doi.org/10.3390/math11091982
#'
#' Cureton, E. E. (1959). Note on Phi/Phi max. \emph{Psychometrika, 24}, 89-91.
#'
#' Davenport, E. C., & El-Sanhurry, N. A. (1991). Phi/Phimax: Review and synthesis.
#' \emph{Educational and Psychological Measurement, 51}, 821-828.
#' https://doi.org/10.1177/001316449105100403
#'
#' Sakoda, J.M. (1977). Measures of association for multivariate contingency tables.
#' \emph{Proceedings of the Social Statistics Section of the American Statistical
#' Association (Part III)}, 777-780.
#'
#' @note
#' This function is based on modified copies of the functions \code{chisq_to_phi},
#' \code{chisq_to_cramers_v}, \code{chisq_to_tschuprows_t}, \code{chisq_to_pearsons_c},
#' \code{chisq_to_cohens_w}, and \code{chisq_to_fei} from the \pkg{effectsize}
#' package (Ben-Shachar, Lüdecke & Makowski, 2020).
#'
#' @export
#'
#' @examples
#' # Example 1a: Phi coefficient for 'vs' and 'am'
#' effsize(mtcars[, c("vs", "am")])
#'
#' # Example 1a: Alternative specification using the 'data' argument
#' effsize(vs, am, data = mtcars)
#'
#' # Example 2: Bias-corrected Cramer's V for 'gear' and 'carb'
#' effsize(gear, carb, data = mtcars)
#'
#' # Example 3: Cramer's V (without bias-correction) for 'gear' and 'carb'
#' effsize(gear, carb, data = mtcars, adjust = FALSE)
#'
#' # Example 4: Adjusted Pearson's contingency coefficient for 'gear' and 'carb'
#' effsize(gear, carb, data = mtcars, type = "cont")
#'
#' # Example 5: Fei for 'gear'
#' effsize(gear, data = mtcars)
#'
#' # Example 6a: Bias-corrected Cramer's V for 'cyl' and 'vs', 'am', 'gear', and 'carb'
#' effsize(mtcars[, c("cyl", "vs", "am", "gear", "carb")])
#'
#' # Example 6b: Alternative specification using the 'data' argument
#' effsize(cyl, vs:carb, data = mtcars)
#'
#' \dontrun{
#' # Example 7b: Write Results into a text file
#' effsize(cyl, vs:carb, data = mtcars, write = "Cramer.txt")
#'
#' # Example 7b: Write Results into a Excel file
#' effsize(cyl, vs:carb, data = mtcars, write = "Cramer.xlsx")
#' }
effsize <- function(..., data = NULL, type = c("phi", "cramer", "tschuprow", "cont", "w", "fei"),
                    alternative = c("two.sided", "less", "greater"), conf.level = 0.95,
                    adjust = TRUE, indep = TRUE, p = NULL, digits = 3, as.na = NULL,
                    write = NULL, append = TRUE, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input '...' is missing
  if (isTRUE(missing(...))) { stop("Please specify the argument '...'.", call. = FALSE) }

  # Check if input '...' is NULL
  if (isTRUE(is.null(substitute(...)))) { stop("Input specified for the argument '...' is NULL.", call. = FALSE) }

  # Check if input 'data' is data frame
  if (isTRUE(!is.null(data) && !is.data.frame(data))) { stop("Please specify a data frame for the argument 'data'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data using the argument 'data' ####

  if (isTRUE(!is.null(data))) {

    # Variable names
    var.names <- .var.names(..., data = data, check.chr = "a vector, factor, matrix or data frame")

    # Extract variables
    x <- data[, var.names]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument 'data' ####

  } else {

    # Extract data
    x <- eval(..., enclos = parent.frame())

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check input 'type'
    if (isTRUE(!all(type %in% c("phi", "cramer", "tschuprow", "cont", "w", "fei")))) { stop("Character string in the argument 'type' does not match with \"phi\", \"cramer\", \"tschuprow\", \"cont\", \"w\", or \"fei\".", call. = FALSE) }

    # Check input 'alternative'
    if (isTRUE(!all(alternative %in% c("two.sided", "less", "greater")))) { stop("Character string in the argument 'alternative' does not match with \"two.sided\", \"less\", or \"greater\".", call. = FALSE) }

    # Check input 'conf.level'
    if (isTRUE(conf.level >= 1L || conf.level <= 0L)) { stop("Please specifiy a numeric value between 0 and 1 for the argument 'conf.level'.", call. = FALSE) }

    # Check input 'adjust'
    if (isTRUE(!is.logical(adjust))) { stop("Please specify TRUE or FALSE for the argument 'adjust'.", call. = FALSE) }

    # Check input 'indep'
    if (isTRUE(!is.logical(indep))) { stop("Please specify TRUE or FALSE for the argument 'indep'.", call. = FALSE) }

    # Check input 'p'
    if (isTRUE(!is.null(p)) && (!indep || is.null(dim(x)))) {

      if (isTRUE(any(apply(as.data.frame(x), 2L, function(y) length(table(y)) != length(p))))) { stop("Number of elements in 'p' does not match with the number of categories in the variable(s) specified in '...'.", call. = FALSE) }

      if (isTRUE(any(p <= 0L | p >= 1L))) { stop("Please specify expected proportions greater 0 and smaller 1 for the argument 'p'.", call. = FALSE) }

      if (isTRUE(sum(p) != 1L)) { stop("Expected proportions specified in 'p' do not add to 1.", call. = FALSE) }

    }

    # Check input 'digits'
    if (isTRUE(digits %% 1L != 0L || digits < 0L)) { stop("Specify a positive integer number for the argument 'digits'", call. = FALSE) }

    # Check input 'write'
    if (isTRUE(!is.null(write) && !is.character(write))) { stop("Please specify a character string for the argument 'write'.", call. = FALSE) }

    # Check input 'append'
    if (isTRUE(!is.logical(append))) { stop("Please specify TRUE or FALSE for the argument 'append'.", call. = FALSE) }

    # Check input 'output'
    if (isTRUE(!is.logical(output))) { stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #...................
  ### Type of analysis ####

  # Chi-Square Goodness-of-Fit Test in case of one variable
  if (isTRUE(is.null(dim(x)))) { indep <- FALSE }

  #...................
  ### Type of effect size ####

  ##### Default Setting
  if (isTRUE(all(c("phi", "cramer", "tschuprow", "cont", "w", "fei") %in% type))) {

    # Chi-Square Goodness-of-Fit Test
    if (isTRUE(!indep)) {

      type <- "fei"

    # Chi-Square Test of Independence
    } else {

      # All dichotomous variables
      if (isTRUE(all(apply(x, 2L, function(y) length(unique(y)) == 2L)))) {

        type <- "phi"

      # At least one polytomous variables
      } else {

        type <- "cramer"

      }

    }

  ##### User-Defined Setting
  } else {

    # Chi-Square Goodness-of-Fit Test
    if (isTRUE(!indep)) {

      # Phi
      if (isTRUE(type == "phi")) { stop("Phi is only available for chi-square test of independence.", call. = FALSE) }

      # Cramer's V
      if (isTRUE(type == "cramer")) { stop("Cramer's V is only available for chi-square test of independence.", call. = FALSE) }

      # Tschuprow's T
      if (isTRUE(type == "tschuprow")) { stop("Tschuprow's T is only available for chi-square test of independence.", call. = FALSE) }

    # Chi-Square Test of Independence
    } else {

      # Phi
      if (isTRUE(any(apply(x, 2L, function(y) length(na.omit(unique(y))) != 2L)) && type == "phi")) { stop("Phi is only available for dichotomous variables.", call. = FALSE) }

      # Fei
      if (isTRUE(indep && type == "fei")) { stop("Fei is only available for chi-square goodness-of-fit test.", call. = FALSE) }

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Alternative hypothesis ####

  if (isTRUE(all(c("two.sided", "less", "greater") %in% alternative))) { alternative <- "two.sided" }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## One variable ####

  #...................
  ### Chi-Square Goodness-of-Fit Test ####

  if (isTRUE(!indep && is.null(dim(x)))) {

    # Cohen's W
    switch(type, w = {

      result <- .cohen.w(x, adjust = FALSE, p = p, conf.level = conf.level, alternative = alternative)

    # Pearson's contingency coefficient
    }, cont = {

      result <- .cont(x, adjust = FALSE, p = p, conf.level = conf.level, alternative = alternative)

    # Fei
    }, fei = {

      result <- .fei(x, adjust = FALSE, p = p, conf.level = conf.level, alternative = alternative)

    })

  #...................
  ### Chi-Square Test of Independence ####

  } else if (isTRUE(indep && ncol(x) == 2L)) {

    # Phi
    switch(type, phi =  {

      result <- .phi(x, adjust = adjust, p = NULL, conf.level = conf.level, alternative = alternative)

    # Cramer's V
    }, cramer = {

      result <- .cramer(x, adjust = adjust, conf.level = conf.level, alternative = alternative)

    # Tschuprow's T
    }, tschuprow = {

      result <- .tschuprow(x, adjust = adjust, conf.level = conf.level, alternative = alternative)

    # Pearson's contingency coefficient
    }, cont = {

      result <- .cont(x, adjust = adjust, p = NULL, conf.level = conf.level, alternative = alternative)

    # Cohen's w
    }, w = {

      result <- .cohen.w(x, adjust = FALSE, p = NULL, conf.level = conf.level, alternative = alternative)

    # Fei
    }, fei = {

      result <- .fei(x, adjust = FALSE, p = NULL, conf.level = conf.level, alternative = alternative)

    })

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## More than Two Variables ####
  } else {

    #...................
    ### Chi-Square Goodness-of-Fit Test ####

    if (isTRUE(!indep)) {

      result <- data.frame(var = colnames(x), do.call("rbind", lapply(x, function(y) misty::effsize(y, type = type, adjust = adjust, indep = indep, p = p, alternative = alternative, check = FALSE, output = FALSE)$result)), row.names = NULL)

    #...................
    ### Chi-Square Test of Independence ####

    } else {

      result <- list()
      for (i in seq_len(ncol(x))[-1L]) {

        result[[i]] <- misty::effsize(x[, c(1L, i)], type = type, adjust = adjust, indep = TRUE, alternative = alternative, check = FALSE, output = FALSE)$result

      }

      result <- data.frame(var = colnames(x)[-1L], do.call("rbind", result))

    }

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  # Return object
  object <- list(call = match.call(),
                 type = "effsize",
                 data = x,
                 args = list(type = type, alternative = alternative, conf.level = conf.level,
                             adjust = adjust, indep = indep, p = p, digits = digits,
                             as.na = as.na, write = write, append = append, check = check,
                             output = output),
                 result = result)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Write Results --------------------------------------------------------------

  if (isTRUE(!is.null(write))) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Text file ####

    if (isTRUE(grepl("\\.txt", write))) {

      # Send R output to textfile
      sink(file = write, append = ifelse(isTRUE(file.exists(write)), append, FALSE), type = "output", split = FALSE)

      if (isTRUE(append && file.exists(write))) { write("", file = write, append = TRUE) }

      # Print object
      print(object, check = FALSE)

      # Close file connection
      sink()

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Excel file ####

    } else {

      misty::write.result(object, file = write)

    }

  }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
