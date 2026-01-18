#' Centering Predictor Variables in Single-Level and Multilevel Data
#'
#' This function centers predictor variables in single-level data, two-level data,
#' and three-level data at the grand mean (CGM, i.e., grand mean centering) or
#' within clusters (CWC, i.e., group mean centering).
#'
#' @param data     a numeric vector for centering a predictor variable, or a
#'                 data frame for centering more than one predictor variable.
#' @param ...      an expression indicating the variable names in \code{data} e.g.,
#'                 \code{center(dat, x1, x2)} for centering the variables \code{x1}
#'                 and \code{x2} in the data frame \code{dat}. Note that the
#'                 \code{+}, \code{-}, \code{~}, \code{:}, \code{::}, and \code{!}
#'                 can also be used to select variables, see 'Details' in the
#'                 \code{\link{df.subset}} function.
#' @param cluster  a character string indicating the name of the cluster variable
#'                 in \code{data} for a two-level model (e.g., \code{cluster = "level2"}),
#'                 a character vector indicating the names of the cluster variables
#'                 in \code{data} for a three-level model (e.g., \code{cluster = c("level3", "level2")}),
#'                 or a vector (e.g., \code{data$level2}) or data frame
#'                 (e.g., \code{data[, c("level3", "level2"]}) representing
#'                 the nested grouping structure (i.e., group or cluster variables).
#'                 Note that the cluster variable at Level 3 come first in a
#'                 three-level model, i.e., \code{cluster = c("level3", "level2")}.
#' @param type     a character string indicating the type of centering, i.e.,
#'                 \code{"CGM"} for centering at the grand mean (i.e., grand mean
#'                 centering, default when \code{cluster = NULL}), \code{"CWC"}
#'                 for centering within clusters (i.e., group mean centering, default
#'                 when specifying the argument \code{cluster}, or \code{"latent"}
#'                 for the two-step latent mean centering method (see 'Details').
#'                 Note that two-step latent mean centering method can only be
#'                 applied to one predictor variable at a time.
#' @param cwc.mean a character string indicating the type of centering of a level-1
#'                 predictor variable in a three-level model, i.e., \code{L2}
#'                 (default) for centering the predictor variable at the level-2
#'                 cluster means, and  \code{L3} for centering the predictor
#'                 variable at the level-3 cluster means. Note that this argument
#'                 is only used when specifying two cluster variables for the
#'                 argument \code{"cluster"}.
#' @param value    a numeric value for centering on a specific user-defined value.
#'                 Note that this option is only available when specifying predictor
#'                 variables in single-level data i.e., \code{cluster = NULL}.
#' @param name     a character string or character vector indicating the names of
#'                 the centered predictor variables. By default, centered predictor
#'                 variables are named with the ending \code{".c"} resulting in
#'                 e.g. \code{"x1.c"} and \code{"x2.c"}. Variable names can also
#'                 be specified by using a character vector matching the number
#'                 of variables specified in \code{...} (e.g., \code{name = c("center.x1", "center.x2")}).
#'                 Note that when specifying \code{type = "latent"}, centered
#'                 predictor variables in a two-level model are named with the
#'                 endings \code{".l1"} and \code{".l2"} (e.g., \code{name = c("x.l1", "x.l2")}),
#'                 while centered predictor variables in a three-level model are
#'                 named with the endings \code{".l1"}, \code{".l2"}, and \code{".l3"}
#'                 (e.g., \code{name = c("x.l1", "x.l2", "x.l3")}) by default.
#'                 Alternatively, a character vector of length 2 for centered
#'                 predictor variables in a two-level model or a character vector
#'                 of length 3 centered predictor variables in a three-level model
#'                 can be specified.
#' @param append   logical: if \code{TRUE} (default), centered predictor variable(s)
#'                 are appended to the data frame specified in the argument \code{data}.
#' @param as.na    a numeric vector indicating user-defined missing values, i.e.
#'                 these values are converted to \code{NA} before conducting the
#'                 analysis. Note that \code{as.na()} function is only applied to
#'                 \code{...} but not to \code{cluster}.
#' @param check    logical: if \code{TRUE} (default), argument specification is
#'                 checked.
#'
#' @details
#' \strong{Single-Level Data}
#'
#' Predictor variables are centered at the grand mean (CGM) by default:
#'
#' \deqn{x_{i} - \bar{x}_{.}}
#'
#' where \eqn{x_{i}} is the predictor value of observation \eqn{i} and
#' \eqn{\bar{x}_{.}} is the average \eqn{x} score. Note that predictor variables
#' can be centered on any meaningful value specifying the argument \code{value},
#' e.g., a predictor variable centered at 5 by applying following formula:
#'
#'   \deqn{x_{i} - \bar{x}_{.} + 5}
#'
#' resulting in a mean of the centered predictor variable of 5.
#'
#' \strong{Two-Level Data}
#'
#' In two-level data, there are predictor variables at Level-1 (L1) and Level-2 (L2)
#' with L1 predictor variables centered within L2 clusters (CWC) and L2 predictors
#' centered at the average L2 cluster scores (CGM) by default:
#' \itemize{
#'   \item{\strong{Level-1 (L1) Predictor Variables}}: L1 predictor variable can be
#'   centered within L2 clusters (CWC) or at the average L2 cluster scores (CGM):
#'     \itemize{
#'       \item L1 predictor variables are centered within L2 clusters by specifying
#'       \code{type = "CWC"} (Default):
#'
#'       \deqn{x_{ij} - \bar{x}_{.j}}
#'
#'       where \eqn{\bar{x_{.j}}} is the average \eqn{x} score in cluster \eqn{j}.
#'
#'       \item L1 predictor variables are centered at the grand-mean by specifying
#'       \code{type = "CGM"}:
#'
#'       \deqn{x_{ij} - \bar{x}_{..}}
#'
#'       where \eqn{x_{ij}} is the predictor value of observation \eqn{i} in L2 cluster
#'       \eqn{j} and \eqn{\bar{x}_{..}} is the average \eqn{x} score.
#'     }
#'
#'   \item{\strong{Level-2 (L2) Predictor Variables}}: L2 predictor variables are
#'   centered at the average L2 cluster score:
#'
#'   \deqn{x_{.j} - \bar{x}_{..}}
#'
#'   where \eqn{x_{.j}} is the predictor value of L2 cluster \eqn{j} and
#'   \eqn{\bar{x}_{..}} is the average L2 cluster score. Note that the cluster
#'   membership variable needs to be specified when centering a L2 predictor
#'   variable in two-level data. Otherwise the average \eqn{x_{ij}} individual
#'   score instead of the average \eqn{x_{.j}} cluster score is used to center
#'   the predictor variable.
#' }
#'
#' \strong{Three-Level Data}
#'
#' In three-level data, there are predictor variables at Level-1 (L1), Level-2 (L2),
#' and Level-3 (L3) with L1 predictor variables centered within L2 clusters (CWC L2),
#' L2 predictors centered within L3 clusters (CWC L3), and L3 predictors centered at
#' the average L3 cluster scores (CGM) by default:
#'   \itemize{
#'     \item{\strong{Level-1 (L1) Predictor Variables}}:
#'      L1 predictor variables can be centered within L2 clusters (CWC L2), within
#'      L3 clusters (CWC L3) or at the grand-mean (CGM):
#'       \itemize{
#'         \item L1-predictor variables are centered within cluster (CWC) by specifying
#'         \code{type = "CWC"} (Default). Note that L1 predictor variables can be either
#'         centered within L2 clusters (\code{cwc.mean = "L2"}, Default, see
#'         Brincks et al., 2017):
#'
#'         \deqn{x_{ijk} - \bar{x}_{.jk}}
#'
#'         or within L3 clusters (\code{cwc.mean = "L3"}, see Enders, 2013):
#'
#'         \deqn{x_{ijk} - \bar{x}_{..k}}
#'
#'         where \eqn{\bar{x}_{.jk}} is the average \eqn{x} score in L2 cluster
#'         \eqn{j} within Level-3 cluster \eqn{k} and \eqn{\bar{x}_{..k}} is the
#'         average \eqn{x} score in L3 cluster \eqn{k}.
#'
#'         \item L1 predictor variables are centered at the grand mean (CGM) by
#'         specifying \code{type = "CGM"}:
#'
#'         \deqn{x_{ijk} - \bar{x}_{...}}
#'
#'         where \eqn{x_{ijk}} is the predictor value of observation \eqn{i} in L2
#'         cluster \eqn{j} within L3 cluster \eqn{k} and \eqn{\bar{x}_{...}} is
#'         the average \eqn{x} score.
#'       }
#'
#'       \item{\strong{Level-2 (L2) Predictor Variables}}:
#'       L2 predictor variables can be centered within L3 clusters (CWC) or at the
#'       L2 grand-mean (CGM):
#'         \itemize{
#'           \item L2 predictor variables are centered within cluster by specifying
#'           \code{type = "CWC"} (Default):
#'
#'           \deqn{x_{.jk} - \bar{x}_{..k}}
#'
#'           where \eqn{\bar{x}_{..k}} is the average \eqn{x} score in L3 cluster
#'           \eqn{k}.
#'
#'           \item L2 predictor variables are centered at the grand mean by specifying
#'           \code{type = "CGM"}:
#'
#'           \deqn{x_{.jk} - \bar{x}_{...}}
#'
#'           where \eqn{x_{.jk}} is the predictor value of L2 cluster \eqn{j} within
#'           L3 cluster \eqn{k} and \eqn{\bar{x}_{...}} is the average L2 cluster score.
#'         }
#'
#'       \item{\strong{Level-3 (L3) Predictor Variables}}: L3-predictor variables
#'       are centered at the L3 grand mean:
#'
#'       \deqn{x_{..k} - \bar{x}_{...}}
#'
#'       where \eqn{x_{..k}} is the predictor value of L3 cluster \eqn{k} and
#'       \eqn{\bar{x}_{...}} is the average L3 cluster score.
#'   }
#'
#' \strong{Two-Step Latent Mean Centering}
#' The latent mean centering approach (Asparouhov & Muthén, 2019) in a two-level
#' model decomposes the Level-1 predictor variable \eqn{x_{ij}} as within and
#' between compoments as follows:
#'
#' \deqn{x_{ij} = x_{w,ij} + x_{b,.j}}
#'
#' where \eqn{x_{w,ij}} is the individual specific contribution and \eqn{x_{b,.j}}
#' is the cluster specific contribution to the predictor variable \eqn{x_{ij}}.
#' Here, \eqn{x_{b,.j}} can be interpreted as the intercepts and \eqn{x_{w,ij}}
#' can be interpreted as the residuals in the random intercept model. Note that
#' \eqn{x_{w,ij}} is equivalent to a L1 predictor centered within L2 clusters (CWC),
#' while \eqn{x_{b,.j}} is equivalent to a L2 predictor centered at the average
#' L2 cluster scores (CGM).
#' Latent mean centering treats \eqn{x_{b,.j}} as unknown quantity that is estimated
#' while taking into the sampling error in the mean estimate under the assumption
#' of large cluster sizes in the population and less than 5% of the cluster
#' population sampled. As a result, this approach resolves problems that occur
#' with the traditional observed centering methods, e.g., Lüdtke's bias (Lüdtke
#' et al., 2008) in the estimation of contextual effects or Nickell's bias
#' (Asparouhov et al., 2018) in the estimation of the autocorrelations in
#' time-series models.
#' The latent mean centering approach requires a latent variable modeling program,
#' e.g., commercial software Mplus (Muthen & Muthen, 1998-2017) or the R package
#' lavaan (Rosseel, 2012) and cannot be used in mixed-effects modeling programs
#' like lme4 (Bates et al., 2015) or nlme (Pinheiro & Bates, 2000). In order to
#' mimic the latent mean centering approach, a two-step approach is proposed in
#' the \code{center()} function, where a random intercept model is fit to the
#' L1 predictor variable to extract the intercepts representing \eqn{x_{b,.j}}
#' and residuals representing \eqn{x_{w,ij}}. These two components can be used as
#' L1 predictor centered within clusters and L2 predictor centered at the grand
#' mean. Note that compared to the latent mean centering approach, this two-step
#' approach will result in bias because \eqn{x_{w,ij}} and \eqn{x_{b,.j}} are
#' treated as observed instead of latent variables. However, the magnitude of the
#' bias is unclear without conducting a simulation study. Hence, the latent mean
#' centering using a latent variable modeling program is recommended whenever
#' possible, while the two-step latent mean centering approach implemented in the
#' \code{center())} function is just an 'experimental' approach that cannot be
#' recommend at this time.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{coding}}, \code{\link{cluster.scores}}, \code{\link{rec}},
#' \code{\link{item.reverse}}, \code{\link{cluster.rwg}}, \code{\link{item.scores}}.
#'
#' @references
#' Asparouhov, T., Hamaker, E. L., & Muthén, B. (2017). Dynamic Structural Equation Models.
#' \emph{Structural Equation Modeling: A Multidisciplinary Journal, 25}(3), 359-388.
#' https://doi.org/10.1080/10705511.2017.1406803
#'
#' Asparouhov, T., & Muthén, B. (2019). Latent variable centering of predictors
#' and mediators in multilevel and time-series models. \emph{Structural Equation Modeling, 26}(1),
#' 119-142. https://doi.org/10.1080/10705511.2018.1511375
#'
#' Bates, D., Mächler, M., Bolker, B., & Walker, S. (2015). Fitting linear mixed-effects
#' models using lme4. \emph{Journal of Statistical Software, 67}(1), 1–48.
#' https://doi.org/10.18637/jss.v067.i01
#'
#' Brincks, A. M., Enders, C. K., Llabre, M. M., Bulotsky-Shearer, R. J., Prado, G.,
#' & Feaster, D. J. (2017). Centering predictor variables in three-level contextual
#' models. \emph{Multivariate Behavioral Research, 52}(2), 149–163.
#' https://doi.org/10.1080/00273171.2016.1256753
#'
#' Chang, C.-N., & Kwok, O.-M. (2022) Partitioning Variance for a Within-Level
#' Predictor in Multilevel Models. \emph{Structural Equation Modeling: A
#' Multidisciplinary Journal}. Advance online publication.
#' https://doi.org/10.1080/10705511.2022.2051175
#'
#' Enders, C. K. (2013). Centering predictors and cont
#' extual effects. In M. A.
#' Scott, J. S. Simonoff, & B. D. Marx (Eds.), \emph{The Sage handbook of
#' multilevel modeling} (pp. 89-109). Sage. https://dx.doi.org/10.4135/9781446247600
#'
#' Enders, C. K., & Tofighi, D. (2007). Centering predictor variables in
#' cross-sectional multilevel models: A new look at an old issue. \emph{Psychological
#' Methods, 12}, 121-138. https://doi.org/10.1037/1082-989X.12.2.121
#'
#' Lüdtke, O., Marsh, H. W., Robitzsch, A., Trautwein, U., Asparouhov, T., & Muthén,
#' B. (2008). The multilevel latent covariate model: A new, more reliable approach
#' to group-level effects in contextual studies. \emph{Psychological Methods, 13}(3),
#' 203-229. https://doi.org/10.1037/a0012869
#'
#' Muthén, L. K., & Muthén, B. O. (1998-2017). \emph{Mplus User’s Guide} (8th ed).
#' Muthén & Muthén.
#'
#' Pinheiro, J. C., & Bates, D. M. (2000). \emph{Mixed-Effects Models in S and S-PLUS}.
#' Springer. https://doi.org/10.1007/b98882
#'
#' Rights, J. D., Preacher, K. J., & Cole, D. A. (2020). The danger of conflating
#' level-specific effects of control variables when primary interest lies in
#' level-2 effects. \emph{British Journal of Mathematical & Statistical Psychology,
#' 73}, 194-211. https://doi.org/10.1111/bmsp.12194
#'
#' Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling.
#' \emph{Journal of Statistical Software, 48}(2), 1-36. https://doi.org/10.18637/jss.v048.i02
#'
#' Yaremych, H. E., Preacher, K. J., & Hedeker, D. (2021). Centering categorical
#' predictors in multilevel models: Best practices and interpretation.
#' \emph{Psychological Methods}. Advance online publication.
#' https://doi.org/10.1037/met0000434
#'
#' @return
#' Returns a numeric vector or data frame with the same length or same number of
#' rows as \code{data} containing the centered variable(s).
#'
#' @export
#'
#' @examples
#' #----------------------------------------------------------------------------
#' # Single-Level Data
#'
#' # Example 1a: Center predictor 'disp' at the grand mean
#' center(mtcars, disp, append = FALSE)
#'
#' # Alternative specification without using the '...' argument
#' center(mtcars$disp)
#'
#' # Example 1b: Center predictors 'disp' and 'hp' at the grand mean and append to 'mtcars'
#' center(mtcars, disp, hp)
#'
#' # Alternative specification without using the '...' argument
#' cbind(mtcars, center(mtcars[, c("disp", "hp")]))
#'
#' # Example 1c: Center predictor 'disp' at the value 3
#' center(mtcars, disp, value = 3)
#'
#' # Example 1d: Center predictors 'disp' and 'hp' and label with the suffix ".v"
#' center(mtcars, disp, hp, name = ".v")
#'
#' #----------------------------------------------------------------------------
#' # Two-Level Data
#'
#' # Load data set "Demo.twolevel" in the lavaan package
#' data("Demo.twolevel", package = "lavaan")
#'
#' #.........................................
#' # Level-1 (L1) Predictor
#'
#' # Example 2a: Center L1 predictor 'y1' within L2 clusters
#' center(Demo.twolevel, y1, cluster = "cluster", append = FALSE)
#'
#' # Alternative specification without using the '...' argument
#' center(Demo.twolevel$y1, cluster = Demo.twolevel$cluster)
#'
#' # Example 2b: Center L1 predictor 'y1' at the grand-mean
#' #             Note that cluster ID is ignored when type = "CGM"
#' center(Demo.twolevel, y1, cluster = "cluster", type = "CGM")
#'
#' # Alternative specification
#' center(Demo.twolevel, y1)
#'
#' #.........................................
#' # Level-2 (L2) Predictor
#'
#' # Example 2c: Center L2 predictor 'w2' at the average L2 cluster scores
#' #             Note that cluster ID is needed
#' center(Demo.twolevel, w1, cluster = "cluster")
#'
#' #.........................................
#' # L1 and L2 Predictors
#'
#' # Example 2d: Center L1 predictor 'y1' within L2 clusters
#' #             and L2 predictor 'w1' at the average L2 cluster scores
#' center(Demo.twolevel, y1, w1, cluster = "cluster")
#'
#' #.........................................
#' # Twp-Step Latent Mean Centering
#'
#' # Example 2e: Decompose L1 predictor 'y1' as within-between components
#' center(Demo.twolevel, y1, cluster = "cluster", type = "latent")
#'
#' # Example 2d: Decompose L1 predictor 'y1' as within-between components
#' #             label variables as 'l1.y1' and 'l2.y1'
#' center(Demo.twolevel, y1, cluster = "cluster", type = "latent", name = c("l1.y1", "l2.y1"))
#'
#' \dontrun{
#' #----------------------------------------------------------------------------
#' # Three-Level Data
#'
#' # Load data set "Demo.twolevel" in the lavaan package
#' data("Demo.twolevel", package = "lavaan")
#'
#' # Create arbitrary three-level data
#' Demo.threelevel <- data.frame(Demo.twolevel, cluster2 = Demo.twolevel$cluster,
#'                                              cluster3 = rep(1:10, each = 250))
#'
#' # Compute L3 cluster scores for the L2 predictor 'w1'
#' Demo.threelevel <- cluster.scores(Demo.threelevel, w1, cluster = "cluster3", name = "w1.l3")
#'
#' #.........................................
#' # Level-1 (L1) Predictor
#'
#' # Example 3a: Center L1 predictor 'y1' within L2 clusters (CWC L2)
#' #             Note that L3 cluster IDs are ignored when type = "CWC"
#' center(Demo.threelevel, y1, cluster = c("cluster3", "cluster2"))
#'
#' # Alternative specification when L2 cluster IDs are unique across L3 clusters
#' center(Demo.threelevel, y1, cluster = "cluster2")
#'
#' # Example 3b: Center L1 predictor 'y1' within L3 clusters (CWC L3)
#' #             Note that both L3 and L2 cluster IDs are needed
#' center(Demo.threelevel, y1, cluster = c("cluster3", "cluster2"), cwc.mean = "L3")
#'
#' # Example 3c: Center L1 predictor 'y1' at the grand-mean (CGM)
#' #             Note that the cluster argument is ignored when type = "CGM",
#' center(Demo.threelevel, y1, cluster = c("cluster3", "cluster2"), type = "CGM")
#'
#' # Alternative specification
#' center(Demo.threelevel, y1)
#'
#' #.........................................
#' # Level-2 (L2) Predictor
#'
#' # Example 3d: Center L2 predictor 'w1' within L3 cluster
#' #             Note that both L3 and L2 cluster IDs are needed
#' center(Demo.threelevel, w1, cluster = c("cluster3", "cluster2"))
#'
#' # Example 3e: Center L2 predictor 'w1' at the grand-mean (CGM)
#' #             Note that both L3 and L2 cluster IDs are needed
#' center(Demo.threelevel, y1, cluster = c("cluster3", "cluster2"), type = "CGM")
#'
#' #.........................................
#' # Level-3 (L3) Predictor
#'
#' # Example 3f: Center L3 predictor 'w1.l3' at the average L3 cluster scores
#' #             Note that L2 cluster ID is ignored
#' center(Demo.threelevel, w1.l3, cluster = c("cluster3", "cluster2"))
#'
#' # Alternative specification
#' center(Demo.threelevel, w1.l3, cluster = "cluster3")
#'
#' #.........................................
#' # L1, L2, and L3 Predictors
#'
#' # Example 3g: Center L1 predictor 'y1' within L2 cluster, L2 predictor 'w1' within
#' #            L3 clusters, and L3 predictor 'w1.l3' at the average L3 cluster scores
#' center(Demo.threelevel, y1, w1, w1.l3, cluster = c("cluster3", "cluster2"))
#'
#' #.........................................
#' # Two-Step Latent Mean Centering
#'
#' # Load data set "Demo.twolevel" in the lavaan package
#' data("Demo.twolevel", package = "lavaan")
#'
#' # Create arbitrary three-level data
#' Demo.threelevel <- data.frame(Demo.twolevel, cluster2 = Demo.twolevel$cluster,
#'                                              cluster3 = rep(1:10, each = 250))
#'
#' # Example 3h: Decompose L1 predictor 'y1' as within-between components
#' center(Demo.threelevel, y1, cluster = "cluster2", type = "latent")
#'
#' # Example 3i: Decompose L1 predictor 'y1' as within-between components
#' #             label variables as 'l1.y1' and 'l2.y1'
#' center(Demo.threelevel, y1, cluster = "cluster2", type = "latent",
#'        name = c("l1.y1", "l2.y2"))
#'
#' # Example 3j: Decompose L1 predictor 'y1' as within-between components
#' center(Demo.threelevel, y1, cluster = c("cluster3", "cluster2"), type = "latent")
#'
#' # Example 3k: Decompose L1 predictor 'y1' as within-between components
#' #             label variables as 'l1.y1', 'l2.y1', and 'l3.y1'
#' center(Demo.threelevel, y1, cluster = c("cluster3", "cluster2"), type = "latent",
#'        name = c("l1.y1", "l2.y1", "l3.y1"))
#'}
center <- function(data, ..., cluster = NULL, type = c("CGM", "CWC", "latent"),
                   cwc.mean = c("L2", "L3"), value = NULL, append = TRUE,
                   name = ".c", as.na = NULL, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing or NULL
  if (isTRUE(missing(data) || is.null(data))) { stop("Please specify a numeric vector or data frame for the argument 'data'", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data using the argument '...' ####

  if (isTRUE(!missing(...))) {

    # Variable names
    var.names <- .var.names(data = data, ..., cluster = cluster)

    # Extract data and convert tibble into data frame or vector
    x <- data[, var.names] |> (\(p) if (isTRUE("tbl" %in% substr(class(p), 1L, 3L))) { if (isTRUE(ncol(as.data.frame(p)) == 1L)) { unname(unlist(p)) } else { as.data.frame(p) } } else { return(p) })()

    # Cluster variable
    if (isTRUE(!is.null(cluster))) { cluster <- data[, cluster] |>  (\(p) if (isTRUE(!is.null(dim(p)))) { setNames(p, nm = c("cluster3", "cluster2")) } else { return(p) })() }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument '...' ####

  } else {

    # Convert 'data' as tibble into a data frame
    x <- data |> (\(p) if (isTRUE("tbl" %in% substr(class(p), 1L, 3L))) { if (isTRUE(ncol(as.data.frame(p)) == 1L)) { unname(unlist(p)) } else { as.data.frame(p) } } else { return(p) })()

    # Data and cluster
    var.group <- .var.group(data = x, cluster = cluster)

    # Data
    if (isTRUE(!is.null(var.group$data))) { x <- var.group$data }

    # Cluster variable
    if (isTRUE(!is.null(var.group$cluster))) { cluster <- var.group$cluster }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Cluster variables ####

  if (isTRUE(!is.null(cluster))) {

    # Two cluster variables
    if (isTRUE(ncol(as.data.frame(cluster)) == 2L)) {

      l3.cluster <- cluster[, 1L]
      l2.cluster <- cluster[, 2L]

      no.clust <- "two"

    # One cluster variables
    } else {

      no.clust <- "one"

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = "append", numeric = list(value = 1L), s.character = list(type = c("CGM", "CWC", "latent"), cwc.mean = c("L2", "L3")), envir = environment(), input.check = check)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Additional Checks ####

  if (isTRUE(check)) {

    #--------------------------------------
    ### Check Input 'type' ####

    if (isTRUE(all(type == "latent"))) {

      # Multilevel data
      if (isTRUE(is.null(cluster))) { stop("Two-step latent centering method is not available for predictors in single-level data.", call. = FALSE) }

      # One predictor
      if (isTRUE(ncol(as.data.frame(x)) > 1L)) { stop("Two-step latent centering method can only be applied to one predictor variable at a time.", call. = FALSE) }

      # lme4 package
      if (isTRUE(!nzchar(system.file(package = "lme4")))) { stop("Package \"lme4\" is needed for type = \"latent\", please install the package.", call. = FALSE) }

    }

    #--------------------------------------
    ### Check Input 'name' ####

    if (isTRUE(!is.character(name))) { stop("Please specify a character string or characster vector for the argument 'name'.", call. = FALSE) }

    # Single variable
    if (isTRUE(is.null(dim(x)))) {

      # Character string
      if (isTRUE(length(name != 1L))) {stop("Please specify a character string for the argument 'name'.", call. = FALSE) }

    # Multiple variables
    } else {

      if (isTRUE(length(name) > 1L && length(name) != ncol(as.data.frame(x)))) { stop("Length of the vector specified in 'name' does not match with the number of variables.", call. = FALSE) }

    }

  }

  #_____________________________________________________________________________
  #
  # Main Function: Single Variable Centering -----------------------------------

  if (isTRUE(is.null(dim(x)))) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Predictor Level ####

    if (isTRUE(!is.null(cluster))) {

      #--------------------------------------
      ### One Cluster Variable ####

      if (isTRUE(no.clust == "one")) {

        # Level 1 Variable
        if (isTRUE(any(na.omit(as.vector(tapply(x, cluster, var, na.rm = TRUE))) > .Machine$double.eps^0.5))) {

          vartype <- "L1"

          if (isTRUE(all(type == "latent") && all(name != ".c") && length(name) != 2L)) { stop("Please specify a character vector of length 2 for the argument 'name' when using latent mean centering method for a L1 predictor.", call. = FALSE) }

        # Level 2 Variable
        } else {

          vartype <- "L2"

          if (isTRUE(all(type == "latent"))) { stop("Latent centering method is not available for L2 predictors in two-level data.", call. = FALSE) }

        }

      #--------------------------------------
      ### Two Cluster Variables ####

      } else if (isTRUE(no.clust == "two")) {

        # Level 1 Variable
        if (isTRUE(any(na.omit(as.vector(tapply(x, apply(cluster, 1L, paste, collapse = ""), var, na.rm = TRUE))) > .Machine$double.eps^0.5))) {

          vartype <- "L1"

          if (isTRUE(all(type == "latent") && all(name != ".c") && length(name) != 3L)) { stop("Please specify a character vector of length 3 for the argument 'name' when using latent mean centering method for a L1 predictor.", call. = FALSE) }


        # Level 2 Variable
        } else if (isTRUE(all(na.omit(as.vector(tapply(x, apply(cluster, 1L, paste, collapse = ""), var, na.rm = TRUE))) < .Machine$double.eps^0.5) && any(as.vector(tapply(x, cluster[, 1L], var, na.rm = TRUE)) > .Machine$double.eps^0.5))) {

          vartype <- "L2"

          if (isTRUE(all(type == "latent") && all(name != ".c") && length(name) != 2L)) { stop("Please specify a character vector of length 2 for the argument 'name' when using latent mean centering method for a L2 predictor.", call. = FALSE) }

        # Level 3 Variable
        } else if (isTRUE(all(na.omit(as.vector(tapply(x, apply(cluster, 1L, paste, collapse = ""), var, na.rm = TRUE))) < .Machine$double.eps^0.5) && all(na.omit(as.vector(tapply(x, cluster[, 1L], var, na.rm = TRUE))) < .Machine$double.eps^0.5))) {

          vartype <- "L3"

          if (isTRUE(all(type == "latent"))) { stop("Latent centering method is not available for L3 predictors in three-level data.", call. = FALSE) }

        }

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Type of Centering ####

    if (isTRUE(all(c("CGM", "CWC", "latent") %in% type))) {

      #--------------------------------------
      ### Single-level Data ####

      if (isTRUE(is.null(cluster))) {

        type <- "CGM"

      #--------------------------------------
      ### Multilevel Data ####

      } else {

        # One cluster variable
        if (isTRUE(no.clust == "one")) {

          type <- switch(vartype, L1 = "CWC", L2 = "CGM")

        # Two cluster variables
        } else if (isTRUE(no.clust == "two")) {

          type <- switch(vartype, L1 = "CWC", L2 = "CWC", L3 = "CGM")

        }

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## CGM and CWC Centering ####

    if (isTRUE(type != "latent")) {

      #--------------------------------------
      ### Single-Level Data: No Cluster Variable ####

      if (isTRUE(is.null(cluster))) {

        #...................
        #### Mean centering ####
        if (isTRUE(is.null(value))) {

          object <- as.numeric(scale(x, scale = FALSE))

        #...................
        #### Centering on a user-defined value ####
        } else {

          object <- x - mean(x, na.rm = TRUE) + value

        }

      #--------------------------------------
      ### Two-Level Data: One Cluster Variable ####

      } else if (isTRUE(no.clust == "one")) {

        #...................
        #### Centering at the grand mean (CGM) ####
        if (isTRUE(type == "CGM")) {

          switch(vartype,
                 # Level-1 predictor
                 L1 = {

                   object <- as.numeric(scale(x, scale = FALSE))

                 # Level-2 predictor
                 }, L2 = {

                   object <- x - mean(x[which(!duplicated(cluster))], na.rm = TRUE)

                 })

        #...................
        #### Centering within cluster (CWC) ####
        } else if (isTRUE(type == "CWC")) {

          switch(vartype,
                 # Level-1 predictor
                 L1 = {

                   object <- unname(x - misty::cluster.scores(x, cluster = cluster, fun = "mean", check = FALSE, expand = TRUE))

                 # Level-2 predictor
                 }, L2 = {

                   # Note, level 2 predictor can only be centered at the grand mean
                   object <- x - mean(x[which(!duplicated(cluster))], na.rm = TRUE)

                 })

        }

      #--------------------------------------
      ### Three-Level Data: Two Cluster Variables ####

      } else if (isTRUE(no.clust == "two")) {

        #...................
        #### Centering at the grand mean (CGM) ####
        if (isTRUE(type == "CGM")) {

          switch(vartype,
                 # Level-1 predictor
                 L1 = {

                   object <- as.numeric(scale(x, scale = FALSE))

                 # Level-2 predictor
                 }, L2 = {

                   object <- x - mean(x[which(!duplicated(apply(cluster, 1L, paste, collapse = "")))], na.rm = TRUE)

                 # Level-3 predictor
                 }, L3 = {

                   object <- x - mean(x[which(!duplicated(cluster[, 1L]))], na.rm = TRUE)

                 })

        #...................
        #### Centering within cluster (CWC) ####
        } else if (isTRUE(type == "CWC")) {

          ##### Level-2 or Level-3 Cluster Means
          if (isTRUE(all(c("L2", "L3") %in% cwc.mean))) { cwc.mean <- "L2" }

          switch(vartype,
                 # Level-1 predictor
                 L1 = {

                  switch(cwc.mean,
                         # Deviation from the Level-2 cluster mean
                         L2 = {

                           object <- unname(x - misty::cluster.scores(x, cluster = apply(cluster, 1L, paste, collapse = ""), fun = "mean", check = FALSE, expand = TRUE))

                         # Deviation from the Level-3 cluster mean
                         }, L3 = {

                           object <- x - misty::cluster.scores(x, cluster = cluster[, 1L])

                         })

                 # Level-2 predictor
                 }, L2 = {

                   object <- (data.frame(x, cluster3 = cluster[, 1L], cluster2 = cluster[, 2]) |>
                                (\(p) misty::cluster.scores(p[!duplicated(p$cluster2), ], x, cluster = "cluster3"))() |>
                                (\(q) data.frame(q, object = q$x - q$x.a))() |>
                                (\(r) merge(data.frame(by = apply(cluster, 1L, paste, collapse = "")), data.frame(object = r$object, by = apply(r[, c("cluster3", "cluster2")], 1L, paste, collapse = "")), by = "by", sort = FALSE))())[, "object"]

                 # Level-3 predictor
                 }, L3 = {

                   # Note, level 3 predictor can only be centered at the grand mean
                   object <- x - mean(x[which(!duplicated(cluster[, 1L]))], na.rm = TRUE)

                 })

        }

      }

      #--------------------------------------
      ### Append ####

      if (isTRUE(!missing(...) && append)) {

        # Default setting
        if (isTRUE(name == ".c")) {

          object <- setNames(as.data.frame(object), nm = paste0(var.names, ".c"))

        # User-specified names
        } else {

          object <- setNames(as.data.frame(object), nm = name)

        }

        object <- data.frame(data, object)

      }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Two-Step Latent Mean Centering ####

    } else {

      #--------------------------------------
      ### Two-Level Data: One Cluster Variable ####

      if (isTRUE(no.clust == "one")) {

        # Estimate model
        mod <- suppressMessages(lme4::lmer(x ~ 1 + (1|cluster), REML = TRUE, control = lme4::lmerControl(optimizer = "bobyqa")))

        # Within component
        x.l1 <- resid(mod)

        # Between component
        x.l2 <- lme4::ranef(mod)[[1L]] |> (\(p) unname(unlist(p)[match(as.character(cluster), row.names(p))]))()

        # Return object
        object <- data.frame(x.l1, x.l2)

      #--------------------------------------
      ### Three-Level Data: Two Cluster Variables ####

      } else if (isTRUE(no.clust == "two")) {

        #...................
        #### L1 Predictor ####

        if (isTRUE(vartype == "L1")) {

          # Cluster variables
          l3.cluster <- cluster[, 1L]
          l2.cluster <- cluster[, 2L]

          # Estimate model
          mod <- suppressMessages(lme4::lmer(x ~ 1 + (1|l3.cluster/l2.cluster), REML = TRUE, control = lme4::lmerControl(optimizer = "bobyqa")))

          # Within component
          x.l1 <- resid(mod)

          # Between component: Level 2
          x.l2 <- lme4::ranef(mod)[[1L]] |> (\(p) unname(unlist(p)[match(apply(cluster[, c(2L, 1L)], 1, paste, collapse = ":"), row.names(p))]))()

          # Between component: Level 3
          x.l3 <- lme4::ranef(mod)[[2L]] |> (\(p) unname(unlist(p)[match(as.character(cluster[, 1L]), row.names(p))]))()

          # Return object
          object <- data.frame(x.l1, x.l2, x.l3)

        #...................
        #### L2 Predictor ####

        } else if (isTRUE(vartype == "L2")) {

          # Extract unique L2 data
          x.l2 <- x[!duplicated(cluster[, 2L])]

          cluster.l3 <- cluster[!duplicated(cluster[, 2L]), 1L]

          # Estimate model
          mod <- suppressMessages(lme4::lmer(x.l2 ~ 1 + (1|cluster.l3), REML = TRUE, control = lme4::lmerControl(optimizer = "bobyqa")))

          # Within component
          x.l2 <- resid(mod) |> (\(p) unname(unlist(p)[match(as.character(cluster[, 2L]), names(p))]))()

          # Between component
          x.l3 <- lme4::ranef(mod)[[1L]] |> (\(p) unname(unlist(p)[match(as.character(cluster[, 1L]), row.names(p))]))()

          # Return object
          object <- data.frame(x.l2, x.l3)

        }

      }

      #--------------------------------------
      ### Append ####

      if (isTRUE(!missing(...) && append)) {

        # Default setting
        if (isTRUE(name == ".c")) {

          #...................
          #### Two-Level Data: One cluster variable ####

          if (isTRUE(no.clust == "one")) {

            object <- setNames(as.data.frame(object), nm = c(paste0(var.names, ".l1"), paste0(var.names, ".l2")))

          #...................
          #### Three-Level Data: Two cluster variables ####

          } else if (isTRUE(no.clust == "two")) {

            ##### L1 Predictor
            if (isTRUE(vartype == "L1")) {

              object <- setNames(as.data.frame(object), nm = c(paste0(var.names, ".l1"), paste0(var.names, ".l2"), paste0(var.names, ".l3")))

            ##### L2 Predictor
            } else if (isTRUE(vartype == "L2")) {

              object <- setNames(as.data.frame(object), nm = c(paste0(var.names, ".l2"), paste0(var.names, ".l3")))

            }

          }

        # User-specified names
        } else {

          object <- setNames(as.data.frame(object), nm = name)

        }

        # Append to the Data Frame
        object <- data.frame(data, object)

      }

    }

  #_____________________________________________________________________________
  #
  # Main Function: Multiple Variable Centering ---------------------------------

  } else {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Call center() Function ####

    object <- data.frame(vapply(x, misty::center, cluster = cluster, type = type, cwc.mean = cwc.mean, value = value, as.na = as.na, check = FALSE, FUN.VALUE = double(nrow(x))))

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Variable Names ####

    if (isTRUE(length(name) == 1L)) {

      colnames(object) <- paste0(colnames(object), name)

    } else {

      colnames(object) <- name

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Append to the Data Frame ####

    if (isTRUE(!missing(...) && append)) {  object <- data.frame(data, object) }

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  return(object)

}

#_______________________________________________________________________________
