#' R-Squared Measures for Multilevel and Linear Mixed Effects Models
#'
#' This function computes R-squared measures by Raudenbush and Bryk (2002),
#' Snijders and Bosker (1994), Nakagawa and Schielzeth (2013) as extended by
#' Johnson (2014), and Rights and Sterba (2019) for multilevel and linear mixed
#' effects models estimated by using the \code{lmer()} function in the package
#' \pkg{lme4} or \code{lme()} function in the package \pkg{nlme}.
#'
#' @param model  a fitted model of class \code{"lmerMod"} from the \pkg{lme4}
#'               package or \code{"lme"} from the \pkg{nlme} package.
#' @param print  a character vector indicating which R-squared measures to be
#'               printed on the console, i.e., \code{RB} for measures from
#'               Raudenbush and Bryk (2002), \code{SB} for measures from Snijders
#'               and Bosker (1994), \code{NS} for measures from Nakagawa and
#'               Schielzeth (2013) as extended by Johnson (2014), and \code{RS}
#'               for measures from Rights and Sterba (2019). The default setting
#'               is \code{print = "RS"}.
#' @param digits an integer value indicating the number of decimal places to be used.
#' @param plot   logical: if \code{TRUE}, bar chart showing the decomposition of
#'               scaled total, within-cluster, and between-cluster outcome variance
#'               into five (total), three (within-cluster), and two (between-cluster)
#'               proportions is drawn. Note that the \pkg{ggplot2} package is required
#'               to draw the bar chart.
#' @param gray   logical: if \code{TRUE}, graphical parameter to draw the bar chart
#'               in gray scale.
#' @param start  a numeric value between 0 and 1, graphical parameter to specify
#'               the gray value at the low end of the palette.
#' @param end    a numeric value between 0 and 1, graphical parameter to specify
#'               the gray value at the high end of the palette.
#' @param color  a character vector, graphical parameter indicating the color of
#'               bars in the bar chart in the following order: Fixed slopes (Within),
#'               Fixed slopes (Between), Slope variation (Within), Intercept variation
#'               (Between), and Residual (Within). By default, colors from the
#'               colorblind-friendly palettes are used
#' @param check  logical: if \code{TRUE}, argument specification is checked.
#' @param output logical: if \code{TRUE}, output is shown on the console.
#'
#' @details
#' A number of R-squared measures for multilevel and linear mixed effects models have
#' been developed in the methodological literature (see Rights & Sterba, 2018).
#' Based on these measures, following measures were implemented in the current function:
#' \describe{
#' \item{\strong{Raudenbush and Bryk (2002)}}{R-squared measures by Raudenbush
#' and Bryk (2002) are based on the proportional reduction of unexplained variance
#' when predictors are added. More specifically, variance estimates from the
#' baseline/null model (i.e., \eqn{\sigma^2_{e|b}} and \eqn{\sigma^2_{u0|b}})
#' and variance estimates from the model including predictors (i.e., \eqn{\sigma^2_{e|m}}
#' and \eqn{\sigma^2_{u0|m}}) are used to compute the proportional reduction in
#' variance between baseline/null model and the complete model by:
#'
#' \deqn{R^2_1(RB) = \frac{\sigma^2_{e|b} - \sigma^2_{e|m}}{\sigma^2_{e|b}}}
#'
#' for the proportional reduction at level-1 (within-cluster) and by:
#'
#' \deqn{R^2_2(RB) = \frac{\sigma^2_{u0|b} - \sigma^2_{u0|m}}{\sigma^2_{u0|b}}}
#'
#' for the proportional reduction at level-2 (between-cluster), where \eqn{|b}
#' and \eqn{|m} represent the baseline and full models, respectively (Hox et al.,
#' 2018; Roberts et al., 2010).
#'
#' A major disadvantage of these measures is that adding predictors can increases
#' rather than decreases some of the variance components and it is even possible
#' to obtain negative values for \eqn{R^2} with these formulas (Snijders & Bosker,
#' 2012). According to Snijders and Bosker (1994) this can occur because the
#' between-group variance is a function of both level-1 and level-2 variance:
#'
#' \deqn{var(\bar{Y}_j) = \sigma^2_{u0} + \frac{\sigma^2_e}{n_j}}
#'
#' Hence, adding a predictor (e.g., cluster-mean centered predictor) that explains
#' proportion of the within-group variance will decrease the estimate of \eqn{\sigma^2_e}
#' and increase the estimate \eqn{\sigma^2_{u0}} if this predictor does not explain
#' a proportion of the between-group variance to balance out the decrease in
#' \eqn{\sigma^2_e} (LaHuis et al., 2014). Negative estimates for \eqn{R^2} can
#' also simply occur due to chance fluctuation in sample estimates from the two
#' models.
#'
#' Another disadvantage of these measures is that \eqn{R^2_2(RB)} for the explained
#' variance at level-2 has been shown to perform poorly in simulation studies even
#' with \eqn{j = 200} clusters with group cluster size of \eqn{n_j = 50} (LaHuis
#' et al., 2014; Rights & Sterba, 2019).
#'
#' Moreover, when there is missing data in the level-1 predictors, it is possible
#' that sample sizes for the baseline and complete models differ.
#'
#' Finally, it should be noted that R-squared measures by Raudenbush and Bryk (2002)
#' are appropriate for random intercept models, but not for random intercept and
#' slope models. For random slope models, Snijders and Bosker (2012) suggested to
#' re-estimate the model as random intercept models with the same predictors while
#' omitting the random slopes to compute the R-squared measures. However, the
#' simulation study by LaHuis (2014) suggested that the R-squared measures showed
#' an acceptable performance when there was little slope variance, but did not
#' perform well in the presence of higher levels of slope variance.}
#'
#' \item{\strong{Snijders and Bosker (1994)}}{R-squared measures by Snijders and
#' Bosker (1994) are based on the proportional reduction of mean squared prediction
#' error and is computed using the formula:
#'
#' \deqn{R^2_1(SB) = \frac{\hat{\sigma}^2_{e|m} + \hat{\sigma}^2_{u0|m}}{\hat{\sigma}^2_{e|b} + \hat{\sigma}^2_{u0|b}}}
#'
#' for computing the proportional reduction of error at level-1 representing
#' the total amount of explained variance and using the formula:
#'
#' \deqn{R^2_2(SB) = \frac{\hat{\sigma}^2_{e|m} / n_j + \hat{\sigma}^2_{u0|m}}{\hat{\sigma}^2_{e|b} / n_j + \hat{\sigma}^2_{u0|b}}}
#'
#' for computing the proportional reduction of error at level-2 by dividing the
#' \eqn{\hat{\sigma}^2_e} by the group cluster size \eqn{n_j} or by the average
#' cluster size for unbalanced data (Roberts et al., 2010). Note that the function
#' uses the harmonic mean of the group sizes as recommended by Snijders and Bosker
#' (1994). The population values of \eqn{R^2} based on these measures cannot be
#' negative because the interplay of level-1 and level-2 variance components is
#' considered. However, sample estimates of \eqn{R^2} can be negative either due
#' to chance fluctuation when sample sizes are small or due to model misspecification
#' (Snijders and Bosker, 2012).
#'
#' When there is missing data in the level-1 predictors, it is possible that sample
#' sizes for the baseline and complete models differ.
#'
#' Similar to the R-squared measures by Raudenbush and Bryk (2002), the measures
#' by Snijders and Bosker (1994) are appropriate for random intercept models, but
#' not for random intercept and slope models. Accordingly, for random slope models,
#' Snijders and Bosker (2012) suggested to re-estimate the model as random intercept
#' models with the same predictors while omitting the random slopes to compute the
#' R-squared measures. The simulation study by LaHuis et al. (2014) revealed that
#' the R-squared measures showed an acceptable performance, but it should be noted
#' that \eqn{R^2_2(SB)} the explained variance at level-2 was not investigated in
#' their study.}
#'
#' \item{\strong{Nakagawa and Schielzeth (2013)}}{R-squared measures by Nakagawa
#' and Schielzeth (2013) are based on partitioning model-implied variance from a
#' single fitted model and uses the variance of predicted values of \eqn{var(\hat{Y}_{ij})}
#' to form both the outcome variance in the denominator and the explained variance
#' in the numerator of the formulas:
#'
#' \deqn{R^2_m(NS) = \frac{var(\hat{Y}_{ij})}{var(\hat{Y}_{ij}) + \sigma^2_{u0} + \sigma^2_{e}}}
#'
#' for marginal total \eqn{R^2_m(NS)} and:
#'
#' \deqn{R^2_c(NS) = \frac{var(\hat{Y}_{ij}) + \sigma^2_{u0}}{var(\hat{Y}_{ij}) + \sigma^2_{u0} + \sigma^2_{e}}}
#'
#' for conditional total \eqn{R^2_c(NS)}. In the former formula \eqn{R^2} predicted
#' scores are marginalized across random effects to indicate the variance explained
#' by fixed effects and in the latter formula \eqn{R^2} predicted scores are conditioned
#' on random effects to indicate the variance explained by fixed and random effects
#' (Rights and Sterba, 2019).
#'
#' The advantage of these measures is that they can never become negative and
#' that they can also be extended to generalized linear mixed effects models (GLMM)
#' when outcome variables are not continuous (e.g., binary outcome variables).
#' Note that currently the function does not provide \eqn{R^2} measures for GLMMs,
#' but these measures can be obtained using the \code{r.squaredGLMM()} function in
#' the \pkg{MuMIn} package.
#'
#' A disadvantage is that these measures do not allow random slopes and are restricted
#' to the simplest random effect structure (i.e., random intercept model). In other
#' words, these measures do not fully reflect the structure of the fitted model when
#' using random intercept and slope models. However, Johnson (2014) extended these
#' measures to allow random slope by taking into account the contribution of random
#' slopes, intercept-slope covariances, and the covariance matrix of random slope
#' to the variance in \eqn{Y_{ij}}. As a result, R-squared measures by Nakagawa
#' and Schielzeth (2013) as extended by Johnson (2014) can be used for both random
#' intercept, and random intercept and slope models.
#'
#' The major criticism of the R-squared measures by Nakagawa and Schielzeth (2013)
#' as extended by Johnson (2014) is that these measures do not decompose outcome
#' variance into each of total, within-cluster, and between-cluster variance which
#' precludes from computing level-specific \eqn{R^2} measures. In addition, these
#' measures do not distinguish variance attributable to level-1 versus level-2
#' predictors via fixed effects, and they also do not distinguish between random
#' intercept and random slope variation (Rights and Sterba, 2019).}
#'
#' \item{\strong{Rights and Sterba (2019)}}{R-squared measures by Rights and Sterba
#' (2019) provide an integrative framework of R-squared measures for multilevel
#' and linear mixed effects models with random intercepts and/or slopes. Their
#' measures are also based on partitioning model implied variance from a single
#' fitted model, but they provide a full partitioning of the total outcome variance
#' to one of five specific sources:
#'
#' \itemize{
#'
#'    \item variance attributable to level-1 predictors via fixed slopes (shorthand:
#'    variance attributable to \code{f1})
#'    \item variance attributable to level-2 predictors via fixed slopes (shorthand:
#'    variance attributable to \code{f2})
#'    \item variance attributable to level-1 predictors via random slope variation/
#'    covariation (shorthand: variance attributable to \code{v})
#'    \item variance attributable to cluster-specific outcome means via random
#'    intercept variation (shorthand: variance attributable to \code{m})
#'    \item variance attributable to level-1 residuals
#' }
#' \eqn{R^2} measures are based on the outcome variance of interest (total,
#' within-cluster, or between-cluster) in the denominator, and the source contributing
#' to explained variance in the numerator:
#'   \describe{
#'
#'     \item{\strong{Total \eqn{R^2} measures}}{incorporate both within-cluster
#'     and between cluster variance in the denominator and quantify variance
#'     explained in an omnibus sense:
#'        \itemize{
#'           \item{\eqn{R^{2(f_1)}_t}}: Proportion of total outcome variance explained
#'           by level-1 predictors via fixed slopes.
#'           \item{\eqn{R^{2(f_2)}_t}}: Proportion of total outcome variance explained
#'           by level-2 predictors via fixed slopes.
#'           \item{\eqn{R^{2(f)}_t}}: Proportion of total outcome variance explained
#'           by all predictors via fixed slopes.
#'           \item{\eqn{R^{2(v)}_t}}: Proportion of total outcome variance explained
#'           by level-1 predictors via random slope variation/covariation.
#'           \item{\eqn{R^{2(m)}_t}}: Proportion of total outcome variance explained
#'           by cluster-specific outcome means via random intercept variation.
#'           \item{\eqn{R^{2(fv)}_t}}: Proportion of total outcome variance explained
#'           by predictors via fixed slopes and random slope variation/covariation.
#'           \item{\eqn{R^{2(fvm)}_t}}: Proportion of total outcome variance explained
#'           by predictors via fixed slopes and random slope variation/covariation
#'           and by cluster-specific outcome means via random intercept variation.
#'        }
#'      }
#'     \item{\strong{Within-Cluster \eqn{R^2} measures}}{incorporate only within-cluster
#'     variance in the denominator and indicate
#'     the degree to which within-cluster variance can be explained by a given model:
#'        \itemize{
#'           \item{\eqn{R^{2(f_1)}_w}}: Proportion of within-cluster outcome variance
#'           explained by level-1 predictors via fixed slopes.
#'           \item{\eqn{R^{2(v)}_w}}: Proportion of within-cluster outcome variance
#'           explained by level-1 predictors via random slope variation/covariation.
#'           \item{\eqn{R^{2(f_1v)}_w}}: Proportion of within-cluster outcome variance
#'           explained by level-1 predictors via fixed slopes and random slope
#'           variation/covariation.
#'        }
#'      }
#'     \item{\strong{Between-Cluster \eqn{R^2} measures}}{incorporate only between-cluster
#'     variance in the denominator and indicate the degree to which between-cluster
#'     variance can be explained by a given model:
#'        \itemize{
#'           \item{\eqn{R^{2(f_2)}_b}}: Proportion of between-cluster outcome variance
#'           explained by level-2 predictors via fixed slopes.
#'           \item{\eqn{R^{2(m)}_b}}: Proportion of between-cluster outcome variance
#'           explained by cluster-specific outcome means via random intercept variation.
#'        }
#'     }
#'   }
#'   The decomposition of the total outcome variance can be visualized in a bar
#'   chart by specifying \code{plot = TRUE}. The first column of the bar chart
#'   decomposes scaled total variance into five distinct proportions (i.e.,
#'   \eqn{R^{2(f_1)}_t}, \eqn{R^{2(f_2)}_t}, \eqn{R^{2(f)}_t}, \eqn{R^{2(v)}_t},
#'   \eqn{R^{2(m)}_t}, \eqn{R^{2(fv)}_t}, and \eqn{R^{2(fvm)}_t}), the second
#'   column decomposes scaled within-cluster variance into three distinct proportions
#'   (i.e., \eqn{R^{2(f_1)}_w}, \eqn{R^{2(v)}_w}, and \eqn{R^{2(f_1v)}_w}), and
#'   the third column decomposes scaled between-cluster variance into two distinct
#'   proportions (i.e., \eqn{R^{2(f_2)}_b}, \eqn{R^{2(m)}_b}).
#'
#'   Note that the function assumes that all level-1 predictors are centered within
#'   cluster (i.e., group-mean or cluster-mean centering) as has been widely recommended
#'   (e.g., Enders & Tofighi, D., 2007; Rights et al., 2019). In fact, it does not
#'   matter whether a lower-level predictor is merely a control variable, or is
#'   quantitative or categorical (Yaremych et al., 2021), cluster-mean centering
#'   should always be used for lower-level predictors to obtain an orthogonal
#'   between-within partitioning of a lower-level predictor's variance that directly
#'   parallels what happens to a level-1 outcome (Hoffman & Walters, 2022). In the
#'   absence of cluster-mean-centering, however, the function provides total \eqn{R^2}
#'   measures, but does not provide any within-cluster or between-cluster \eqn{R^2}
#'   measures.}
#' }
#' By default, the function only computes R-squared measures by Rights and Sterba
#' (2019) because the other R-squared measures reflect the same population quantity
#' provided by Rights and Sterba (2019). That is, R-squared measures \eqn{R^2_1(RB)}
#' and \eqn{R^2_2(RB)} by Raudenbush and Bryk (2002) are equivalent to \eqn{R^{2(f_1v)}_w}
#' and \eqn{R^{2(f_2)}_b}, R-squared measures \eqn{R^2_1(SB)} and \eqn{R^2_2(SB)}
#' are equivalent to \eqn{R^{2(f)}_t} and \eqn{R^{2(f_2)}_b}, and R-squared measures
#' \eqn{R^2_m(NS)} and \eqn{R^2_c(NS)} by Nakagawa and Schielzeth (2013) as extended
#' by Johnson (2014) are equivalent to \eqn{R^{2(f)}_t} and \eqn{R^{2(fvm)}_t}
#' (see Rights and Sterba, Table 3).
#'
#' Note that none of these measures provide an \eqn{R^2} for the random slope
#' variance explained by cross-level interactions, a quantity that is frequently
#' of interest (Hoffman & Walters, 2022).
#'
#' @author
#' Simon Grund, Alexander Robitzsch, Oliver Luedtk, Mairead Shaw, Jason D. Rights,
#' Sonya K. Sterba, Jessica K. Flake, and Takuya Yanagida
#'
#' @seealso
#' \code{\link{multilevel.r2.manual}}, \code{\link{multilevel.cor}},
#' \code{\link{multilevel.descript}}, \code{\link{multilevel.icc}},
#' \code{\link{multilevel.indirect}}
#'
#' @references
#' Enders, C. K., & Tofighi, D. (2007). Centering predictor variables in
#' cross-sectional multilevel models: A new look at an old issue.
#' \emph{Psychological Methods, 12}, 121-138. https://doi.org/10.1037/1082-989X.12.2.121
#'
#' Hoffmann, L., & Walter, W. R. (2022). Catching up on multilevel modeling.
#' \emph{Annual Review of Psychology, 73}, 629-658. https://doi.org/10.1146/annurev-psych-020821-103525
#'
#' Hox, J., Moerbeek, M., & van de Schoot, R. (2018). \emph{Multilevel Analysis:
#' Techniques and Applications} (3rd ed.) Routledge.
#'
#' Johnson, P. C. D. (2014). Extension of Nakagawa & Schielzeth’s R2 GLMM to random
#' slopes models. \emph{Methods in Ecology and Evolution, 5}(9), 944-946.
#' https://doi.org/10.1111/2041-210X.12225
#'
#' LaHuis, D. M., Hartman, M. J., Hakoyama, S., & Clark, P. C. (2014). Explained
#' variance measures for multilevel models. \emph{Organizational Research Methods, 17},
#' 433-451. https://doi.org/10.1177/1094428114541701
#'
#' Nakagawa, S., & Schielzeth, H. (2013). A general and simple method for obtaining
#' R2 from generalized linear mixed-effects models. \emph{Methods in Ecology and Evolution, 4}(2),
#' 133-142. https://doi.org/10.1111/j.2041-210x.2012.00261.x
#'
#' Raudenbush, S. W., & Bryk, A. S., (2002). \emph{Hierarchical linear models: Applications
#' and data analysis methods}. Sage.
#'
#' Rights, J. D., Preacher, K. J., & Cole, D. A. (2020). The danger of conflating
#' level‐specific effects of control variables when primary interest lies in level‐2
#' effects. \emph{British Journal of Mathematical and Statistical Psychology, 73}(Suppl 1),
#' 194-211. https://doi.org/10.1111/bmsp.12194
#'
#' Rights, J. D., & Cole, D. A. (2018). Effect size measures for multilevel models
#' in clinical child and adolescent research: New r-squared methods and recommendations.
#' \emph{Journal of Clinical Child and Adolescent Psychology, 47}, 863-873.
#'  https://doi.org/10.1080/15374416.2018.1528550
#'
#' Rights, J. D., & Sterba, S. K. (2019). Quantifying explained variance in multilevel
#' models: An integrative framework for defining R-squared measures. \emph{Psychological Methods, 24},
#' 309-338. https://doi.org/10.1037/met0000184
#'
#' Roberts, K. J., Monaco, J. P., Stovall, H., & Foster, V. (2011). Explained variance
#' in multilevel models (pp. 219-230). In J. J. Hox & J. K. Roberts (Eds.), \emph{Handbook
#' of Advanced Multilevel Analysis}. Routledge.
#'
#' Snijders, T. A. B., & Bosker, R. (1994). Modeled variance in two-level models.
#' \emph{Sociological methods and research, 22}, 342-363. https://doi.org/10.1177/0049124194022003004
#'
#' Snijders, T. A. B., & Bosker, R. J. (2012). \emph{Multilevel analysis: An introduction
#' to basic and advanced multilevel modeling} (2nd ed.). Sage.
#'
#' Yaremych, H. E., Preacher, K. J., & Hedeker, D. (2021). Centering categorical
#' predictors in multilevel models: Best practices and interpretation. \emph{Psychological
#' Methods}. Advance online publication. https://doi.org/10.1037/met0000434
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries: function call (\code{call}), type of analysis \code{type}, model
#' specified in \code{model} (\code{model}), specification of function arguments
#' (\code{args}), and list with results (\code{result}).
#'
#' @note
#' This function is based on the \code{multilevelR2()} function from the \pkg{mitml}
#' package by Simon Grund, Alexander Robitzsch and Oliver Luedtke (2021) and
#' calls the \code{r2mlm()} function in the \pkg{r2mlm} package by Mairead Shaw,
#' Jason Rights, Sonya Sterba, and Jessica Flake.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load misty, lme4, nlme, and ggplot2 package
#' library(misty)
#' library(lme4)
#' library(nlme)
#' library(ggplot2)
#'
#' # Load data set "Demo.twolevel" in the lavaan package
#' data("Demo.twolevel", package = "lavaan")
#'
#' #---------------------------
#'
#' # Cluster mean centering, center() from the misty package
#' Demo.twolevel$x2.c <- center(Demo.twolevel$x2, type = "CWC",
#'                              cluster = Demo.twolevel$cluster)
#'
#' # Compute group means, cluster.scores() from the misty package
#' Demo.twolevel$x2.b <- cluster.scores(Demo.twolevel$x2,
#'                                      cluster = Demo.twolevel$cluster)
#'
#' # Estimate multilevel model using the lme4 package
#' mod1a <- lmer(y1 ~ x2.c + x2.b + w1 + (1 + x2.c | cluster), data = Demo.twolevel,
#'               REML = FALSE, control = lmerControl(optimizer = "bobyqa"))
#'
#' #---------------------------
#'
#' # R-squared measures according to Rights and Sterba (2019)
#' multilevel.r2(mod1a)
#'
#' # Estimate multilevel model using the nlme package
#' mod1b <- lme(y1 ~ x2.c + x2.b + w1, random = ~ 1 + x2.c | cluster, data = Demo.twolevel,
#'              method = "ML")
#'
#' # R-squared measures according to Rights and Sterba (2019)
#' multilevel.r2(mod1b)
#'
#' #-----------------------------------------
#'
#' # Bar chart showing the decomposition of scaled total, within-cluster,
#' # and between-cluster outcome variance
#' multilevel.r2(mod1a, plot = TRUE)
#'
#' # Bar chart in gray scale
#' multilevel.r2(mod1a, plot = TRUE, gray = TRUE)
#'
#' # Save bar chart, ggsave() from the ggplot2 package
#' ggsave("Proportion_of_Variance.png", dpi = 600, width = 5.5, height = 5.5)
#'
#' #-----------------------------------------
#'
#' # Estimate multilevel model without random slopes
#' # Note. R-squared measures by Raudenbush and Bryk (2002), and  Snijders and
#' # Bosker (2012) should be computed based on the random intercept model
#' mod2 <- lmer(y1 ~ x2.c + x2.b + w1 + (1 | cluster), data = Demo.twolevel,
#'              REML = FALSE, control = lmerControl(optimizer = "bobyqa"))
#'
#' # Print all available R-squared measures
#' multilevel.r2(mod2, print = "all")
#'
#' #-----------------------------------------
#'
#' # Draw bar chart manually
#' mod1a.r2 <- multilevel.r2(mod1a, output = FALSE)
#'
#' # Prepare data frame for ggplot()
#' df <- data.frame(var = factor(rep(c("Total", "Within", "Between"), each = 5),
#'                               level = c("Total", "Within", "Between")),
#'                  part = factor(c("Fixed Slopes (Within)", "Fixed Slopes (Between)",
#'                                  "Slope Variation (Within)", "Intercept Variation (Between)",
#'                                  "Residual (Within)"),
#'                  level = c("Residual (Within)", "Intercept Variation (Between)",
#'                            "Slope Variation (Within)", "Fixed Slopes (Between)",
#'                            "Fixed Slopes (Within)")),
#'                  y = as.vector(mod1a.r2$result$rs$decomp))
#'
#' # Draw bar chart in line with the default setting of multilevel.r2()
#' ggplot(df, aes(x = var, y = y, fill = part)) +
#'   theme_bw() +
#'   geom_bar(stat = "identity") +
#'   scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7", "#0072B2", "#D55E00")) +
#'   scale_y_continuous(name = "Proportion of Variance", breaks = seq(0, 1, by = 0.1)) +
#'   theme(axis.title.x = element_blank(),
#'         axis.ticks.x = element_blank(),
#'         legend.title = element_blank(),
#'         legend.position = "bottom",
#'         legend.box.margin = margin(-10, 6, 6, 6)) +
#'   guides(fill = guide_legend(nrow = 2, reverse = TRUE))
#' }
multilevel.r2 <- function(model, print = c("all", "RB", "SB", "NS", "RS"), digits = 3,
                          plot = FALSE, gray = FALSE, start = 0.15, end = 0.85,
                          color = c("#D55E00", "#0072B2", "#CC79A7", "#009E73", "#E69F00"),
                          check = TRUE, output = TRUE) {

  ####################################################################################
  # Data

  #......
  # Check if input 'model' is missing or null
  if (isTRUE(missing(model) || is.null(model))) { stop("Please specify a fitted model of class \"lmerMod\" or \"lme\" for the argument 'x'.", call. = FALSE) }

  #......
  # Check method
  if (isTRUE(inherits(model, "merMod"))) {

    method <- "lme4"

  } else if (isTRUE(inherits(model, "lme"))) {

    method <- "nlme"

  } else {

    stop("Calculation of multilevel R-squared measures not supported for models of class \"",
         class(model), "\".", call. = TRUE)

  }

  ####################################################################################
  # Input Check

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  #-----------------------------------------

  if (isTRUE(check)) {

    #......
    # ggplot2 package
    if (isTRUE(!nzchar(system.file(package = "ggplot2"))))  { warning("Package \"ggplot2\" is needed for drawing a bar chart, please install the package.", call. = FALSE) }

    #......
    # Check input 'print'
    if (isTRUE(!all(print %in%  c("all", "RB", "SB", "NS", "RS")))) { stop("Character strings in the argument 'print' do not all match with \"all\", \"RB\", \"SB\", \"NS\", or \"RS\".", call. = FALSE) }

    #......
    # Check input 'digits'
    if (isTRUE(digits %% 1L != 0L || digits < 0L)) { stop("Specify a positive integer value for the argument 'digits'.", call. = FALSE) }

    #......
    # Check input 'plot'
    if (isTRUE(!is.logical(plot))) { stop("Please specify TRUE or FALSE for the argument 'plot'", call. = FALSE) }

    #......
    # Check input 'gray'
    if (isTRUE(!is.logical(gray))) { stop("Please specify TRUE or FALSE for the argument 'gray'", call. = FALSE) }

    #......
    # Check input 'start'
    if (isTRUE(start < 0L || start > 1L)) { stop("Please specify a numeric value between 0 and 1 for the argument 'start'", call. = FALSE) }

    #......
    # Check input 'end'
    if (isTRUE(end < 0L || end > 1L)) { stop("Please specify a numeric value between 0 and 1 for the argument 'end'", call. = FALSE) }

    #......
    # Check input 'output'
    if (isTRUE(!is.logical(output))) { stop("Please specify TRUE or FALSE for the argument 'output'", call. = FALSE) }

  }

  ####################################################################################
  # Data and Arguments

  if (isTRUE(all(c("all", "RB", "SB", "NS", "RS") %in% print))) { print <- "RS" }

  if (isTRUE(length(print) == 1L && print == "all")) { print <- c("RB", "SB", "NS", "RS") }

  if (isTRUE(plot) & isTRUE(!"RS" %in% print)) { warning("Bar char is only available when \"RS\" is specified in the argument 'print'.", call. = FALSE) }

  ####################################################################################
  # Main Function

  #-----------------------------------------
  # RB, SB, or NS R-squared measures

  if (isTRUE(any(c("RB", "SB", "NS") %in% print))) {

    # Check if refit is necessary
    refit <- any(c("RB", "SB") %in% print)

    #.......................................
    # lme4
    switch(method, lme4 = {

      # Model terms
      trm <- terms(model)

      if (isTRUE(!as.logical(attr(trm, "intercept")))) stop("Model must contain intercept.", call. = FALSE)

      # Outcome variable
      yvr <- as.character(attr(trm, "variables")[-1L])[attr(trm, "response")]

      # Cluster variable
      flist <- lme4::getME(model, "flist")
      cvr <- names(flist)
      if (isTRUE(length(cvr) > 1L)) stop("Calculation of R-squared only support for models with a single cluster variable.", call. = FALSE)

      # Cluster size harmonic mean
      n.j <- 1 / mean(1 / table(flist[1]))

      # Check for random slopes
      random <- length(lme4::getME(model, "theta")) > 1L

      if (isTRUE(refit)) {

        # Fit null model
        model0 <- update(model, formula(paste0(yvr, "~ 1 + (1 |", cvr, ")")))

        # Variance components under Null
        vc0 <- lme4::VarCorr(model0)
        s0 <- attr(vc0, "sc")^2L
        t0.0 <- vc0[[cvr]][1L, 1L]

      }

      # Alternative model components
      beta <- lme4::fixef(model)[-1L]
      X <- lme4::getME(model, "X")[, -1L, drop = FALSE]
      Z <- lme4::getME(model, "mmList")[[1L]][, -1L, drop = FALSE]
      muZ <- colMeans(Z)
      vZ <- cov(Z)

      # Predicted and total variance
      vc1 <- lme4::VarCorr(model)
      t0.1 <- vc1[[cvr]][1L, 1L]
      t10.1 <- vc1[[cvr]][1L, -1L]
      t11.1 <- vc1[[cvr]][-1L, -1L, drop = FALSE]
      s1 <- attr(vc1, "sc")^2L

    #.......................................
    # nlme
    }, nlme = {

      # Model terms
      trm <- terms(model)
      if(isTRUE(!as.logical(attr(trm, "intercept")))) stop("Model must contain intercept.", call. = FALSE)

      # Outcome variable
      yvr <- as.character(attr(trm, "variables")[-1L])[attr(trm, "response")]

      # Cluster variable
      cvr <- attr(nlme::getGroups(model), "label")
      if(isTRUE(length(nlme::getGroupsFormula(model, asList = TRUE)) > 1L)) stop("Calculation of R-squared only support for models with a single cluster variable.", call. = FALSE)

      # Cluster size harmonic mean
      n.j <- 1L / mean(1L / table(nlme::getGroups(model)))

      # Check for random slopes
      random <- ncol(nlme::ranef(model)) > 1L

      if (isTRUE(refit)) {

        # Fit Null model
        ffml0 <- formula(paste0(yvr, "~ 1"))
        rfml0 <- formula(paste0("~ 1 |", cvr, ""))
        if (is.null(nlme::getData(model))) stop("No data sets found in 'lme' fit. See '?testModels' for an example.", call. = FALSE)

        model0 <- update(model, fixed = ffml0, random = rfml0, data = model$data)

        # Variance components under Null
        s0 <- model0$sigma^2L
        t0.0 <- nlme::getVarCov(model0)[1L, 1L]

      }

      # Alternative model components
      beta <- nlme::fixef(model)[-1L]
      fe <- model$terms
      X <- model.matrix(fe, nlme::getData(model))[, -1L, drop = FALSE]
      re <- attr(model$modelStruct$reStruct[[1L]], "formula")
      Z <- model.matrix(re, nlme::getData(model))[, -1L, drop = FALSE]
      muZ <- colMeans(Z)
      vZ <- cov(Z)

      # Predicted and total variance
      vc1 <- nlme::getVarCov(model)
      t0.1 <- vc1[1L, 1L]
      t10.1 <- vc1[1L, -1L]
      t11.1 <- vc1[-1L, -1L, drop = FALSE]
      s1 <- model$sigma^2L

    })

    #-----------------------------------------
    # Random slope model

    if (isTRUE(random)) {

      if (isTRUE(all(c("RB", "SB") %in% print))) {

        warning("R-squared measures by Raudenbush and Bryk (2002), and Snijders and Bosker (1994) should be computed based on the random intercept model.", call. = FALSE)

      } else if (isTRUE("RB" %in% print)) {

        warning("R-squared measure by Raudenbush and Bryk should be computed based on the random intercept model.", call. = FALSE)

      } else if (isTRUE("SB" %in% print)) {

        warning("R-squared measure by Snijders and Bosker (1994) should be computed based on the random intercept model.", call. = FALSE)

      }

    }

  }

  #-----------------------------------------
  # Calculate R2

  # Raudenbush and Bryk (2002)
  if (isTRUE("RB" %in% print)) {

    # Within-Cluster R2
    rb1 <- 1L - s1 / s0

    # Between-Cluster R2
    rb2 <- 1L - t0.1 / t0.0

  } else {

    rb1 <- rb2 <- NA

  }

  # Snijders and Bosker (1994)
  if (isTRUE("SB" %in% print)) {

    # Within-Cluster R2
    sb1 <- 1L - (s1 + t0.1) / (s0 + t0.0)

    # Between-Cluster R2
    sb2 <- 1L - (s1 / n.j + t0.1) / (s0 / n.j + t0.0)

  } else {

    sb1 <- sb2 <- NA

  }

  # Nakagawa and Schielzeth (2013); Johnson (2014)
  if (isTRUE("NS" %in% print)) {

    vyhat <- var(X %*% beta)
    vy <- vyhat + t0.1 + 2L*(muZ %*% t10.1) + muZ %*% t11.1 %*% muZ + sum(diag(t11.1 %*% vZ)) + s1

    # Marginal R2
    marg <- as.vector(vyhat / vy)

    # Conditional R2
    cond <- as.vector((vyhat + t0.1 + 2L*(muZ %*% t10.1) + muZ %*% t11.1 %*% muZ + sum(diag(t11.1 %*% vZ))) / vy)

  } else {

    marg <- cond <- NA

  }

  #-----------------------------------------
  # Rights and Sterba (2019) R-squared measures

  if (isTRUE("RS" %in% print)) {

    # r2mlm package
    if (isTRUE("RS" %in% print & !nzchar(system.file(package = "r2mlm")))) { stop("Package \"r2mlm\" is needed for to compute R-squared according to Rights and Sterba (2019), please install the package.", call. = FALSE) }

    r2mlm.out <- r2mlm::r2mlm(model, bargraph = FALSE)

    rs <- suppressWarnings(list(decomp = matrix(apply(r2mlm.out$Decomposition, 2L, as.numeric),
                                                ncol = ncol(r2mlm.out$Decomposition),
                                                dimnames = list(rownames(r2mlm.out$Decompositions),
                                                                colnames(r2mlm.out$Decompositions))),
                                r2 = matrix(apply(r2mlm.out$R2s, 2L, as.numeric), ncol = ncol(r2mlm.out$R2s),
                                            dimnames = list(rownames(r2mlm.out$R2s),
                                                            colnames(r2mlm.out$R2s)))))

    #.......................................
    # Plot

    part <- NULL

    # Predictors are not cluster-mean-centered
    if (isTRUE(ncol(rs$decomp) == 1L)) {

      df <- data.frame(var = factor(rep("Total", times = 4L)),
                       part = factor(c("Fixed Slopes", "Slope Variation", "Intercept Variation", "Residual"),
                                     levels = c("Residual", "Intercept Variation", "Slope Variation", "Fixed Slopes")),
                       y = as.vector(rs$decomp))

    # Predictors are cluster-mean-centered
    } else {

      df <- data.frame(var = factor(rep(c("Total", "Within", "Between"), each = 5L),
                                    levels = c("Total", "Within", "Between")),
                       part = factor(c("Fixed Slopes (Within)", "Fixed Slopes (Between)","Slope Variation (Within)", "Intercept Variation (Between)", "Residual (Within)"),
                                     levels = c("Residual (Within)", "Intercept Variation (Between)", "Slope Variation (Within)", "Fixed Slopes (Between)", "Fixed Slopes (Within)")),
                       y = as.vector(rs$decomp))

    }

    p <- ggplot2::ggplot(df, ggplot2::aes(x = var, y = y, fill = part)) +
           ggplot2::geom_bar(stat = "identity") +
           ggplot2::scale_y_continuous(name = "Proportion of Variance",
                                       breaks = seq(0L, 1L, by = 0.1)) +
           ggplot2::theme_bw() +
           ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                          axis.ticks.x = ggplot2::element_blank(),
                          legend.title = ggplot2::element_blank(),
                          legend.position = "bottom",
                          legend.box.margin = ggplot2::margin(-10L, 6L, 6L, 6L)) +
           ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2L, reverse = TRUE))

    # Gray color scales
    if (isTRUE(gray)) {

      p <- p + ggplot2::scale_fill_grey(start = end, end = start)

    } else {

      p <- p + ggplot2::scale_fill_manual(values = rev(color))

    }

    # Print plot
    if (isTRUE(plot)) { suppressWarnings(print(p)) }

    #.......................................

  } else {

    p <- NULL

    rs <- list(decomp = matrix(NA, ncol = 3L, nrow = 5L,
                               dimnames = list(c("fixed, within", "fixed, between", "slope variation", "mean variation", "sigma2"),
                                               c("total", "within", "between"))),
               r2 = matrix(NA, ncol = 3L, nrow = 7L,
                           dimnames = list(c("f1", "f2", "v", "m", "f", "fv", "fvm"),
                                           c("total", "within", "between"))))

  }

  ####################################################################################
  # Return object

  object <- list(call = match.call(),
                 type = "multilevel.r2",
                 model = model,
                 plot = p,
                 args = list(print = print, digits = digits, plot = plot, gray = gray,
                             start = start, end = end, color = color, check = check,
                             output = output),
                 result = list(rb = data.frame(rb1 = rb1, rb2 = rb2),
                               sb = data.frame(sb1 = sb1, sb2 = sb2),
                               ns = data.frame(marg = marg, cond = cond),
                               rs = list(decomp = rs$decomp,
                                         total = data.frame(f1 = ifelse(ncol(rs$r2) > 1L, rs$r2[row.names(rs$r2) == "f1", "total"], NA),
                                                            f2 = ifelse(ncol(rs$r2) > 1L, rs$r2[row.names(rs$r2) == "f2", "total"], NA),
                                                            f = rs$r2[row.names(rs$r2) == "f", "total"],
                                                            v = rs$r2[row.names(rs$r2) == "v", "total"],
                                                            m = rs$r2[row.names(rs$r2) == "m", "total"],
                                                            fv = rs$r2[row.names(rs$r2) == "fv", "total"],
                                                            fvm = rs$r2[row.names(rs$r2) == "fvm", "total"]),
                                         within = data.frame(f1 = ifelse(ncol(rs$r2) > 1L, rs$r2[row.names(rs$r2) == "f1", "within"], NA),
                                                             v = ifelse(ncol(rs$r2) > 1L, rs$r2[row.names(rs$r2) == "v", "within"], NA),
                                                             f1v = ifelse(ncol(rs$r2) > 1L, rs$r2[row.names(rs$r2) == "fv", "within"], NA)),
                                         between = data.frame(f2 = ifelse(ncol(rs$r2) > 1L, rs$r2[row.names(rs$r2) == "f2", "between"], NA),
                                                              m  = ifelse(ncol(rs$r2) > 1L, rs$r2[row.names(rs$r2) == "m", "between"], NA)))))

  class(object) <- "misty.object"

  ####################################################################################
  # Output

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
