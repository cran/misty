### misty 0.7.2 (2025-05-20)

##### New features
* New function `uniq` for extracting unique elements in a vector, matrix, or data 
frame and function `uniq.n` for counting the number of unique elements in a vector
or for each column in a matrix or data frame.

##### Minor features and improvements
* The function `std.coef` computes standardized coefficients for multilevel and linear 
mixed-effects models estimated by using the `lmer` or `lme` function from the lme4 
or nlme package.
* The function `item.alpha` computes coefficient alpha by estimating an essentially 
tau-equivalent measurement model allowing full information maximum likelihood  (FIML) method 
for missing data handling by specifying `missing = "fiml"` along with `estimator = "ML"`. 
* Added the argument `rescov` to the function `item.alpha` for specifying residual 
covariances when computing coefficient alpha.

##### Bug fix
* Fixed a bug in the functions `item.invar()`, functions did not allow specifying 
more than two residual covariances (thanks to Lydia Laninga-Wijnen).

##### User-visible changes
* Changed the default setting of the argument `print` in the functions `item.alpha` 
and `item.omega` to `alpha` and `omega`.
* Removed the argument `na.omit` in the function `item.omega` and added the 
arguments `estimator` and `missing`.

### misty 0.7.1 (2025-03-10)

##### New features
* New function `plot.misty.object()` for plotting a misty object.

##### Minor features and improvements
* Added the argument `factor.labels` to the function `df.head` and `df.tail`.
* Added the arguments `filename`, `width`, `height`, `units`, and `dpi` to the 
functions `aov.b`, `aov.w`, `multilevel.r2`, `multilevel.r2.manual`, `na.pattern`,
`test.levene`, `test.t`, `test.welch`, and `test.z`.

* The function `df.rename` can rename columns in a matrix or variables in a
data frame using `old_name = new_name` and using the functions `toupper`, 
`tolower`, `gsub`, and `sub` similar to the `rename` function in the `dplyr` package. 

##### Bug fix
* Fixed a bug in the function `multilevel.icc()`, computation of the ICC(1) at
Level 2 was wrong in case of a three-level data when specifying `type = "1b"`
(thanks to David S. DeGarmo).
* Fixed a bug in the function `center()`, within-cluster centering of a Level-2 
predictor variable in three-level was wrong (thanks to Stefanos Mastrotheodoros).
* Fixed a bug in the functions `df.head()` and `df.tail()`, function could not
handle date and times. 

##### User-visible changes
* The argument `subset` in the function `df.subset` is specified without quotation
marks in line with the argument `subset` in the function `subset` function.
* Moved the argument `data` in the functions `as.na`, `na.as`, `center`, 
`ci.cor`, `ci.cor`, `ci.mean`, `ci.median`, `ci.mean.w`, `ci.var`, `ci.sd`,
`cluster.scores`, `coding`, `cor.matrix`, `crosstab`, `descript`, `df.duplicated`,
`df.unique`, `df.move`, `df.subset`, `effsize`, `freq`, `item.alpha`, `item.cfa`,
`item.invar`, `item.omega`, `item.reverse`, `item.scores`, `lagged`, `multilevel.cfa`,
`multilevel.cor`, `multilevel.descript`, `multilevel.icc`, `multilevel.invar`,
`multilevel.omega`, `na.auxiliary`, `na.coverage`, `na.descript`, `na.indicator`,
 `na.pattern`,  `na.prop`, `na.test`, `rec`,  `rwg.lindell`, `skewness` 
 to the first position.
* Renamed the argument `x` in the functions `df.check`, `df.head`, `df.rename`,
`df.sort` to `data`.
* Renamed the argument `x` in the function `multilevel.fit` to `model`.
* Renamed the arguments `alpha`, `ci.plot`, `plot.point`, `saveplot` 
in the functions `ci.cor`, `ci.mean`, `ci.median`, `ci.prop`, `ci.var`, and `ci.sd`
to `hist.alpha`, `confint`, `point` and `filename`.
* Renamed the argument `fill.col` in the functions `na.pattern` to `color`.
* Renamed the argument `file` in the functions `blimp.plot`, `mplus.plot`, and
`na.pattern` to `filename`.
* Removed the argument `saveplot` from the functions `na.pattern`.
* Changed the default setting of the argument `na` in the functions `na.indicator()`, 
to `1`.

### misty 0.7.0 (2025-02-02)

##### New features
* New function `ci.cor()` for computing and potting Fisher z' confidence interval 
for the Pearson product-moment correlation coefficient adjusted via sample joint 
moments method or via approximate distribution method (Bishara et al., 2018), 
Spearman's rank-order correlation coefficient with Fieller et al. (1957) standard 
error, Bonett and Wright (2000) standard error or rank-based inverse normal (RIN) 
transformation, and Kendall's Tau-b, and Kendall-Stuart's Tau-c. The function also
supports five types of bootstrap confidence intervals
* New function `chr.trunc()` for truncating a character vector, so that the number 
of characters of each element of the character vector is always less than or equal 
to the specified width.
* New function `df.head()` and `df.tail()` for printing the first or last rows
of a data frame and displaying only as many columns as fit on the console.
* New function `df.check()` which is a wrapper function around the functions 
`dim()`, `names()`, `df.head()`, and `df.tail()`.

##### Minor features and improvements
* The functions `skewness` and `kurtosis` can also compute Mardia's multivariate
skewness and kurtosis.
* The functions `ci.mean`, `ci.median`, `ci.prop`, `ci.var`, and `ci.sd` can
also compute and plot bootstrap confidence intervals.
* Added the argument `sample` to the function `descript`.
* Added the argument `append` to the function `check.outlier`.
* Added the arguments `sep` and `dec` to the function `read.data`.
* Added the options `gray1`, `gray2`, and `gray3` to the argument `color` of the 
function `chr.color`.

##### Bug fix
* Fixed a bug in the function `blimp.print()`, function did print error messages.
* Fixed bugs in functions that could not handle a tibble as input (thanks to David S. DeGarmo). 

##### User-visible changes
* Changed the arguments `nrow`, `ncol` and `scales` into `facet.nrow`, `facet.ncol`
and `facet.scales` in the functions `blimp.plot()` and `mplus.plot()`.
* Changed the argument `error.width` into `errorbar.width` in the functions 
`aov.b()`, `aov.w()`, `result.lca()`, `test.t()` and `test.z()`.
* Changed the argument `line.size` and `line.type` into `linewidth` and `linetype` 
in the functions `test.t()` and `test.z()`.
* Changed the arguments `line.color1`, `line.color2`, `line.type1`, `line.type2`,
`line.width1`, `line.width2`, `bar.color`, `axis.size`, `strip.size`, `xlimits`,
and `ylimits` into `line.col1`, `line.col2`, `linetype1`, `linetype2`,
`linewidth1`, `linewidth2`, `bar.col`, `axis.text.size`, `strip.text.size`, `xlim`,
and `ylim`  in the function `check.resid()`.
* Changed the default setting of the argument `line` in the functions `aov.w()`, 
to `FALSE`.
* Merged help pages for the functions `size.mean`, `size.prop` and `size.cor` 
into one help page.
* Changed the default setting of the argument `missing` in the functions `multilevel.cfa()`, 
to `"fiml"`.
* Changed the default setting of the argument `estimator` in the functions `multilevel.cor()`, 
to `"MLR"`.
* Changed the default setting of the argument `na` in the functions `na.indicator()`, 
to `1`.

### misty 0.6.8 (2024-10-24)

##### New features
* New functions `na.satcor()`, `cfa.satcor()`, `sem.satcor()`, `growth.satcor()`, 
and `lavaan.satcor()` to estimate a confirmatory factor analysis model, structural 
equation model, growth curve model, or latent variable model in the `lavaan` package
using full information maximum likelihood  (FIML) method to missing data handling 
while automatically specifying a saturated correlates model to incorporate auxiliary 
variables into a substantive model.
* New function `read.data()` to read data files in CSV, DAT, TXT, SPSS, Excel, or
Stata DTA format.

##### User-visible changes
* Changed the default setting of the argument `print` in the functions `na.test()`, 
to `little`.

##### Bug fix
* Fixed a bug in the function `mplus.plot()`, which caused an error message when 
requesting a loop plot by specifying `plot = "loop"`.
* Fixed a bug in the function `mplus.print()`, which caused an error message when 
printing a Mplus output for an automatic testing of measurement invariance.

### misty 0.6.7 (2024-09-10)

##### Minor features and improvements
* The functions `mplus` and `blimp` do not require the `...;` specification 
in the `VARIABLES` section anymore when specifying variable names with the argument
`data`.
* Added the argument `labels` in the function `blimp.plot()` to show parameter 
labels in the facet labels.

##### User-visible changes
* The function `na.auxiliary()` does not print full `NA` rows of the Cohen's d 
matrix anymore.
* The function `na.indicator()` creates a missing data indicator matrix with 
`0 = observed` and `1 = missing`.
* Added the arguments `na`, `append` and `name` to the function `na.indicator()`.

##### Bug fix
* Fixed a bug in the function `mplus.print()`, function did not the print input
result when specifying `print = "all"`.
* Fixed a bug in the function `blimp()`, which caused an error message when 
specifying a `posterior = TRUE` and saving the posterior distribution failed.
* Fixed a bug in the function `blimp.print()`, which caused an error message when
specifying a `misty.object` for the argument  `x`.
* Fixed a bug in the function `blimp.plot()`, function did not save and plots 
regardless of the setting of the argument `saveplot`.

### misty 0.6.6 (2024-08-26)

##### New features
* New function `mplus.plot()` to read a Mplus GH5 file to display trace plots, 
posterior distribution plots, autocorrelation plots, posterior predictive check 
plots, and loop plots.
* New function `blimp.run()` to run a group of Blimp models located within a single 
directory or nested within subdirectories.
* New function `blimp.print()` to print  a Blimp output file on the R console.
* New function`blimp.plot()` to read the posterior distribution for all parameters
to display trace plots and posterior distribution plots.
* New function `blimp()` to create and run a Blimp input to print the output on 
* New function `blimp.update()` to update specific input command sections of a 
`misty.object` of type `blimp` to create an updated Blimp input file, run the 
updated input file, and print the updated Blimp output.
* New function `mplus.bayes()` to read a Mplus GH5 file and `blimp.bayes()` 
to read the posterior distribution for all parameters to compute point estimates,
measures of dispersion, measures of shape, credible intervals, convergence and 
efficiency diagnostics, probability of direction, and probability of being in 
the ROPE for the posterior distribution for each parameter.
* The `na.test` function provides Jamshidian and Jalalꞌs approach for testing the
missing completely at random (MCAR) assumption.
* New function `clear()` to clear the console equivalent to `Ctrl + L` in RStudio.
* New function `chr.color()` to add color and style to output texts on terminals 
that support 'ANSI' color and highlight codes.

##### Minor features and improvements
* Added the option `default` to the argument `print` of the `descript` function.
* Added the argument `comment` to the `mplus` function.
* The function `na.test` performs Little's MCAR test using the `mlest` function 
from the `mvnmle` package that can handle up to 50 variables instead of using 
the `prelim.norm` function in the `norm` package that can only handle about 30 
variables.
* The function `na.pattern` plots the missing data pattern when specifying 
`plot = TRUE` and runs faster. 
* The function `na.auxiliary` computes semi-partial correlations of an outcome variable
conditional on the predictor variables of a substantive model with a set of
candidate auxiliary variables to identify correlates of an incomplete outcome
variable as suggested by Raykov and West (2016).. 

##### User-visible changes
* Changed the default setting of the argument `print` in the functions `mplus.print`, 
to `result`.
* The function `mplus.print` does not print the section `MODEL FIT INFORMATION` 
if the degrees of freedom is zero.
* Renamed the argument `run.mplus` in the function `mplus.lca()` to `mplus.run`.
* Changed the default setting of the argument `ls.fit` in the function `multilevel.cfa()`
to `FALSE`.

##### Bug fix
* Fixed a bug in the function `mplus.update()`, which caused an error message when
specifying `output = FALSE`.
* Fixed a bug in the function `mplus.lca()`, which caused an error message when 
checking the input for the argument `processors`.
* Fixed a bug in the functions `item.cfa()` and `multilevel.cfa()`, functions did
not allow specifying more than two residual covariances (thanks to Lydia Laninga-Wijnen).
* Fixed a bug in the function `multilevel.descript()`, average, minimum, and 
maximum cluster size at Level 3 were calculated incorrectly.
* Fixed a bug in the function `item.omega()`, function did not provide item 
statistics regardless of the `print` argument setting (thanks to Ainhoa Coloma Carmona).

### misty 0.6.5 (2024-06-29)

##### Minor features and improvements
* Updated the function `mplus.run()` according to the latest version of the 
function `runModels()` in the MplusAutomation package.

##### Bug fix
* Fixed a bug in the function `mplus.print()`, function did not print result of 
a misty.object of type mplus.

### misty 0.6.4 (2024-06-26)

##### New features
* New function `mplus.print()` for printing a Mplus output file on the R console.
* New function `mplus()` to create and run a Mplus input to print the output on 
the console.
* New function `update.mplus()` to update specific Mplus input command sections
in the `mplus` object, run the updated input file, and print the output on the console.
* New functions `chr.grep()` and `chr.grepl()` for multiple pattern matching, i.e., 
`grep()` and `grepl()` functions for matching a vector of character strings.

##### User-visible changes
* The function `write.mplus()` is not restricted to variable names with up to 8 
characters anymore.
* Renamed the function `run.mplus()` to `mplus.run()`.
* Changed the default setting of the argument `posthoc` in the functions `aov.b()`, 
`aov.w()` and `test.welch()` to `FALSE`.
* Changed the option of the argument `replace` from `modifiedDate` to `modified`
in the functions `mplus.lca()` and `mplus.run()`.
* Changed the arguments `showOutput` into `show.out` and `replaceOutfile` into
`replace.out` in the function `mplus.run()`.
* Added the argument `message` to the function `mplus.run()`.

##### Bug fix
* Fixed a bug in the function `test.welch()`, function did not print post hoc 
tests when specifying `posthoc = TRUE`.

### misty 0.6.3 (2024-05-15)

##### Bug fix
* Fixed a bug in the function `result.lca()`, function excluded all outputs 
which involved the word `ERROR` even though results were available (thanks to Michael Weber).
* Fixed a bug in the function `multilevel.fit()`, function used the 
number of observations at the Within level instead of the Between level for 
computing RMSEA at the Between Level (thanks to Maurizio Sicorello).
* Fixed a bug in the function `descript()` which caused an error message when
specifying a split variable.
* Fixed a bug in the function `robust.coef()` which caused an error message in
the presence of missing data on predictor variables.
* Fixed a bug in the functions `multilevel.icc()` and `multilevel.descript()` 
which caused an error message in when specifying a tibble instead of a data frame
(thanks to Tanja Held).

##### Minor features and improvements
* In the function `mplus.lca()`, the argument `processors` allows to specify the 
number of processors and threads separately.
* In the function `item.omega()`, residual covariances can be specified when `type = "categ"`.

### misty 0.6.2 (2024-02-05)

##### Bug fix
* Fixed a bug related to variable selection using the operators 
`.`, `+`, `-`, `~`, `:`, `::`, functions which caused an warning message. 
* Fixed a bug in the functions `center()`, `multilevel.icc()`, and `multilevel.descript()` 
which caused an error message in three-level data with ambiguously coded cluster 
variables common in longitudinal data. 

##### User-visible changes
* Revised the function `multilevel.descript()` to take into account missing 
values, e.g., `No. of cases` and `No. of clusters` show the number observations 
and clusters after excluding missing values. 
* Variable attributes in the function `write.sav()` do not require specifying all
three columns `label`, `values`, and `missing` anymore.

##### Minor features and improvements
* Added the argument `na` to the function `read.mplus()`.

### misty 0.6.1 (2024-01-19)

##### User-visible changes
* Removed the Fortran implementation of the polychoric correlation coefficient
because it causes problems when loading the package on Mac computers.

##### Bug fix
* Fixed a bug in the function `freq()`, function did not provide an output. 

### misty 0.6.0 (2024-01-12)

##### New features
* New function `df.subset()` for subsetting data frames using the operators 
`.`, `+`, `-`, `~`, `:`, `::`, and `!` similar to functions from the R package `tidyselect`.
* New function `lagged()` to compute lagged values of variables.
* New function `df.move()` to move variable(s) in a data frame.
* New functions `read.dta()` and `write.dta()` to read and write Stata DTA files.
* New function `coding()` to code categorical variables, i.e., dummy, simple, 
unweighted and weighted effect, repeated, forward Helmert, reverse Helmert, and 
orthogonal polynomial coding.
* New function `effsize()` to compute effect sizes for categorical variables, i.e.,
(adjusted) phi coefficient, (bias-corrected) Cramer's V, (bias-corrected) Tschuprow's T, 
(adjusted) Pearson's contingency coefficient, Cohen's w, and Fei.
* New function `script.copy()` to save a copy of the current script in RStudio
with the current date and time.

##### Minor features and improvements
* Functions `as.na()`, `na.as()``center()`, `ci.mean()`, `ci.mean.w()`, `ci.median()`, 
`ci.prop()`, `ci.var()`, `ci.sd()`, `cluster.scores()`, `cor.matrix()`,
`crosstab()`, `descript()`, `freq()`, `item.alpha()`, `item.cfa()`, `item.invar()`,
`item.omega()`, `item.reverse()`, `item.scores()`, `multilevel.cfa()`, `multilevel.cor()`,
`multilevel.descript()`, `multilevel.fit()`, `multilevel.icc()`, `multilevel.invar()`,
`multilevel.omega()`, `na.auxiliary()`, `na.coverage()`, `na.descript()`,
`na.indicator()`, `na.pattern()`, `na.prop()`, `na.test()` `rec()`, `rwg.lindell()`,
`skewness()`, and `kurtosis()` provide the argument `...` instead of the argument 
`x` to specify variables from the data frame specified in `data` using the operators 
`.`, `+`, `-`, `~`, `:`, `::`. 
* Function `multilevel.icc()` computes intraclass correlation coefficients in
three-level data.
* Function `multilevel.descript()` computes multilevel descriptive statistics
in three-level data.
* Function `center()` centers predictor variables in three-level data.
* Function `na.descript()` provides descriptive statistics for missing data in
two-level and three-level data.
* Function `cor.matrix()` computes tetrachoric and polychoric correlation coefficients.
* Added the arguments `write` and `append` to all functions providing a 
print function to save the print output into a text file.

##### User-visible changes
* Changed the default setting of the argument `names` in the function `rec()` to `.e`.
* Changed the default setting of the arguments `label` and `labels` in the `read.sav` function to `FALSE`.
* Changed the argument `value` in the function `na.as()` to `na` to make it consistent with the arguments of the function `as.na()`.
* Changed the argument `resid.cov` in the function `item.omega()` to `resocv` to make it consistent with the arguments of the functions `item.cfa()` and `multilevel.cfa()`.
* Changed the argument `names` in the functions `center`, `cluster.scores`, 
`item.reverse`, and `rec` to `name` to make it consistent with the arguments of the functions 
`item.scores()`, `na.prop()`, and `lwg.lindell()`.
* Changed the argument `x` and `...` in the functions `df.duplicated()` and `df.unique()`to `...` and `data` to make it consistent with all other functions using the `...` argument.
* Merged help pages for the functions `as.na` and `na.as` into one help page.
* Merged help pages for the functions `script.open`, `script.close`, and `script.save` into one help page.
* Merged help pages for the functions `skewness` and `kurtosis` into one help page.
* Merged help pages for the functions `ci.mean` and `ci.median` into one help page.
* Merged help pages for the functions `ci.var` and `ci.sd` into one help page.
* Removed the function `shift()` and replaced it by the function `lagged()`.
* Removed the function `dummy.c()` and replaced it by the function `coding()`
* Removed the functions `cor.phi()`, `cor.cont()`, `cor.cramer()`, and `eta.sq()` and replaced them by the function `effsize()`.
* Removed the function `cor.poly()` and integrated polychoric correlation coefficient into the function `cor.matrix()`.

##### Bug fix
* Fixed a bug in the function `multilevel.descript()`, function led to a node stack overflow. 

### misty 0.5.4 (2023-11-14)

##### New features
* New function `shift()` to compute lagged or leading values of a vector.

##### Bug fix
* Fixed a bug in the function `libraries()`, version of the packages were not correctly displayed. 

### misty 0.5.3 (2023-09-17)

##### Bug fix
* Fixed a bug in the function `test.welch()`, to remove errors for r-devel from a recent change in r-devel.

### misty 0.5.2 (2023-08-24)

##### Minor features and improvements
* Added the argument `group.ind` to the function `result.lca()` to specify. 
latent class indicators as grouping variable in the bar charts. 

### misty 0.5.1 (2023-08-22)

##### Minor features and improvements
* Function `mplus.lca()` can be used to conduct latent class analysis with
count, unordered categorical, and ordered categorical indicator variables.
* Function `result.lca()` can be used to save bar charts with error bars for confidence 
intervals for each of the latent class solutions.

##### Bug fix
* Fixed a bug in the function `dominance.manual()`, function provided the wrong rank ordering.

##### User-visible changes
* Renamed the functions `mplus.lpa()` and `results.lpa()` to `mplus.lca()` and `results.lca()`.

### misty 0.5.0 (2023-08-08)

##### New features
* New function `item.invar()` for evaluating configural, metric, scalar, and strict
between-group or longitudinal (partial) measurement invariance.
* New function `robust.coef()` for computing heteroscedasticity-consistent standard 
errors and significance values for linear models estimated by using the `lm()` 
function and generalized linear models estimated by using the `glm()` function.
* New function `dominance()` for linear models estimated by using the `lm()` function
and `dominance.manual()` to conduct dominance analysis based on a (model-implied) 
correlation matrix of the manifest or latent variables.
* New function `check.resid()` for performing residual diagnostics to detect 
nonlinearity (partial residual or component-plus-residual plots), nonconstant 
error variance (predicted values vs. residuals plot), and non-normality of residuals 
(Q-Q plot and histogram with density plot).
* New function `mplus.lpa()` for writing Mplus input files for conducting latent
profile analysis based on six different variance-covariance structures.
* New function `result.lpa()` for creating a summary result table for latent profile
analysis from multiple Mplus output files within subfolders.

##### Minor features and improvements
* Added the argument `order` to the function `multilevel.cor()` to order variables 
in the output table so that variables specified in the argument `between` are 
shown first.
* Added modification indices for parameter constraints to the function `multilevel.cfa()`
and `multilevel.invar()`.
* Added residual correlation matrix to the function `item.cfa()`, `multilevel.cfa()`,
and `multilevel.invar()`.
* Function `write.result()` can also write results based on the return object of
the `std.coef` function.

##### User-visible changes
* Renamed the argument `min.value` in the function `item.cfa()`, `multilevel.cfa()`,
and `multilevel.invar()` to `mod.minval` and changed the default setting to `6.63`.

### misty 0.4.12 (2023-07-08)

##### User-visible changes
* Removed the R package `r2mlm` from the `Imports` field in the `DESCRIPTION` due to dependencies issues.

##### Minor features and improvements
* Function `multilevel.descript()` can also deal with between-cluster variables by reporting means and standard deviations at the cluster level.
* Added the argument `print` to the function `multilevel.descript()` to request standard deviation of the variance components. 

### misty 0.4.11 (2023-06-10)

##### New features
* New function `multilevel.fit()` for computing simultaneous and level-specific model
fit information for a fitted multilevel model containing no cross-level constraints from the R package lavaan.
* New function `multilevel.cfa()` for conducting multilevel confirmatory factor analysis using the R package lavaan to investigate four types
of constructs, i.e., within-cluster, shared, configural, and simultaneous shared and configural cluster constructs.
* New function `multilevel.invar()` for evaluating configural, metric, and scalar cross-level measurement invariance using multilevel confirmatory factor
analysis.
* New function `multilevel.omega()` for computing point estimate and Monte Carlo confidence interval for the multilevel composite reliability defined by Lai (2021) for a within-cluster construct, shared cluster-level construct, and configural cluster construct.

##### Minor features and improvements
* Added convergence checks to the function `multilevel.cor()`, e.g., warning message is printed when absolute correlations are greater than 1.
* Argument `cluster` in the function `multilevel.cor()`, `multilevel.descript()`, and `multilevel.icc()` can also be specified using the variable name of the cluster variable in `x`.

##### User-visible changes
* Revised the output of the `item.cfa()` function, e.g., loglikelihood and information criteria are shown above chi-square test of model fit and label `Ad Hoc` changed to `Scaled`. 

### misty 0.4.10 (2023-05-18)

##### Bug fix
* Fixed a bug in the function `multilevel.cor()`, which caused an error message (thanks to Richard Janzen).

### misty 0.4.9 (2023-05-02)

##### New features
* New function `libraries()` to load and attach multiple add-on packages at once. 
* New function `check.outlier()` computes statistical measures for leverage, distance, 
and influence for linear models estimated by using the `lm()` function

##### Minor features and improvements
* When using function `write.result()`, result tables are in line with the arguments
`print`, `tri`, `digits`, `p.digits`, and `icc.digits` specified in the object `x` (thanks to Stefan Kulakow).
* Function `crosstab()` displays marginal row-wise, column-wise, and total percentages in the output (thanks to Joachim Fritz Punter and Lisa Bucher). Note that the function now also returns the crosstable in the list element `result$crosstab` of the return object .

##### User-visible changes
* Revised the `Value` sections in the documentation of the functions.
* Changed the default setting of the argument `weighted` in the `test.t` and the `na.auxiliary` function 
to `FALSE` in line with the recommendation by Delacre et al. (2021).
* Renamed the function `collin.diag()` to `check.collin()`.

##### Bug fix
* Fixed a bug in the function `read.mplus()`, an error message was printed if comments in the Mplus input file contains special characters (e.g., ä, ü, ö).
* Fixed a bug in the function `std.coef()`, the function was not applicable to predictors specified as character vector or factor.

### misty 0.4.8 (2023-03-10)

##### New features
* New functions `script.close()`, `script.new()`, `script.open()`, and `script.save()` to close, open, and save R scripts in RStudio. 
* New function `setsource()` to set the working directory to the source file location in RStudio equivalent to using the menu item `Session - Set Working Directory - To Source File Location`.
* New function `restart()` to restart the RStudio session equivalent to using the menu item `Session - Restart R`.
* New function `multilevel.r2.manual()` to compute R-squared measures by Rights and Sterba (2019) for
multilevel and linear mixed effects models by manually inputting parameter estimates. 

##### Minor features and improvements
* Functions `center()`, `cluster.scores()`, `rec()`, and `item.reverse()` can be applied to more than one variable at once.

### misty 0.4.7 (2023-01-06)

##### New features
* New function `aov.w()` for performing repeated measures analysis of variance (within-subject ANOVA) including paired-samples t-tests for multiple comparison, descriptive statistics, effect size measures, and a plot showing error bars for within-subject confidence intervals.
* New function `ci.mean.w()` for computing difference-adjusted Cousineau-Morey within-subject confidence intervals.

##### Minor features and improvements
* Function `ci.mean.diff()` computes the confidence interval for the difference for an arithmetic mean in a one-sample design.
* Functions `aov.b()`, `test.t()`, `test.welch()`, and `test.z()` plot difference-adjusted confidence intervals in two-sample design by default.
* Added the argument `jitter.height` to the functions `aov.b()`, `test.levene()`, `test.t()`, `aov.welch()`, and `test.z()`.
* Added the argument `adjust` to the function `ci.mean()`, to apply difference-adjustment for the confidence interval.

##### User-visible changes
* Function `test.t()` displays the confidence interval for the mean difference in the one-sample t-test.

##### Bug fix
* Fixed a bug in the function `test.t()`, result table provided by the function did not display the confidence interval correctly.

### misty 0.4.6 (2022-06-08)

##### New features
* New function `aov.b()` for performing between-subject analysis of variance including Tukey HSD post hoc test for multiple comparison.

##### Minor features and improvements
* Function `as.na()` is also applicable to arrays
* Added the argument `plot` and arguments for various graphical parameters for plotting results to the functions `test.levene()`, `test.t()`, `test.welch()`, and `test.z()`.
* Added the argument `write` for writing results into an Excel file to the functions `cor.matrix()`, `crosstab()`,
`descript()`, `freq()`, `item.alpha()`, `item.cfa()`, `item.omega()`, `multilevel.cor()`, `multilevel.descript()`,
`na.coverage()`, `na.descript()`, and `na.pattern()`
* Added the argument `posthoc` for conducting Games-Howell post hoc test for multiple comparison 
to the functions `test.welch()`.

### misty 0.4.5 (2022-04-29)

##### New features
* New function `item.cfa()` for conducting confirmatory factor analysis using the R package lavaan.

##### Minor features and improvements
* Function `write.result()` can also write results based on the return object of the `item.cfa()` function.
* Argument `exclude` of the function `freq()` can also be set to `FALSE`.

##### User-visible changes
* Revised the output of the function `multilevel.cor()` to make it consistent with the output of the function `item.cfa()`.
* Changed the argument `na.omit` in the function `multilevel.cor()` to `missing` to make it consistent with the arguments of the function `item.cfa()`.
* Changed the default setting of the argument `estimator` in the function `multilevel.cor()` to `ML`, so that full information maximum likelihood method is used for missing data handling.

##### Bug fix
* Fixed a bug in the function `multilevel.cor()`, function did not use Huber-White 
robust standard errors, but conventional standard errors when specifying `estimator = "MLR"`. 

### misty 0.4.4 (2022-02-24)

##### New features
* New function `multilevel.r2()` for computing R-squared measures for multilevel and linear mixed effects models.
* New function `write.xlsx()` for writing Excel files (.xlsx).
* New function `write.result()` for writing results of a misty object into an Excel file.

##### Minor features and improvements
* Added mean and variance components to the output of the function `multilevel.descript()`.
* Added the argument `round` to the function `freq()` for rounding numeric variables.

##### User-visible changes
* Added a warning message in the `na.test()` function when running into numerical problems.
* Changed the default setting of the argument `sig` in the functions `cor.matrix()`
and `multilevel.cor()` to `FALSE`.

##### Bug fix

### misty 0.4.3 (2021-09-30)

##### User-visible changes
* Examples added to the documentation of the `collin.diag()` function. 

##### Bug fix
* Fixed a bug in the function `print.misty.object()`, function did not print the result object of the the function `crosstab()` correctly when requesting percentages.

### misty 0.4.2 (2021-08-19)

##### New features
* New function `multilevel.cor()` for computing the within-group and between-group correlation matrix using the lavaan package.
* New function `na.test()` for performing Little's missing completely at random (MCAR) test.
* New function `indirect()` for computing confidence intervals for the indirect effect using the asymptotic normal method, the distribution of the product method, and the Monte Carlo method.
* New function `multilevel.indirect()` for computing confidence intervals for the indirect effect in a 1-1-1 multilevel mediation model using the Monte Carlo method.

##### Minor features and improvements
* Function `cor.matrix()` highlights statistically significant correlation coefficients in boldface.
* Function `cor.matrix()` shows the results in a table when computing a correlation coefficient for two variables.
* Added test statistic (`stat`) and degrees of freedom (`df`) to the argument `print` in the function `cor.matrix()`.
* Added the argument `continuity` for continuity correction to the function `cor.matrix()` for testing Spearman's rank-order correlation coefficient and Kendall's Tau-b correlation.
* Substantial speed improvement for the function `cor.matrix()` when computing Spearman's rank-order correlation coefficient or Kendall's Tau-b correlation.

##### User-visible changes
* Changed the argument `group` in the functions `center()`, `group.scores()`, `multilevel.descript()`, `multilevel.icc()`, and `rwg.lindell()` to `cluster`.
* Renamed the function `group.scores()` to `cluster.scores()`.

##### Bug fix
* Fixed a bug in the function `cor.matrix()`, function did not print sample sizes when specifying a grouping variable and using listwise deletion.

### misty 0.4.1 (2021-06-07)

##### Minor features and improvements
* Function `write.mplus()` writes a Mplus input template with variables names specified in the DATA command along with the tab-delimited data file by default.

##### User-visible changes
* Removed the argument `print()` in the `write.mplus()` function.
* Changed the default setting of the argument `weighted` in the `test.welch()` function into `FALSE` following the recommendation by Delacre et al. (2021).

##### Bug fix
* Fixed a bug in the function `cohens.d()`, function printed warning messages of the `pt()` function.
* Fixed a bug in the function `cohens.d()`, function could not deal with more than one variable in a one-sample design.

### misty 0.4.0 (2021-05-13)

##### New features
* New function `test.t()` for performing one-sample, two-sample, and paired-sample t-tests including Cohen's d effect size measure.
* New function `test.welch()` for performing Welch's t-test including Cohen's d effect size measure and Welch's ANOVA including $\eta^2$ and $\omega^2$ effect size measures.

##### Minor features and improvements
* Added standard error of the mean to the argument `print` in the function `descript()`.
* Added the arguments `format`, `label`, `labels`, `missing` to the function `read.sav()` to remove variable formats, variable labels, value labels, value labels for user-defined missings, and widths from attributes of the variable.
* Function `item.reverse()` can also be applied to to items with non-integer values.
* Return object of the function `cor.matrix()` when specifying a grouping variable comprises the combined results of both groups in the matrices. 
* Function `read.mplus()` can also deal with consecutive variables (e.g., `x1-x5`).
* Added `group` and `split` arguments to the function `cohens.d()`.
* Added Cohen's d effect size measure to the output of the `test.z` function.
* Function `cohens.d()` computes various kinds of Cohen's d, Hedges' d, and Glass's $\Delta$ including confidence intervals, e.g., weighted and unweighted pooled standard deviation in a two-sample design, with and without controlling for the correlation between the two sets of measurement in a paired-sample design, or with and without the small-sample correction factor. 

##### User-visible changes
* Renamed following functions: `alpha.coef()` to `item.alpha()`, `cont.coef()` to `cor.cont()`, `cramers.v()` to `cor.cramer()`, `levenes.test()` to `test.levene()`, `mgsub()` to `chr.gsub()`, `omega.coef()` to `item.omega()`, `reverse.item()` to `item.reverse()`, `phi.coef()` to `cor.phi()`, `poly.cor()` to `cor.poly()`, `scores()` to `item.scores()`, `stromit()` to `chr.omit()`, `trim()` to `chr.trim()`, `z.test()` to `test.z()`,
* Changed the argument `use` in the `cor.matrix()` function into `na.omit`.
* Changed the default setting of the argument `method` in the functions `multilevel.descript()` and `multilevel.icc()` to `"lme4"`; if the lme4 package is not installed, `"aov"` will be used.
* Changed the output of the functions `ci.mean.diff()` and `ci.mean.prop()` when computing confidence intervals in two-sample designs, i.e., results are divided in two rows according to the grouping variable.
* Changed the output of the functions `ci.mean.diff()` and `ci.mean.prop()` when computing confidence intervals in paired-sample designs, i.e., output reports the number of missing data pairs (`nNA`), instead of number of missing values for each variable separately (`nNA1` and `nNA2`).
* Changed the output of the functions `descript()` when specifying the argument `levenes.test()`, i.e., duplicated labels in the column `group` or `variable` are not shown.
* Changed the functions `cohens.d()` into a generic function with the methods `cohens.d.default()` and `cohens.d.formula()`.
* Added arguments `hypo` and `descript` to the functions `test.levene()` and `test.z()`.
* Added titles to the output of the `freq`, `descript`, and `crosstab` function.
* Changed the argument `as.na` in the `as.na()` function into `na`.

##### Bug fix
* Fixed a bug in the function `center()` which caused an error message in case of groups with only one observation when trying to apply group mean centering. 
* Fixed a bug in the function `center()` which caused an error message when trying to apply grand mean centering of a Level 1 predictor.
* Fixed a bug in the function `cohens.d()`, an error message was printed in the between subject design whenever specifying a grouping variable with missing values.
* Fixed a bug in the function `cor.matrix()`, which caused an error when using listwise deletion for missing data while specifying a grouping variable.
* Fixed a bug in the function `descript()`, which caused an error message when selection only one or two argument statistical measures using the argument `print`.
* Fixed a bug in the function `freq()`, where the argument `split` was broken.
* Fixed a bug in the function `test.zz()`, where the alternative hypothesis was displayed wrong when specifying `alternative = "greater"` or `alternative = "less"`.

### misty 0.3.2 (2020-06-08)

##### New features
* New function `collin.diag()` for collinearity diagnostics including tolerance, (generalized) standard error inflation factor, (generalized) variance inflation factor, eigenvalues, conditional indices, and variance proportions for linear, generalized linear, and mixed-effects models.
* New function `std.coef()` for computing standardized coefficients (StdX, StdY, and StdYX) for linear models estimated by using the `lm()` function.
* New function `mgsub()` for multiple pattern matching and replacements, i.e., `gsub()` function for matching and replacing a vector of character strings.
* New functions `df.duplicated()` and `df.unique()` extracting duplicated or unique rows of a matrix or data frame.

##### Bug fix
* Fixed a bug in the function `read.xlsx()`, default setting of the argument `progress` was wrong.

##### User-visible changes
* Merged all print functions to a single print function called `print.misty.object()`.

### misty 0.3.1 (2020-04-25)

##### New features
* New function `z.test()` for performing one sample, two sample, and paired sample z-test.

##### Bug fix
* Function `omega.coef()` does not access internal slots of a fitted lavaan object anymore (requested by Yves Rosseel).

##### User-visible changes
* Added descriptive statistics and confidence intervals to the function `levenes.test()`.
* Changed the output of the functions `size.mean()`, `size.prop()`, and `size.cor()` to include Greek letters.
* Changed the argument `theta` in the `size.mean()` function into `delta`.

### misty 0.3.0 (2020-04-06)

##### New features
* New functions `ci.mean()`, `ci.mean.diff()`, `ci.median()`, `ci.prop()`, `ci.prop.diff()`, `ci.sd()`, `ci.var()` for computing confidence interval for the arithmetic mean, the difference in arithmetic means, the median, the proportion, the difference in proportions, the variance, and the standard deviation.
* New function `levenes.test()` for conducting Levene's test for homogeneity of variance. 
* New function `omega.coef()` for computing coefficient omega (McDonald, 1978), hierarchical omega (Kelley & Pornprasertmanit, 2016), and categorical omega (Green & Yang, 2009).
* New function `read.xlsx()` for reading Excel files (.xlsx).

##### Minor features and improvements
* Added ordinal coefficient alpha to the function `coef.alpha()`.
* Added Kendall-Stuart's Tau-c correlation coefficient to the function `cor.matrix()`.
* Function `as.na()` can also replace user-specified values with missing values in lists.

##### User-visible changes
* Changed the argument `use` in the `alpha.coef()` function into a logical argument `na.omit`.
* Changed the argument `pval.digits` in the `cor.matrix()` function into `p.digits`.
* Merged print functions `print.cont.coef()`, `print.cramers.v()`, `print.na.auxiliary()`, `print.na.coverage()`, `print.phi.coef()`, and `print.poly.cor()` into `print.square.matrix()`

##### Bug fix

* Fixed a bug in several function, where `is.vector()` function was used to test if an object is a vector. Instead `is.atomic()` function is used to test if an object is a vector.
* Fixed a bug in the function `as.na()`, function converted strings in data frames to factors.

### misty 0.2.2 (2020-02-26)

##### New features

* New function `trim()` for removing whitespace from start and/or end of a string. Note that this function is equivalent to the function `trimws()` in the `base` package. However, the `trimws()` function fails to remove whitespace in some instances.

##### Bug fix

* Fixed a bug in the function `cohens.d()`, function returned `NA` for Cohen's d in within-subject design in the presence of missing values
* Fixed a bug in the function `alpha.coef()`, function did not provide any item statistics irrespective of the argument `print`
* Fixed a bug in the function `as.na()`, function always generated a warning message irrespective of the argument `as.na`.
