### misty 0.5.3 (2023-09-17)

##### Bug fix
*  Fixed a bug in the function `test.welch()`, to remove errors for r-devel from a recent change in r-devel.

### misty 0.5.2 (2023-08-24)

##### Minor features and improvements
* Added the argument `group.ind` to the function `result.lca()` to specify 
latent class indicators as grouping variable in the bar charts. 

### misty 0.5.1 (2023-08-22)

##### Minor features and improvements
* Function `mplus.lca()` can  be used to conduct latent class analysis with
count, unordered categorical, and ordered categorical indicator variables.
* Function `result.lca()` can be used to save bar charts with error bars for confidence 
intervals for each of the latent class solutions.

##### Bug fix
* Fixed a bug in the function `dominance.manual()`, function provided the wrong rank orderning.

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
* Changed the argument `na.omit` in the functions `multilevel.cor()` to `missing` to make it consistent with the arguments of the function `item.cfa()`.
* Changed the default setting of the argument `estimator` in the functions `multilevel.cor()` to `ML`, so that full information maximum likelihood method is used for dealing with missing data.

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
* Changed the output of the functions `size.mean()`, `size.prop()`, and `size.cor()` to include greek letters.
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
* Fixed a bug in the function `as.na()`, function always generated a warning message irrespective of the argument `as.na`  
