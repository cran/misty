### misty 0.4.4 (2021-12-31)

##### New features
* New function `multilevel.r2()` for computing R-squared measures for multilevel and linear mixed effects models.
* New function `write.xlsx()` for writing Excel files (.xlsx).
* New function `write.result()` for writing results of a misty object into a Excel file.

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
