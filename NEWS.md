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
* New function `z.test()` for computing one sample, two sample, and paired sample z-test.

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
