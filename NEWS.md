### misty 0.2.2

##### New features

* New function `trim()` for removing whitespace from start and/or end of a string. Note that this function is equivalent to the function `trimws()` in the `base` package. However, the `trimws()` function fails to remove whitespace in some instances.

##### Bug fix

* Fixed a bug in the function `cohens.d()`, function returned `NA` for Cohen's d in within-subject design in the presence of missing values.

* Fixed a bug in the function `alpha.coef()`, function did not provide any item statistics irrespective of the argument `print`

* Fixed a bug in the function `as.na()`, function always generated a warning meassage irrespective of the argument `as.na`  
