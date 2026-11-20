# jaspPanelDataAnalysis

JASP module for linear panel data models, built on the [plm](https://cran.r-project.org/package=plm) package.

| Analysis | Estimator |
| --- | --- |
| Fixed Effects Model | within estimator with individual, time or two-way effects |
| Random Effects Model | GLS with Swamy-Aurora, Wallace-Hussain, Amemiya or Nerlove variance components |
| Hausman-Taylor Model | instrumental variables with the Hausman-Taylor, Amemiya-MaCurdy or Breusch-Mizon-Schmidt instrument set |
| Pooling Model | pooled OLS, with Lagrange multiplier tests for unobserved effects |
| First-Difference Model | OLS on one-period differences |
| Between Model | OLS on group means |
| Variable Coefficients Models | separate regressions per group, or Swamy random coefficients |
| Generalized Method of Moments | Arellano-Bond difference GMM and Blundell-Bond system GMM |
| General Feasible Generalized Least Squares | FGLS with an unrestricted error covariance matrix |

## Tests

Tests live in `tests/testthat` and run through `jaspTools`:

```r
library(jaspTools)
setPkgOption("module.dirs", ".")
testAll()
```
