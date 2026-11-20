#' @import jaspBase
#' @export
pgglsModel <- function(jaspResults, dataset, options, analysis = "pggls") {
  options <- .rewriteOptionsPD(options, analysis)
  ready   <- .isReadyPD(options)

  if (ready)
    .checkErrorsPD(dataset, options)

  .fitModelPD(jaspResults, dataset, options, ready)

  .modelSummaryTablePD(jaspResults, dataset, options, ready)

  .coefficientsTablePD(jaspResults, dataset, options, ready)

  if (options[["plot"]])
    .createPlmPlot(jaspResults, dataset, options, ready)

  return()
}

# The unrestricted covariance matrix has one row per period (per individual with time
# effects), minus one after demeaning or differencing, and is estimated from the
# groups in the other dimension (Wooldridge, 2002, pp. 277-278).
.fitPgglsPD <- function(formula, plmDf, options) {
  estimator  <- options[["pgglsEstimator"]]
  dimensions <- plm::pdim(plmDf)[["nT"]]
  timeEffect <- options[["effects"]] == "time"
  nGroups    <- if (timeEffect) dimensions[["T"]] else dimensions[["n"]]
  nEquations <- (if (timeEffect) dimensions[["n"]] else dimensions[["T"]]) - (estimator != "pooling")

  if (nGroups <= nEquations)
    stop(gettextf("General FGLS estimates a %1$s by %1$s error covariance matrix, which requires more than %1$s groups, but only %2$s are available.",
                  nEquations, nGroups), call. = FALSE)

  if (estimator == "fd" && .consecutiveSpellsPD(plmDf, options)[["nSkipped"]] > 0)
    stop(gettext("General FGLS on first differences requires consecutive time periods without missing values."), call. = FALSE)

  fit <- plm::pggls(formula, data = plmDf, model = estimator, effect = options[["effects"]])

  return(fit)
}

# plm divides the residual sum of squares of the transformed model by the total sum
# of squares of the untransformed dependent variable
.pgglsRSquaredPD <- function(fit, options) {
  response <- as.numeric(plm::pmodel.response(fit[["model"]], model = options[["pgglsEstimator"]],
                                              effect = options[["effects"]]))

  return(1 - sum(as.numeric(stats::residuals(fit))^2) / sum((response - mean(response))^2))
}

.addPgglsFitFootnotesPD <- function(table, options) {
  if (options[["pgglsEstimator"]] != "pooling")
    table$addFootnote(gettext("R-squared refers to the transformed dependent variable."))

  return()
}
