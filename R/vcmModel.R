#' @import jaspBase
#' @export
vcmModel <- function(jaspResults, dataset, options, analysis = "vcm") {
  options <- .rewriteOptionsPD(options, analysis)
  ready   <- .isReadyPD(options)

  if (ready)
    .checkErrorsPD(dataset, options)

  .fitModelPD(jaspResults, dataset, options, ready)

  .modelSummaryTablePD(jaspResults, dataset, options, ready)

  if (options[["vcmEstimator"]] == "within") {
    # the no-pooling estimator produces one set of coefficients per group
    .vcmGroupCoefficientsTablePD(jaspResults, options, ready)
  } else {
    .coefficientsTablePD(jaspResults, dataset, options, ready)
    .vcmVarianceTablePD(jaspResults, dataset, options, ready)
    .vcmHomogeneityTablePD(jaspResults, options, ready)
  }

  if (options[["plot"]])
    .createPlmPlot(jaspResults, dataset, options, ready)

  return()
}

# plm's own value divides by the total sum of squares of the dependent variable and
# all regressors stacked together
.vcmRSquaredPD <- function(fit) {
  response <- as.numeric(plm::pmodel.response(fit[["model"]]))

  return(1 - sum(as.numeric(stats::residuals(fit))^2) / sum((response - mean(response))^2))
}

.addVcmFitFootnotesPD <- function(table, options) {
  if (options[["vcmEstimator"]] == "random")
    table$addFootnote(gettext("R-squared is evaluated at the mean coefficients."))

  return()
}

.vcmGroupCoefficientsTablePD <- function(jaspResults, options, ready) {
  if (!is.null(jaspResults[["vcmGroupCoefTable"]]))
    return()

  groupTitle <- if (options[["effects"]] == "time") gettext("Time") else gettext("Individual")

  vcmGroupCoefTable <- createJaspTable(title = gettext("Coefficients per Group"))
  vcmGroupCoefTable$position <- 2
  vcmGroupCoefTable$dependOn(.pdModelDeps)
  vcmGroupCoefTable$addCitation(.pdCitation)

  vcmGroupCoefTable$addColumnInfo(name = "group",    title = groupTitle,          type = "string")
  vcmGroupCoefTable$addColumnInfo(name = "coef",     title = gettext("Name"),     type = "string")
  vcmGroupCoefTable$addColumnInfo(name = "estimate", title = gettext("Estimate"), type = "number")
  vcmGroupCoefTable$addColumnInfo(name = "se",       title = gettext("SE"),       type = "number")

  jaspResults[["vcmGroupCoefTable"]] <- vcmGroupCoefTable

  if (!ready)
    return()

  fit <- jaspResults[["modelFit"]]$object

  if (isTryError(fit))
    return()

  modelSummary <- try(summary(fit), silent = TRUE)

  if (isTryError(modelSummary)) {
    vcmGroupCoefTable$setError(gettextf("The coefficients could not be computed: %s", .cleanErrorPD(modelSummary)))
    return()
  }

  estimates      <- modelSummary[["coefficients"]]
  standardErrors <- modelSummary[["std.error"]]
  groups         <- rownames(estimates)
  terms          <- jaspBase::decodeColNames(colnames(estimates))

  rows <- list()

  for (i in seq_along(groups))
    rows[[i]] <- data.frame(
      group       = groups[i],
      coef        = terms,
      estimate    = as.numeric(estimates[i, ]),
      se          = as.numeric(standardErrors[i, ]),
      .isNewGroup = c(TRUE, rep(FALSE, length(terms) - 1)),
      stringsAsFactors = FALSE
    )

  rows <- do.call(rbind, rows)
  rownames(rows) <- NULL
  vcmGroupCoefTable$setData(rows)

  vcmGroupCoefTable$addFootnote(gettext("plm estimates a separate regression per group; no inferential statistics are reported for the individual coefficients."))

  if (anyNA(rows[["estimate"]]) || anyNA(rows[["se"]]))
    vcmGroupCoefTable$addFootnote(gettext("Blank or undefined cells belong to groups with too few observations or collinear regressors."))

  return()
}

.vcmVarianceTablePD <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["vcmVarianceTable"]]))
    return()

  vcmVarianceTable <- createJaspTable(title = gettext("Variance of the Coefficients"))
  vcmVarianceTable$position <- 3
  vcmVarianceTable$dependOn(.pdModelDeps)
  vcmVarianceTable$addCitation(.pdCitation)

  vcmVarianceTable$addColumnInfo(name = "coef",     title = gettext("Name"),      type = "string")
  vcmVarianceTable$addColumnInfo(name = "variance", title = gettext("Variance"),  type = "number")
  vcmVarianceTable$addColumnInfo(name = "stddev",   title = gettext("Std. Dev."), type = "number")
  vcmVarianceTable$showSpecifiedColumnsOnly <- TRUE

  jaspResults[["vcmVarianceTable"]] <- vcmVarianceTable

  if (!ready)
    return()

  fit <- jaspResults[["modelFit"]]$object

  if (isTryError(fit))
    return()

  modelSummary <- try(summary(fit), silent = TRUE)

  if (isTryError(modelSummary)) {
    vcmVarianceTable$setError(gettextf("The variance components could not be computed: %s", .cleanErrorPD(modelSummary)))
    return()
  }

  # Delta holds the estimated variance-covariance matrix of the random coefficients
  variances <- diag(as.matrix(modelSummary[["Delta"]]))

  vcmVarianceTable$setData(data.frame(
    coef     = jaspBase::decodeColNames(names(variances)),
    variance = unname(variances),
    stddev   = sqrt(unname(variances)),
    stringsAsFactors = FALSE
  ))

  if (.vcmUsesUncorrectedDeltaPD(jaspResults, dataset, options, modelSummary[["Delta"]]))
    vcmVarianceTable$addFootnote(gettext("The bias-corrected covariance matrix was not positive semidefinite, so the uncorrected covariance matrix of the group estimates is used, here and for the weights of the mean coefficients."))

  return()
}

# plm replaces Delta = D1 - D2 by D1, the covariance of the group estimates
.vcmUsesUncorrectedDeltaPD <- function(jaspResults, dataset, options, delta) {
  groupFit <- try(
    plm::pvcm(.createFormulaPD(options), data = .pdataFramePD(jaspResults, dataset, options, TRUE),
              model = "within", effect = options[["effects"]]),
    silent = TRUE
  )

  if (isTryError(groupFit) || anyNA(stats::coef(groupFit)))
    return(FALSE)

  return(isTRUE(all.equal(unname(stats::cov(as.matrix(stats::coef(groupFit)))), unname(as.matrix(delta)))))
}

.vcmHomogeneityTablePD <- function(jaspResults, options, ready) {
  if (!is.null(jaspResults[["vcmHomogeneityTable"]]))
    return()

  vcmHomogeneityTable <- createJaspTable(title = gettext("Test for Parameter Homogeneity"))
  vcmHomogeneityTable$position <- 4
  vcmHomogeneityTable$dependOn(.pdModelDeps)
  vcmHomogeneityTable$addCitation(.pdCitation)

  vcmHomogeneityTable$addColumnInfo(name = "chiSq", title = gettext("χ²"), type = "number")
  vcmHomogeneityTable$addColumnInfo(name = "df",    title = gettext("df"),           type = "integer")
  vcmHomogeneityTable$addColumnInfo(name = "pVal",  title = gettext("p"),            type = "pvalue")
  vcmHomogeneityTable$showSpecifiedColumnsOnly <- TRUE

  jaspResults[["vcmHomogeneityTable"]] <- vcmHomogeneityTable

  if (!ready)
    return()

  fit <- jaspResults[["modelFit"]]$object

  if (isTryError(fit))
    return()

  modelSummary <- try(summary(fit), silent = TRUE)

  if (isTryError(modelSummary) || is.null(modelSummary[["chisq.test"]]))
    return()

  statistic <- unname(modelSummary[["chisq.test"]][["statistic"]])

  # plm counts individuals here even when the groups are time periods
  dimensions       <- plm::pdim(fit[["model"]])[["nT"]]
  nGroups          <- if (options[["effects"]] == "time") dimensions[["T"]] else dimensions[["n"]]
  degreesOfFreedom <- length(stats::coef(fit)) * (nGroups - 1)

  vcmHomogeneityTable$addRows(list(
    chiSq = statistic,
    df    = degreesOfFreedom,
    pVal  = stats::pchisq(statistic, degreesOfFreedom, lower.tail = FALSE)
  ))

  vcmHomogeneityTable$addFootnote(gettext("A significant result indicates that the coefficients differ across groups."))

  return()
}
