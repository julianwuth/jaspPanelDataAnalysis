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
    .vcmVarianceTablePD(jaspResults, options, ready)
    .vcmHomogeneityTablePD(jaspResults, options, ready)
  }

  if (options[["plot"]])
    .createPlmPlot(jaspResults, dataset, options, ready)

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

  return()
}

.vcmVarianceTablePD <- function(jaspResults, options, ready) {
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

  return()
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

  homogeneityTest <- modelSummary[["chisq.test"]]

  vcmHomogeneityTable$addRows(list(
    chiSq = unname(homogeneityTest[["statistic"]]),
    df    = unname(homogeneityTest[["parameter"]]),
    pVal  = unname(homogeneityTest[["p.value"]])
  ))

  vcmHomogeneityTable$addFootnote(gettext("A significant result indicates that the coefficients differ across groups."))

  return()
}
