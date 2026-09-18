# Shared backend for all panel data analyses.
#
# Sections:
#   1. Dependencies & constants
#   2. Readiness & validation
#   3. Data preparation
#   4. Formula building
#   5. Model fitting
#   6. Model summary table
#   7. Coefficients table
#   8. Effect tables (fixed / random)
#   9. Plots
#  10. Assumption checks
#  11. Utilities

# tidy evaluation pronoun used inside ggplot2::aes()
utils::globalVariables(".data")

# pvcm(), pggls() and pgmm() build their internal model frame by rewriting their
# own call to `plm(...)` and evaluating it with eval(mf, parent.frame()), i.e. in
# *our* frame. Namespacing the call as plm::pvcm() is therefore not enough: the
# bare symbol `plm` has to be resolvable from this namespace, otherwise those
# three estimators fail with 'could not find function "plm"'.
#' @importFrom plm plm
NULL


####################### 1. Dependencies & constants #######################

# Variable-input options. Not every analysis defines every name; $dependOn()
# simply never fires for names the analysis' QML does not provide.
.pdInputDeps <- c(
  "dependent", "covariates", "factors", "id", "time", "idOnly",
  "endogenousCovariates",
  "timeVaryingExogenous", "timeInvariantExogenous", "timeInvariantEndogenous"
)

# Everything that changes the estimated model.
.pdModelDeps <- c(
  .pdInputDeps,
  "effects", "estimators", "htMethod", "vcmEstimator", "pgglsEstimator",
  "gmmSteps", "transformation", "lagsDependent",
  "instrumentLagMin", "instrumentLagMax", "collapseInstruments"
)

.pdCitation <- "Croissant, Y., & Millo, G. (2008). Panel data econometrics in R: The plm package. Journal of Statistical Software, 27(2), 1-43."


####################### 2. Readiness & validation #######################

.rewriteOptionsPD <- function(options, analysis) {
  options[["analysis"]] <- analysis
  return(options)
}

# an empty variable slot arrives as an empty list rather than an empty character
# vector, and c() would then turn the whole combination into a list
.variablesPD <- function(...) {
  return(as.character(unlist(c(...))))
}

# regressors entering the right hand side of the model formula
.regressorsPD <- function(options) {
  switch(options[["analysis"]],
    "ht"  = .variablesPD(options[["timeVaryingExogenous"]],
                         options[["timeInvariantExogenous"]], options[["timeInvariantEndogenous"]]),
    "gmm" = .variablesPD(options[["covariates"]], options[["factors"]], options[["endogenousCovariates"]]),
    .variablesPD(options[["covariates"]], options[["factors"]])
  )
}

# regressors that are factors; the Hausman-Taylor interface does not separate
# them from the numeric ones, so nothing is checked for factor levels there
.factorsPD <- function(options) {
  if (options[["analysis"]] == "ht")
    return(character(0))

  return(.variablesPD(options[["factors"]]))
}

.indexVariablesPD <- function(options) {
  if (options[["idOnly"]])
    return(options[["id"]])

  return(c(options[["id"]], options[["time"]]))
}

.isReadyPD <- function(options) {
  hasDependent <- length(options[["dependent"]]) == 1L && options[["dependent"]] != ""
  hasIndex     <- options[["id"]] != "" && (options[["idOnly"]] || options[["time"]] != "")

  if (!hasDependent || !hasIndex)
    return(FALSE)

  ready <- switch(options[["analysis"]],
    # the lagged dependent variable supplies the right hand side on its own
    "gmm" = TRUE,
    # at least one exogenous time-varying regressor is needed as an instrument
    "ht"  = length(.regressorsPD(options)) > 0 && length(options[["timeVaryingExogenous"]]) > 0,
    length(.regressorsPD(options)) > 0
  )

  return(ready)
}

.checkErrorsPD <- function(dataset, options) {
  indexVariables <- .indexVariablesPD(options)

  # index variables and factors need at least two levels
  .hasErrors(dataset              = dataset,
             type                 = "factorLevels",
             factorLevels.target  = c(.factorsPD(options), indexVariables),
             factorLevels.amount  = "< 2",
             exitAnalysisIfErrors = TRUE)

  modelVariables <- c(options[["dependent"]], .regressorsPD(options), indexVariables)

  .hasErrors(dataset              = dataset,
             type                 = "observations",
             all.target           = modelVariables,
             observations.amount  = "< 2",
             exitAnalysisIfErrors = TRUE)

  # only numeric columns can hold an infinite value, and checking a factor
  # coerces it to NA
  .hasErrors(dataset              = dataset,
             type                 = "infinity",
             all.target           = modelVariables[vapply(dataset[modelVariables], is.numeric, logical(1))],
             exitAnalysisIfErrors = TRUE)

  # the between estimator collapses the panel to one row per group, so a
  # standard error needs at least three groups
  if (options[["analysis"]] == "between") {
    groupVariable <- if (options[["effects"]] == "time") options[["time"]] else options[["id"]]

    if (groupVariable != "" && !is.null(dataset[[groupVariable]])) {
      nGroups <- length(unique(dataset[[groupVariable]]))

      if (nGroups < 3)
        .quitAnalysis(gettextf("The between estimator requires at least 3 groups, but only %1$s were found.", nGroups))
    }
  }

  return()
}


####################### 3. Data preparation #######################

# Builds (and caches) the plm data frame. A character index is used throughout:
# with both an id and a time variable plm groups on the pair, with `idOnly` the
# time index is derived from the row order within each individual.
.pdataFramePD <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["panelData"]]))
    return(jaspResults[["panelData"]]$object)

  panelData <- createJaspState()
  panelData$dependOn(.pdInputDeps)
  jaspResults[["panelData"]] <- panelData

  if (!ready)
    return(NULL)

  panelData$object <- try(plm::pdata.frame(dataset, index = .indexVariablesPD(options)), silent = TRUE)

  return(panelData$object)
}


####################### 4. Formula building #######################

.createFormulaPD <- function(options) {
  rightHandSide <- switch(options[["analysis"]],
    "gmm" = .gmmRegressorsPD(options),
    paste(.regressorsPD(options), collapse = " + ")
  )

  # the extra parts are separated by "|" and hold the instruments
  parts <- switch(options[["analysis"]],
    "ht"  = c(rightHandSide, paste(c(options[["timeVaryingExogenous"]], options[["timeInvariantExogenous"]]),
                                   collapse = " + ")),
    "gmm" = c(rightHandSide, .gmmInstrumentsPD(options)),
    rightHandSide
  )

  formula <- stats::as.formula(paste(options[["dependent"]], "~", paste(parts, collapse = " | ")))

  return(formula)
}

.gmmRegressorsPD <- function(options) {
  terms <- c(
    sprintf("lag(%1$s, 1:%2$s)", options[["dependent"]], options[["lagsDependent"]]),
    .regressorsPD(options)
  )

  return(paste(terms, collapse = " + "))
}

# the dependent variable and every endogenous regressor are instrumented by
# their own lags
.gmmInstrumentsPD <- function(options) {
  endogenous <- .variablesPD(options[["dependent"]], options[["endogenousCovariates"]])
  lagRange   <- paste(options[["instrumentLagMin"]], options[["instrumentLagMax"]], sep = ":")

  return(paste(sprintf("lag(%1$s, %2$s)", endogenous, lagRange), collapse = " + "))
}


####################### 5. Model fitting #######################

.fitModelPD <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["modelFit"]]))
    return()

  modelFit <- createJaspState()
  modelFit$dependOn(.pdModelDeps)
  jaspResults[["modelFit"]] <- modelFit

  plmDf <- .pdataFramePD(jaspResults, dataset, options, ready)

  if (!ready)
    return()

  if (isTryError(plmDf)) {
    modelFit$object <- plmDf
    return()
  }

  modelFit$object <- try(.callEstimatorPD(.createFormulaPD(options), plmDf, options), silent = TRUE)

  return()
}

.callEstimatorPD <- function(formula, plmDf, options) {
  analysis <- options[["analysis"]]

  fit <- switch(analysis,
    # first differences are only defined for individual effects, and the
    # pooling estimator applies no transformation at all
    "fd"      = plm::plm(formula, data = plmDf, model = "fd"),
    "pooling" = plm::plm(formula, data = plmDf, model = "pooling"),
    "random"  = plm::plm(formula, data = plmDf, model = "random",
                         effect        = options[["effects"]],
                         random.method = options[["estimators"]]),
    # the Hausman-Taylor estimator is only defined for individual effects
    "ht"      = plm::plm(formula, data = plmDf, model = "random",
                         effect        = "individual",
                         random.method = "ht",
                         inst.method   = options[["htMethod"]]),
    "vcm"     = plm::pvcm(formula, data = plmDf,
                          model  = options[["vcmEstimator"]],
                          effect = options[["effects"]]),
    "pggls"   = plm::pggls(formula, data = plmDf,
                           model  = options[["pgglsEstimator"]],
                           effect = options[["effects"]]),
    "gmm"     = plm::pgmm(formula, data = plmDf,
                          effect         = options[["effects"]],
                          model          = options[["gmmSteps"]],
                          transformation = options[["transformation"]],
                          collapse       = options[["collapseInstruments"]]),
    # "within" and "between"
    plm::plm(formula, data = plmDf, model = analysis, effect = options[["effects"]])
  )

  return(fit)
}

# Refits the panel with a different plm model, cached so that several tests can
# share one estimation. Returns the fit or a try-error.
.refitPD <- function(jaspResults, dataset, options, model) {
  # the analysis' own fit is the same model, so reuse it
  if (identical(options[["analysis"]], model) && !is.null(jaspResults[["modelFit"]]))
    return(jaspResults[["modelFit"]]$object)

  stateName <- paste0("modelFit_", model)

  if (!is.null(jaspResults[[stateName]]))
    return(jaspResults[[stateName]]$object)

  refit <- createJaspState()
  refit$dependOn(c(.pdInputDeps, "effects"))
  jaspResults[[stateName]] <- refit

  plmDf <- .pdataFramePD(jaspResults, dataset, options, TRUE)

  if (isTryError(plmDf)) {
    refit$object <- plmDf
    return(refit$object)
  }

  refit$object <- try(
    plm::plm(.createFormulaPD(options), data = plmDf, model = model, effect = options[["effects"]]),
    silent = TRUE
  )

  return(refit$object)
}


####################### 6. Model summary table #######################

# which overall test summary() reports for this analysis: an F test, a Wald
# chi-square test, or nothing at all
.overallTestPD <- function(options) {
  testType <- switch(options[["analysis"]],
    "within" = , "between" = , "pooling" = , "fd" = "f",
    "random" = , "ht" = , "gmm" = "chisq",
    "vcm"    = if (options[["vcmEstimator"]] == "random") "chisq" else "none",
    "none"
  )

  return(testType)
}

.modelLabelPD <- function(options) {
  label <- switch(options[["analysis"]],
    "within"  = gettext("Fixed effects"),
    "random"  = gettext("Random effects"),
    "pooling" = gettext("Pooling"),
    "between" = gettext("Between"),
    "fd"      = gettext("First-difference"),
    "ht"      = gettext("Hausman-Taylor"),
    "vcm"     = gettext("Variable coefficients"),
    "pggls"   = gettext("General FGLS"),
    "gmm"     = gettext("Generalized method of moments")
  )

  return(label)
}

.modelSummaryTablePD <- function(jaspResults, dataset, options, ready, deps = character(0)) {
  if (!is.null(jaspResults[["modelSummaryTable"]]))
    return()

  modelSummaryTable <- createJaspTable(title = gettext("Model Summary"))
  modelSummaryTable$position <- 1
  modelSummaryTable$dependOn(c(.pdModelDeps, deps))
  modelSummaryTable$addCitation(.pdCitation)
  jaspResults[["modelSummaryTable"]] <- modelSummaryTable

  testType <- .overallTestPD(options)

  modelSummaryTable$addColumnInfo(name = "model", title = gettext("Model"), type = "string")

  # pgmm reports no R-squared at all, pggls and pvcm no adjusted one
  if (options[["analysis"]] != "gmm")
    modelSummaryTable$addColumnInfo(name = "rSq", title = gettext("R<sup>2</sup>"), type = "number")

  if (!options[["analysis"]] %in% c("vcm", "pggls", "gmm"))
    modelSummaryTable$addColumnInfo(name = "adjRSq", title = gettext("Adj. R<sup>2</sup>"), type = "number")

  if (testType == "f") {
    modelSummaryTable$addColumnInfo(name = "statistic", title = gettext("F"),   type = "number")
    modelSummaryTable$addColumnInfo(name = "df1",       title = gettext("df1"), type = "integer")
    modelSummaryTable$addColumnInfo(name = "df2",       title = gettext("df2"), type = "integer")
  } else if (testType == "chisq") {
    modelSummaryTable$addColumnInfo(name = "statistic", title = gettext("χ²"), type = "number")
    modelSummaryTable$addColumnInfo(name = "df1",       title = gettext("df"),           type = "integer")
  }

  if (testType != "none")
    modelSummaryTable$addColumnInfo(name = "pVal", title = gettext("p"), type = "pvalue")

  modelSummaryTable$showSpecifiedColumnsOnly <- TRUE

  if (!ready)
    return()

  .fillModelSummaryTablePD(jaspResults, options)

  return()
}

.fillModelSummaryTablePD <- function(jaspResults, options) {
  modelSummaryTable <- jaspResults[["modelSummaryTable"]]
  fit               <- jaspResults[["modelFit"]]$object

  if (isTryError(fit)) {
    modelSummaryTable$setError(gettextf("The model could not be estimated: %s", .cleanErrorPD(fit)))
    return()
  }

  modelSummary <- try(.modelSummaryObjectPD(fit, options), silent = TRUE)

  if (isTryError(modelSummary)) {
    modelSummaryTable$setError(gettextf("The model could not be summarised: %s", .cleanErrorPD(modelSummary)))
    return()
  }

  row <- list(model = .modelLabelPD(options))

  # summary.plm reports rsq/adjrsq, summary.pggls and summary.pvcm a single rsqr
  if (!is.null(modelSummary[["r.squared"]])) {
    row[["rSq"]]    <- unname(modelSummary[["r.squared"]][["rsq"]])
    row[["adjRSq"]] <- unname(modelSummary[["r.squared"]][["adjrsq"]])
  } else if (!is.null(modelSummary[["rsqr"]])) {
    row[["rSq"]]    <- unname(modelSummary[["rsqr"]])
  }

  overallTest <- .overallTestObjectPD(modelSummary, options)

  if (!is.null(overallTest)) {
    row[["statistic"]] <- unname(overallTest[["statistic"]])
    row[["df1"]]       <- unname(overallTest[["parameter"]][1])
    row[["pVal"]]      <- unname(overallTest[["p.value"]])

    if (.overallTestPD(options) == "f")
      row[["df2"]] <- unname(overallTest[["parameter"]][2])
  }

  modelSummaryTable$addRows(row)

  return()
}

# the Windmeijer correction for two-step GMM is applied by summary(), not by a
# separate covariance matrix
.modelSummaryObjectPD <- function(fit, options) {
  if (options[["analysis"]] == "gmm")
    return(summary(fit, robust = options[["robustSE"]]))

  return(summary(fit))
}

.overallTestObjectPD <- function(modelSummary, options) {
  overallTest <- switch(options[["analysis"]],
    "gmm" = modelSummary[["wald.coef"]],
    "vcm" = modelSummary[["waldstatistic"]],
    modelSummary[["fstatistic"]]
  )

  return(overallTest)
}


####################### 7. Coefficients table #######################

# t for the estimators with a finite-sample distribution, z for the rest
.coefTestLabelPD <- function(options) {
  if (.overallTestPD(options) == "f")
    return(gettext("t"))

  return(gettext("z"))
}

.coefficientsTablePD <- function(jaspResults, dataset, options, ready, robust = FALSE, deps = character(0)) {
  if (!is.null(jaspResults[["coefTable"]]))
    return()

  coefTable <- createJaspTable(title = gettext("Coefficients"))
  coefTable$position <- 2
  coefTable$dependOn(c(.pdModelDeps, deps))
  coefTable$addCitation(.pdCitation)

  coefTable$addColumnInfo(name = "coef",          title = gettext("Name"),        type = "string")
  coefTable$addColumnInfo(name = "estimate",      title = gettext("Estimate"),    type = "number")
  coefTable$addColumnInfo(name = "se",            title = gettext("SE"),          type = "number")
  coefTable$addColumnInfo(name = "testStatistic", title = .coefTestLabelPD(options), type = "number")
  coefTable$addColumnInfo(name = "pVal",          title = gettext("p"),           type = "pvalue")
  coefTable$showSpecifiedColumnsOnly <- TRUE

  jaspResults[["coefTable"]] <- coefTable

  if (!ready)
    return()

  fit <- jaspResults[["modelFit"]]$object

  # the model summary table already reports why the fit failed
  if (isTryError(fit))
    return()

  coefficients <- try(.coefficientMatrixPD(fit, options, robust), silent = TRUE)

  if (isTryError(coefficients)) {
    coefTable$setError(gettextf("The coefficients could not be computed: %s", .cleanErrorPD(coefficients)))
    return()
  }

  coefTable$setData(.coefficientRowsPD(coefficients))

  if (robust)
    coefTable$addFootnote(gettext("Standard errors are heteroskedasticity and autocorrelation consistent (Arellano)."))

  return()
}

.coefficientMatrixPD <- function(fit, options, robust) {
  if (robust)
    return(.robustCoefficientMatrixPD(fit, options))

  modelSummary <- .modelSummaryObjectPD(fit, options)

  # summary.pggls stores the coefficient matrix under a different name
  if (options[["analysis"]] == "pggls")
    return(modelSummary[["CoefTable"]])

  return(modelSummary[["coefficients"]])
}

# Arellano heteroskedasticity and autocorrelation consistent standard errors,
# clustered on the individuals. The tests are built here rather than pulling in
# lmtest for a five line computation.
.robustCoefficientMatrixPD <- function(fit, options) {
  robustVcov    <- plm::vcovHC(fit, method = "arellano", type = "HC0")
  estimate      <- stats::coef(fit)[rownames(robustVcov)]
  standardError <- sqrt(diag(robustVcov))
  statistic     <- estimate / standardError

  pValue <- if (.overallTestPD(options) == "f")
    2 * stats::pt(-abs(statistic), df = stats::df.residual(fit))
  else
    2 * stats::pnorm(-abs(statistic))

  return(cbind(estimate, standardError, statistic, pValue))
}

.coefficientRowsPD <- function(coefficients) {
  rows <- data.frame(
    coef          = jaspBase::decodeColNames(rownames(coefficients)),
    estimate      = coefficients[, 1],
    se            = coefficients[, 2],
    testStatistic = coefficients[, 3],
    pVal          = coefficients[, 4],
    stringsAsFactors = FALSE
  )
  rownames(rows) <- NULL

  return(rows)
}


####################### 8. Effect tables #######################

.fixedEffTablePD <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["fixedEffTable"]]))
    return()

  twoways <- options[["effects"]] == "twoways"

  fixedEffTable <- createJaspTable(title = gettext("Fixed Effects"))
  fixedEffTable$position <- 3
  fixedEffTable$dependOn(c(.pdModelDeps, "fixedEffects", "fixedEffectsType"))
  fixedEffTable$addCitation(.pdCitation)

  # for two-ways models plm::fixef() only returns the estimates themselves
  if (twoways)
    fixedEffTable$addColumnInfo(name = "type", title = gettext("Effect"), type = "string")

  fixedEffTable$addColumnInfo(name = "effect",   title = gettext("Name"),     type = "string")
  fixedEffTable$addColumnInfo(name = "estimate", title = gettext("Estimate"), type = "number")

  if (!twoways) {
    fixedEffTable$addColumnInfo(name = "se",            title = gettext("SE"), type = "number")
    fixedEffTable$addColumnInfo(name = "testStatistic", title = gettext("t"),  type = "number")
    fixedEffTable$addColumnInfo(name = "pVal",          title = gettext("p"),  type = "pvalue")
  }

  jaspResults[["fixedEffTable"]] <- fixedEffTable

  if (!ready)
    return()

  .fillFixedEffTablePD(jaspResults, options)

  return()
}

.fillFixedEffTablePD <- function(jaspResults, options) {
  fixedEffTable <- jaspResults[["fixedEffTable"]]
  fit           <- jaspResults[["modelFit"]]$object

  if (isTryError(fit))
    return()

  twoways <- options[["effects"]] == "twoways"
  effects <- if (twoways) c("individual", "time") else options[["effects"]]
  rows    <- list()

  for (effect in effects) {
    fixedEffects <- try(
      summary(plm::fixef(fit, effect = effect, type = options[["fixedEffectsType"]])),
      silent = TRUE
    )

    if (isTryError(fixedEffects)) {
      fixedEffTable$setError(gettextf("The fixed effects could not be computed: %s", .cleanErrorPD(fixedEffects)))
      return()
    }

    fixedEffects <- as.data.frame(fixedEffects)

    block <- data.frame(
      effect   = rownames(fixedEffects),
      estimate = fixedEffects[["Estimate"]],
      stringsAsFactors = FALSE
    )

    if (twoways) {
      # plm reports the estimates of a two-ways model without standard errors,
      # so the two blocks are labelled and separated instead
      block <- cbind(
        type        = if (effect == "individual") gettext("Individual") else gettext("Time"),
        block,
        .isNewGroup = c(TRUE, rep(FALSE, nrow(block) - 1))
      )
    } else {
      block[["se"]]            <- fixedEffects[["Std. Error"]]
      block[["testStatistic"]] <- fixedEffects[["t-value"]]
      block[["pVal"]]          <- fixedEffects[["Pr(>|t|)"]]
    }

    rows[[effect]] <- block
  }

  rows <- do.call(rbind, rows)
  rownames(rows) <- NULL
  fixedEffTable$setData(rows)

  if (twoways)
    fixedEffTable$addFootnote(gettext("plm does not provide standard errors for the effects of a two-ways model."))

  return()
}

.randEffTablePD <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["randEffTable"]]))
    return()

  randEffTable <- createJaspTable(title = gettext("Random Effects Variance Components"))
  randEffTable$position <- 3
  randEffTable$dependOn(c(.pdModelDeps, "randomEffects"))
  randEffTable$addCitation(.pdCitation)

  randEffTable$addColumnInfo(name = "component", title = gettext("Component"), type = "string")
  randEffTable$addColumnInfo(name = "variance",  title = gettext("Variance"),  type = "number")
  randEffTable$addColumnInfo(name = "stddev",    title = gettext("Std. Dev."), type = "number")
  randEffTable$addColumnInfo(name = "share",     title = gettext("Share"),     type = "number")
  randEffTable$showSpecifiedColumnsOnly <- TRUE

  jaspResults[["randEffTable"]] <- randEffTable

  if (!ready)
    return()

  .fillRandEffTablePD(jaspResults, options)

  return()
}

.fillRandEffTablePD <- function(jaspResults, options) {
  randEffTable <- jaspResults[["randEffTable"]]
  fit          <- jaspResults[["modelFit"]]$object

  if (isTryError(fit))
    return()

  errorComponents <- try(plm::ercomp(fit), silent = TRUE)

  if (isTryError(errorComponents)) {
    randEffTable$setError(gettextf("The variance components could not be computed: %s", .cleanErrorPD(errorComponents)))
    return()
  }

  variance       <- errorComponents[["sigma2"]]
  totalVariance  <- sum(variance)
  componentNames <- list(
    idios = gettext("Idiosyncratic"),
    id    = gettext("Individual"),
    time  = gettext("Time")
  )

  for (component in names(variance)) {
    componentVariance <- variance[[component]]

    randEffTable$addRows(list(
      component = if (!is.null(componentNames[[component]])) componentNames[[component]] else component,
      variance  = componentVariance,
      stddev    = sqrt(componentVariance),
      share     = componentVariance / totalVariance
    ))
  }

  thetaFootnote <- .thetaFootnotePD(errorComponents[["theta"]])

  if (!is.null(thetaFootnote))
    randEffTable$addFootnote(thetaFootnote)

  return()
}

# theta is the quasi-demeaning parameter; a single number for a balanced one-way
# panel, one value per individual for unbalanced panels, and a list of
# id/time/total components for two-ways models
.thetaFootnotePD <- function(theta) {
  if (is.null(theta))
    return(NULL)

  if (is.list(theta)) {
    parts <- vapply(names(theta), function(component) {
      gettextf("%1$s: %2$s", component, .formatThetaPD(theta[[component]]))
    }, character(1))

    return(gettextf("Transformation parameter θ — %s.", paste(parts, collapse = ", ")))
  }

  return(gettextf("Transformation parameter θ = %s.", .formatThetaPD(theta)))
}

.formatThetaPD <- function(theta) {
  theta <- unname(theta)

  if (length(theta) == 1)
    return(format(theta, digits = 3))

  return(gettextf("%1$s to %2$s", format(min(theta), digits = 3), format(max(theta), digits = 3)))
}


####################### 9. Plots #######################

.createPlmPlot <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["plmPlot"]]))
    return()

  plmPlot <- createJaspPlot(title = gettext("Line Plot"), width = 750, height = 400)
  plmPlot$position <- 10
  plmPlot$dependOn(c(.pdInputDeps, "plot"))
  jaspResults[["plmPlot"]] <- plmPlot

  if (!ready)
    return()

  plmDf <- .pdataFramePD(jaspResults, dataset, options, ready)

  if (isTryError(plmDf)) {
    plmPlot$setError(.cleanErrorPD(plmDf))
    return()
  }

  plotObject <- try(.plmLinePlotPD(plmDf, options), silent = TRUE)

  if (isTryError(plotObject)) {
    plmPlot$setError(.cleanErrorPD(plotObject))
    return()
  }

  plmPlot$plotObject <- plotObject

  return()
}

.plmLinePlotPD <- function(plmDf, options) {
  panelIndex <- plm::index(plmDf)

  # with `idOnly` the time index is generated by plm and simply counts the rows
  # within each individual
  timeValues <- suppressWarnings(as.numeric(as.character(panelIndex[[2]])))

  if (anyNA(timeValues))
    timeValues <- as.numeric(panelIndex[[2]])

  plotData <- data.frame(
    time = timeValues,
    y    = as.numeric(plmDf[[options[["dependent"]]]]),
    id   = factor(panelIndex[[1]])
  )
  plotData <- plotData[stats::complete.cases(plotData), ]

  timeRange <- range(plotData[["time"]])
  xBreaks   <- unique(c(jaspGraphs::getPrettyAxisBreaks(timeRange), timeRange))

  xLabel  <- if (options[["idOnly"]]) gettext("Time") else jaspBase::decodeColNames(options[["time"]])
  yLabel  <- jaspBase::decodeColNames(options[["dependent"]])
  idLabel <- jaspBase::decodeColNames(options[["id"]])

  plot <- ggplot2::ggplot(plotData, ggplot2::aes(x = .data[["time"]], y = .data[["y"]], colour = .data[["id"]])) +
    ggplot2::geom_line() +
    jaspGraphs::scale_x_continuous(name = xLabel, breaks = xBreaks, limits = timeRange) +
    ggplot2::labs(x = xLabel, y = yLabel, colour = idLabel) +
    jaspGraphs::geom_rangeframe(sides = "bl") +
    jaspGraphs::themeJaspRaw(legend.position = "right")

  return(plot)
}


####################### 10. Assumption checks #######################

# Each analysis offers its own set of checks, so the container is keyed on the
# checkboxes that the analysis actually has.
.assumptionCheckContainerPD <- function(jaspResults, toggles) {
  if (!is.null(jaspResults[["assumptionChecks"]]))
    return(jaspResults[["assumptionChecks"]])

  assumptionChecks <- createJaspContainer(title = gettext("Assumption Checks"))
  assumptionChecks$position <- 5
  assumptionChecks$dependOn(c(.pdInputDeps, toggles))
  jaspResults[["assumptionChecks"]] <- assumptionChecks

  return(assumptionChecks)
}

.hausmanTestPD <- function(jaspResults, dataset, options, ready) {
  if (!options[["hausmanTest"]])
    return()

  container <- .assumptionCheckContainerPD(jaspResults, "hausmanTest")

  if (!is.null(container[["hausmanTestTable"]]))
    return()

  hausmanTestTable <- createJaspTable(title = gettext("Hausman Test"))
  hausmanTestTable$position <- 1
  hausmanTestTable$dependOn("effects")
  hausmanTestTable$addCitation(.pdCitation)

  hausmanTestTable$addColumnInfo(name = "chiSq", title = gettext("χ²"), type = "number")
  hausmanTestTable$addColumnInfo(name = "df",    title = gettext("df"),           type = "integer")
  hausmanTestTable$addColumnInfo(name = "pVal",  title = gettext("p"),            type = "pvalue")
  hausmanTestTable$showSpecifiedColumnsOnly <- TRUE

  container[["hausmanTestTable"]] <- hausmanTestTable

  if (!ready)
    return()

  # the test compares the fixed and the random effects model, so both are needed
  # regardless of which analysis was selected
  fixedFit  <- .refitPD(jaspResults, dataset, options, "within")
  randomFit <- .refitPD(jaspResults, dataset, options, "random")

  if (isTryError(fixedFit) || isTryError(randomFit)) {
    failedFit <- if (isTryError(fixedFit)) fixedFit else randomFit
    hausmanTestTable$setError(gettextf("The Hausman test could not be performed: %s", .cleanErrorPD(failedFit)))
    return()
  }

  hausmanTest <- try(plm::phtest(fixedFit, randomFit), silent = TRUE)

  if (isTryError(hausmanTest)) {
    hausmanTestTable$setError(gettextf("The Hausman test could not be performed: %s", .cleanErrorPD(hausmanTest)))
    return()
  }

  hausmanTestTable$addRows(list(
    chiSq = unname(hausmanTest[["statistic"]]),
    df    = unname(hausmanTest[["parameter"]]),
    pVal  = unname(hausmanTest[["p.value"]])
  ))

  return()
}

.lmTestPD <- function(jaspResults, options, ready) {
  if (!options[["lmTest"]])
    return()

  container <- .assumptionCheckContainerPD(jaspResults, "lmTest")

  if (!is.null(container[["lmTestTable"]]))
    return()

  lmTestTable <- createJaspTable(title = gettext("Lagrange Multiplier Test for Random Effects"))
  lmTestTable$position <- 2
  lmTestTable$dependOn(c("lmTestEffects", "lmTestType"))
  lmTestTable$addCitation(.pdCitation)

  lmTestTable$addColumnInfo(name = "test",      title = gettext("Test"),      type = "string")
  lmTestTable$addColumnInfo(name = "effect",    title = gettext("Effect"),    type = "string")
  lmTestTable$addColumnInfo(name = "statistic", title = gettext("Statistic"), type = "number")
  lmTestTable$addColumnInfo(name = "df",        title = gettext("df"),        type = "integer")
  lmTestTable$addColumnInfo(name = "pVal",      title = gettext("p"),         type = "pvalue")
  lmTestTable$showSpecifiedColumnsOnly <- TRUE

  container[["lmTestTable"]] <- lmTestTable

  if (!ready)
    return()

  fit <- jaspResults[["modelFit"]]$object

  if (isTryError(fit))
    return()

  # the Gourieroux-Holly-Monfort statistic is only defined for two-ways effects
  if (options[["lmTestType"]] == "ghm" && options[["lmTestEffects"]] != "twoways") {
    lmTestTable$setError(gettext("The Gourieroux-Holly-Monfort test requires individual and time effects."))
    return()
  }

  lmTest <- try(
    plm::plmtest(fit, effect = options[["lmTestEffects"]], type = options[["lmTestType"]]),
    silent = TRUE
  )

  if (isTryError(lmTest)) {
    lmTestTable$setError(gettextf("The Lagrange Multiplier test could not be performed: %s", .cleanErrorPD(lmTest)))
    return()
  }

  # honda and king-wu are standard normal and report no degrees of freedom,
  # the ghm statistic follows a mixture and reports several
  degreesOfFreedom <- lmTest[["parameter"]]

  lmTestTable$addRows(list(
    test      = lmTest[["method"]],
    effect    = .effectLabelPD(options[["lmTestEffects"]]),
    statistic = unname(lmTest[["statistic"]]),
    df        = if (length(degreesOfFreedom) == 1) unname(degreesOfFreedom) else NA,
    pVal      = unname(lmTest[["p.value"]])
  ))

  lmTestTable$addFootnote(gettextf("The test statistic follows a %s distribution.", names(lmTest[["statistic"]])))

  return()
}

.effectLabelPD <- function(effect) {
  label <- switch(effect,
    "individual" = gettext("Individual"),
    "time"       = gettext("Time"),
    "twoways"    = gettext("Individual and time")
  )

  return(label)
}


####################### 11. Utilities #######################

.cleanErrorPD <- function(tryError) {
  condition <- attr(tryError, "condition")

  if (!is.null(condition))
    return(trimws(conditionMessage(condition)))

  return(trimws(gsub("^Error[^:]*: *", "", as.character(tryError)[1])))
}
