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
  "timeVaryingExogenous", "timeVaryingEndogenous",
  "timeInvariantExogenous", "timeInvariantEndogenous"
)

# Everything that changes the estimated model.
.pdModelDeps <- c(
  .pdInputDeps,
  "effects", "estimators", "htMethod", "vcmEstimator", "pgglsEstimator",
  "gmmSteps", "transformation", "lagsDependent",
  "instrumentLagMin", "instrumentLagMax", "endogenousLagMin", "collapseInstruments"
)

# the analyses whose QML offers the "effects" option
.pdAnalysesWithEffects <- c("within", "random", "between", "vcm", "pggls", "gmm")

.pdMaxLegendEntries <- 20

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
    "ht"  = .variablesPD(options[["timeVaryingExogenous"]], options[["timeVaryingEndogenous"]],
                         options[["timeInvariantExogenous"]], options[["timeInvariantEndogenous"]]),
    "gmm" = .variablesPD(options[["covariates"]], options[["endogenousCovariates"]]),
    .variablesPD(options[["covariates"]], options[["factors"]])
  )
}

# regressors that are factors; the Hausman-Taylor interface does not separate them
# from the numeric ones, and the GMM interface only takes numeric regressors
.factorsPD <- function(options) {
  if (options[["analysis"]] %in% c("ht", "gmm"))
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

  .hasErrors(dataset              = dataset,
             type                 = "variance",
             all.target           = options[["dependent"]],
             exitAnalysisIfErrors = TRUE)

  .checkPanelStructurePD(dataset, options)

  if (options[["analysis"]] == "ht")
    .checkHausmanTaylorVariablesPD(dataset, options)

  return()
}

.checkPanelStructurePD <- function(dataset, options) {
  if (options[["idOnly"]])
    return()

  indexVariables <- .indexVariablesPD(options)
  indexData      <- dataset[stats::complete.cases(dataset[indexVariables]), indexVariables, drop = FALSE]

  # plm only warns about these and then estimates a different model
  if (anyDuplicated(indexData) > 0)
    .quitAnalysis(gettextf("Each combination of %1$s and %2$s must occur only once, but some combinations occur more than once.",
                           jaspBase::decodeColNames(options[["id"]]), jaspBase::decodeColNames(options[["time"]])))

  if (.timeOrderMattersPD(options) && !.hasTimeOrderPD(dataset[[options[["time"]]]]))
    .quitAnalysis(gettextf("This model depends on the order of the time periods, so %s must be an ordinal variable or have numeric labels.",
                           jaspBase::decodeColNames(options[["time"]])))

  return()
}

# differences and lags use the order of the periods, the other estimators do not
.timeOrderMattersPD <- function(options) {
  analysis <- options[["analysis"]]

  return(analysis %in% c("fd", "gmm") || (analysis == "pggls" && options[["pgglsEstimator"]] == "fd"))
}

.hasTimeOrderPD <- function(time) {
  if (is.ordered(time) || is.numeric(time))
    return(TRUE)

  return(!anyNA(suppressWarnings(as.numeric(levels(as.factor(time))))))
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

  panelData$object <- try(
    plm::pdata.frame(.panelDatasetPD(dataset, options), index = .indexVariablesPD(options)),
    silent = TRUE
  )

  return(panelData$object)
}

.panelDatasetPD <- function(dataset, options) {
  # a row without an id or period has no place in the panel, and lag() crashes R on it
  dataset <- dataset[stats::complete.cases(dataset[.indexVariablesPD(options)]), , drop = FALSE]

  # ordered factors would otherwise enter with polynomial contrasts
  for (regressor in .regressorsPD(options))
    if (is.ordered(dataset[[regressor]]))
      dataset[[regressor]] <- factor(dataset[[regressor]], ordered = FALSE)

  if (!options[["idOnly"]])
    dataset[[options[["time"]]]] <- .sortTimeLevelsPD(dataset[[options[["time"]]]])

  return(dataset)
}

# plm orders the periods by factor level, and numeric labels may be sorted as text
.sortTimeLevelsPD <- function(time) {
  time          <- droplevels(as.factor(time))
  numericLabels <- suppressWarnings(as.numeric(levels(time)))

  if (anyNA(numericLabels))
    return(time)

  return(factor(time, levels = levels(time)[order(numericLabels)]))
}


####################### 4. Formula building #######################

.createFormulaPD <- function(options) {
  rightHandSide <- switch(options[["analysis"]],
    "gmm" = .gmmRegressorsPD(options),
    paste(.regressorsPD(options), collapse = " + ")
  )

  # the extra parts are separated by "|" and hold the instruments
  parts <- switch(options[["analysis"]],
    "ht"  = c(rightHandSide, .htInstrumentPartsPD(options)),
    "gmm" = c(rightHandSide, .gmmInstrumentsPD(options)),
    rightHandSide
  )

  formula <- stats::as.formula(paste(options[["dependent"]], "~", paste(parts, collapse = " | ")))

  return(formula)
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
    "fd"      = .fitFirstDifferencePD(formula, plmDf, options),
    "pooling" = plm::plm(formula, data = plmDf, model = "pooling"),
    "random"  = .fitRandomEffectsPD(formula, plmDf, options),
    "ht"      = .fitHausmanTaylorPD(formula, plmDf, options),
    "vcm"     = plm::pvcm(formula, data = plmDf,
                          model  = options[["vcmEstimator"]],
                          effect = options[["effects"]]),
    "pggls"   = .fitPgglsPD(formula, plmDf, options),
    "gmm"     = .fitGmmPD(formula, plmDf, options),
    # "within" and "between"
    plm::plm(formula, data = plmDf, model = analysis, effect = options[["effects"]])
  )

  # without residual degrees of freedom every standard error is undefined
  if (inherits(fit, "plm") && stats::df.residual(fit) <= 0)
    stop(gettext("There are too few observations for the number of estimated coefficients."), call. = FALSE)

  return(fit)
}

.fitRandomEffectsPD <- function(formula, plmDf, options) {
  fit <- plm::plm(formula, data = plmDf, model = "random",
                  effect        = options[["effects"]],
                  random.method = options[["estimators"]])

  # for this case plm stores the GLS covariance matrix without its sigma_u^2 factor
  if (.isUnbalancedTwoWaysRandomPD(fit))
    fit[["vcov"]] <- fit[["vcov"]] * fit[["ercomp"]][["sigma2"]][["idios"]]

  return(fit)
}

.isUnbalancedTwoWaysRandomPD <- function(fit) {
  return(fit[["args"]][["model"]] == "random" && fit[["args"]][["effect"]] == "twoways" &&
           !plm::pdim(fit)[["balanced"]])
}

# plm differences adjacent rows, whatever the time between them, so that a row lost
# to a gap or a missing value would enter as a one-period change. Each run of
# consecutive periods is therefore differenced separately.
# (Wooldridge, 2002, pp. 279-280, Eqs. 10.63, 10.65)
.fitFirstDifferencePD <- function(formula, plmDf, options) {
  spells <- .consecutiveSpellsPD(plmDf, options)

  if (spells[["nSkipped"]] == 0)
    return(plm::plm(formula, data = plmDf, model = "fd"))

  spellData <- as.data.frame(plmDf)[spells[["rows"]], , drop = FALSE]
  spellData[[".pdSpell"]] <- factor(spells[["spell"]], levels = unique(spells[["spell"]]))
  spellData[[".pdPeriod"]] <- spells[["time"]]

  fit <- plm::plm(formula, data = plm::pdata.frame(spellData, index = c(".pdSpell", ".pdPeriod")), model = "fd")

  # each spell loses its first row, and inference still clusters on the individual
  cluster <- droplevels(spells[["id"]][duplicated(spells[["spell"]])])
  fit[["pdDifferences"]] <- list(cluster = cluster, nSkipped = spells[["nSkipped"]])

  return(fit)
}

.consecutiveSpellsPD <- function(plmDf, options) {
  variables <- c(options[["dependent"]], .regressorsPD(options))
  rows      <- stats::complete.cases(as.data.frame(plmDf)[variables])
  index     <- plm::index(plmDf)
  positions <- .timePositionsPD(index[[2]])[rows]
  id        <- index[[1]][rows]

  sameIndividual <- c(FALSE, id[-1] == id[-length(id)])
  nextPeriod     <- c(FALSE, diff(positions) == 1)

  return(list(
    rows     = rows,
    id       = id,
    time     = index[[2]][rows],
    spell    = cumsum(!(sameIndividual & nextPeriod)),
    nSkipped = sum(sameIndividual & !nextPeriod)
  ))
}

# Position of each observation on the time scale. Numeric labels keep their spacing,
# in units of their greatest common step (so waves 1990, 1995, 2000 are adjacent),
# other labels are taken to be consecutive.
.timePositionsPD <- function(time) {
  time          <- droplevels(as.factor(time))
  numericLabels <- suppressWarnings(as.numeric(levels(time)))

  if (anyNA(numericLabels) || any(numericLabels %% 1 != 0))
    return(as.integer(time))

  step <- Reduce(.greatestCommonDivisorPD, diff(sort(numericLabels)), 0)

  return(if (step == 0) rep(1, length(time)) else ((numericLabels - min(numericLabels)) / step)[as.integer(time)])
}

.greatestCommonDivisorPD <- function(a, b) {
  while (b != 0) {
    remainder <- a %% b
    a         <- b
    b         <- remainder
  }

  return(a)
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

  refitOptions               <- options
  refitOptions[["analysis"]] <- model

  # the fixed effects analysis has no estimator choice for the comparison model
  if (options[["analysis"]] != "random")
    refitOptions[["estimators"]] <- "swar"

  refit$object <- try(.callEstimatorPD(.createFormulaPD(options), plmDf, refitOptions), silent = TRUE)

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
    modelSummaryTable$addColumnInfo(name = "statistic", title = "F",   type = "number")
    modelSummaryTable$addColumnInfo(name = "df1",       title = "df1", type = "integer")
    modelSummaryTable$addColumnInfo(name = "df2",       title = "df2", type = "integer")
  } else if (testType == "chisq") {
    modelSummaryTable$addColumnInfo(name = "statistic", title = "\u03C7\u00B2", type = "number")
    modelSummaryTable$addColumnInfo(name = "df1",       title = "df",  type = "integer")
  }

  if (testType != "none")
    modelSummaryTable$addColumnInfo(name = "pVal", title = "p", type = "pvalue")

  modelSummaryTable$showSpecifiedColumnsOnly <- TRUE

  if (!ready)
    return()

  .fillModelSummaryTablePD(jaspResults, dataset, options)

  return()
}

.fillModelSummaryTablePD <- function(jaspResults, dataset, options) {
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

  overallTest <- try(.overallTestObjectPD(fit, modelSummary, options), silent = TRUE)

  if (isTryError(overallTest)) {
    modelSummaryTable$setError(gettextf("The overall test could not be computed: %s", .cleanErrorPD(overallTest)))
    return()
  }

  row <- c(list(model = .modelLabelPD(options)), .rSquaredPD(fit, modelSummary, options))

  if (!is.null(overallTest)) {
    row[["statistic"]] <- unname(overallTest[["statistic"]])
    row[["df1"]]       <- unname(overallTest[["parameter"]][1])
    row[["pVal"]]      <- unname(overallTest[["p.value"]])

    if (.overallTestPD(options) == "f")
      row[["df2"]] <- unname(overallTest[["parameter"]][2])
  }

  modelSummaryTable$addRows(row)
  .addPanelFootnotesPD(modelSummaryTable, dataset, options)
  .addFitFootnotesPD(modelSummaryTable, fit, options)

  return()
}

.rSquaredPD <- function(fit, modelSummary, options) {
  # plm's single rsqr for these two does not compare like with like
  if (options[["analysis"]] == "vcm")
    return(list(rSq = .vcmRSquaredPD(fit)))

  if (options[["analysis"]] == "pggls")
    return(list(rSq = .pgglsRSquaredPD(fit, options)))

  if (options[["analysis"]] == "gmm")
    return(list())

  # here plm computes it from the untransformed data
  if (inherits(fit, "plm") && .isUnbalancedTwoWaysRandomPD(fit))
    return(list(rSq = NA, adjRSq = NA))

  rSquared <- unname(modelSummary[["r.squared"]][["rsq"]])
  adjusted <- unname(modelSummary[["r.squared"]][["adjrsq"]])

  # plm uses n - 1 total degrees of freedom, but the demeaned data have n - N
  # (Wooldridge, 2002, p. 271, Eq. 10.56)
  if (options[["analysis"]] == "within") {
    residualDf <- stats::df.residual(fit)
    adjusted   <- 1 - (1 - rSquared) * (residualDf + length(stats::coef(fit))) / residualDf
  }

  return(list(rSq = rSquared, adjRSq = adjusted))
}

.addFitFootnotesPD <- function(table, fit, options) {
  if (.robustRequestedPD(options))
    table$addFootnote(gettext("The overall test uses the cluster-robust covariance matrix."))

  switch(options[["analysis"]],
    "gmm"   = .addGmmFitFootnotesPD(table, fit, options),
    "pggls" = .addPgglsFitFootnotesPD(table, options),
    "vcm"   = .addVcmFitFootnotesPD(table, options),
    "ht"    = .addHtFitFootnotesPD(table, fit)
  )

  if (!inherits(fit, "plm"))
    return()

  if (.isUnbalancedTwoWaysRandomPD(fit))
    table$addFootnote(gettext("R-squared is not reported for two-way random effects on an unbalanced panel."))

  nSkipped <- fit[["pdDifferences"]][["nSkipped"]]

  if (!is.null(nSkipped))
    table$addFootnote(gettextf("Changes spanning more than one period were not used: %s.", nSkipped))

  if (options[["analysis"]] == "within" && options[["effects"]] == "twoways") {
    index <- plm::index(fit)

    if (.connectedComponentsPD(index[[1]], index[[2]]) > 1)
      table$addFootnote(gettext("The individuals and periods do not form one connected panel, so the residual degrees of freedom are understated and the standard errors slightly too large."),
                        symbol = gettext("Warning:"))
  }

  return()
}

# number of groups of individuals and periods linked by observations
.connectedComponentsPD <- function(id, time) {
  component <- as.integer(factor(id))

  repeat {
    linked <- stats::ave(stats::ave(component, time, FUN = min), id, FUN = min)

    if (all(linked == component))
      break

    component <- linked
  }

  return(length(unique(component)))
}

# describes the panel that was actually used
.addPanelFootnotesPD <- function(table, dataset, options) {
  indexVariables <- .indexVariablesPD(options)
  used           <- stats::complete.cases(dataset[c(options[["dependent"]], .regressorsPD(options), indexVariables)])
  individuals    <- dataset[[options[["id"]]]][used]
  nPeriods       <- if (options[["idOnly"]]) max(table(individuals)) else length(unique(dataset[[options[["time"]]]][used]))

  table$addFootnote(gettextf("Estimated from %1$s observations on %2$s individuals over %3$s time periods.",
                             sum(used), length(unique(individuals)), nPeriods))

  if (!all(used))
    table$addFootnote(gettextf("Rows excluded because of missing values: %s.", sum(!used)))

  if (options[["idOnly"]] && options[["analysis"]] %in% .pdAnalysesWithEffects && options[["effects"]] != "individual")
    table$addFootnote(gettext("With 'ID only' the periods are numbered within each individual, so time effects compare the n-th observation of each individual rather than calendar periods."),
                      symbol = gettext("Warning:"))

  if (!options[["idOnly"]] && .timeOrderMattersPD(options) && .timeLabelsLookUnsortedPD(dataset[[options[["time"]]]]))
    table$addFootnote(gettextf("The numbers in the labels of %s do not increase with the order of its levels; check that the levels are in chronological order.",
                               jaspBase::decodeColNames(options[["time"]])),
                      symbol = gettext("Warning:"))

  return()
}

# for example "t1", "t10", "t2"
.timeLabelsLookUnsortedPD <- function(time) {
  timeLevels <- levels(droplevels(as.factor(time)))

  if (!anyNA(suppressWarnings(as.numeric(timeLevels))))
    return(FALSE)

  labelNumbers <- suppressWarnings(as.numeric(gsub("[^0-9]+", "", timeLevels)))

  return(!anyNA(labelNumbers) && anyDuplicated(labelNumbers) == 0 && is.unsorted(labelNumbers))
}

.modelSummaryObjectPD <- function(fit, options) {
  if (options[["analysis"]] == "gmm")
    return(.gmmSummaryPD(fit, .gmmRobustPD(options)))

  return(summary(fit))
}

.overallTestObjectPD <- function(fit, modelSummary, options) {
  if (.robustRequestedPD(options))
    return(.robustOverallTestPD(fit, options))

  overallTest <- switch(options[["analysis"]],
    "gmm" = modelSummary[["wald.coef"]],
    "vcm" = modelSummary[["waldstatistic"]],
    modelSummary[["fstatistic"]]
  )

  return(overallTest)
}

# The sum of squares form of the F statistic is invalid once the errors may be
# heteroskedastic or serially correlated, so the Wald form with the robust matrix is
# used (Wooldridge, 2002, p. 276).
.robustOverallTestPD <- function(fit, options) {
  robustVcov <- .robustVcovPD(fit)
  terms      <- setdiff(rownames(robustVcov), "(Intercept)")
  estimate   <- stats::coef(fit)[terms]
  statistic  <- drop(crossprod(estimate, solve(robustVcov[terms, terms, drop = FALSE], estimate)))
  nTerms     <- length(terms)

  if (.overallTestPD(options) == "chisq")
    return(list(statistic = statistic, parameter = nTerms,
                p.value   = stats::pchisq(statistic, nTerms, lower.tail = FALSE)))

  denominatorDf <- .nClustersPD(fit) - 1

  return(list(statistic = statistic / nTerms, parameter = c(nTerms, denominatorDf),
              p.value   = stats::pf(statistic / nTerms, nTerms, denominatorDf, lower.tail = FALSE)))
}


####################### 7. Coefficients table #######################

# t for the estimators with a finite-sample distribution, z for the rest
.coefTestLabelPD <- function(options) {
  if (.overallTestPD(options) == "f")
    return("t")

  return("z")
}

.coefficientsTablePD <- function(jaspResults, dataset, options, ready, deps = character(0)) {
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
  coefTable$addColumnInfo(name = "pVal",          title = "p",                    type = "pvalue")
  coefTable$showSpecifiedColumnsOnly <- TRUE

  jaspResults[["coefTable"]] <- coefTable

  if (!ready)
    return()

  fit <- jaspResults[["modelFit"]]$object

  # the model summary table already reports why the fit failed
  if (isTryError(fit))
    return()

  coefficients <- try(.coefficientMatrixPD(fit, options), silent = TRUE)

  if (isTryError(coefficients)) {
    coefTable$setError(gettextf("The coefficients could not be computed: %s", .cleanErrorPD(coefficients)))
    return()
  }

  coefTable$setData(.coefficientRowsPD(coefficients))
  .addCoefficientFootnotesPD(coefTable, fit, options)

  return()
}

# robust inference is offered for the estimators with a valid plm covariance matrix;
# the option can still be stored for analyses that hide its checkbox
.robustRequestedPD <- function(options) {
  return(options[["analysis"]] %in% c("within", "random", "pooling", "fd") && options[["robustEstimates"]])
}

.coefficientMatrixPD <- function(fit, options) {
  if (.robustRequestedPD(options))
    return(.robustCoefficientMatrixPD(fit, options))

  modelSummary <- .modelSummaryObjectPD(fit, options)

  # summary.pggls stores the coefficient matrix under a different name
  if (options[["analysis"]] == "pggls")
    return(modelSummary[["CoefTable"]])

  return(modelSummary[["coefficients"]])
}

# Cluster-robust covariance matrix (Wooldridge, 2002, p. 275, Eq. 10.59).
.robustVcovPD <- function(fit) {
  if (.isUnbalancedTwoWaysRandomPD(fit))
    stop(gettext("Robust standard errors are not available for two-way random effects on an unbalanced panel."), call. = FALSE)

  differences <- fit[["pdDifferences"]]

  if (is.null(differences))
    return(plm::vcovHC(fit, method = "arellano", type = "HC0"))

  regressors <- stats::model.matrix(fit)[, names(stats::coef(fit)), drop = FALSE]

  return(.clusterSandwichPD(regressors, as.numeric(stats::residuals(fit)), differences[["cluster"]]))
}

.clusterSandwichPD <- function(regressors, residuals, cluster) {
  bread  <- solve(crossprod(regressors))
  scores <- rowsum(regressors * residuals, as.character(cluster))

  return(bread %*% crossprod(scores) %*% bread)
}

.nClustersPD <- function(fit) {
  differences <- fit[["pdDifferences"]]

  return(if (is.null(differences)) plm::pdim(fit)[["nT"]][["n"]] else nlevels(differences[["cluster"]]))
}

# The tests are built here rather than pulling in lmtest for a five line computation.
.robustCoefficientMatrixPD <- function(fit, options) {
  robustVcov    <- .robustVcovPD(fit)
  estimate      <- stats::coef(fit)[rownames(robustVcov)]
  standardError <- sqrt(diag(robustVcov))
  statistic     <- estimate / standardError

  # the same reference distribution as the robust overall test
  pValue <- if (.overallTestPD(options) == "f")
    2 * stats::pt(-abs(statistic), df = .nClustersPD(fit) - 1)
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

.addCoefficientFootnotesPD <- function(coefTable, fit, options) {
  if (.robustRequestedPD(options)) {
    coefTable$addFootnote(gettext("Standard errors are robust to heteroskedasticity and serial correlation (Arellano) and clustered on the individuals."))

    # (Wooldridge, 2002, pp. 275-276): justified for T small relative to N
    panelDimensions <- plm::pdim(fit)[["nT"]]

    if (panelDimensions[["n"]] <= panelDimensions[["T"]])
      coefTable$addFootnote(gettext("Robust standard errors require many individuals relative to the number of time periods."),
                            symbol = gettext("Warning:"))
  }

  if (options[["analysis"]] == "gmm")
    coefTable$addFootnote(.gmmStandardErrorFootnotePD(options))

  if (options[["analysis"]] == "pggls")
    coefTable$addFootnote(gettext("Standard errors assume a common error covariance matrix and rely on many groups relative to the size of that matrix; with few groups they can be far too small."))

  if (!inherits(fit, "plm"))
    return()

  dropped <- .droppedRegressorsPD(fit, options)

  if (length(dropped) > 0)
    coefTable$addFootnote(gettextf("Not estimated because they are constant after the transformation or collinear: %s.",
                                   paste(jaspBase::decodeColNames(dropped), collapse = ", ")))

  if (options[["analysis"]] == "fd" && "(Intercept)" %in% names(stats::coef(fit)))
    coefTable$addFootnote(gettext("The intercept of the differenced equation is the coefficient of a linear time trend in the model in levels."))

  if (options[["analysis"]] == "random")
    .addZeroVarianceFootnotePD(coefTable, fit)

  return()
}

.droppedRegressorsPD <- function(fit, options) {
  if (!options[["analysis"]] %in% c("within", "random", "pooling", "fd", "between"))
    return(character(0))

  requested <- colnames(stats::model.matrix(fit, model = "pooling"))

  return(setdiff(requested, c(names(stats::coef(fit)), "(Intercept)")))
}

# plm truncates a negative variance estimate at zero, which turns the estimator into
# one without that effect (Wooldridge, 2002, p. 261).
.addZeroVarianceFootnotePD <- function(table, fit) {
  variance <- plm::ercomp(fit)[["sigma2"]]
  zero     <- setdiff(names(variance)[variance <= 0], "idios")

  if (length(zero) > 0)
    table$addFootnote(gettextf("The estimated variance of the %s effect was zero or negative and was set to zero, so the estimates equal those of a model without this effect. This can indicate negative serial correlation or a misspecified model.",
                               paste(vapply(zero, .componentLabelPD, character(1)), collapse = ", ")),
                      symbol = gettext("Warning:"))

  return()
}

.componentLabelPD <- function(component) {
  label <- switch(component,
    "idios" = gettext("idiosyncratic"),
    "id"    = gettext("individual"),
    "time"  = gettext("time"),
    "total" = gettext("combined"),
    component
  )

  return(label)
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

  # for two-ways models plm provides no standard errors
  if (twoways)
    fixedEffTable$addColumnInfo(name = "type", title = gettext("Effect"), type = "string")

  fixedEffTable$addColumnInfo(name = "effect",   title = gettext("Name"),     type = "string")
  fixedEffTable$addColumnInfo(name = "estimate", title = gettext("Estimate"), type = "number")

  if (!twoways) {
    fixedEffTable$addColumnInfo(name = "se",            title = gettext("SE"), type = "number")
    fixedEffTable$addColumnInfo(name = "testStatistic", title = "t",           type = "number")
    fixedEffTable$addColumnInfo(name = "pVal",          title = "p",           type = "pvalue")
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

  rows <- try(.fixedEffectRowsPD(fit, options), silent = TRUE)

  if (isTryError(rows)) {
    fixedEffTable$setError(gettextf("The fixed effects could not be computed: %s", .cleanErrorPD(rows)))
    return()
  }

  fixedEffTable$setData(rows)

  # (Wooldridge, 2002, p. 273)
  fixedEffTable$addFootnote(gettext("The estimated effects are unbiased but not consistent when the number of time periods is fixed."))

  if (options[["effects"]] == "twoways") {
    fixedEffTable$addFootnote(gettext("Standard errors are not available for the effects of a two-ways model."))

    if (options[["fixedEffectsType"]] == "level")
      fixedEffTable$addFootnote(gettext("Each level includes the effect of the first category of the other dimension, so an individual and a time level cannot simply be added."))
  } else if (.robustRequestedPD(options)) {
    fixedEffTable$addFootnote(gettext("The standard errors in this table are not cluster-robust."))
  }

  return()
}

.fixedEffectRowsPD <- function(fit, options) {
  if (options[["effects"]] == "twoways")
    return(.twoWaysEffectRowsPD(fit, options))

  effect       <- options[["effects"]]
  type         <- options[["fixedEffectsType"]]
  fixedEffects <- as.data.frame(summary(plm::fixef(fit, effect = effect, type = type)))

  rows <- data.frame(
    effect        = rownames(fixedEffects),
    estimate      = fixedEffects[["Estimate"]],
    se            = fixedEffects[["Std. Error"]],
    testStatistic = fixedEffects[["t-value"]],
    pVal          = fixedEffects[["Pr(>|t|)"]],
    stringsAsFactors = FALSE
  )

  # plm reports the standard error of the level for this type
  if (type == "dmean") {
    rows[["se"]]            <- .demeanedEffectStandardErrorsPD(fit, effect)
    rows[["testStatistic"]] <- rows[["estimate"]] / rows[["se"]]
    rows[["pVal"]]          <- 2 * stats::pt(-abs(rows[["testStatistic"]]), df = stats::df.residual(fit))
  }

  return(rows)
}

# Standard error of c_i (Wooldridge, 2002, p. 273, Eq. 10.58) minus the
# observation-weighted mean of all c_i.
.demeanedEffectStandardErrorsPD <- function(fit, effect) {
  terms    <- names(stats::coef(fit))
  means    <- stats::model.matrix(stats::model.frame(fit), rhs = 1, model = "between", effect = effect)[, terms, drop = FALSE]
  counts   <- plm::pdim(fit)[["Tint"]][[if (effect == "time") "nt" else "Ti"]]
  nTotal   <- sum(counts)
  centered <- sweep(means, 2, colSums(means * counts) / nTotal)
  sigma2   <- stats::deviance(fit) / stats::df.residual(fit)

  return(unname(sqrt(sigma2 * (1 / counts - 1 / nTotal) +
                       rowSums((centered %*% stats::vcov(fit)[terms, terms, drop = FALSE]) * centered))))
}

# plm::fixef() is only correct for balanced two-ways panels: it removes the other
# effect with one-way means and can attach values to the wrong period labels.
.twoWaysEffectRowsPD <- function(fit, options) {
  effects <- .twoWaysEffectsPD(fit)
  type    <- options[["fixedEffectsType"]]
  rows    <- list()

  for (effect in c("individual", "time")) {
    level  <- effects[[effect]][["level"]]
    counts <- effects[[effect]][["counts"]]
    values <- switch(type,
      "level"  = level,
      "dfirst" = level[-1] - level[1],
      "dmean"  = level - stats::weighted.mean(level, counts)
    )

    rows[[effect]] <- data.frame(
      type        = if (effect == "individual") gettext("Individual") else gettext("Time"),
      effect      = names(values),
      estimate    = unname(values),
      .isNewGroup = c(TRUE, rep(FALSE, length(values) - 1)),
      stringsAsFactors = FALSE
    )
  }

  rows <- do.call(rbind, rows)
  rownames(rows) <- NULL

  return(rows)
}

# Solves the two-way normal equations for the effects, with the first time effect as
# the reference.
.twoWaysEffectsPD <- function(fit) {
  index <- plm::index(fit)
  id    <- droplevels(as.factor(index[[1]]))
  time  <- droplevels(as.factor(index[[2]]))
  terms <- names(stats::coef(fit))

  remainder <- as.numeric(fit[["model"]][[1]]) -
    drop(stats::model.matrix(fit, model = "pooling")[, terms, drop = FALSE] %*% stats::coef(fit))

  counts      <- unclass(table(id, time))
  nPerId      <- rowSums(counts)
  nPerPeriod  <- colSums(counts)
  sumPerId    <- as.numeric(tapply(remainder, id, sum))
  sumPerTime  <- as.numeric(tapply(remainder, time, sum))
  system      <- diag(nPerPeriod, nrow = length(nPerPeriod)) - t(counts) %*% (counts / nPerId)
  rightSide   <- sumPerTime - drop(t(counts) %*% (sumPerId / nPerId))
  estimated   <- seq_along(nPerPeriod)[-1]

  if (qr(system[estimated, estimated, drop = FALSE])[["rank"]] < length(estimated))
    stop(gettext("The individuals and periods do not form one connected panel, so the effects are not separately identified."), call. = FALSE)

  timeEffects <- c(0, solve(system[estimated, estimated, drop = FALSE], rightSide[estimated]))
  idEffects   <- (sumPerId - drop(counts %*% timeEffects)) / nPerId

  return(list(
    individual = list(level = stats::setNames(idEffects, levels(id)), counts = nPerId),
    time       = list(level = stats::setNames(timeEffects + idEffects[1], levels(time)), counts = nPerPeriod)
  ))
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

  variance      <- errorComponents[["sigma2"]]
  totalVariance <- sum(variance)

  componentNames <- list(
    idios = gettext("Idiosyncratic"),
    id    = gettext("Individual"),
    time  = gettext("Time")
  )

  for (component in names(variance)) {
    componentVariance <- variance[[component]]

    randEffTable$addRows(list(
      component = componentNames[[component]],
      variance  = componentVariance,
      stddev    = sqrt(componentVariance),
      share     = componentVariance / totalVariance
    ))
  }

  thetaFootnote <- .thetaFootnotePD(errorComponents[["theta"]])

  if (!is.null(thetaFootnote))
    randEffTable$addFootnote(thetaFootnote)

  .addZeroVarianceFootnotePD(randEffTable, fit)

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
      paste0(.componentLabelPD(component), " ", .formatThetaPD(theta[[component]]))
    }, character(1))

    return(gettextf("Transformation parameter \u03B8: %s.", paste(parts, collapse = ", ")))
  }

  return(gettextf("Transformation parameter \u03B8 = %s.", .formatThetaPD(theta)))
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
  plmPlot$dependOn(c("dependent", "id", "time", "idOnly", "plot"))
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
  panelIndex     <- plm::index(plmDf)
  timeFactor     <- droplevels(as.factor(panelIndex[[2]]))
  timeLabels     <- levels(timeFactor)
  numericTime    <- suppressWarnings(as.numeric(timeLabels))
  hasNumericTime <- !anyNA(numericTime)

  plotData <- data.frame(
    time     = if (hasNumericTime) numericTime[as.integer(timeFactor)] else as.integer(timeFactor),
    position = .timePositionsPD(timeFactor),
    y        = as.numeric(plmDf[[options[["dependent"]]]]),
    id       = factor(panelIndex[[1]])
  )
  plotData <- plotData[!is.na(plotData[["y"]]), ]
  plotData <- plotData[order(plotData[["id"]], plotData[["position"]]), ]

  # lines are not drawn across missing periods
  startsSpell <- c(TRUE, plotData[["id"]][-1] != plotData[["id"]][-nrow(plotData)] | diff(plotData[["position"]]) != 1)
  plotData[["spell"]] <- cumsum(startsSpell)
  isolated <- plotData[!plotData[["spell"]] %in% plotData[["spell"]][duplicated(plotData[["spell"]])], ]

  timeRange <- range(plotData[["time"]])

  if (hasNumericTime) {
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(timeRange)

    if (all(plotData[["time"]] %% 1 == 0))
      xBreaks <- xBreaks[xBreaks %% 1 == 0]

    xBreaks <- sort(unique(c(xBreaks[xBreaks >= timeRange[1] & xBreaks <= timeRange[2]], timeRange)))
    xLabels <- as.character(xBreaks)
  } else {
    xBreaks <- unique(round(seq(timeRange[1], timeRange[2], length.out = min(diff(timeRange) + 1, 5))))
    xLabels <- timeLabels[xBreaks]
  }

  xLabel  <- if (options[["idOnly"]]) gettext("Time") else jaspBase::decodeColNames(options[["time"]])
  yLabel  <- jaspBase::decodeColNames(options[["dependent"]])
  idLabel <- jaspBase::decodeColNames(options[["id"]])

  # a legend entry per individual would leave no room for the data
  legendPosition <- if (nlevels(plotData[["id"]]) <= .pdMaxLegendEntries) "right" else "none"

  plot <- ggplot2::ggplot(plotData, ggplot2::aes(x = .data[["time"]], y = .data[["y"]], colour = .data[["id"]], group = .data[["spell"]])) +
    ggplot2::geom_line() +
    ggplot2::geom_point(data = isolated) +
    jaspGraphs::scale_x_continuous(name = xLabel, breaks = xBreaks, labels = xLabels, limits = timeRange) +
    ggplot2::labs(x = xLabel, y = yLabel, colour = idLabel) +
    jaspGraphs::geom_rangeframe(sides = "bl") +
    jaspGraphs::themeJaspRaw(legend.position = legendPosition)

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
  hausmanTestTable$dependOn(c("effects", "estimators", "robustEstimates"))
  hausmanTestTable$addCitation(.pdCitation)

  hausmanTestTable$addColumnInfo(name = "chiSq", title = "\u03C7\u00B2", type = "number")
  hausmanTestTable$addColumnInfo(name = "df",    title = "df",           type = "integer")
  hausmanTestTable$addColumnInfo(name = "pVal",  title = "p",            type = "pvalue")
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

  hausmanTest <- try(.hausmanStatisticPD(fixedFit, randomFit, options), silent = TRUE)

  if (isTryError(hausmanTest)) {
    hausmanTestTable$setError(gettextf("The Hausman test could not be performed: %s", .cleanErrorPD(hausmanTest)))
    return()
  }

  hausmanTestTable$addRows(list(
    chiSq = hausmanTest[["statistic"]],
    df    = hausmanTest[["df"]],
    pVal  = stats::pchisq(hausmanTest[["statistic"]], hausmanTest[["df"]], lower.tail = FALSE)
  ))

  hausmanTestTable$addFootnote(gettext("The null hypothesis is that the individual effects are uncorrelated with the regressors. Strict exogeneity is assumed under both hypotheses."))

  hausmanTestTable$addFootnote(if (options[["robustEstimates"]])
    gettext("Cluster-robust Wald test on the time-demeaned regressors added to the quasi-demeaned model.")
  else
    gettext("The classical form of the test assumes homoskedastic and serially uncorrelated errors."))

  return()
}

# Only coefficients of variables that vary across individuals and over time are
# compared (Wooldridge, 2002, p. 289).
.hausmanVariablesPD <- function(fixedFit, randomFit, options) {
  common     <- setdiff(intersect(names(stats::coef(fixedFit)), names(stats::coef(randomFit))), "(Intercept)")
  regressors <- stats::model.matrix(randomFit, model = "pooling")[, common, drop = FALSE]
  index      <- plm::index(randomFit)
  dimension  <- if (options[["effects"]] == "time") index[[1]] else index[[2]]

  isAggregate <- apply(regressors, 2, function(regressor) {
    all(tapply(regressor, dimension, function(values) length(unique(values)) == 1))
  })

  variables <- common[!isAggregate]

  if (length(variables) == 0)
    stop(gettext("No regressor varies across both individuals and time."), call. = FALSE)

  return(variables)
}

# (Wooldridge, 2002, p. 289, Eq. 10.78)
.hausmanStatisticPD <- function(fixedFit, randomFit, options) {
  variables <- .hausmanVariablesPD(fixedFit, randomFit, options)

  if (options[["robustEstimates"]])
    return(.robustHausmanPD(fixedFit, randomFit, variables))

  difference <- stats::coef(fixedFit)[variables] - stats::coef(randomFit)[variables]

  # the same estimate of sigma_u^2 in both matrices keeps their difference positive
  # definite (Wooldridge, 2002, p. 290)
  fixedSigma2  <- stats::deviance(fixedFit) / stats::df.residual(fixedFit)
  randomSigma2 <- if (.isUnbalancedTwoWaysRandomPD(randomFit))
    randomFit[["ercomp"]][["sigma2"]][["idios"]]
  else
    stats::deviance(randomFit) / stats::df.residual(randomFit)

  varianceDifference <- stats::vcov(fixedFit)[variables, variables, drop = FALSE] -
    stats::vcov(randomFit)[variables, variables, drop = FALSE] * fixedSigma2 / randomSigma2

  if (min(eigen(varianceDifference, symmetric = TRUE, only.values = TRUE)[["values"]]) <= 0)
    stop(gettext("The difference between the two covariance matrices is not positive definite."), call. = FALSE)

  statistic <- drop(crossprod(difference, solve(varianceDifference, difference)))

  return(list(statistic = statistic, df = length(variables)))
}

# Robust form: Wald test of xi = 0 in the regression of the quasi-demeaned data on
# the quasi-demeaned regressors and the time-demeaned variables
# (Wooldridge, 2002, pp. 290-291, Eq. 10.79).
.robustHausmanPD <- function(fixedFit, randomFit, variables) {
  if (.isUnbalancedTwoWaysRandomPD(randomFit))
    stop(gettext("Robust standard errors are not available for two-way random effects on an unbalanced panel."), call. = FALSE)

  response <- as.numeric(plm::pmodel.response(randomFit, model = "random"))
  demeaned <- stats::model.matrix(fixedFit)[, variables, drop = FALSE]
  colnames(demeaned) <- paste0(".demeaned", seq_along(variables))

  regressors  <- cbind(stats::model.matrix(randomFit, model = "random"), demeaned)
  estimate    <- drop(solve(crossprod(regressors), crossprod(regressors, response)))
  residuals   <- response - drop(regressors %*% estimate)
  robustVcov  <- .clusterSandwichPD(regressors, residuals, plm::index(randomFit)[[1]])
  tested      <- colnames(demeaned)
  statistic   <- drop(crossprod(estimate[tested], solve(robustVcov[tested, tested, drop = FALSE], estimate[tested])))

  return(list(statistic = statistic, df = length(variables)))
}

.lmTestPD <- function(jaspResults, options, ready) {
  if (!options[["lmTest"]])
    return()

  container <- .assumptionCheckContainerPD(jaspResults, "lmTest")

  if (!is.null(container[["lmTestTable"]]))
    return()

  lmTestTable <- createJaspTable(title = gettext("Lagrange Multiplier Test for Unobserved Effects"))
  lmTestTable$position <- 2
  lmTestTable$dependOn(c("lmTestEffects", "lmTestType"))
  lmTestTable$addCitation(.pdCitation)

  lmTestTable$addColumnInfo(name = "test",      title = gettext("Test"),      type = "string")
  lmTestTable$addColumnInfo(name = "effect",    title = gettext("Effect"),    type = "string")
  lmTestTable$addColumnInfo(name = "statistic", title = gettext("Statistic"), type = "number")
  lmTestTable$addColumnInfo(name = "df",        title = "df",                 type = "integer")
  lmTestTable$addColumnInfo(name = "pVal",      title = "p",                  type = "pvalue")
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
    test      = .lmTestLabelPD(options[["lmTestType"]]),
    effect    = .effectLabelPD(options[["lmTestEffects"]]),
    statistic = unname(lmTest[["statistic"]]),
    df        = if (length(degreesOfFreedom) == 1) unname(degreesOfFreedom) else NA,
    pVal      = unname(lmTest[["p.value"]])
  ))

  lmTestTable$addFootnote(.lmDistributionFootnotePD(names(lmTest[["statistic"]])))

  # (Wooldridge, 2002, pp. 264-265)
  lmTestTable$addFootnote(gettext("The null hypothesis is that the variance of the unobserved effect is zero. The test assumes homoskedastic, serially uncorrelated errors; a rejection can also reflect serial correlation or heteroskedasticity and does not establish a random effects structure."))

  return()
}

.lmTestLabelPD <- function(type) {
  label <- switch(type,
    "honda" = gettext("Honda"),
    "bp"    = gettext("Breusch-Pagan"),
    "kw"    = gettext("King-Wu"),
    "ghm"   = gettext("Gourieroux-Holly-Monfort")
  )

  return(label)
}

.lmDistributionFootnotePD <- function(distribution) {
  footnote <- switch(distribution,
    "normal"   = gettext("The statistic is asymptotically standard normal under the null hypothesis, and the p-value is one-sided."),
    "chisq"    = gettext("The statistic is asymptotically chi-squared under the null hypothesis."),
    "chibarsq" = gettext("The statistic asymptotically follows a mixture of chi-squared distributions under the null hypothesis.")
  )

  return(footnote)
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
  message   <- if (!is.null(condition)) conditionMessage(condition) else gsub("^Error[^:]*: *", "", as.character(tryError)[1])

  return(.explainErrorPD(trimws(message)))
}

# plm and base R report these from deep inside the estimation
.explainErrorPD <- function(message) {
  if (grepl("empty model", message, fixed = TRUE))
    return(gettext("No regressors remain after the transformation: they are all constant or collinear."))

  if (grepl("singular", message, fixed = TRUE))
    return(gettext("The estimating equations are singular. Check for collinear or constant regressors, and for too few observations or instruments."))

  rawErrors <- "subscript out of bounds|negative subscripts|number of rows of matrices|invalid 'nrow'|NA/NaN/Inf in|infinite or missing values|Zero-length component|incorrect number of subscripts"

  if (grepl(rawErrors, message))
    return(gettext("The panel is too small for this model, or it contains constant or collinear regressors."))

  return(message)
}
