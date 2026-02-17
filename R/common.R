.isReadyPD <- function(dataset, options) {

  # check if dependent variable and at least one independent variable is provided
  ready <- length(options$dependent) > 0 && (length(options$factors) > 0 || length(options$covariates) > 0)

  # Check that id and time factors are provided
  # or only id in case the checkbox is checked
  idTimeProvided <- (options$id != "" && options$time != "") || (options$idOnly && options$id != "")

  # combine
  ready <- ready && idTimeProvided

  return(ready)
}

.checkErrorsPD <- function(dataset, options) {
  #TODO: check for balanced panel. This needs some handling when the user chooses idOnly although it is not appropiate

  # check that factors have at least 2 levels
  .hasErrors(dataset = dataset, type = "factorLevels",
             factorLevels.target  = ifelse(
               options$idOnly,
               c(options$factors, options$id),
               c(options$factors, options$id, options$time)
             ),
             factorLevels.amount  = "< 2",
             exitAnalysisIfErrors = TRUE)

  # check for infinities and at least two observations
  .hasErrors(dataset = dataset, type = c("infinity", "observations"),
             all.target = if (!options$idOnly) c(
               options$dependent,
               options$factors,
               options$id,
               options$covariates,
               options$time
             ) else c(
               options$dependent,
               options$factors,
               options$id,
               options$covariates
             ), observations.amount = "<2",
             exitAnalysisIfErrors = TRUE)
}

.rewriteOptionsPD <- function(options, analysis) {
  options[["analysis"]] <- analysis
  return(options)
}

.fitModelPD <- function(jaspResults, dataset, options, ready) {
  if(!is.null(jaspResults[["modelFit"]]))
    return()

  modelFit <- createJaspState()
  modelFit$dependOn(c(.inputFieldNamesPD(), "estimators", "effects"))
  jaspResults[["modelFit"]] <- modelFit

  if(!ready)
    return()

  # if there is only an id specified, the plm dataframe needs to be created
  # with different arguments
  if (!options$idOnly) {
    plmDf <- plm::pdata.frame(dataset,
                              index = c(options$id, options$time))
  } else {
    plmDf <- plm::pdata.frame(dataset, index = length(unique(dataset[[options$id]]))) #TODO: check if this actually works!
  }

  form <- .createFormulaPD(options)

  # for some models, different estimators can be specified through the interface
  if(!is.null(options$estimators)) {

    plmFit <- try(
      plm::plm(formula = form, data = plmDf, model = options$analysis, effect = options$effects, random.method = options$estimators),
      silent = TRUE
    )

  } else {

    plmFit <- try(
      plm::plm(formula = form, data = plmDf, model = options$analysis, effect = options$effects),
      silent = TRUE
    )

  }

  modelFit$object <- plmFit

  return()

}

.createFormulaPD <- function(options) {
  depVarNames <- options$dependent

  indVarNames <- c()

  # if covariates are specified, add them to the IVs
  if(length(options$covariates) > 0)
    indVarNames <- c(indVarNames, options$covariates)

  # if factors are specified, add them to the IVs
  if(length(options$factors) > 0)
    indVarNames <- c(indVarNames, options$factors)

  # paste them together in lm formula style
  indVarFormula <- paste(indVarNames, collapse = " + ")

  fullFormula <- as.formula(paste(depVarNames, indVarFormula, sep = " ~ "))

  return(fullFormula)

}

.modelSummaryTablePD <- function(jaspResults, dataset, options, ready) {
  if(!is.null(jaspResults[["modelSummaryTable"]]))
    return()

  modelSummaryTable <- createJaspTable(title = gettext("Model Summary"))
  modelSummaryTable$position <- 1
  modelSummaryTable$dependOn(c(.inputFieldNamesPD(), "effects"))
  jaspResults[["modelSummaryTable"]] <- modelSummaryTable

  modelSummaryTable$addColumnInfo(name = "model", title = gettext("Model"), type = "string")
  modelSummaryTable$addColumnInfo(name = "r", title = gettext("R"), type = "number")
  modelSummaryTable$addColumnInfo(name = "rSq", title = gettext("R<sup>2</sup>"), type = "number")
  modelSummaryTable$addColumnInfo(name = "adjRSq", title = gettext("Adj. R<sup>2</sup>"), type = "number")
  modelSummaryTable$addColumnInfo(name = "df1", title = gettext("df1"), type = "integer")
  modelSummaryTable$addColumnInfo(name = "df2", title = gettext("df2"), type = "integer")
  modelSummaryTable$addColumnInfo(name = "pVal", title = gettext("p"), type = "pvalue")


  if(!ready)
    return()

  .fillModelSummaryTable(jaspResults, dataset, options)

  return()
}

.fillModelSummaryTable <- function(jaspResults, dataset, options) {

  # check if the model fitting resulted in an error
  # in that case, we would like an infomative error message
  if(isTryError(jaspResults[["modelFit"]]$object)) { # TODO: check if this works
    e <- jaspResults[["modelFit"]]$object[1]
    jaspResults[["modelSummaryTable"]]$setError(
      gettext(gettext(paste("The model fitting procedure failed because\n", e)))
    )
    return()
  }

  # relevant numbers can be extracted from the summary
  modelSum <- summary(jaspResults[["modelFit"]]$object)

  df1 <- modelSum$fstatistic$parameter[1]
  df2 <- modelSum$fstatistic$parameter[2]
  p <- modelSum$fstatistic$p.value
  rSq <- modelSum$r.squared[1]
  adjRSq <- modelSum$r.squared[2]
  r <- sqrt(rSq)

  jaspResults[["modelSummaryTable"]]$addRows(
    list(
      model = options$analysis,
      r = r,
      rSq = rSq,
      adjRSq = adjRSq,
      df1 = df1,
      df2 = df2,
      pVal = p
    )
  )

  return()
}

.coefficientsTablePD <- function(jaspResults, dataset, options, ready) {
  if(!is.null(jaspResults[["coefTable"]]))
    return()

  coefTable <- createJaspTable(title = gettext("Coefficients"))
  coefTable$position <- 2
  coefTable$dependOn(c(.inputFieldNamesPD(), "estimates", "effects"))

  coefTable$addColumnInfo(name = "coef", title = gettext("Name"), type = "string")
  coefTable$addColumnInfo(name = "estimate", title = gettext("Estimate"), type = "number")
  coefTable$addColumnInfo(name = "se", title = gettext("SE"), type = "number")
  coefTable$addColumnInfo(name = "testStatistic", title = gettext("Test Stat"), type = "number") #TODO: this is either z or t dependent on the model
  coefTable$addColumnInfo(name = "pVal", title = gettext("p"), type = "pvalue")

  jaspResults[["coefTable"]] <- coefTable

  # if the model could not be fitted to begin with, we just return this empty
  # since the model summary table already contains the error message
  if(!ready || jaspResults[["modelSummaryTable"]]$getError())
    return()

  coefDat <- summary(jaspResults[["modelFit"]]$object)$coefficients
  coefDat <- cbind(rownames(coefDat), coefDat) # add names
  colnames(coefDat) <- NULL # remove colnames, so we can use setData below
  coefTable$setData(coefDat)

  return()
}

.randEffTablePD <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["randEffTable"]]))
    return()

  randEffTable <- createJaspTable(title = gettext("Random Effects Variance Components"))
  randEffTable$position <- 3
  randEffTable$dependOn(c(.inputFieldNamesPD(), "randomEffects", "estimators", "effects"))

  randEffTable$addColumnInfo(name = "component", title = gettext("Component"),   type = "string")
  randEffTable$addColumnInfo(name = "variance",  title = gettext("Variance"),    type = "number")
  randEffTable$addColumnInfo(name = "stddev",    title = gettext("Std. Dev."),   type = "number")
  randEffTable$addColumnInfo(name = "share",     title = gettext("Share"),       type = "number")

  jaspResults[["randEffTable"]] <- randEffTable

  # if the model could not be fitted to begin with, we just return this empty
  # since the model summary table already contains the error message
  if (!ready || jaspResults[["modelSummaryTable"]]$getError())
    return()

  .fillRandEffTablePD(jaspResults, dataset, options)

  return()
}

.fillRandEffTablePD <- function(jaspResults, dataset, options) {
  plmFit <- jaspResults[["modelFit"]]$object

  ec <- plm::ercomp(plmFit) # gets random effect variances
  variance <- ec$sigma2
  totalVar <- sum(variance)
  # TODO: also add the correlation (theta) to the output
  componentNames <- list(
    idios = gettext("Idiosyncratic"),
    id    = gettext("Individual"),
    time  = gettext("Time")
  )

  for (comp in names(variance)) {
    varVal <- variance[[comp]]
    jaspResults[["randEffTable"]]$addRows(list(
      component = if (!is.null(componentNames[[comp]])) componentNames[[comp]] else comp,
      variance  = varVal,
      stddev    = sqrt(varVal),
      share     = varVal / totalVar
    ))
  }

  return()
}

.fixedEffTablePD <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["fixedEffTable"]]))
    return()

  fixedEffTable <- createJaspTable(title = gettext("Fixed Effects"))
  fixedEffTable$position <- 3
  fixedEffTable$dependOn(c(.inputFieldNamesPD(), "fixedEffects", "effects"))

  fixedEffTable$addColumnInfo(name = "effect",   title = gettext("Name"),      type = "string")
  fixedEffTable$addColumnInfo(name = "estimate",       title = gettext("Estimate"),  type = "number")

  # For effects == "twoways", the below are not available
  if(options$effects != "twoways") {
    fixedEffTable$addColumnInfo(name = "se",             title = gettext("SE"),        type = "number")
    fixedEffTable$addColumnInfo(name = "testStatistic",  title = gettext("t"),         type = "number")
    fixedEffTable$addColumnInfo(name = "pVal",           title = gettext("p"),         type = "pvalue")
  }
  fixedEffTable$showSpecifiedColumnsOnly <- TRUE

  jaspResults[["fixedEffTable"]] <- fixedEffTable

  # if the model could not be fitted to begin with, we just return this empty
  # since the model summary table already contains the error message
  if (!ready || jaspResults[["modelSummaryTable"]]$getError())
    return()

  .fillFixedEffTablePD(jaspResults, dataset, options)

  return()
}

.fillFixedEffTablePD <- function(jaspResults, dataset, options) {
  plmFit <- jaspResults[["modelFit"]]$object

  fe      <- plm::fixef(plmFit) # gets fixed effect estimates
  feSumm  <- summary(fe) # tests on fixed effect estimates
  feTable <- as.data.frame(feSumm)

  for (i in seq_len(nrow(feTable))) {
    jaspResults[["fixedEffTable"]]$addRows(list(
      effect         = rownames(feTable)[i],
      estimate       = feTable[i, "Estimate"],
      se             = feTable[i, "Std. Error"],
      testStatistic  = feTable[i, "t-value"],
      pVal           = feTable[i, "Pr(>|t|)"]
    ))
  }

  return()
}



####################### Dependency Helpers #######################
.inputFieldNamesPD <- function() {
  # helper for dependencies
  inputFieldNames <- c("covariates", "dependent", "factors", "id", "time", "idOnly")
  return(inputFieldNames)
}
