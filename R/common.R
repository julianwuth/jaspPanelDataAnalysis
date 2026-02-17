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
  modelFit$dependOn(c(.inputFieldNamesPD()))
  jaspResults[["modelFit"]] <- modelFit

  if(!ready)
    return()

  saveRDS(dataset, "/Users/julian/Documents/Jasp files/dataset.rds")

  if (!options$idOnly) {
    plmDf <- plm::pdata.frame(dataset,
                              index = c(options$id, options$time))
  } else {
    plmDf <- plm::pdata.frame(dataset, index = length(unique(dataset[[options$id]]))) #TODO: check if this actually works!
  }

  form <- .createFormulaPD(options)
  plmFit <- plm::plm(
    formula = form, data = plmDf, model = options$analysis
  )
  modelFit$object <- plmFit

  return()

}

.createFormulaPD <- function(options) {
  depVarNames <- options$dependent

  indVarNames <- c()

  if(length(options$covariates) > 0)
    indVarNames <- c(indVarNames, options$covariates)

  if(length(options$factors) > 0)
    indVarNames <- c(indVarNames, options$factors)

  indVarFormula <- paste(indVarNames, collapse = " + ")

  fullFormula <- as.formula(paste(depVarNames, indVarFormula, sep = " ~ "))

  return(fullFormula)

}

.modelSummaryTablePD <- function(jaspResults, dataset, options, ready) {
  if(!is.null(jaspResults[["modelSummaryTable"]]))
    return()

  modelSummaryTable <- createJaspTable(title = gettext("Model Summary"))
  modelSummaryTable$position <- 1
  modelSummaryTable$dependOn(c(.inputFieldNamesPD(), "idOnly"))
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




####################### Dependency Helpers #######################
.inputFieldNamesPD <- function() {
  # helper for dependencies
  inputFieldNames <- c("covariates", "dependent", "factors", "id", "time")
  return(inputFieldNames)
}
