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
             all.target = c(
               options$dependent,
               options$factors,
               options$id,
               options$covariates,
               options$time
             ), observations.amount = "<2",
             exitAnalysisIfErrors = TRUE)
}


.modelSummaryTablePD <- function(jaspResults, dataset, options, ready) {
  if(!is.null(jaspResults[["modelSummaryTable"]]))
    return()

  modelSummaryTable <- createJaspTable(title = gettext("Model Summary"))
  modelSummaryTable$position <- 1
  modelSummaryTable$dependOn(.inputFieldNamesPD())
  jaspResults[["modelSummaryTable"]] <- modelSummaryTable

  modelSummaryTable$addColumnInfo(name = "model", title = gettext("Model"), type = "string")

  if(!ready)
    return()

  .fillModelSummaryTable(jaspResults, dataset, options)

  return()
}

.fillModelSummaryTable <- function(jaspResults, dataset, options) {
  return()
}




####################### Dependency Helpers #######################
.inputFieldNamesPD <- function() {
  # helper for dependencies
  inputFieldNames <- c("covariates", "dependent", "factors", "id", "time")
  return(inputFieldNames)
}
