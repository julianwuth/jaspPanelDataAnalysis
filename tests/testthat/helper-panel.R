# Shared helpers for the panel data tests.
#
# The analyses are run on plain data frames from plm with explicit, GUI-equivalent
# options: jaspTools::analysisOptions() cannot see the QML components imported from
# inst/qml/Common, and test-qml-options.R checks these defaults against the QML.

# pgmm(), pggls() and pvcm() evaluate a bare `plm(...)` call in their caller's frame
plm <- plm::plm

.panelBaseOptions <- list(
  dependent = "",
  covariates = list(),
  factors = list(),
  id = "",
  time = "",
  idOnly = FALSE,
  plot = FALSE
)

.panelCoefficientOptions <- list(estimates = FALSE, robustEstimates = FALSE)

.panelDefaults <- list(
  fixedModel = c(.panelBaseOptions, .panelCoefficientOptions, list(
    effects = "individual", fixedEffects = FALSE, fixedEffectsType = "level", hausmanTest = FALSE
  )),
  randomModel = c(.panelBaseOptions, .panelCoefficientOptions, list(
    effects = "individual", randomEffects = FALSE, estimators = "swar", hausmanTest = FALSE
  )),
  poolingModel = c(.panelBaseOptions, .panelCoefficientOptions, list(
    lmTest = FALSE, lmTestEffects = "individual", lmTestType = "honda"
  )),
  firstDifferenceModel = c(.panelBaseOptions, .panelCoefficientOptions),
  betweenModel = c(.panelBaseOptions, .panelCoefficientOptions, list(effects = "individual")),
  htModel = c(.panelBaseOptions[c("dependent", "id", "time", "idOnly", "plot")], .panelCoefficientOptions, list(
    timeVaryingExogenous = list(), timeVaryingEndogenous = list(),
    timeInvariantExogenous = list(), timeInvariantEndogenous = list(), htMethod = "baltagi"
  )),
  vcmModel = c(.panelBaseOptions, list(vcmEstimator = "within", effects = "individual")),
  pgglsModel = c(.panelBaseOptions, list(pgglsEstimator = "within", effects = "individual")),
  gmmModel = c(.panelBaseOptions[c("dependent", "covariates", "id", "time", "idOnly", "plot")], list(
    endogenousCovariates = list(), lagsDependent = 1L, instrumentLagMin = 2L, instrumentLagMax = 99L,
    endogenousLagMin = 2L, collapseInstruments = FALSE, gmmSteps = "onestep", transformation = "d",
    effects = "individual", robustSE = TRUE
  ))
)

# panelOptions("fixedModel", dependent = "inv", covariates = c("value", "capital"), ...)
panelOptions <- function(analysis, ...) {
  options   <- .panelDefaults[[analysis]]
  arguments <- list(...)

  # by position, so that a later argument overrides an earlier one of the same name
  for (i in seq_along(arguments))
    options[[names(arguments)[i]]] <- if (length(arguments[[i]]) > 1) as.list(arguments[[i]]) else arguments[[i]]

  # multi-variable slots stay lists, as the GUI sends them
  for (name in intersect(names(options), .panelListOptions))
    options[[name]] <- as.list(options[[name]])

  return(options)
}

.panelListOptions <- c(
  "covariates", "factors", "endogenousCovariates", "timeVaryingExogenous",
  "timeVaryingEndogenous", "timeInvariantExogenous", "timeInvariantEndogenous"
)

panelData <- function(name) {
  environment <- new.env()
  utils::data(list = name, package = "plm", envir = environment)

  return(environment[[name]])
}

grunfeld <- function() {
  data      <- panelData("Grunfeld")
  data$firm <- factor(data$firm)
  data$year <- ordered(data$year)

  return(data)
}

# the same panel with rows removed at random
grunfeldUnbalanced <- function() {
  data <- grunfeld()
  set.seed(1)

  return(data[-sample(nrow(data), 30), ])
}

emplUK <- function() {
  data      <- panelData("EmplUK")
  data$firm <- factor(data$firm)
  data$year <- ordered(data$year)

  return(data)
}

wages <- function() {
  data        <- panelData("Wages")
  data$id     <- factor(rep(seq_len(595), each = 7))
  data$period <- ordered(rep(seq_len(7), times = 595))
  data$female <- as.numeric(data$sex == "female")
  data$isBlack <- as.numeric(data$black == "yes")
  data$isMarried <- as.numeric(data$married == "yes")
  data$isSouth <- as.numeric(data$south == "yes")
  data$isSmsa  <- as.numeric(data$smsa == "yes")
  data$isUnion <- as.numeric(data$union == "yes")

  return(data)
}

runPanel <- function(analysis, data, options) {
  set.seed(1)

  return(jaspTools::runAnalysis(analysis, data, options, view = FALSE, quiet = TRUE))
}

panelTable <- function(results, name) {
  table <- results[["results"]][[name]]

  if (is.null(table))
    table <- results[["results"]][["assumptionChecks"]][["collection"]][[paste0("assumptionChecks_", name)]]

  return(table)
}

# one column of a table as a vector
panelColumn <- function(results, name, column) {
  values <- lapply(panelTable(results, name)[["data"]], `[[`, column)
  values[vapply(values, is.null, logical(1))] <- NA

  return(unlist(values))
}

# the value of `column` in the row whose `key` column equals `row`
panelCell <- function(results, name, column, row, key = "coef") {
  return(panelColumn(results, name, column)[match(row, panelColumn(results, name, key))])
}

panelFootnotes <- function(results, name) {
  return(vapply(panelTable(results, name)[["footnotes"]], `[[`, character(1), "text"))
}

panelError <- function(results, name) {
  return(panelTable(results, name)[["error"]][["errorMessage"]])
}

expect_panelFootnote <- function(results, name, pattern) {
  testthat::expect_true(
    any(grepl(pattern, panelFootnotes(results, name))),
    info = paste("Footnotes:", paste(panelFootnotes(results, name), collapse = " | "))
  )
}
