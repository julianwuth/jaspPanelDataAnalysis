#' @import jaspBase
#' @export
poolingModel <- function(jaspResults, dataset, options, analysis = "pooling") {
  ready <- .isReadyPD(dataset, options)

  if(ready) {
    .checkErrorsPD(dataset, options)
    options <- .rewriteOptionsPD(options, analysis)
  }

  .fitModelPD(jaspResults, dataset, options, ready)

  .modelSummaryTablePD(jaspResults, dataset, options, ready)

  if(options$estimates)
    .coefficientsTablePD(jaspResults, dataset, options, ready)

  if(options$plot)
    .createPlmPlot(jaspResults, dataset, options, ready)

  .plmTestPoolability(jaspResults, options)

  return()
}

.plmTestPoolability <- function(jaspResults, options, ready) {

  # 1. Check if the Master Checkbox (lmt) is checked
  if (!options$lmt)
    return()

  # 2. Check if table already exists
  if (!is.null(jaspResults[["lmtTable"]]))
    return()

  # 3. Create the Table
  lmtTable <- createJaspTable(title = "Lagrange Multiplier Test for Poolability")
  lmtTable$dependOn(c("lmt", "effects", "type", "dependent", "covariates"))

  # 4. Define Columns
  lmtTable$addColumnInfo(name = "test",   title = "Test Statistic", type = "string")
  lmtTable$addColumnInfo(name = "effect", title = "Effect",         type = "string")
  lmtTable$addColumnInfo(name = "stat",   title = "Statistic",      type = "number")
  lmtTable$addColumnInfo(name = "df",     title = "df",             type = "integer")
  lmtTable$addColumnInfo(name = "p",      title = "p",              type = "pvalue")

  # 5. Assign to Output
  jaspResults[["lmtTable"]] <- lmtTable

  if (!ready) {
    return()
  }

  # 6. Run the Test
  # We pass options$effects and options$type directly because your QML values
  # ("individual", "honda", etc.) match the plm::plmtest arguments exactly.

  testResult <- try(plm::plmtest(jaspResults[["modelFit"]]$object,
                                 effect = options$effects,
                                 type   = options$type))

  if (inherits(testResult, "try-error")) {
    lmtTable$setError("The Lagrange Multiplier test could not be performed.")
    return()
  }

  # 7. Handle Degrees of Freedom
  # Honda and King-Wu tests are N(0,1) distributed and return 'parameter = NULL'.
  # We ensure this doesn't crash the table by checking for NULL.
  dfValue <- if (!is.null(testResult$parameter)) as.numeric(testResult$parameter) else NA

  # 8. Create Row
  row <- list(
    test   = testResult$method, # e.g. "Lagrange Multiplier Test - (Honda)"
    effect = options$effects,   # e.g. "individual"
    stat   = as.numeric(testResult$statistic),
    df     = dfValue,
    p      = as.numeric(testResult$p.value)
  )

  lmtTable$addRows(row)
}
