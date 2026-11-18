context("QML and R option contract")

# Runs in a separate R process: once jaspSyntax is loaded, jaspBase reads datasets
# through it, which breaks the analyses run on plain data frames in the other tests.
readFormOptions <- function(modulePath, analyses) {
  script <- tempfile(fileext = ".R")
  output <- tempfile(fileext = ".rds")
  on.exit(unlink(c(script, output)))

  writeLines(c(
    sprintf(".libPaths(%s)", paste(deparse(.libPaths()), collapse = "")),
    sprintf("analyses <- %s", paste(deparse(analyses), collapse = "")),
    sprintf("forms <- lapply(analyses, function(analysis) jaspSyntax::readDefaultAnalysisOptions(%s, analysis, includeMeta = FALSE, includeTypeOptions = FALSE))",
            deparse(modulePath)),
    "names(forms) <- analyses",
    sprintf("saveRDS(forms, %s)", deparse(output))
  ), script)

  status <- system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(script)), stdout = FALSE, stderr = FALSE)
  testthat::skip_if_not(status == 0 && file.exists(output), "the QML forms could not be read")

  return(readRDS(output))
}

test_that("the test defaults are the defaults of the QML forms", {
  testthat::skip_if_not_installed("jaspSyntax")

  modulePath <- normalizePath(testthat::test_path("..", ".."))
  testthat::skip_if_not(dir.exists(file.path(modulePath, "inst", "qml")))

  variableOptions <- c("dependent", "id", "time", .panelListOptions)
  forms           <- readFormOptions(modulePath, names(.panelDefaults))

  for (analysis in names(.panelDefaults)) {
    formOptions <- forms[[analysis]]
    formOptions <- formOptions[!names(formOptions) %in% c("plotWidth", "plotHeight")]
    expected    <- .panelDefaults[[analysis]]

    expect_setequal(names(formOptions), names(expected))

    for (option in setdiff(names(expected), variableOptions))
      expect_equal(formOptions[[option]], expected[[option]], info = paste(analysis, option))
  }
})

test_that("every option R reads exists in the QML of some analysis", {
  modulePath <- normalizePath(testthat::test_path("..", ".."))
  testthat::skip_if_not(dir.exists(file.path(modulePath, "R")))

  sources <- unlist(lapply(list.files(file.path(modulePath, "R"), pattern = "\\.R$", full.names = TRUE), readLines, warn = FALSE))
  read    <- unique(unlist(regmatches(sources, gregexpr('options\\[\\["[A-Za-z]+"\\]\\]', sources))))
  read    <- setdiff(gsub('options\\[\\["|"\\]\\]', "", read), "analysis")

  qmlFiles <- list.files(file.path(modulePath, "inst", "qml"), pattern = "\\.qml$", recursive = TRUE, full.names = TRUE)
  qml      <- unlist(lapply(qmlFiles, readLines, warn = FALSE))
  names    <- gsub('.*name\\s*:\\s*"([A-Za-z]+)".*', "\\1", grep('name\\s*:\\s*"', qml, value = TRUE))

  expect_length(setdiff(read, names), 0)
})
