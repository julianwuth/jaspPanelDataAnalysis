context("Other: EmplUKdata")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("fixedModel (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "EmplUKdata.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("fixedModel", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["coefTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("jaspColumn1", 1.11056701550742, 9.77204513229158e-15, 0.140988993855871,
     7.87697667126211, "jaspColumn2", -0.554439687520499, 7.46587025045282e-05,
     0.139319849762306, -3.97961732277511, "jaspColumn3", -0.0336719878994147,
     0.908267643656738, 0.292147543234665, -0.115256789520109))

  table <- results[["results"]][["modelSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.0441504037974934, 3, 888, "Fixed effects", 4.04430448557162e-20,
     0.0998004285707047, 32.8159752509378))

})

test_that("randomModel (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "EmplUKdata.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("randomModel", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["assumptionChecks"]][["collection"]][["assumptionChecks_hausmanTestTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(102.289263531546, 3, 5.00275119864689e-22))

  table <- results[["results"]][["coefTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("(Intercept)", 104.355572427301, 0, 1.6718635097849, 62.4187152937664,
     "jaspColumn1", 0.0474676083923376, 0.267525510530252, 0.0428107010258536,
     1.10877904951081, "jaspColumn2", -0.0249276350851194, 0.70787725117983,
     0.0665254014786927, -0.374708525330785, "jaspColumn3", -0.125017632835039,
     0.254181236836011, 0.109640332621601, -1.140252221475))

  table <- results[["results"]][["modelSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.0367352969587039, 3, "Random effects", 0.626103910968269, 0.0395409223073679,
     1.74896166925695))

})

test_that("betweenModel (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "EmplUKdata.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("betweenModel", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["coefTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("(Intercept)", 100.979513768507, 2.32117812960778e-95, 1.81756215908933,
     55.5576673202207, "jaspColumn1", -0.00655994976492045, 0.885164183086302,
     0.0453357144696541, -0.144697173997587, "jaspColumn2", 0.127684415274174,
     0.0808750132282726, 0.0726009455309816, 1.75871559716376, "jaspColumn3",
     -0.0548495163124116, 0.646023189235851, 0.119155729487082, -0.460317909583678
    ))

  table <- results[["results"]][["modelSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.0127448194292353, 3, 136, "Between", 0.192747415761172, 0.0340524851969497,
     1.59813306479336))

})

