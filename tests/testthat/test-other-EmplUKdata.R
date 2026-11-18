context("Other: EmplUKdata")

# Reference values for the EmplUK panel. The analyses use the same specification as
# tests/testthat/jaspfiles/other/EmplUKdata.jasp.

emplUKOptions <- function(analysis, ...) {
  return(panelOptions(
    analysis,
    dependent  = "output",
    covariates = c("emp", "wage", "capital"),
    id         = "firm",
    time       = "year",
    estimates  = TRUE,
    ...
  ))
}

test_that("fixedModel results match", {
  results <- runPanel("fixedModel", emplUK(), emplUKOptions("fixedModel", effects = "individual"))

  table <- results[["results"]][["coefTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("emp", 1.11056701550742, 9.77204513229158e-15, 0.140988993855871,
     7.87697667126211, "wage", -0.554439687520499, 7.46587025045282e-05,
     0.139319849762306, -3.97961732277511, "capital", -0.0336719878994147,
     0.908267643656738, 0.292147543234665, -0.115256789520109))

  table <- results[["results"]][["modelSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.0967592138023625, 3, 888, "Fixed effects", 4.04430448557162e-20,
     0.0998004285707047, 32.8159752509378))
})

test_that("randomModel results match", {
  options <- emplUKOptions("randomModel", effects = "individual", hausmanTest = TRUE)
  results <- runPanel("randomModel", emplUK(), options)

  table <- results[["results"]][["assumptionChecks"]][["collection"]][["assumptionChecks_hausmanTestTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(101.325902964414, 3, 8.06091671890478e-22))

  table <- results[["results"]][["coefTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("(Intercept)", 104.355572427301, 0, 1.6718635097849, 62.4187152937664,
     "emp", 0.0474676083923376, 0.267525510530252, 0.0428107010258536,
     1.10877904951081, "wage", -0.0249276350851194, 0.70787725117983,
     0.0665254014786927, -0.374708525330785, "capital", -0.125017632835039,
     0.254181236836011, 0.109640332621601, -1.140252221475))

  table <- results[["results"]][["modelSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.0367352969587039, 3, "Random effects", 0.626103910968269, 0.0395409223073679,
     1.74896166925695))
})

test_that("betweenModel results match", {
  results <- runPanel("betweenModel", emplUK(), emplUKOptions("betweenModel", effects = "individual"))

  table <- results[["results"]][["coefTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("(Intercept)", 100.979513768507, 2.32117812960778e-95, 1.81756215908933,
     55.5576673202207, "emp", -0.00655994976492045, 0.885164183086302,
     0.0453357144696541, -0.144697173997587, "wage", 0.127684415274174,
     0.0808750132282726, 0.0726009455309816, 1.75871559716376, "capital",
     -0.0548495163124116, 0.646023189235851, 0.119155729487082, -0.460317909583678
    ))

  table <- results[["results"]][["modelSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.0127448194292353, 3, 136, "Between", 0.192747415761172, 0.0340524851969497,
     1.59813306479336))
})
