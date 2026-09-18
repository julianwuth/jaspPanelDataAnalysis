# jaspPanelDataAnalysis — Code Audit & Implementation Plan

Date: 2026-09-18
Reference implementation: **plm 2.6.7** (Croissant & Millo, *JSS* 27(2), 2008)
Scope: `R/*.R`, `inst/qml/**`, `inst/Description.qml`, `tests/`

---

## 1. Current State

### 1.1 Analysis inventory

| Description.qml entry | R function | QML | Backend status |
|---|---|---|---|
| Panel data analysis | `panelDataAnalysis` | `panelDataAnalysis.qml` | **Broken** — duplicates `randomModel`, reads options its QML does not define |
| Fixed Effects Model | `fixedModel` | `FixedModel.qml` | Working (`model = "within"`) |
| Random Effects Model | `randomModel` | `RandomModel.qml` | Working (`model = "random"`) |
| Hausman-Taylor Model | `htModel` | `HTModel.qml` | **Empty stub** — `return()` |
| Pooling Model | `poolingModel` | `PoolingModel.qml` | Partial — `model = "pooling"`; LM-test GUI not wired |
| First-Difference Model | `firstDifferenceModel` | `FirstDifferenceModel.qml` | Partial — invalid `effect` choices offered |
| Between Model | `betweenModel` | `BetweenModel.qml` | **Empty stub** — `return()` |
| Variable Coefficients Models | `vcmModel` | `VCMModel.qml` | **Empty stub** — `return()` |
| Generalized Method of Moments | `gmmModel` | `GMMModel.qml` | **Empty stub** — `return()` |
| General FGLS | `pgglsModel` | `PGGLSModel.qml` | **Empty stub** — `return()` |

Shared backend lives in `R/common.R` (449 lines). Five of ten registered analyses produce no output at all.

### 1.2 Leftover module-template code

`R/addOne.R`, `R/parabola.R`, `R/interface.R`, `R/loadingData.R` are unexported, unregistered dead code from `jaspModuleTemplate`. `DESCRIPTION` still says *"Example module showing basic functionality"*, `Date: 2020-10-15`, `Author: JASP Team`. `inst/icons/exampleIcon.png` is still the module icon. `inst/help/` is empty. `tests/testthat/` contains only `.gitkeep` — **zero tests**, while `.github/workflows/unittests.yml` runs on every push.

---

## 2. Confirmed Defects

Each item below was verified against plm 2.6.7 in an R session, not inferred.

### B1 — `idOnly` produces a wrong panel index (`R/common.R:111`, `R/common.R:408`)

```r
plmDf <- plm::pdata.frame(dataset, index = length(unique(dataset[[options$id]])))
```

`pdata.frame(index = <integer n>)` means *"balanced panel, n individuals, rows already sorted by individual"*. It **discards the user's ID column** and synthesises fresh `id`/`time` columns:

```
> names(pdata.frame(Grunfeld, index = 10))
[1] "firm" "year" "inv" "value" "capital" "id" "time"
```

So on unbalanced or unsorted data the grouping is silently wrong. The intended behaviour ("time index derived from within-individual row order") is what a **character** index does:

```r
plmDf <- plm::pdata.frame(dataset, index = options$id)   # correct
```

Also: a `length(unique(...))` value only accidentally equals "number of individuals" when the panel is balanced. Fix both call sites and delete the duplicated index-building logic (see R5).

### B2 — Multiple dependent variables silently corrupt the formula (`R/common.R:140`, `inst/qml/Common/VariableInput.qml:11`)

`VariableInput.qml` declares `dependent` with `singleVariable: false`, but `.createFormulaPD()` does `paste(depVarNames, indVarFormula, sep = " ~ ")`. With two dependents this yields a length-2 character vector; `as.formula()` then silently returns `` `~`() `` (plus a deprecation warning) instead of erroring. Either set `singleVariable: true` (recommended — `plm` takes one LHS) or implement a real multi-outcome loop.

### B3 — Model Summary mislabels the random-effects test (`R/common.R:202-206`)

For `model = "random"`, `summary.plm()$fstatistic` is a **Wald chi-square** test with a single df:

```
Chisq = 657.67, df = 2, p-value < 2.2e-16
```

The table hardcodes `df1`/`df2` columns and an unnamed `p`, so `df2` is always empty for random models and the statistic itself is never shown. The overall test statistic (F or χ²) is missing from the table entirely.

### B4 — Crash when "Estimates" is ticked before variables are assigned (`R/common.R:233`)

```r
testStatTitle <- if (options$analysis == "random") gettext("z") else gettext("t")
```

`options$analysis` is injected by `.rewriteOptionsPD()` **only inside `if (ready)`**. When not ready, `options$analysis` is `NULL`, so the condition is `logical(0)` → `Error: argument is of length zero`. Reproduce: drag in the analysis, tick *Estimates*, assign nothing.

### B5 — `panelDataAnalysis` reads options its QML does not define (`R/panelDataAnalysis.R`)

`panelDataAnalysis.qml` contains only `Common.VariableInput` + `Common.Coefficients`. The R function reads `options$effects` (→ `plm(effect = NULL)`) and `.fixedEffTablePD()` reads `options$effects != "twoways"` (→ `logical(0)` → crash). Line 22 (`if (analysis == "within")`) is dead code, since `analysis` defaults to `"random"` and is never overridden. This analysis is a redundant copy of `randomModel`.

### B6 — `Common/Effects.qml` offers effects plm rejects for most models

`Effects.qml` is shared by Between, First-Difference, Fixed, Random and HT, and offers `individual / time / twoways / nested`. Verified errors:

| combination | plm result |
|---|---|
| `model="fd", effect="twoways"` | `effect = "twoways" is not defined for first-difference models` |
| `model="between", effect="twoways"` | `twoways effect only relevant for within, random, and pooling models` |
| `model="within", effect="nested"` | `effect = "nested" only valid for model = "random"` |
| `model="random", effect="nested"` | `the nested error component model requires three-indexed data` |

`nested` is unusable anywhere in the module because there is no third (group) index variable in the GUI. `fd` additionally accepts only `individual` and `time`.

### B7 — GUI options that the backend never reads

| QML `name` | File | Status |
|---|---|---|
| `robust_estimates` | `Common/Coefficients.qml:23` | Never read in R. Also snake_case, violating the module's camelCase rule. |
| `lmt`, `type` | `PoolingModel.qml:22,41` | Lagrange Multiplier test GUI is fully built; no R implementation exists. |
| `model`, `effect`, `transformation` | `GMMModel.qml`, `PGGLSModel.qml`, `VCMModel.qml` | Backends are stubs. Note `effect` (singular) is inconsistent with `effects` used elsewhere. |

### B8 — `effects` name collision in `PoolingModel.qml` (`PoolingModel.qml:30`)

The LM-test effect radio group is named `effects` — the same name `.fitModelPD()` uses for `plm(effect = ...)`. Changing the *Lagrange Multiplier test* effect therefore silently re-estimates the pooling model and invalidates the Model Summary table. Rename to `lmTestEffects`.

### B9 — Stale output: missing `$dependOn` entries

| Element | File | Missing dependency |
|---|---|---|
| `modelSummaryTable` | `common.R:167` | `"estimators"` — changing the random-effects estimator leaves the summary stale |
| `coefTable` | `common.R:230` | `"estimators"` — same |
| `plmPlot` | `common.R:23` | `"idOnly"`, `"covariates"`, `"factors"` |

### B10 — Plot breaks under `idOnly` and is not translation/encoding safe (`R/common.R:17-57`)

- `.plmFillPlotDescriptives()` reads `dataset[[options$time]]` unconditionally; with `idOnly` checked, `options$time` is `""` → error.
- `createJaspPlot(title = "Line Plot")` — not wrapped in `gettext()`.
- `ggplot2::aes(dataset[[options$time]], ...)` uses the external-vector form rather than `.data[[...]]`; fragile and produces encoded column names in axis labels. Should use `jaspBase::decodeColNames()` for the labels.
- No `try()` wrapper / `$setError()` path.

### B11 — Untranslatable error strings (`R/common.R:194`, `:420`, `:429`)

```r
gettext(gettext(paste("The model fitting procedure failed because\n", e)))
```

Double `gettext()` on a runtime-interpolated string. The extractor cannot produce a usable msgid. Use `gettextf("The model could not be estimated: %s", e)`.

### B12 — Coefficients table coerces everything to character (`R/common.R:248-251`)

```r
coefDat <- cbind(rownames(coefDat), coefDat)
```

`cbind()` of a character vector with a numeric matrix coerces the whole matrix to character, then feeds it to `setData()` against columns declared `type = "number"` / `"pvalue"`. Build a `data.frame` instead. Also the coefficient names are the raw encoded column names — decode them.

### B13 — Hausman test refits both models on every invocation (`R/common.R:398-441`)

`.fillHausmanTestTablePD()` rebuilds the `pdata.frame` and re-estimates both the within and random models inline, outside any `createJaspState()`. Every unrelated option change re-runs two estimations. Move to a cached state object.

### B14 — Two-ways fixed effects table (`R/common.R:339-357`)

For `effect = "twoways"`, `summary(fixef(fit))` returns only an `Estimate` column with integer row names — no labels, no SEs. The existing `if (options$effects != "twoways")` guard hides the empty columns, but `.fillFixedEffTablePD()` still writes `feTable[i, "Std. Error"]` → subscript error. `plm::fixef()` takes `effect = c("individual", "time")` for twoways models; call it twice and emit two labelled blocks.

### B15 — `stats` not imported

`as.formula()` is called unqualified (`common.R:155`). `DESCRIPTION` has no `stats` in `Imports`, `NAMESPACE` has no `importFrom(stats, ...)`. Use `stats::as.formula()`.

### B16 — `Description.qml` data flags contradict themselves

Module-level `requiresData: true`, but every `Analysis` block sets `requiresData: false`. Every panel analysis requires data. Set `requiresData: true` per analysis (or drop the per-analysis override).

---

## 3. Coverage Gap vs. plm

Legend: ✅ implemented · 🟡 partial · ❌ missing

### 3.1 Estimators

| plm | Module |
|---|---|
| `plm(model="within")` | ✅ |
| `plm(model="random")` | ✅ (`random.method`: swar/walhus/amemiya/nerlove) |
| `plm(model="pooling")` | ✅ |
| `plm(model="between")` | ❌ stub |
| `plm(model="fd")` | ✅ |
| `plm(model="ht")` / `pht()` — Hausman-Taylor, Amemiya-MaCurdy, Breusch-Mizon-Schmidt | ❌ stub |
| `pvcm()` — variable coefficients (`model="within"`/`"random"`, Swamy) | ❌ stub |
| `pgmm()` — Arellano-Bond / Blundell-Bond | ❌ stub |
| `pggls()` / `pggls(model="fd")` — general FGLS | ❌ stub |
| `pcce()` — common correlated effects (mg/p) | ❌ not registered |
| `pmg()` — mean groups / demeaned MG / CCEMG | ❌ not registered |
| `pldv()` — panel limited dependent variables (tobit) | ❌ not registered |
| IV / 2SLS via `y ~ x | z` and `inst.method` (bvk, baltagi, am, bms) | ❌ no instrument slot in any QML |
| `random.method = "ht"` | ❌ missing from `RandomModel.qml` |
| Nested error components (3-index) | ❌ offered in GUI but unusable (B6) |
| `weights`, `subset`, `na.action` | ❌ |

### 3.2 Specification & diagnostic tests

| plm function | Purpose | Module |
|---|---|---|
| `phtest()` | Hausman, FE vs RE | 🟡 chisq form only; `method = "aux"` (robust/regression-based) missing |
| `pFtest()` | F test for individual/time effects (within vs pooling) | ❌ |
| `plmtest()` | Breusch-Pagan LM for random effects — honda/bp/kw/ghm × individual/time/twoways | ❌ (GUI exists, B7) |
| `pooltest()` | Poolability of coefficients across individuals | ❌ |
| `pwtest()` | Wooldridge test for unobserved effects | ❌ |
| `pbsytest()` | Bera-Sosa-Escudero-Yoon (joint / RE / AR) | ❌ |
| `pbgtest()` | Breusch-Godfrey/Wooldridge serial correlation | ❌ |
| `pdwtest()` | Durbin-Watson for panel models | ❌ |
| `pbltest()` | Baltagi-Li serial correlation LM | ❌ |
| `pwartest()` | Wooldridge AR(1) test for FE models | ❌ |
| `pwfdtest()` | Wooldridge first-difference serial correlation | ❌ |
| `pbnftest()` | Bhargava-Franzini-Narendranathan DW / Baltagi-Wu LBI | ❌ |
| `pcdtest()` | Cross-sectional dependence: Pesaran CD, BP LM, scaled LM, Friedman, Frees | ❌ |
| `purtest()` | Panel unit roots: levinlin, ips, madwu, Pm, invnormal, logit, hadri | ❌ |
| `cipstest()` | Pesaran CIPS (2nd-generation unit root) | ❌ |
| `phansitest()` | Simes-based nonstationarity | ❌ |
| `pgrangertest()` | Panel Granger non-causality | ❌ |
| `aneweytest()` | Angrist-Newey specification test | ❌ |
| `piest()` | Chamberlain π test | ❌ |
| `pwaldtest()` | Wald test with user-supplied vcov | ❌ |
| `sargan()`, `mtest()` | GMM overidentification / AR in residuals | ❌ |
| `detect.lindep()` | Linear dependence among regressors | ❌ |

### 3.3 Robust covariance matrices

Entirely missing despite a "Robust Estimates" checkbox in the GUI.

| plm | Notes |
|---|---|
| `vcovHC()` | `method = "arellano"/"white1"/"white2"`, `type = "HC0".."HC4"`, `cluster = "group"/"time"` |
| `vcovBK()` | Beck-Katz panel-corrected SEs |
| `vcovDC()` | Double-clustering (group + time) |
| `vcovNW()` | Newey-West |
| `vcovSCC()` | Driscoll-Kraay |
| `vcovG()` | General group-wise sandwich |
| `pvcovHC()` | For `pvcm` objects |

Application layer: `lmtest::coeftest(fit, vcov. = ...)` and `plm::pwaldtest(fit, vcov = ...)`. Both `lmtest` and `sandwich` are already in the renv library but not in `DESCRIPTION`.

### 3.4 Post-estimation

| plm | Module |
|---|---|
| `fixef()` (`type = "level"/"dfirst"/"dmean"`, `effect =` for twoways) | 🟡 `type` and `effect` not exposed; twoways broken (B14) |
| `ranef()` — random-effect BLUPs | ❌ |
| `ercomp()` — variance components | 🟡 `theta` (transformation parameter) missing — existing TODO at `common.R:287` |
| `within_intercept()` | ❌ |
| `r.squared(type =, dfcor =)` | ❌ only the default from `summary()` |
| `predict()` / `fitted()` / `residuals()` | ❌ no residual diagnostics anywhere |
| `lag()`, `lead()`, `diff()` on `pseries` | ❌ no dynamic model terms |
| Interactions / polynomial terms | ❌ no model-terms builder |
| Confidence intervals on coefficients | ❌ |

### 3.5 Panel structure utilities

| plm | Module |
|---|---|
| `pdim()` — n, T, N, balanced | ❌ no panel-structure output at all |
| `is.pbalanced()`, `punbalancedness()` (Ahrens-Pincus gamma/nu) | ❌ (existing TODO at `common.R:60`) |
| `is.pconsecutive()` — gaps in time | ❌ |
| `make.pbalanced()`, `make.pconsecutive()` | ❌ |
| `pvar()` — variables constant over id or time | ❌ (would catch a very common user error: a time-invariant regressor in a within model) |
| `make.dummies()` | ❌ |

---

## 4. Implementation Plan

Eight phases, ordered so each builds on the previous. Phases 0-2 are prerequisites; 3-7 are largely independent and can be reordered by priority.

Work on a feature branch per phase. Run `agentTestAll()` after each phase (300+ s — never cancel).

---

### Phase 0 — Cleanup & foundations

**Goal:** remove template residue, put the shared plumbing on a sound footing before adding features.

1. Delete `R/addOne.R`, `R/parabola.R`, `R/interface.R`, `R/loadingData.R`.
2. Update `DESCRIPTION`: real `Title`/`Description`/`Author`/`Date`; add `stats` to `Imports`; add `lmtest` and `sandwich` (needed from Phase 4 — both are already in `renv.lock`). Regenerate `NAMESPACE` with roxygen, adding `importFrom(stats, as.formula, ...)`.
3. Replace `inst/icons/exampleIcon.*` with a panel-data icon; update `Description.qml`.
4. Fix `Description.qml` `requiresData` (B16).
5. Remove the `panelDataAnalysis` entry from `Description.qml` and `NAMESPACE`, and delete `R/panelDataAnalysis.R` + `inst/qml/panelDataAnalysis.qml` (B5). It duplicates `randomModel` and is broken. *(If a generic "any model" entry point is wanted instead, rebuild it properly in Phase 2 with its own complete QML.)*
6. Rename `robust_estimates` → `robustEstimates` in `Common/Coefficients.qml` (B7). No `Upgrades.qml` entry needed — the module is unreleased (version 0.1, not in a JASP release).
7. Create `R/panelCommon.R` as the canonical shared file and reorganise `R/common.R` into clearly separated sections: readiness/validation → data prep → fitting → tables → plots → tests. Keep the `PD` suffix convention.

**Deliverable:** module loads with no dead code; `devtools::check()` produces no undefined-global notes.

---

### Phase 1 — Fix the confirmed defects

Address B1-B4, B6, B8-B16 in `R/common.R` and the QML.

**1a. Single data-prep helper (B1).** Replace both inline `pdata.frame` constructions with one function, and cache the result:

```r
.pdataFramePD <- function(dataset, options) {
  index <- if (options[["idOnly"]]) options[["id"]]
           else c(options[["id"]], options[["time"]])
  plm::pdata.frame(dataset, index = index)
}
```

Store the `pdata.frame` in a `createJaspState()` keyed on `.inputFieldNamesPD()` so the Hausman test and every future test reuses it (fixes B13 too).

**1b. Formula builder (B2, B15).** `stats::as.formula()`; enforce a single dependent variable by setting `singleVariable: true` on the `dependent` list in `VariableInput.qml`. Extend the builder to accept an optional instrument part (`| z1 + z2`) — needed by Phase 2's IV/HT/GMM work — and an optional lag specification.

**1c. Model summary table (B3).** Branch on the test type returned by `summary()`:

```r
fstat <- modelSum[["fstatistic"]]
isChisq <- grepl("Chisq", names(fstat[["statistic"]]), fixed = TRUE)
```

Emit columns: Model · R² · Adj. R² · statistic (labelled F or χ²) · df1 · df2 (empty for χ²) · p. Show the statistic value, which is currently dropped entirely. Add `$addCitation()` for Croissant & Millo (2008).

**1d. Guard `options$analysis` (B4).** Move `options <- .rewriteOptionsPD(options, analysis)` **out** of the `if (ready)` block — it is a static per-analysis constant, not data-dependent:

```r
options <- .rewriteOptionsPD(options, analysis)
ready   <- .isReadyPD(dataset, options)
if (ready) .checkErrorsPD(dataset, options)
```

Apply to all five working entry points.

**1e. Model-aware effects GUI (B6).** Replace the single shared `Common/Effects.qml` with a parameterised component:

```qml
// Common/Effects.qml
RadioButtonGroup {
    name: "effects"
    title: qsTr("Effects")
    property bool allowTwoways: true
    property bool allowNested:  false          // needs 3-index data; keep false until Phase 6
    RadioButton { value: "individual"; label: qsTr("Individual"); checked: true }
    RadioButton { value: "time";       label: qsTr("Time") }
    RadioButton { value: "twoways";    label: qsTr("Individual and Time"); visible: allowTwoways }
    RadioButton { value: "nested";     label: qsTr("Nested");               visible: allowNested  }
}
```

Set `allowTwoways: false` in `BetweenModel.qml` and `FirstDifferenceModel.qml`. Remove `Common.Effects` from `BetweenModel.qml` altogether if only `individual`/`time` remain meaningful there — the between estimator's `effect` argument selects which mean to take, so keep both.

**1f. Rename the LM-test effect group (B8).** `PoolingModel.qml`: `effects` → `lmTestEffects`, `type` → `lmTestType`. Add `Common.Effects` to `PoolingModel.qml` so the pooling model gets its own (independent) effect selector.

**1g. Dependencies (B9).** Introduce shared vectors at the top of `panelCommon.R`:

```r
.pdInputDeps <- c("dependent", "covariates", "factors", "id", "time", "idOnly")
.pdModelDeps <- c(.pdInputDeps, "effects", "estimators", "instruments", "instMethod")
```

Put `.pdModelDeps` on the fit state and on a top-level container; children then declare only their own options.

**1h. Plot (B10).** Guard the `idOnly` case by using the `pdata.frame`'s generated time index; `gettext()` the title; switch to `.data[[...]]`; decode names via `jaspBase::decodeColNames()`; wrap in `try()` + `$setError()`.

**1i. Error strings (B11).** Replace every `gettext(paste(...))` with `gettextf()`. Add a `.cleanErrorPD()` helper that strips the `Error in <call> :` prefix from `try` objects.

**1j. Coefficients table (B12).** Build a `data.frame` with typed columns; decode term names; add optional CI columns (`overtitle = gettextf("%s%% CI", 100 * options[["ciLevel"]])`) behind a new `ciLevel` `CIField` in `Common/Coefficients.qml`.

**1k. Two-ways fixed effects (B14).** When `effects == "twoways"`, call `plm::fixef(fit, effect = "individual")` and `plm::fixef(fit, effect = "time")` separately and emit two labelled row groups (`.isNewGroup`). Expose `type` (`level` / `dfirst` / `dmean`) as a DropDown.

**1l. `ercomp` theta (`common.R:287`).** Add a `theta` row (or footnote) to the Random Effects Variance Components table; for `effects == "twoways"` `theta` is a list of `id`, `time`, `total`.

**Deliverable:** all five working analyses run cleanly on balanced and unbalanced data, with `idOnly` on and off, and with every effect option the GUI still offers.

---

### Phase 2 — Complete the five stub analyses

Each gets: a real QML, a real entry point delegating to the shared builders, and a baseline test.

**2a. `betweenModel` — trivial.** Identical to `poolingModel` with `analysis = "between"`. `R/betweenModel.R`:

```r
betweenModel <- function(jaspResults, dataset, options, analysis = "between") { ... }
```
No fixed/random effect tables. Note the between estimator collapses to n (or T) observations — the observations check in `.checkErrorsPD()` should require at least 3 individuals.

**2b. `htModel` — Hausman-Taylor.** Requires an instrument/exogeneity classification in the GUI, which no current QML has. Both plm entry points need the formula `y ~ x1 + x2 + x3 | x1 + x2`, where the RHS of `|` lists the **exogenous** regressors (time-varying and time-invariant):

- `plm::pht(formula, data, model = c("ht", "am", "bms"))` — the dedicated wrapper; `model` selects the instrument set.
- `plm::plm(formula, data, model = "random", random.method = "ht", inst.method = c("bvk", "baltagi", "am", "bms"))` — the general path.

Prefer `pht()` for a first implementation (simpler, purpose-built `summary` method); note its help page marks it deprecated in favour of the `plm()` form, so confirm which to target before building the GUI.

New QML (`HTModel.qml`):
- `AssignedVariablesList` `timeVaryingExogenous`
- `AssignedVariablesList` `timeInvariantExogenous`
- `DropDown` `htMethod`: `ht` (Hausman-Taylor), `am` (Amemiya-MaCurdy), `bms` (Breusch-Mizon-Schmidt)

Backend uses the extended formula builder from 1b. Add `plm::pvar()` output to warn when a variable classed time-invariant actually varies.

**2c. `vcmModel` — variable coefficients (`plm::pvcm`).** `VCMModel.qml` already has `model` (`within`/`random`) and `effect`. Rename `model` → `vcmModel`/`effect` → `effects` for consistency with the rest of the module.
Output:
- `model = "within"`: per-individual (or per-time) coefficient table — one block per group; plus the pooling F test that `summary.pvcm` reports.
- `model = "random"`: Swamy estimator — mean coefficients table + between-group variance-covariance table.
- Optional coefficient-spread plot (caterpillar of per-individual slopes) — high value, visualises heterogeneity.
- Robust vcov via `plm::pvcovHC()`.

**2d. `pgglsModel` — general FGLS (`plm::pggls`).** `PGGLSModel.qml` has `model` (`within`/`pooling`/`fd`) and `effect`. Add `CheckBox` for the "general FGLS vs. within-FGLS" distinction. Backend: `plm::pggls(formula, data, model =, effect =)`. `summary.pggls` gives `$rsqr` (multiple R²) rather than `r.squared` — the Model Summary builder needs a small adapter. Note `pggls` requires n > T.

**2e. `gmmModel` — Arellano-Bond / Blundell-Bond (`plm::pgmm`).** The largest of the five. `GMMModel.qml` has `model` (onestep/twosteps), `effect`, `transformation` (d/ld), but no way to specify the dynamic structure. Add:
- `IntegerField` `lagsDependent` — number of lags of y on the RHS (default 1)
- `FormulaField` or two `IntegerField`s for the GMM instrument lag range (`lag.form`, e.g. `2:99`)
- `AssignedVariablesList` `gmmInstruments` (endogenous, instrumented by their own lags) and `normalInstruments` (strictly exogenous)
- `CheckBox` `collapseInstruments` (`collapse = TRUE`) — important, instrument proliferation is the classic pitfall
- `CheckBox` `robustSE` (→ `summary(fit, robust = TRUE)`, the Windmeijer correction)

Formula construction: `y ~ lag(y, 1:p) + x | lag(y, 2:99)`. Build with `plm::dynformula()` or by pasting `lag()` terms and parsing with `stats::as.formula()`.

Required output tables (these are what referees ask for, so they are not optional):
- Coefficients (with Windmeijer-corrected SEs when two-step)
- `plm::sargan(fit)` — Sargan/Hansen overidentification test
- `plm::mtest(fit, order = 1)` and `order = 2` — Arellano-Bond AR(1)/AR(2) tests
- Instrument count vs. number of individuals (footnote warning when instruments ≥ n)

**Deliverable:** all ten (nine after removing `panelDataAnalysis`) registered analyses produce output.

---

### Phase 3 — Panel structure & data diagnostics

A new **Panel Structure** table shown by default (or behind a checkbox) in every analysis. This is cheap and prevents most user errors.

New `Common/PanelStructure.qml` section with checkboxes `panelSummary`, `panelVariation`, `panelBalance`.

| Output | plm call |
|---|---|
| Panel Summary: n individuals, T periods, N observations, balanced yes/no, min/max/mean T | `plm::pdim()`, `plm::is.pbalanced()` |
| Unbalancedness measures (gamma, nu) | `plm::punbalancedness()` |
| Gaps in the time index | `plm::is.pconsecutive()` |
| Variation check: which regressors are constant within individual / within time | `plm::pvar()` |
| Linear dependence among regressors | `plm::detect.lindep()` |

The `pvar` output deserves an automatic footnote: *"X is constant within individuals and cannot be estimated by the within estimator."* This is the single most common panel-data user error and currently surfaces only as a raw plm error string.

Also add GUI options `makeBalanced` and `makeConsecutive` (→ `plm::make.pbalanced()`, `plm::make.pconsecutive()`) with an explicit footnote stating how many rows were added or dropped.

---

### Phase 4 — Robust covariance matrices

Wire the existing (renamed) `robustEstimates` checkbox.

New `Common/RobustSE.qml`:

```qml
CheckBox {
    name: "robustEstimates"; label: qsTr("Robust standard errors")
    DropDown {
        name: "vcovEstimator"; label: qsTr("Estimator")
        values: [
            { label: qsTr("Arellano (cluster-robust)"),      value: "arellano" },
            { label: qsTr("White 1"),                        value: "white1"   },
            { label: qsTr("White 2"),                        value: "white2"   },
            { label: qsTr("Beck-Katz (PCSE)"),               value: "bk"       },
            { label: qsTr("Double-clustered (Thompson)"),    value: "dc"       },
            { label: qsTr("Newey-West"),                     value: "nw"       },
            { label: qsTr("Driscoll-Kraay"),                 value: "scc"      }
        ]
    }
    DropDown { name: "vcovType";    values: ["HC0","HC1","HC2","HC3","HC4"] }
    DropDown { name: "vcovCluster"; values: [{label: qsTr("Individual"), value: "group"},
                                             {label: qsTr("Time"),       value: "time"}] }
}
```

Backend: a single `.vcovPD(fit, options)` dispatcher returning the matrix, applied via `lmtest::coeftest(fit, vcov. = V)` for the coefficients table and `plm::pwaldtest(fit, vcov = V)` for the model summary's overall test. Add a footnote naming the estimator used, and a citation for each.

Gate availability per model: `vcovSCC`/`vcovNW` need reasonably large T; `vcovBK` assumes a balanced panel — footnote rather than hide.

---

### Phase 5 — Specification & diagnostic tests

Expand `Common/AssumptionChecks.qml` from a single Hausman checkbox into a structured section. All tables live in the `assumptionChecks` container, each gated by its own checkbox, each with its own narrow `$dependOn`.

Restructure `fixedModel`/`randomModel` to always call `.assumptionCheckContainerPD()` and gate individual tables inside — this resolves the existing TODO at `R/fixedModel.R:26`.

**5a. Effects / poolability**

| Checkbox | Test | Available in |
|---|---|---|
| `fTest` | `plm::pFtest(within, pooling)` — F test for individual/time effects | Fixed |
| `lmTest` | `plm::plmtest(fit, effect =, type = honda/bp/kw/ghm)` | Pooling, Random *(GUI already exists — B7)* |
| `hausmanTest` | `plm::phtest(within, random, method = "chisq"/"aux", vcov =)` | Fixed, Random ✅ (extend with `method` + robust `vcov`) |
| `poolabilityTest` | `plm::pooltest(pooling, within_per_group)` | Pooling, Fixed |
| `wooldridgeUnobsTest` | `plm::pwtest()` — unobserved effects | Pooling |
| `bsyTest` | `plm::pbsytest(test = "j"/"re"/"ar")` | Pooling, Random |

**5b. Serial correlation**

| Checkbox | Test | Notes |
|---|---|---|
| `breuschGodfrey` | `plm::pbgtest(fit, order =)` | all |
| `durbinWatson` | `plm::pdwtest(fit)` | all |
| `baltagiLi` | `plm::pbltest(formula, data, alternative =)` | random only; takes formula + data, not a fit |
| `wooldridgeAR` | `plm::pwartest(fit)` | within |
| `wooldridgeFD` | `plm::pwfdtest(fit, h0 = "fd"/"fe")` | fd, within |
| `bhargavaDW` | `plm::pbnftest(fit, test = "bnf"/"lbi")` | handles unbalanced/gapped panels |

**5c. Cross-sectional dependence**

`plm::pcdtest(fit, test = "cd"/"lm"/"sclm"/"bcsclm"/"rho"/"absrho")` — one table, `test` as a DropDown. Pair with the Driscoll-Kraay vcov from Phase 4 in a footnote: *"Consider Driscoll-Kraay standard errors when cross-sectional dependence is present."*

**5d. Heteroskedasticity**

`lmtest::bptest()` on the plm object; footnote pointing at Phase 4's robust SEs.

**Implementation note:** most of these take the *fitted* model plus, sometimes, a *different* model. Build a `.refitPD(jaspResults, dataset, options, model = "within")` helper that returns a cached fit for any `model` string, so `pFtest`, `phtest`, and `pooltest` share one estimation each rather than re-fitting per test (generalises the Phase-1 fix for B13).

---

### Phase 6 — Advanced panel econometrics

Lower priority; each is a new analysis or a new section.

**6a. Panel unit root tests** — a standalone analysis (`panelUnitRoot`), since it operates on a single series, not a regression model.
- `plm::purtest(object, test = "levinlin"/"ips"/"madwu"/"Pm"/"invnormal"/"logit"/"hadri", exo = "none"/"intercept"/"trend", lags = "SIC"/"AIC"/"Hall"/<int>, pmax =, ips.stat =)`
- `plm::cipstest()` — Pesaran CIPS, second generation (allows cross-sectional dependence)
- `plm::phansitest()` — Simes-based, reports which individuals are nonstationary

**6b. Panel Granger causality** — `plm::pgrangertest(formula, data, test = "Ztilde"/"Zbar"/"Wbar", order =)`. Small standalone analysis.

**6c. Nested error components (B6).** Add an optional third `group` index variable to `VariableInput.qml`; when assigned, build a 3-element index and enable `effect = "nested"` with `random.method = "swar"/"walhus"/"amemiya"`. Until then, `nested` must stay hidden.

**6d. Additional estimators.** Register `pcce` (common correlated effects, `model = "mg"/"p"`) and `pmg` (mean groups / demeaned MG / CCE-MG) as new analyses — both are standard for large-T heterogeneous panels. `pldv` (panel tobit) if there is demand.

**6e. IV estimation for the standard models.** Add an `instruments` variable slot to `Common/VariableInput.qml`, enabled per analysis, feeding the `| z` part of the formula and an `inst.method` DropDown. This also unlocks `plm::aneweytest()` and `plm::piest()`.

---

### Phase 7 — Output completeness, tests, documentation

**7a. Post-estimation output**
- Random effects BLUPs table via `plm::ranef()` (currently only variance components are shown).
- `plm::within_intercept()` for within models — users routinely expect an intercept.
- Residual diagnostic plots: residuals vs fitted, Q-Q, residuals by individual.
- Fixed-effects caterpillar plot (estimate ± CI, sorted) — reuse the Phase-2c plotting code.
- Predicted-values plot overlaid on the existing line plot.
- `plm::r.squared(fit, type =, dfcor =)` as an alternative R² option.

**7b. Model comparison table.** When more than one estimator is selected, a side-by-side coefficient table (within / random / pooling / between / fd) is the single most useful addition for teaching and for reporting — plm's own vignette leads with this.

**7c. Tests.** `tests/testthat/` is empty. Add one file per analysis (`test-fixedModel.R`, …) covering:
- Balanced panel (`plm::Grunfeld`) — reference values taken from plm directly.
- Unbalanced panel (`plm::Produc` subset, or `plm::Hedonic`).
- `idOnly = TRUE` path (regression test for B1).
- Not-ready state (no variables assigned) — regression test for B4.
- Each effect × estimator combination the GUI offers.
- Plot snapshots via `jaspTools::expect_equal_plots()`.

Per `AGENTS.md`, test files are human-owned — these should be written or reviewed by the maintainer, and new snapshots must be inspected manually.

**7d. Help & documentation**
- `inst/help/*.md` for each analysis (currently empty directory).
- Fill the empty `info:` strings in `Common/VariableInput.qml`.
- `$addCitation()` on every table: Croissant & Millo (2008) for plm generally, plus the method-specific reference (Arellano-Bond 1991, Blundell-Bond 1998, Driscoll-Kraay 1998, Pesaran 2004/2007, Hausman-Taylor 1981, Swamy 1970, …).

---

## 5. Suggested Ordering

| Priority | Phase | Rationale |
|---|---|---|
| P0 | 1 | Silent wrong results (B1), crashes (B4, B5), invalid GUI states (B6) |
| P0 | 0 | Prerequisite hygiene, small |
| P1 | 2a, 2d | Between and PGGLS are near-free given the existing shared builders |
| P1 | 4 | The GUI already promises robust SEs; panel work is not publishable without them |
| P1 | 5a, 5b | Effects tests and serial-correlation tests are the standard workflow; `plmtest` GUI already exists |
| P2 | 3 | Prevents the most common user errors; cheap |
| P2 | 2b, 2c | HT and VCM need new GUI concepts |
| P2 | 7a, 7b | Output completeness |
| P3 | 2e | GMM is the largest single piece of work |
| P3 | 5c, 5d, 6 | Advanced / large-panel methods |
| Continuous | 7c, 7d | Tests and help alongside every phase |

---

## 6. Open Questions for the Maintainer

1. **`panelDataAnalysis`** — delete as a broken duplicate of `randomModel` (my recommendation), or rebuild it as a single generic "Panel Regression" analysis with a model DropDown that subsumes Fixed/Random/Pooling/Between/FD? The latter is closer to how `plm()` itself works and would shrink the module from six entry points to one, but it is a bigger GUI change.
2. **Multiple dependent variables** (B2) — was `singleVariable: false` deliberate (intending separate models per outcome), or an oversight? Affects 1b.
3. **`idOnly`** — is the intent "no time variable exists, use row order within individual" (which `index = <id>` gives) or "data are a balanced stacked panel"? Confirms the B1 fix.
4. **GMM scope** — full `pgmm` with instrument-lag control, or a simplified Arellano-Bond preset? Drives whether Phase 2e is one sprint or three.
5. **`nested` effects** — add a third group-index variable (Phase 6c), or drop the option from the GUI entirely?
