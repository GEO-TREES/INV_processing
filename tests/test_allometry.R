# Tests for taper correction, AGB and height allometry, and parameter
# functions in ./func.R
# Run from the repository root: testthat::test_file("./tests/test_allometry.R")

library(testthat)
suppressPackageStartupMessages(library(dplyr))

# Repository root, whether run from the root or from ./tests/
root <- if (file.exists("func.R")) "." else ".."

source(file.path(root, "func.R"))

test_that("taperB() gives the Cushman et al. (2014) taper parameter", {
  expect_equal(taperB(50, 3, "cushman2014"),
    exp(-2.0205 - 0.5053 * log(50) + 0.3748 * log(3)))
  expect_equal(taperB(50, 3, "cushman2014", sp_term = 0.4865),
    exp(-2.0205 + 0.4865 - 0.5053 * log(50) + 0.3748 * log(3)))
})

test_that("taperB() gives the Cushman et al. (2021) taper parameter, at least 0", {
  expect_equal(taperB(50, 3, "cushman2021", wd = 0.6),
    0.151 - 0.025 * log(50) - 0.02 * log(3) - 0.021 * log(0.6))
  expect_equal(taperB(300, 10, "cushman2021", wd = 0.6), 0)
  expect_error(taperB(50, 3, "none"), "Unknown taper method")
})

test_that("taperDiam() replaces diameters with taper-corrected diameters", {
  stem <- data.frame(record_id = 1:3, diam_cm = c(50, 20, NA))
  stem_height <- data.frame(record_id = c(1, 2, 3),
    diam_cm_taper = c(55, NA, NA))
  out <- taperDiam(stem, stem_height)
  expect_equal(out$diam_cm, c(55, 20, NA))
  expect_equal(out$diam_cm_meas, c(50, 20, NA))
  # Without taper correction, diameters are unchanged
  out <- taperDiam(stem, stem_height[, "record_id", drop = FALSE])
  expect_equal(out$diam_cm, stem$diam_cm)
  expect_equal(out$diam_cm_meas, stem$diam_cm)
})

test_that("agbAllomMean() gives the mean of the Goodman et al. (2013) palm allometry", {
  D <- c(10, 25)
  expect_equal(agbAllomMean(agb_allometries$goodman2013, D = D),
    exp(-3.3488 + 2.7483 * log(D) + 0.5876^2 / 2) / 1000)
})

test_that("agbCustom() builds an allometry in Mg from an equation in param.yaml", {
  D <- c(10, 40); H <- c(12, 30); WD <- c(0.5, 0.7)
  # AGB in kg, log-normal error: the formula gives the median
  x <- list(formula = "a * (WD * D^2 * H)^b", parameters = list(a = 0.0673, b = 0.976),
    sigma = 0.357, link = "log", units = "kg", reference = "Chave et al. (2014) Eq. 4")
  allom <- agbCustom(x)
  expect_setequal(allom$variables, c("D", "H", "WD"))
  expect_equal(allom$link, "log")
  expect_equal(agbAllomMean(allom, D = D, H = H, WD = WD),
    0.0673 * (WD * D^2 * H)^0.976 * exp(0.357^2 / 2) / 1000)
  # The same in Mg
  x_Mg <- modifyList(x, list(parameters = list(a = 0.0673 / 1000, b = 0.976), units = "Mg"))
  expect_equal(agbAllomMean(agbCustom(x_Mg), D = D, H = H, WD = WD),
    agbAllomMean(allom, D = D, H = H, WD = WD))
  # Normal error: the formula gives the mean, and sigma is converted to Mg
  x_id <- list(formula = "a * D^b", parameters = list(a = 0.1, b = 2.5), sigma = 50,
    link = "identity", units = "kg")
  allom_id <- agbCustom(x_id)
  expect_equal(agbAllomMean(allom_id, D = D), 0.1 * D^2.5 / 1000)
  expect_equal(allom_id$parameters$sigma, 0.05)
  expect_equal(allom_id$variables, "D")
  # Errors
  expect_error(agbCustom(modifyList(x, list(units = NULL))), "units")
  expect_error(agbCustom(modifyList(x, list(formula = "a * dbh^b"))), "dbh")
  expect_error(agbCustom(modifyList(x, list(sigma = NULL))), "formula")
})

test_that("agbCustom() takes coefficient uncertainty from draws or a covariance matrix", {
  D <- c(10, 40)
  x <- list(formula = "exp(la) * D^b", parameters = list(la = -2, b = 2.5), 
    sigma = 0.3, link = "log", units = "kg")
  # One set of values
  expect_equal(nrow(agbCustom(x)$parameters), 1)
  # Covariance matrix: draws centred on the estimates, with their covariance
  x_v <- modifyList(x, list(vcov = list(la = list(la = 0.04, b = -0.008), 
    b = list(la = -0.008, b = 0.0025))))
  seed <- if (exists(".Random.seed")) .Random.seed else NULL
  a <- agbCustom(x_v)
  expect_identical(if (exists(".Random.seed")) .Random.seed else NULL, seed)
  expect_equal(dim(a$parameters), c(1000, 3))
  expect_equal(colMeans(a$parameters[, c("la", "b")]), c(la = -2, b = 2.5), tolerance = 0.01)
  expect_equal(cov(a$parameters$la, a$parameters$b), -0.008, tolerance = 0.1)
  expect_true(all(a$parameters$sigma == 0.3))
  expect_identical(a$parameters, agbCustom(x_v)$parameters)
  expect_error(agbCustom(modifyList(x, list(vcov = list(la = list(la = 0.04))))), "vcov")
  # Draws from a file, with and without a sigma column
  f <- tempfile(fileext = ".csv")
  d <- data.frame(la = c(-2.1, -2, -1.9), b = c(2.55, 2.5, 2.45))
  write.csv(d, f, row.names = FALSE)
  x_d <- list(formula = "exp(la) * D^b", draws = f, sigma = 0.3, link = "log", units = "kg")
  a <- agbCustom(x_d)
  expect_equal(a$parameters$la, d$la)
  expect_equal(agbAllomMean(a, D = D),
    rowMeans(sapply(1:3, function(i) exp(d$la[i]) * D^d$b[i])) * exp(0.3^2 / 2) / 1000)
  write.csv(cbind(d, sigma = c(0.2, 0.3, 0.4)), f, row.names = FALSE)
  expect_equal(agbCustom(modifyList(x_d, list(sigma = NULL)))$parameters$sigma, c(0.2, 0.3, 0.4))
  # The draws run in BIOMASS::AGBmonteCarlo()
  mc <- BIOMASS::AGBmonteCarlo(D = c(15, 60, 30), WD = rep(0.6, 3), errWD = rep(0.05, 3),
    Dpropag = "chave2004", n = 100, 
    fitted_allom = agbCustom(x_v)[c("formula", "parameters", "link")])
  expect_true(all(is.finite(mc$AGB_simu)))
})

test_that("agbAllometryParam() follows the height parameters", {
  eq <- list(formula = "a * D^b", parameters = list(a = 0.1, b = 2.5), sigma = 0.3, 
    link = "log", units = "kg")
  # Default: Chave et al. (2014), stem allometries not used
  a <- agbAllometryParam(list(height_method = "regional"))
  expect_equal(a[c("method", "default", "by_stem")], 
    list(method = "regional", default = "chave2014", by_stem = FALSE))
  expect_equal(names(a$allometries), names(agb_allometries))
  # Custom allometry for the site
  a <- agbAllometryParam(list(height_method = "regional", agb_method = "custom", agb_custom = eq))
  expect_equal(a$default, "custom")
  expect_true("custom" %in% names(a$allometries))
  expect_error(agbAllometryParam(list(agb_method = "custom")), "agb_custom")
  expect_error(agbAllometryParam(list(agb_method = "local")), "agb_method")
  # `agb_custom` is not used unless the method is "custom", as `height_custom`
  a <- agbAllometryParam(list(agb_method = "regional", agb_custom = eq))
  expect_equal(a$default, "chave2014")
  expect_false("custom" %in% names(a$allometries))
  # Allometries for stems, defined in param.yaml
  a <- agbAllometryParam(list(height_method = "regional", stem_allometry = TRUE, 
    stem_allometry_custom = list(palms_local = eq)))
  expect_true(a$by_stem)
  expect_true(all(c("goodman2013", "palms_local") %in% names(a$allometries)))
  expect_error(agbAllometryParam(list(stem_allometry_custom = list(goodman2013 = eq))), "in use")
  expect_error(agbAllometryParam(list(stem_allometry = "yes")), "stem_allometry")
  # Allometries which use height need a height-diameter model
  eq_h <- modifyList(eq, list(formula = "a * (D^2 * H)^b"))
  expect_error(agbAllometryParam(list(height_method = "regional", agb_method = "custom", 
    agb_custom = eq_h)), "height")
  expect_equal(agbAllometryParam(list(height_method = "custom", agb_method = "custom", 
    agb_custom = eq_h))$default, "custom")
})

test_that("agbAllomMean() averages over rows of parameters", {
  allom <- agb_allometries$goodman2013
  allom$parameters <- rbind(allom$parameters, transform(allom$parameters, b = b + 0.1))
  D <- c(10, 25)
  p <- allom$parameters
  expect_equal(agbAllomMean(allom, D = D),
    (exp(p$a[1] + p$b[1] * log(D) + p$sigma[1]^2 / 2) +
      exp(p$a[2] + p$b[2] * log(D) + p$sigma[2]^2 / 2)) / 2)
})

# Fits models with brms, which takes a few minutes, so it is only run if the
# environment variable INV_TEST_BRMS is set
test_that("agbAllomBrms() converts brms models to allometries with posterior draws", {
  skip_if_not(nzchar(Sys.getenv("INV_TEST_BRMS")), "INV_TEST_BRMS not set")
  skip_if_not_installed("brms")
  set.seed(1)
  n <- 150
  dat <- data.frame(dbh_cm = exp(runif(n, log(10), log(100))))
  dat$height_m <- 40 * (1 - exp(-0.03 * dat$dbh_cm)) * exp(rnorm(n, 0, 0.1))
  dat$agb_kg <- exp(-2.5 + 2.1 * log(dat$dbh_cm) + 0.8 * log(dat$height_m) + rnorm(n, 0, 0.3))
  new <- data.frame(dbh_cm = c(15, 60), height_m = c(14, 33))
  vars <- list(agb_kg = "AGB", dbh_cm = "D", height_m = "H")

  # Linear model of log(AGB)
  fit <- brms::brm(log(agb_kg) ~ log(dbh_cm) + log(height_m), data = dat,
    chains = 2, iter = 1000, refresh = 0, seed = 1, silent = 2)
  allom <- agbAllomBrms(fit, variables = vars, units = "kg", n_draw = 1000)
  expect_equal(allom$link, "log")
  expect_setequal(allom$variables, c("D", "H"))
  expect_equal(nrow(allom$parameters), 1000)
  # Mean of the posterior predictive distribution of AGB in kg
  mu <- brms::posterior_linpred(fit, newdata = new)
  sigma <- as.data.frame(brms::as_draws_df(fit))$sigma
  expect_equal(agbAllomMean(allom, D = new$dbh_cm, H = new$height_m, n_max = 1000),
    colMeans(exp(mu + sigma^2 / 2)) / 1000)

  # Non-linear model with a lognormal family, diameter only
  fit_nl <- brms::brm(brms::bf(agb_kg ~ a + b * log(dbh_cm), a + b ~ 1, nl = TRUE),
    data = dat, family = brms::lognormal(),
    prior = c(brms::prior(normal(0, 5), nlpar = "a"), brms::prior(normal(2, 2), nlpar = "b")),
    chains = 2, iter = 1000, refresh = 0, seed = 1, silent = 2)
  allom_nl <- agbAllomBrms(fit_nl, variables = vars, units = "kg")
  expect_equal(allom_nl$link, "log")
  expect_equal(allom_nl$variables, "D")
  expect_equal(agbAllomMean(allom_nl, D = new$dbh_cm, n_max = 1000),
    colMeans(brms::posterior_epred(fit_nl, newdata = new)) / 1000)

  # The allometry runs in BIOMASS::AGBmonteCarlo()
  mc <- BIOMASS::AGBmonteCarlo(D = c(15, 60, 30), WD = rep(0.6, 3), errWD = rep(0.05, 3),
    Dpropag = "chave2004", n = 100,
    fitted_allom = allom_nl[c("formula", "parameters", "link")])
  expect_equal(dim(mc$AGB_simu), c(3, 100))
  expect_true(all(is.finite(mc$AGB_simu)))

  # Models which can't be converted
  expect_error(agbAllomBrms(fit, variables = list(dbh_cm = "D"), units = "kg"), "response")
  expect_error(agbAllomBrms(fit, variables = vars, units = "g"), "units")
})

test_that("heightCustom() builds a height-diameter model with BIOMASS::customHD()", {
  skip_if_not(exists("customHD", envir = asNamespace("BIOMASS")),
    "BIOMASS without customHD()")
  mod <- heightCustom(list(formula = "a * D^b / (k + D^b)",
    parameters = list(a = 57.1, b = 0.73, k = 21.79), sigma = 0.181,
    link = "log", reference = "Test"))
  D <- c(10, 50)
  median_h <- 57.1 * D^0.73 / (21.79 + D^0.73)
  expect_equal(BIOMASS::retrieveH(D = D, model = mod)$H,
    median_h * exp(0.181^2 / 2))
})

test_that("heightCustom() takes parameter uncertainty from draws or a covariance matrix", {
  skip_if_not("param_sets" %in% names(BIOMASS::customHD(~ a * D, c(a = 1), sigma = 0.1)),
    "BIOMASS without sets of parameter values in customHD()")
  x <- list(formula = "a * D^b / (k + D^b)", parameters = list(a = 57.1, b = 0.73, k = 21.79), 
    sigma = 0.181, link = "log", reference = "Martinez-Cano et al. (2019)")
  D <- c(10, 50, 100)
  # One set of values
  m1 <- heightCustom(x)
  expect_equal(nrow(m1$param_sets), 1)
  # Covariance matrix
  v <- list(a = list(a = 9, b = 0, k = 0), b = list(a = 0, b = 0.0004, k = 0), 
    k = list(a = 0, b = 0, k = 1))
  m2 <- heightCustom(modifyList(x, list(vcov = v)))
  expect_equal(nrow(m2$param_sets), 1000)
  expect_equal(unname(m2$parameters["a"]), 57.1, tolerance = 0.01)
  expect_equal(sd(m2$param_sets$a), 3, tolerance = 0.1)
  expect_equal(BIOMASS::retrieveH(D, model = m2)$H, BIOMASS::retrieveH(D, model = m1)$H, 
    tolerance = 0.02)
  # Draws from a file
  f <- tempfile(fileext = ".csv")
  write.csv(data.frame(a = c(55, 57.1, 59), b = 0.73, k = 21.79), f, row.names = FALSE)
  m3 <- heightCustom(list(formula = x$formula, draws = f, sigma = 0.181))
  expect_equal(m3$param_sets$a, c(55, 57.1, 59))
  # The uncertainty in the parameters widens the stand total
  set.seed(1)
  Dt <- rep(c(20, 40, 80), 100)
  mc <- function(m) BIOMASS::AGBmonteCarlo(D = Dt, WD = rep(0.6, 300), errWD = rep(0, 300), 
    HDmodel = m, Dpropag = 0, n = 500)$sdAGB
  expect_gt(mc(m2), 1.2 * mc(m1))
  expect_error(heightCustom(list(formula = "a * D")), "formula")
})

# Fits a model with brms, see above
test_that("heightCustom() takes a height-diameter model fitted with brms", {
  skip_if_not(nzchar(Sys.getenv("INV_TEST_BRMS")), "INV_TEST_BRMS not set")
  skip_if_not_installed("brms")
  set.seed(1)
  dat <- data.frame(dbh_cm = exp(runif(200, log(5), log(100))))
  dat$height_m <- exp(1 + 0.55 * log(dat$dbh_cm) + rnorm(200, 0, 0.15))
  fit <- brms::brm(log(height_m) ~ log(dbh_cm), data = dat, chains = 2, iter = 1000, 
    refresh = 0, seed = 1, silent = 2)
  f <- tempfile(fileext = ".rds")
  saveRDS(fit, f)
  m <- heightCustom(list(model = f, variables = list(height_m = "H", dbh_cm = "D"), 
    reference = "Test model"))
  expect_s3_class(m, "customHD")
  expect_equal(m$link, "log")
  expect_equal(nrow(m$param_sets), 1000)
  new <- data.frame(dbh_cm = c(10, 50))
  mu <- brms::posterior_linpred(fit, newdata = new)
  sigma <- as.data.frame(brms::as_draws_df(fit))$sigma
  expect_equal(BIOMASS::retrieveH(new$dbh_cm, model = m)$H, colMeans(exp(mu + sigma^2 / 2)))
  expect_error(heightCustom(list(model = f, variables = list(dbh_cm = "D"))), "response")
})

test_that("heightPredict() predicts height in groups of stems", {
  set.seed(1)
  D_fit <- runif(200, 5, 100)
  H_fit <- exp(0.9 + 0.6 * log(D_fit) + rnorm(200, 0, 0.1))
  mod <- BIOMASS::modelHD(D = D_fit, H = H_fit, method = "log2",
    bayesian = FALSE, useCache = FALSE)
  # Repeated and missing diameters, in more than one group
  D <- c(10, 50, NA, 10, 75.5, 50)
  expect_equal(heightPredict(D, mod, chunk = 2),
    BIOMASS::retrieveH(D = D, model = mod)$H)
  expect_equal(heightPredict(c(NA_real_, NA_real_), mod), c(NA_real_, NA_real_))
  expect_equal(heightPredict(numeric(0), mod), numeric(0))
})

test_that("paramString() collapses parameters with several or named values", {
  expect_equal(paramString("regional"), "regional")
  expect_equal(paramString(10), 10)
  expect_equal(paramString(c("strangler", "liana")), "strangler, liana")
  expect_equal(paramString(list(a = 0.57, b = 1.34)), "a = 0.57; b = 1.34")
  expect_equal(paramString(list(formula = "a * D^b",
    parameters = list(a = 0.57, b = 1.34))),
    "formula = a * D^b; parameters.a = 0.57; parameters.b = 1.34")
})

test_that("cornerErrorSD() estimates the error of GNSS points from the polygon fit", {
  set.seed(1)
  # 30 plots of 100 x 100 m, one GNSS point per corner, with an error of SD 2 m.
  # The fitted corners are the true corners, as from a fit without error
  sd_true <- 2
  rel <- data.frame(x_rel = c(0, 0, 100, 100), y_rel = c(0, 100, 100, 0))
  cc <- do.call(rbind, lapply(1:30, function(i) cbind(plot_ID = paste0("P", i), rel,
    x_proj = 5e5 + 1000 * i + rel$x_rel, y_proj = 9e6 + rel$y_rel)))
  pt <- data.frame(plot_id = cc$plot_ID, x_rel_m = cc$x_rel, y_rel_m = cc$y_rel,
    rover_easting_utm_m = cc$x_proj + rnorm(120, 0, sd_true),
    rover_northing_utm_m = cc$y_proj + rnorm(120, 0, sd_true), corner = TRUE)
  out <- cornerErrorSD(pt, cc, "procrustes")
  expect_equal(nrow(out$plots), 30)
  expect_true(all(out$plots$df == 5))
  # Residuals are around the true corners here, so the pooled sum of squares 
  # has all 8 coordinates per plot: rescale to compare with the true SD
  expect_equal(out$sd * sqrt(5 / 8), sd_true, tolerance = 0.1)
  expect_true(all(cornerErrorSD(pt, cc, "affine")$plots$df == 2))
  # "exact": no degrees of freedom with one point per corner
  ex <- cornerErrorSD(pt, cc, "exact")
  expect_true(all(ex$plots$df == 0))
  expect_true(is.na(ex$sd))
  # Outliers are not used
  out2 <- cornerErrorSD(pt, cc, "procrustes", outliers = data.frame(plot_ID = pt$plot_id[1],
    x_proj = pt$rover_easting_utm_m[1], y_proj = pt$rover_northing_utm_m[1]))
  expect_equal(out2$plots$n_point[1], 3)
})

test_that("quadAreaSimu() gives the area of each quadrat in each simulation", {
  sq <- function(id, x0, s) data.frame(subplot_ID = id, x_proj = x0 + c(0, 0, s, s), y_proj = c(0, s, s, 0))
  simu <- list(rbind(sq("a", 0, 50), sq("b", 50, 50)), rbind(sq("a", 0, 40), sq("b", 50, 100)))
  a <- quadAreaSimu(simu)
  expect_equal(dim(a), c(2, 2))
  expect_equal(rownames(a), c("a", "b"))
  expect_equal(unname(a), matrix(c(0.25, 0.25, 0.16, 1), nrow = 2))
})
