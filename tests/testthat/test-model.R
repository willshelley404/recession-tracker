make_train <- function(n = 400, seed = 7) {
  set.seed(seed)
  X <- as.data.frame(matrix(rnorm(n * length(PREDICTORS)), n, dimnames = list(NULL, PREDICTORS)))
  # prime_lfpr deliberately *raises* risk — the opposite of its theory sign
  lo <- -1.5 - 1.2 * X$yield_spread + 1.0 * X$u_rise + 1.5 * X$prime_lfpr
  X$rec_next12 <- rbinom(n, 1, plogis(lo))
  X$date <- seq(as.Date("1960-01-01"), by = "1 month", length.out = n)
  X
}

test_that("scaler clamps and imputes", {
  df <- data.frame(a = c(1:20, 1000, NA))
  sc <- build_scaler(df[1:20, , drop = FALSE], "a")
  z <- apply_scaler(df, sc)$a
  expect_equal(z[21], 3)
  expect_equal(z[22], 0)
})

test_that("fit respects theory signs", {
  m <- fit_glm(make_train())
  b <- as.numeric(coef(m$fit, s = "lambda.1se"))[-1]
  names(b) <- PREDICTORS
  expect_equal(unname(b["prime_lfpr"]), 0)
  expect_true(all(b[EXPECTED_SIGN[PREDICTORS] < 0] <= 0))
  expect_true(all(b[EXPECTED_SIGN[PREDICTORS] > 0] >= 0))
  expect_lt(b[["yield_spread"]], 0) # a real effect survives
})

test_that("fit is deterministic", {
  tr <- make_train()
  m1 <- fit_glm(tr)
  m2 <- fit_glm(tr)
  expect_identical(m1$fit$lambda.1se, m2$fit$lambda.1se)
  expect_identical(predict_glm(m1, tr[1:10, ]), predict_glm(m2, tr[1:10, ]))
})

test_that("fit records training metadata", {
  tr <- make_train()
  m <- fit_glm(tr)
  expect_equal(m$n_train, nrow(tr))
  expect_equal(m$base_rate, mean(tr$rec_next12))
  expect_equal(m$train_end, max(tr$date))
})

test_that("cv_folds restores RNG", {
  set.seed(1); a <- runif(1)
  set.seed(1); invisible(cv_folds(rep(0:1, 50))); b <- runif(1)
  expect_equal(a, b)
})

test_that("every fold has a positive", {
  y <- c(rep(0, 95), rep(1, 5))
  f <- cv_folds(y)
  expect_setequal(f[y == 1], 1:5)
})

test_that("logged parameters reproduce the prediction", {
  tr <- make_train()
  m <- fit_glm(tr)
  pr <- model_params(m)
  expect_named(pr, PARAM_COLS)
  x <- tr[1:20, ]
  terms <- sapply(PREDICTORS, function(p) {
    pr[[paste0("b_", p)]] * robust_z(x[[p]], pr[[paste0("med_", p)]], pr[[paste0("mad_", p)]])
  })
  expect_equal(plogis(pr[["b0"]] + rowSums(terms)), predict_glm(m, x), tolerance = 1e-10)
})
