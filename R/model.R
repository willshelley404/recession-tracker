# ─────────────────────────────────────────────────────────────────────────────
# R/model.R — robust scaling + sign-constrained Ridge logistic regression
# ─────────────────────────────────────────────────────────────────────────────

#' Median/MAD from the training rows only (MAD floored at 1e-6)
build_scaler <- function(df, cols, mad_min = 1e-6) {
  center <- vapply(cols, function(v) stats::median(df[[v]], na.rm = TRUE), numeric(1))
  scale <- vapply(cols, function(v) max(stats::mad(df[[v]], na.rm = TRUE), mad_min), numeric(1))
  list(center = center, scale = scale, cols = cols)
}

#' z = clamp((x − median) / MAD, −3, 3); NA → 0 (= the training median)
apply_scaler <- function(df, scaler) {
  for (v in scaler$cols) {
    z <- (df[[v]] - scaler$center[[v]]) / scaler$scale[[v]]
    z[is.na(z)] <- 0
    df[[v]] <- pmax(-3, pmin(3, z))
  }
  df
}

#' glmnet bounds pinning each β to its theory sign (0 allowed)
sign_limits <- function(preds) {
  s <- EXPECTED_SIGN[preds]
  s[is.na(s)] <- 0
  list(lower = unname(ifelse(s > 0, 0, -Inf)), upper = unname(ifelse(s < 0, 0, Inf)))
}

#' Fixed, class-stratified CV folds. cv.glmnet's default random folds moved
#' lambda.1se enough to change the probability between identical runs.
#' Restores the caller's RNG state.
cv_folds <- function(y, k = 5L) {
  had_seed <- exists(".Random.seed", envir = globalenv())
  if (had_seed) old <- get(".Random.seed", envir = globalenv())
  on.exit(
    if (had_seed) assign(".Random.seed", old, envir = globalenv())
    else rm(".Random.seed", envir = globalenv())
  )
  set.seed(CV_SEED)
  f <- integer(length(y))
  for (cl in unique(y)) {
    idx <- which(y == cl)
    f[idx] <- sample(rep(seq_len(k), length.out = length(idx)))
  }
  f
}

#' Fit on labelled training rows (rec_next12 + PREDICTORS [+ date])
fit_glm <- function(train) {
  scaler <- build_scaler(train, PREDICTORS)
  X <- as.matrix(apply_scaler(train, scaler)[, PREDICTORS, drop = FALSE])
  y <- train$rec_next12
  lims <- sign_limits(PREDICTORS)
  fit <- glmnet::cv.glmnet(
    X, y, family = "binomial", alpha = 0, type.measure = "deviance",
    foldid = cv_folds(y), lower.limits = lims$lower, upper.limits = lims$upper
  )
  list(
    fit = fit,
    scaler = scaler,
    base_rate = mean(y),
    n_train = nrow(train),
    train_end = if ("date" %in% names(train)) max(train$date) else as.Date(NA)
  )
}

predict_glm <- function(model, newdata) {
  X <- as.matrix(apply_scaler(newdata, model$scaler)[, PREDICTORS, drop = FALSE])
  as.numeric(stats::predict(model$fit, newx = X, s = "lambda.1se", type = "response"))
}
