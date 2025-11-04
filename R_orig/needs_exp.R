# 初始化结果列表
result <- list(
  exponentiate = FALSE,
  model_type = "unknown"
)

# 逻辑回归 → 计算OR
if (inherits(model, "glm") && model$family$family == "binomial") {
  result$exponentiate <- TRUE
  result$model_type <- "binomial"
  return(result)
}

# 泊松回归 → 计算IRR
if (inherits(model, "glm") && model$family$family == "poisson") {
  result$exponentiate <- TRUE
  result$model_type <- "poisson"
  return(result)
}

# Cox回归 → 计算HR
if (inherits(model, "coxph")) {
  result$exponentiate <- TRUE
  result$model_type <- "coxph"
  return(result)
}

# 多项逻辑回归 → 计算OR
if (inherits(model, "multinom")) {
  result$exponentiate <- TRUE
  result$model_type <- "multinomial logistic regression"
  return(result)
}

# 有序逻辑回归 → 计算OR
if (inherits(model, "polr")) {
  result$exponentiate <- TRUE
  result$model_type <- "ordinal logistic regression"
  return(result)
}

# 线性回归
if (inherits(model, "lm") && !inherits(model, "glm")) {
  exponentiate = FALSE
  result$model_type <- "lm"
  return(result)
}

# 其他情况
result$model_type <- class(model)

