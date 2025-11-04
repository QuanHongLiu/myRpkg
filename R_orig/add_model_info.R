
# 小工具：提取前缀
get_prefix <- function(var) {
  parts <- unlist(strsplit(var, "_"))  # 使用 unlist 将列表转换为向量

  # 如果第一个部分是 "death"，则取下一个部分
  if (parts[1] == "death") {
    # 确保有下一个部分可用
    if (length(parts) > 1) {
      return(parts[2])
    } else {
      # 如果没有下一个部分，则返回空字符串或NA（根据需求选择）
      return(NA)
    }
  } else {
    # 否则返回第一个部分
    return(parts[1])
  }
}
exp_model_type <- needs_exp(model)

# ---- 情况 1: x 分类 + y 分类 ----
if (exp_model_type$exponentiate & x %in% names(model$xlevels)) {
  print('add_model_info  情况 1: x 分类 + y 分类--------------')
  # 添加 reference 行
  df_ref <- data.frame(
    x = paste0(x,'.ref'), y = y,
    estimate = 1, conf.low = 1, conf.high = 1
  )

  model_summary <- model_summary %>%
    mutate(
      estimate = exp(estimate),
      conf.low = exp(conf.low),
      conf.high = exp(conf.high)
    ) %>%
    bind_rows(df_ref, .)

  # 针对 Cox 回归
  if (exp_model_type$model_type == 'coxph') {
    var_prefix <- get_prefix(y)
    event_var <- paste0(var_prefix, "_event2")
    time_var  <- paste0(var_prefix, "_time")

    if (event_var %in% names(data)) {
      tab <- as.data.frame.matrix(table(data[[x]], data[[event_var]]))
      model_summary$case    <- tab[,2]
      model_summary$control <- tab[,1]
    }
    if (time_var %in% names(data)) {
      model_summary$person_years <- as.data.frame(
        tapply(data[[time_var]], data[[x]], sum, na.rm = TRUE)
      )[,1]
    }

    event_var_death <- paste0('death_', var_prefix, "_event2")
    time_var_death  <- paste0('death_', var_prefix, "_time")

    if (event_var_death %in% names(data)) {
      tab <- as.data.frame.matrix(table(data[[x]], data[[event_var_death]]))
      model_summary$case    <- tab[,2]
      model_summary$control <- tab[,1]
    }
    if (time_var_death %in% names(data)) {
      model_summary$person_years <- as.data.frame(
        tapply(data[[time_var_death]], data[[x]], sum, na.rm = TRUE)
      )[,1]
    }
  }

  # 针对 Logistic 回归
  else if (exp_model_type$model_type == 'binomial') {
    tab <- as.data.frame.matrix(table(data[[x]], data[[y]]))
    model_summary$case    <- tab[,2]
    model_summary$control <- tab[,1]
  }
} else if (!exp_model_type$exponentiate & x %in% names(model$xlevels)) { # ---- 情况 2: x 分类 + y 连续 ----
  print('add_model_info 情况 2: x 分类 + y 连续 --------------')
  df_ref <- data.frame(
    x = paste0(x,'.ref'), y = y,
    estimate = 0, conf.low = 0, conf.high = 0
  )
  model_summary <- bind_rows(df_ref, model_summary)
} else if (exp_model_type$exponentiate & !(x %in% names(model$xlevels))) {   # ---- 情况 3: x 连续 + y 分类 ----
  print('add_model_info 情况 3: x 连续 + y 分类 --------------')
  var_prefix <- get_prefix(y)

  model_summary <- model_summary %>%
    mutate(
      estimate = exp(estimate),
      conf.low = exp(conf.low),
      conf.high = exp(conf.high)
    )

  # 针对 Cox 回归
  if (exp_model_type$model_type == 'coxph') {
    event_var <- paste0(var_prefix, "_event2")
    time_var  <- paste0(var_prefix, "_time")

    if (event_var %in% names(data)) {
      tab <- as.data.frame(table(data[[event_var]]))
      model_summary$case    <- tab[2,2]   # y==1
      model_summary$control <- tab[1,2]   # y==0
    }
    if (time_var %in% names(data)) {
      model_summary$person_years <- sum(data[[time_var]], na.rm = TRUE)
    }

    event_var_death <- paste0('death_', var_prefix, "_event2")
    time_var_death  <- paste0('death_', var_prefix, "_time")

    if (event_var_death %in% names(data)) {
      tab <- as.data.frame(table(data[[event_var_death]]))
      model_summary$case    <- tab[2,2]   # y==1
      model_summary$control <- tab[1,2]   # y==0
    }
    if (time_var_death %in% names(data)) {
      model_summary$person_years <- sum(data[[time_var_death]], na.rm = TRUE)
    }

  }

  # 针对 Logistic 回归
  else if (exp_model_type$model_type == 'binomial') {
    tab <- as.data.frame(table(data[[y]]))
    model_summary$case    <- tab[2,2]
    model_summary$control <- tab[1,2]
  }
}

