# 如果有分层变量
if (!is.null(strata_var)) {
  strata_levels <- levels(data[[strata_var]])

  for (x in x_vars) {
    for (y in y_vars) {
      for (index in strata_levels) {
        # 判断是否所有 x 都同时纳入模型
        if (all_x_in == TRUE) {
          covs <- c(covariates, x_vars[x_vars != x])
        } else {
          covs <- covariates
        }

        # 过程显示
        print(paste0(Sys.time(),' -- 开始 自变量：', x,' 因变量：', y, ' 分层：', strata_var, ' = ', index))

        # 分层数据
        tmp_df <- data[data[[strata_var]] == index, ]

        # 确保自变量是因子类型
        if (!is.factor(tmp_df[[x]])) {
          tmp_df[[x]] <- as.factor(tmp_df[[x]])
        }

        # 临时结果
        tmp_results <- data.frame()

        # 运行回归模型
        formula <- as.formula(paste0(y, '~', x, "+", paste(covs, collapse = "+")))
        model <- do.call(model_fun, c(list(formula, data = tmp_df), model_args))
        tmp_results <- do.call(extract_fun, list(x = x, y = y, model = model, results = tmp_results, data = tmp_df))

        # P for trend
        data[[x]] <- as.numeric(data[[x]])
        formula <- as.formula(paste0(y, '~', x, "+", paste(covs, collapse = "+")))
        model <- do.call(model_fun, c(list(formula, data = data), model_args))
        tmp_results <- do.call(extract_fun, list(x = x, y = y, model = model, results = tmp_results, data = data))
        data[[x]] <- as.factor(data[[x]])

        # p modification for factor
        origin_formula <- as.formula(paste0(y, '~', x, '*', strata_var, "+", paste(covs, collapse = "+")))
        origin_model <- do.call(model_fun, c(list(origin_formula, data = data), model_args))
        crude_formula <- as.formula(paste0(y, '~', x, '+', strata_var, "+", paste(covs, collapse = "+")))
        crude_model <- do.call(model_fun, c(list(crude_formula, data = data), model_args))
        anova_result <- anova(crude_model, origin_model)
        anova_result <- as.data.frame(anova_result)
        anova_result <- standardize_tidy_names(anova_result)
        # 从 ANOVA 结果中提取 p 值
        tmp_results$p_ff <- na.omit(anova_result$p.value)[1]

        # 添加分层信息
        labels <- attr(data[[strata_var]], "labels")
        label <- names(labels)[labels == index]
        tmp_results$strata_var <- paste0(index, ' - ',label)
        tmp_results$n_strata <- nrow(tmp_df)

        # 合并结果
        results <- dplyr::bind_rows(results, tmp_results)
      }
    }
  }
} else {
  # 无分层变量的原始逻辑
  for (x in x_vars) {
    for (y in y_vars) {
      # 判断是否所有 x 都同时纳入模型
      if (all_x_in == TRUE) {
        covs <- c(covariates, x_vars[x_vars != x])
      } else {
        covs <- covariates
      }

      # 过程显示
      print(paste0(Sys.time(),' -- 开始 自变量：', x,' 因变量：',y))

      # 确保自变量是因子类型
      if (!is.factor(data[[x]])) {
        data[[x]] <- as.factor(data[[x]])
      }

      # 运行回归模型
      formula <- as.formula(paste0(y, '~', x, "+", paste(covs, collapse = "+")))
      model <- do.call(model_fun, c(list(formula, data = data), model_args))
      results <- do.call(extract_fun, list(x = x, y = y, model = model, results = results, data = data))

      # P for trend
      data[[x]] <- as.numeric(data[[x]])
      formula <- as.formula(paste0(y, '~', x, "+", paste(covs, collapse = "+")))
      model <- do.call(model_fun, c(list(formula, data = data), model_args))
      results <- do.call(extract_fun, list(x = x, y = y, model = model, results = results, data = data))
      data[[x]] <- as.factor(data[[x]])
    }
  }
}

# 格式化结果
results$beta_CI_tidy <- sprintf(paste0("%.", digits, "f (%.", digits, "f, %.", digits, "f)"), results$estimate, results$conf.low, results$conf.high)
if ("p.value" %in% colnames(results)) {results <- results %>% mutate(p.value3 = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value)))}
if ("p_ff" %in% colnames(results)) {results <- results %>% mutate(p_ff3 = ifelse(p_ff < 0.001, "<0.001", sprintf("%.3f", p_ff)))}

# 整理宽表
results_data <- dplyr::select(results, any_of(c("x", "y", "strata_var", "n_strata", "beta_CI_tidy", "p.value3", 'case', 'control', 'person_years', "p_ff3")))
results_plot <- dplyr::select(results, any_of(c("x", "y", "strata_var", "n_strata", 'estimate', 'conf.low', 'conf.high', 'p.value3', 'case', 'control', 'person_years', "p_ff3")))


# 导出
if (!is.null(file)) {
  write_xlsx(list(Sheet1 = results_data,
                  Sheet2 = results_plot,
                  Sheet3 = results),
             file = file,
             font = font)
}
