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
        tmp_df <- quartile_cut(tmp_df, x, n)

        # 标准化 x
        if (scale_x) {tmp_df[[x]] <- scale(tmp_df[[x]])}

        # 临时结果
        tmp_results <- data.frame()

        # 连续 x
        formula <- as.formula(paste0(y,'~',x,"+", paste(covs, collapse = "+")))
        model <- do.call(model_fun, c(list(formula, data = tmp_df), model_args))
        tmp_results <- do.call(extract_fun, list(x = x, y = y, model = model, results = tmp_results, data = tmp_df))

        if (!n == 0) {
          # 分类 x
          formula <- as.formula(paste0(y,'~',x,n,"+", paste(covs, collapse = "+")))
          model <- do.call(model_fun, c(list(formula, data = tmp_df), model_args))
          tmp_results <- do.call(extract_fun, list(x = paste0(x,n), y = y, model = model, results = tmp_results, data = tmp_df))

          # P for trend
          tmp_df[[paste0(x,n)]] <- as.numeric(tmp_df[[paste0(x,n)]])
          formula <- as.formula(paste0(y,'~',x,n,"+", paste(covs, collapse = "+")))
          model <- do.call(model_fun, c(list(formula, data = tmp_df), model_args))
          tmp_results <- do.call(extract_fun, list(x = paste0(x,n), y = y, model = model, results = tmp_results, data = tmp_df))
          tmp_df[[paste0(x,n)]] <- as.factor(tmp_df[[paste0(x,n)]])
        }

        # p modification for numeric-numeric
        origin_formula <- as.formula(paste0(y, '~', x, '*', strata_var, "+", paste(covs, collapse = "+")))
        origin_model <- do.call(model_fun, c(list(origin_formula, data = data), model_args))
        crude_formula <- as.formula(paste0(y, '~', x, '+', strata_var, "+", paste(covs, collapse = "+")))
        crude_model <- do.call(model_fun, c(list(crude_formula, data = data), model_args))
        anova_result <- anova(crude_model, origin_model)
        anova_result <- as.data.frame(anova_result)
        anova_result <- standardize_tidy_names(anova_result)
        # 从 ANOVA 结果中提取 p 值
        tmp_results$p_nn <- na.omit(anova_result$p.value)[1]

        # p modification for numeric-factor
        if (!n == 0) {
          data <- quartile_cut(data, x, n)

          origin_formula <- as.formula(paste0(y, '~', paste0(x,n), '*', strata_var, "+", paste(covs, collapse = "+")))
          origin_model <- do.call(model_fun, c(list(origin_formula, data = data), model_args))
          crude_formula <- as.formula(paste0(y, '~', paste0(x,n), '+', strata_var, "+", paste(covs, collapse = "+")))
          crude_model <- do.call(model_fun, c(list(crude_formula, data = data), model_args))
          anova_result <- anova(crude_model, origin_model)
          anova_result <- as.data.frame(anova_result)
          anova_result <- standardize_tidy_names(anova_result)
          # 从 ANOVA 结果中提取 p 值
          tmp_results$p_nf <- na.omit(anova_result$p.value)[1]
        }

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
      data <- quartile_cut(data, x, n)

      # 标准化 x
      if (scale_x) {data[[x]] <- scale(data[[x]])}

      # 连续 x
      formula <- as.formula(paste0(y,'~',x,"+", paste(covs, collapse = "+")))
      model <- do.call(model_fun, c(list(formula, data = data), model_args))
      results <- do.call(extract_fun, list(x = x, y = y, model = model, results = results, data = data))

      if (!n == 0) {
        # 分类 x
        formula <- as.formula(paste0(y,'~',x,n,"+", paste(covs, collapse = "+")))
        model <- do.call(model_fun, c(list(formula, data = data), model_args))
        results <- do.call(extract_fun, list(x = paste0(x,n), y = y, model = model, results = results, data = data))

        # P for trend
        data[[paste0(x,n)]] <- as.numeric(data[[paste0(x,n)]])
        formula <- as.formula(paste0(y,'~',x,n,"+", paste(covs, collapse = "+")))
        model <- do.call(model_fun, c(list(formula, data = data), model_args))
        results <- do.call(extract_fun, list(x = paste0(x,n), y = y, model = model, results = results, data = data))
        data[[paste0(x,n)]] <- as.factor(data[[paste0(x,n)]])
      }
    }
  }
}
# 格式化
results$beta_CI_tidy <- sprintf(paste0("%.", digits, "f (%.", digits, "f, %.", digits, "f)"), results$estimate, results$conf.low, results$conf.high)
if ("p.value" %in% colnames(results)) {results <- results %>% mutate(p.value3 = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value)))}
if ("p_nn" %in% colnames(results)) {results <- results %>% mutate(p_nn3 = ifelse(p_nn < 0.001, "<0.001", sprintf("%.3f", p_nn)))}
if ("p_nf" %in% colnames(results)) {results <- results %>% mutate(p_nf3 = ifelse(p_nf < 0.001, "<0.001", sprintf("%.3f", p_nf)))}

# 整理宽表
results_data <- dplyr::select(results, any_of(c("x", "y", "beta_CI_tidy", "p.value3",'case','control','person_years', "strata_var", "n_strata", "p_nn3", "p_nf3")))
results_plot <- dplyr::select(results, any_of(c("x", "y", 'estimate', 'conf.low', 'conf.high', 'p.value3','case','control','person_years', "strata_var", "n_strata", "p_nn3", "p_nf3")))


# 无分层变量的原始宽表整理逻辑
raw_names_data <- names(results_data)
raw_names_plot <- names(results_plot)

# 重置 n
if (n == 0) {n = -1}

results_data <- as.data.frame(matrix(t(results_data), ncol = (n+2)*ncol(results_data), byrow = TRUE))
results_plot <- as.data.frame(matrix(t(results_plot), ncol = (n+2)*ncol(results_plot), byrow = TRUE))


names(results_data) <- unlist(lapply(1:(n+2), function(i) paste(raw_names_data, i, sep = "_")))
names(results_plot) <- unlist(lapply(1:(n+2), function(i) paste(raw_names_plot, i, sep = "_")))

results_data <- dplyr::select(results_data, any_of(c('x_1','y_1', "strata_var_1", "n_strata_1",'beta_CI_tidy_1','p.value3_1','beta_CI_tidy_2',
                                                     paste0("beta_CI_tidy_", 3:(n+1)), paste0("p.value3_", n+2), 'case_1', 'control_1','person_years_1', "p_nn3_1", "p_nf3_1")))
results_plot <- dplyr::select(results_plot, any_of(c('x_1','y_1', "strata_var_1", "n_strata_1",'estimate_1','conf.low_1','conf.high_1','p.value3_1','beta_CI_tidy_2',
                                                     as.vector(outer(c("estimate", "conf.low", "conf.high"), 3:(n+1), paste, sep = "_")),
                                                     paste0("p.value3_", n+2), 'case_1', 'control_1','person_years_1', "p_nn3_1", "p_nf3_1")))


# 导出
if (!is.null(file)) {
  write_xlsx(list(Sheet1 = results_data,
                  Sheet2 = results_plot,
                  Sheet3 = results),
             file = file,
             font = font)
}
