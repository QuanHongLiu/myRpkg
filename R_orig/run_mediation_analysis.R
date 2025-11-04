results_list <- list()
results_summary <- list()

strata_levels <- if (!is.null(strata_var)) levels(data[[strata_var]]) else NA

for (x in x_vars) {
  for (m in m_vars) {
    for (y in y_vars) {
      for (s in strata_levels) {
        
        if (!is.na(s)) {
          tmp_data <- data[data[[strata_var]] == s, ]
          strata_label <- paste0(strata_var, "=", s)
        } else {
          tmp_data <- data
          strata_label <- NA
        }
        
        message(Sys.time(), " -- 开始：", x, " → ", m, " → ", y,
                ifelse(is.na(strata_label), "", paste0(" [", strata_label, "]")))
        
        # # Step 0. prepare data
        # message(Sys.time(), "    Step 0. prepare data")
        # # 确保 fitM 和 fitY 使用完全相同行
        # vars_used <- unique(c(x, m, y, covariates))
        # tmp_data <- tmp_data %>%
        #   dplyr::select(all_of(vars_used)) %>%
        #   stats::na.omit()
        # if (nrow(tmp_data) == 0) {
        #   warning(paste("跳过组合：", x, m, y, "（所有数据缺失）"))
        #   next
        # }
        
        # Step 1: 中介模型
        message(Sys.time(), "    Step 1: 中介模型")
        med_formula <- as.formula(paste(m, "~", x,
                                        if (!is.null(covariates)) paste("+", paste(covariates, collapse = "+"))))
        fitM <- tryCatch({
          do.call(mediator_model_fun, c(list(formula = med_formula, data = tmp_data), mediator_model_args))
        }, error = function(e) NULL)
        
        # 针对生存结局的优化
        y_is_surv <- inherits(tmp_data[[y]], "Surv") || grepl("_surv$", y)
        if (y_is_surv) {
          outcome_model_fun <- survival::survreg
          outcome_model_args <- modifyList(list(dist = "weibull"), outcome_model_args)
        }
        
        # Step 2: 结果模型
        message(Sys.time(), "    Step 2: 结果模型")
        out_formula <- as.formula(paste(y, "~", x, "+", m,
                                        if (!is.null(covariates)) paste("+", paste(covariates, collapse = "+"))))
        fitY <- tryCatch({
          do.call(outcome_model_fun, c(list(formula = out_formula, data = tmp_data), outcome_model_args))
        }, error = function(e) NULL)
        
        if (is.null(fitM) || is.null(fitY)) {
          warning(paste("跳过组合：", x, m, y, "（模型拟合失败）"))
          next
        }
        
        # Step 3: 中介分析
        message(Sys.time(), "    Step 3: 中介分析")
        med_res <- tryCatch({
          mediation::mediate(fitM, fitY, treat = x, mediator = m, sims = sims, dropobs = TRUE)
        }, error = function(e) NULL)
        
        if (is.null(med_res)) {
          warning(paste("跳过组合：", x, m, y, "（mediate运行失败）"))
          next
        }
        
        # Step 4: 汇总
        message(Sys.time(), "    Step 4: 汇总")
        sm <- summary(med_res)
        tmp_summary <- data.frame(
          exposure = x,
          mediator = m,
          outcome = y,
          strata = strata_label,
          n = nrow(tmp_data),
          acme_est = sm$d0,                 # ACME 平均中介效应
          # acme_ci_low = sm$d0.ci[1],
          # acme_ci_high = sm$d0.ci[2],
          ade_est = sm$z0,                  # ADE 平均直接效应
          # ade_ci_low = sm$z0.ci[1],
          # ade_ci_high = sm$z0.ci[2],
          total_est = sm$tau.coef,          # TE 总效应
          # total_ci_low = sm$tau.ci[1],
          # total_ci_high = sm$tau.ci[2],
          prop_med = sm$n0,                 # 中介比例
          # prop_med_ci_low = sm$n0.ci[1],
          # prop_med_ci_high = sm$n0.ci[2],
          p_acme = sm$d0.p,                 # ACME 显著性
          # p_ade = sm$z0.p,                  # ADE 显著性
          # p_total = sm$tau.p,               # 总效应的显著性检验
          model_type = ifelse(y_is_surv, "AFT (Weibull)", "Linear")
        )
        
        results_summary <- append(results_summary, list(tmp_summary))
        key <- paste(x, m, y, strata_label, sep = "|")
        if (keep_models)
          results_list[[key]] <- list(mediate = med_res, fitM = fitM, fitY = fitY)
      }
    }
  }
}

# 合并结果
if (length(results_summary) > 0) {
  results_summary <- dplyr::bind_rows(results_summary) %>%
    dplyr::select(any_of(c(
      "exposure", "mediator", "outcome", "strata", "n",
      "acme_est", "acme_ci_low", "acme_ci_high",
      "ade_est", "ade_ci_low", "ade_ci_high",
      "total_est", "total_ci_lowm", "total_ci_high",
      "prop_med", "prop_med_ci_low", "prop_med_ci_high",
      "p_acme", "p_ade", "p_total", "model_type"
    )))
} else {
  results_summary <- data.frame()
}

# 导出结果
if (!is.null(file)) {
  myRpkg::write_xlsx(results_summary, file, font = "Times New Roman")
}
