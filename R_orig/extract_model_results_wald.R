# 提取模型系数
model_summary <- summary(model)$coefficients
model_summary <- as.data.frame(model_summary) %>% tibble::rownames_to_column(var = "x")
model_summary <- cbind(y, model_summary)
model_summary <- model_summary[grepl(x, model_summary$x), ]
model_summary <- standardize_tidy_names(model_summary)

# 提取模型通过 β ± 1.96 × SE 得到的置信区间
model_summary$conf.low <- model_summary$estimate - 1.96 * model_summary$std.error
model_summary$conf.high <- model_summary$estimate + 1.96 * model_summary$std.error

# 如果需要exp(β)，转换系数和置信区间; 如果 x 是level 也要; 还可以用于添加case，control，person-years
model_summary <- add_model_info(model_summary = model_summary,
                                model = model,
                                data = data,
                                x = x,
                                y = y)

# 合并到 results
if (is.data.frame(results)) {
  results <- bind_rows(results, model_summary)
} else {
  results <- data.frame()
  results <- bind_rows(results, model_summary)
}
# 返回结果
