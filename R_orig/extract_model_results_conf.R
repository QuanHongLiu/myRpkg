# 提取模型系数
model_summary <- summary(model)$coefficients
model_summary <- as.data.frame(model_summary) %>% tibble::rownames_to_column(var = "x")
model_summary <- cbind(y, model_summary)
model_summary <- model_summary[grepl(x, model_summary$x), ]

# 提取模型通过 bootstrap 得到的置信区间
model_confint <- confint(model)
model_confint <- as.data.frame(model_confint)
model_confint <- model_confint[grepl(x, rownames(model_confint)),]

# 合并结果
res <- cbind(model_summary, model_confint)
res <- standardize_tidy_names(res)

# 如果需要exp(β)，转换系数和置信区间; 如果 x 是level 也要; 还可以用于添加case，control，person-years
res <- add_model_info(model_summary = res,
                      model = model,
                      data = data,
                      x = x,
                      y = y)

# 合并到 results
if (is.data.frame(results)) {
  results <- bind_rows(results, res)
} else {
  results <- data.frame()
  results <- bind_rows(results, res)
}
# 返回结果
