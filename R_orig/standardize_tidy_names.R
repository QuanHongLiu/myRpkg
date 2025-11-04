# 定义匹配规则（关键词->目标列名）
name_map <- list(
  estimate   = c("estimate", "est", "coef", "coefficient"),
  std.error  = c("std.error", "se", "standard error", "std error", 'Std. Error', 'se(coef)'),
  statistic  = c("t value", "z value", "f value", "statistic", "t.value", "z.value",'z'),
  p.value    = c("p.value", "pvalue", "p_value", "Pr(>|z|)", "Pr(>|t|)", "Pr(>|Chi|)"),
  conf.low   = c("2.5 %", "conf.low", "lower", "CI_low"),
  conf.high  = c("97.5 %", "conf.high", "upper", "CI_high")
)

# 获取原始列名（保留大小写）
original_names <- colnames(df)

# 遍历每个目标列名，精确匹配并替换
for (target in names(name_map)) {
  # 检查原始列名是否在 name_map 的候选列表中（忽略大小写）
  matched_col <- original_names[tolower(original_names) %in% tolower(name_map[[target]])]
  if (length(matched_col) >= 1) {
    # 只替换第一个匹配的列（避免多列冲突）
    colnames(df)[original_names == matched_col[1]] <- target
  }
}
