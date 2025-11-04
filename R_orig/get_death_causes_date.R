# 参数检查
if (!is.list(death_codes_list)) {
  death_codes_list <- list(death_codes_list)
}
if (!is.vector(death_causes)) {
  death_causes <- c(death_causes)
}
data <- as.data.frame(data)

# 加载 icd 数据
data(icd)

# 主要死因（优先用 s_40001_1_0 覆盖 s_40001_0_0）
death_primary <- as.character(data$s_40001_0_0)
death_primary[!is.na(data$s_40001_1_0)] <- as.character(data$s_40001_1_0[!is.na(data$s_40001_1_0)])


# secondary cause of death 次要死因
death_secondary <- as.matrix(data[, grep("^s_40002_0_", names(data), value = TRUE)])
death_secondary_new <- as.matrix(data[, grep("^s_40002_1_", names(data), value = TRUE)])
# 不需要补充了
# na_cols <- matrix(NA, nrow = nrow(death_secondary_new), ncol = 5)
# death_secondary_new <- cbind(death_secondary_new, na_cols)
death_secondary[!is.na(data$s_40002_1_1), ] <- death_secondary_new[!is.na(data$s_40002_1_1), ]

for (i in seq_along(death_causes)) {
  cause <- death_causes[i]
  print(paste(Sys.time(), '---- 开始生成 death caused by', cause ,'的 first_date'))
  codes <- icd$ICD10[icd$ICD10_cls %in% death_codes_list[[i]]]  # 使用 ICD10 编码
  codes <- na.omit(codes)   # 以防万一用户传入 NA

  # primary_secondary
  primary_match <- death_primary %in% codes
  secondary_match <- matrix(death_secondary %in% codes, nrow=nrow(death_secondary), ncol=ncol(death_secondary))
  secondary_match <- apply(secondary_match, 1, any)


  # 根据 primary_secondary 参数选择策略
  target_rows <- switch(
    primary_secondary,
    "primary" = primary_match,
    "secondary" = secondary_match,
    "both" = primary_match | secondary_match
  )

  # 填充日期字段
  data[[paste0('death_', cause, '_date')]] <- as.Date(NA)
  data[[paste0('death_', cause, '_date')]][target_rows] <- data$s_40000_0_0[target_rows]

  # 写入结果
  attr(data[[paste0('death_', cause, '_date')]], "label") <- paste0("Date of death caused by ", cause)
  attr(data[[paste0('death_', cause, '_date')]], "source_field") <- "s_40000_*_*, s_40001_*_*, s_40002_*_*"
}
