# 兼容：如果不是 list，则转为 list
if (!is.list(disease_10_codes_list)) {
  disease_10_codes_list <- list(disease_10_codes_list)
}
if (!is.list(disease_9_codes_list)) {
  disease_9_codes_list <- list(disease_9_codes_list)
}
if (!is.vector(disease_names)) {
  disease_names <- c(disease_names)
}

# 加载 icd 数据
data(icd)
# 防止 data.table 报错
data <- as.data.frame(data)


if (by == 'summary') {
  # icd 10 矩阵
  print(paste(Sys.time(), '---- 正在生成 icd_mat_10、date_mat_10 矩阵 '))
  icd_mat_10 <- as.matrix(data[, c(grep("^s_41270_0_", names(data), value = TRUE),
                                   grep("^s_40006_", names(data), value = TRUE))])

  date_mat_10 <- as.matrix(data[, c(grep("^s_41280_0_", names(data), value = TRUE),
                                    grep("^s_40005_", names(data), value = TRUE))])

  # icd 9 矩阵
  icd_mat_9 <- as.matrix(data[, grep("^s_41271_0_", names(data), value = TRUE)])
  date_mat_9 <- as.matrix(data[, grep("^s_41281_0_", names(data), value = TRUE)])
} else if (by == 'first') {
  # first occr  icd 10 矩阵
  first_icd_10 <- as.matrix(data[, c(grep("^s_first_icd10_0_", names(data), value = TRUE),
                                     grep("^s_40006_", names(data), value = TRUE))])
  first_date_10 <- as.matrix(data[, c(grep("^s_first_date_0_", names(data), value = TRUE),
                                      grep("^s_40005_", names(data), value = TRUE))])
}


# 每个疾病 挨个
for (i in seq_along(disease_names)) {
  x <- disease_names[i]
  print(paste(Sys.time(), '---- 开始生成', x ,'的 first_date'))
  codes_10 <- icd$ICD10[icd$ICD10_cls %in% disease_10_codes_list[[i]]]
  codes_10 <- na.omit(codes_10)   # 以防万一用户传入 NA
  codes_9  <- icd$ICD9[icd$ICD9_cls %in% disease_9_codes_list[[i]]]
  codes_9 <- na.omit(codes_9)   # 以防万一用户传入 NA

  if (by == 'summary') {
    # 筛选 ICD10
    dates_icd10 <- matrix(NA, nrow = nrow(icd_mat_10), ncol = ncol(icd_mat_10))
    dates_icd10[icd_mat_10 %in% codes_10] <- date_mat_10[icd_mat_10 %in% codes_10]

    # 筛选 ICD9
    dates_icd9 <- matrix(NA, nrow = nrow(icd_mat_9), ncol = ncol(icd_mat_9))
    dates_icd9[icd_mat_9 %in% codes_9] <- date_mat_9[icd_mat_9 %in% codes_9]
  } else if (by == 'first') {
    # 筛选 ICD10
    dates_icd10 <- matrix(NA, nrow = nrow(first_icd_10), ncol = ncol(first_icd_10))
    dates_icd10[first_icd_10 %in% codes_10] <- first_date_10[first_icd_10 %in% codes_10]

    # 筛选 ICD9
    dates_icd9 <- matrix(NA, nrow = nrow(first_icd_10), ncol = ncol(first_icd_10))
  }

  # 取最早日期
  first_date <- do.call(
    pmin,
    c(as.data.frame(dates_icd10),
      as.data.frame(dates_icd9),
      na.rm = TRUE)
  )

  # 写入结果
  data[[paste0(x, '_first_date')]] <- as.Date(first_date)
  attr(data[[paste0(x, '_first_date')]], "label") <- paste0("Date of first reported ", x)
  attr(data[[paste0(x, '_first_date')]], "source_field") <- "s_41270_*_*, s_41271_*_*, s_41280_*_*, s_41281_*_*, s_40005_*_*, s_40006_*_*"
}
