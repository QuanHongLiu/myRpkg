# 获取reference_df的列类型
ref_types <- sapply(reference_df, class)

# 遍历target_df的每一列
for (col in names(target_df)) {
  if (col %in% names(ref_types)) {
    # 根据reference_df中的类型转换
    if (ref_types[[col]] == "numeric") {
      target_df[[col]] <- as.numeric(target_df[[col]])
    } else if (ref_types[[col]] == "character") {
      target_df[[col]] <- as.character(target_df[[col]])
    } else if (ref_types[[col]] == "factor") {
      target_df[[col]] <- as.factor(target_df[[col]])
    } else if (ref_types[[col]] == "logical") {
      target_df[[col]] <- as.logical(target_df[[col]])
    } else if (ref_types[[col]] == "integer") {
      target_df[[col]] <- as.integer(target_df[[col]])
    } else if (ref_types[[col]] == "Date") {
      target_df[[col]] <- as.Date(target_df[[col]])
    }
  }
}
