# 找到除合并键以外的重复列
common_cols <- intersect(names(df1), names(df2))
common_cols <- setdiff(common_cols, by)

# 去掉 df1 中的重复列
df1 <- df1 %>% select(-all_of(common_cols))

# 合并
data <- df1 %>% full_join(df2, by = by)
