# 用户不输入"eid"也可以
field_list <- c("eid",field_list)

# 提取用户需要的 filed 的详细信息
Dictionary_Showcase <- read_csv(paste0(system.file(package = 'myRpkg'),"/extdata/Data_Dictionary_Showcase.csv"))
Dictionary_Showcase <- Dictionary_Showcase[Dictionary_Showcase$FieldID %in% field_list, c("FieldID","Field","Field_zh","Notes_zh","ValueType","Units","Stability","Instances","Array")]

# 读取 UKB_variable_dictionary 文件
data_showcase <- read_csv(paste0(system.file(package = 'myRpkg'),"/extdata/UKB_variable_dictionary.csv"))
data_showcase <- data_showcase[!is.na(data_showcase$Start_pos),]


# 提取每个数据的 data showcase
ukb_20201126 <- data_showcase[data_showcase$Download_date == "2020-11-26", ]
ukb_20201222 <- data_showcase[data_showcase$Download_date == "2020-12-22", ]
ukb_20211013 <- data_showcase[data_showcase$Download_date == "2021-10-13", ]
ukb_20220705 <- data_showcase[data_showcase$Download_date == "2022-07-05", ]
ukb_20250412 <- data_showcase[data_showcase$Download_date == "2025-04-12", ]


# 把所需的挑出来
data_showcase <- data_showcase[sapply(strsplit(data_showcase$UDI, "-"), function(x) x[1]) %in% field_list,]

print(paste0("请求提取 ",length(field_list),"个变量"))
print(paste0("共有 ",sum(field_list %in% sapply(strsplit(data_showcase$UDI, "-"), function(x) x[1]))," 个变量在数据库中"))
missing_vars <- field_list[!field_list %in% sapply(strsplit(data_showcase$UDI, "-"), function(x) x[1])]
if (length(missing_vars) > 0) {
  cat(paste0("数据库不包含变量：", paste(missing_vars, collapse = ", "), "\n"))
} else {
  cat("所有变量均存在。\n")
}

# 重复的变量
data_duplicated <- data_showcase[,c("UDI","Download_date","Count")]
data_duplicated <- data_duplicated[!data_duplicated$UDI == "eid",]
print("重复的变量包括如下：")
data_duplicated <- data_duplicated[duplicated(data_duplicated$UDI) | duplicated(data_duplicated$UDI, fromLast = TRUE),]
print(data_duplicated[order(data_duplicated$UDI), ])
print("正在提取变量...")

# 提取每个项目的数据
laf1 <- laf_open_fwf(
  filename = paste0(ukb_data_dir,"/UKB_Data/UKB_data_20201126/ukb44656.sd2"),
  column_widths = ukb_20201126$Length,
  column_types = rep("string", length(ukb_20201126$Length)),
  column_names = ukb_20201126$Variable_name
)
df1 <- laf1[, ukb_20201126$Variable_name[sapply(strsplit(ukb_20201126$UDI, "-"),function(x) x[1]) %in% field_list]]

laf2 <- laf_open_fwf(
  filename = paste0(ukb_data_dir,"/UKB_Data/UKB_data_20201222/ukb44921.sd2"),
  column_widths = ukb_20201222$Length,
  column_types = rep("string", length(ukb_20201222$Length)),
  column_names = ukb_20201222$Variable_name
)
df2 <- laf2[, ukb_20201222$Variable_name[sapply(strsplit(ukb_20201222$UDI, "-"),function(x) x[1]) %in% field_list]]

laf3 <- laf_open_fwf(
  filename = paste0(ukb_data_dir,"/UKB_Data/UKB_data_20211013/ukb48833.sd2"),
  column_widths = ukb_20211013$Length,
  column_types = rep("string", length(ukb_20211013$Length)),
  column_names = ukb_20211013$Variable_name
)
df3 <- laf3[, ukb_20211013$Variable_name[sapply(strsplit(ukb_20211013$UDI, "-"),function(x) x[1]) %in% field_list]]

laf4 <- laf_open_fwf(
  filename = paste0(ukb_data_dir,"/UKB_Data/UKB_data_20220705/ukb52673.sd2"),
  column_widths = ukb_20220705$Length,
  column_types = rep("string", length(ukb_20220705$Length)),
  column_names = ukb_20220705$Variable_name
)
df4 <- laf4[, ukb_20220705$Variable_name[sapply(strsplit(ukb_20220705$UDI, "-"),function(x) x[1]) %in% field_list]]

laf5 <- laf_open_fwf(
  filename = paste0(ukb_data_dir,"/UKB_Data/UKB_data_20250412/metabolism.sd2"),
  column_widths = ukb_20250412$Length,
  column_types = rep("string", length(ukb_20250412$Length)),
  column_names = ukb_20250412$Variable_name
)
df5 <- laf5[, ukb_20250412$Variable_name[sapply(strsplit(ukb_20250412$UDI, "-"),function(x) x[1]) %in% field_list]]

# 合并数据
df_list <- list(df1, df2, df3, df4, df5)

# 示例合并，保留 df5 中的重复列
merged_df <- df1 %>%
  smart_merge(df2, by = "n_eid") %>%
  smart_merge(df3, by = "n_eid") %>%
  smart_merge(df4, by = "n_eid") %>%
  smart_merge(df5, by = "n_eid")

# 如果 output_dir_prefix 为 NA 则不输出，只加载数据
if (!missing(output_dir_prefix) & nchar(output_dir_prefix) > 0) {
  if (file.exists(paste0(output_dir_prefix,".csv"))) {
    # 合并（完全合并）去重（去掉前面的变量）
    library(data.table)
    data_orig <- fread(paste0(output_dir_prefix,".csv"), colClasses = "character")
    merged_df <- data_orig %>% smart_merge(merged_df, by = "n_eid")
    # 合并xlsx
    library(readxl)
    showcase_orig <- read_excel(paste0(output_dir_prefix, "_showcase.xlsx"))
    Dictionary_Showcase <- bind_rows(showcase_orig, Dictionary_Showcase) %>%
      distinct(FieldID, .keep_all = TRUE) %>%  # 按 FieldID 去重，保留所有列
      arrange(FieldID)
    write_csv(merged_df,paste0(output_dir_prefix,".csv"))
    write_xlsx(x = Dictionary_Showcase, file = paste0(output_dir_prefix,"_showcase.xlsx"))
    # 输出
    merged_df[merged_df == ""] <- NA
    return(merged_df)
  } else {
    write_csv(merged_df,paste0(output_dir_prefix,".csv"))
    write_xlsx(x = Dictionary_Showcase, file = paste0(output_dir_prefix,"_showcase.xlsx"))
    merged_df[merged_df == ""] <- NA
    return(merged_df)
  }
} else {
  merged_df[merged_df == ""] <- NA
  return(merged_df)
}
