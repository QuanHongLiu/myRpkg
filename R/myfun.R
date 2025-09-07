

#' 提取模型结果 confint 计算置信区间
#'
#' 提取模型的结果，包括 β、sd、confint等等。该函数使用 confint 计算置信区间，当模型为混合模型时，耗时较长
#'
#' @param x 模型自变量
#' @param y 因变量
#' @param model 模型名称；默认 model
#' @param results 收纳结果的数据框；默认 results
#'
#' @return 结果的数据框
#' @export
#'
#' @examples
extract_model_results_conf <- function(x, y, model = model, results = results) {
  # 提取模型系数
  model_summary <- broom.mixed::tidy(model, conf.int = TRUE)
  model_summary <- cbind(y, model_summary)
  model_summary <- model_summary[grepl(x, model_summary$term), ]
  names(model_summary)[names(model_summary)=='term'] <- 'x'

  # 提取模型通过 bootstrap 得到的置信区间
  model_confint <- confint(model)
  model_confint <- as.data.frame(model_confint)
  model_confint <- model_confint[grepl(x, rownames(model_confint)),]

  # 合并结果
  res <- cbind(model_summary, model_confint)

  # 合并到 results
  if (is.data.frame(results)) {
    results <- bind_rows(results, res)
  } else {
    results <- data.frame()
    results <- bind_rows(results, res)
  }
  # 返回结果
  return(results)
}



#' 提取模型结果-用 broom 默认方式计算置信区间
#'
#' 提取模型的结果，包括 β、sd、confint等等。该函数使用 broom 计算置信区间，即直接使用 β±sd 的方式，耗时较短
#'
#' @param x 模型自变量
#' @param y 因变量
#' @param model 模型名称；默认 model
#' @param results 收纳结果的数据框；默认 results
#'
#' @return 结果的数据框
#' @export
#'
#' @examples
extract_model_results_tidy <- function(x, y, model = model, results = results) {
  # 提取模型系数
  model_summary <- broom.mixed::tidy(model, conf.int = TRUE)
  model_summary <- cbind(y, model_summary)
  model_summary <- model_summary[grepl(x, model_summary$term), ]
  names(model_summary)[names(model_summary)=='term'] <- 'x'

  if (is.data.frame(results)) {
    results <- bind_rows(results, model_summary)
  } else {
    results <- data.frame()
    results <- bind_rows(results, model_summary)
  }
  # 返回结果
  return(results)
}




#' 统一两个数据框的变量类型
#'
#' 将两个数据框相同名称的变量变为相同变量类型
#'
#' @param target_df 目标数据框
#' @param reference_df 参考数据框
#'
#' @return
#' @export
#'
#' @examples
harmonise_types <- function(target_df, reference_df) {
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
      }
    }
  }
  return(target_df)
}




#' 分割变量
#'
#' 分割变量为多分位，默认赋值为1：n
#'
#' @param dataframe 数据框
#' @param var_name 需要分割的变量
#' @param n 将变量分别 n 分位
#' @param reverse 是否分割后反向赋值
#'
#' @returns
#' @export
#'
#' @examples
quartile_cut <- function(dataframe, var_name, n, reverse = FALSE) {
  # 检查输入是否为数据框
  if (!is.data.frame(dataframe)) {
    stop("The input 'dataframe' must be a data frame.")
  }

  # 获取变量名
  var_name_str <- rlang::as_name(rlang::enquo(var_name))

  # 检查变量是否存在且为数值型
  var <- dataframe[[var_name_str]]
  if (is.null(var)) {
    stop("Variable '", var_name_str, "' not found in dataframe")
  }
  if (!is.numeric(var)) {
    stop("Variable '", var_name_str, "' must be numeric")
  }

  # 检查 n 是否有效
  if (!is.numeric(n) || length(n) != 1 || n <= 0) {
    stop("n must be a positive integer")
  }

  # 计算唯一分位数
  quantiles <- unique(quantile(var, probs = seq(0, 1, 1/n), na.rm = TRUE))
  final_n <- length(quantiles) - 1

  if (final_n < n) {
    warning("Only ", final_n, " unique quantile groups created due to ties in the data")
  }

  # 创建分组变量
  new_var_name <- paste0(var_name_str, final_n)
  dataframe[[new_var_name]] <- cut(
    var,
    breaks = quantiles,
    include.lowest = TRUE,
    labels = if (reverse) as.character(final_n:1) else as.character(1:final_n)
  )

  return(dataframe)
}




#' 输出 xlsx 文件
#'
#' 输出的 xlsx 文件会自动调整列宽
#'
#' @param x 需要输出的R对象
#' @param file 输出文件的路径
#' @param sheetName 将内容写到哪个表；默认Sheet1
#' @param row_height 行高；默认18
#' @param auto_width  列宽；自动
#' @param ...
#'
#' @returns
#' @export
#'
#' @examples
write_xlsx <- function(
    x,
    file,
    sheetName = "Sheet1",
    row_height = 18,
    auto_width = TRUE,
    ...
) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheetName = sheetName)
  openxlsx::writeData(wb, sheet = sheetName, x = x, ...)
  openxlsx::setRowHeights(wb, sheet = sheetName, rows = 1:(nrow(x) + 1), heights = row_height)

  if (auto_width) {
    openxlsx::setColWidths(wb, sheet = sheetName, cols = 1:ncol(x), widths = "auto")
  }

  openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
}



#' 前处理 ukb 数据
#'
#' 根据 ukb 的 data showcase，变更变量类型、添加变量标签
#'
#' @param data 需要处理的 ukb 数据框
#'
#' @returns
#' @export
#'
#' @examples
process_ukb_data <- function(data){
  # 0. 读取 data_showcase 文件
  data_showcase <- read_csv(paste0(system.file(package = 'myRpkg'),"/extdata/Data_Dictionary_Showcase.csv"))
  # 1. 读取 codings_showcase 文件
  codings_showcase <- read_csv(paste0(system.file(package = 'myRpkg'),"/extdata/Codings_Showcase.csv"))
  # 2. 更改变量类型
  var_type <- setNames(as.list(data_showcase$ValueType), data_showcase$FieldID)
  var_coding <- setNames(as.list(data_showcase$Coding), data_showcase$FieldID)

  data[] <- lapply(names(data), function(col_name) {
    field_id <-  strsplit(col_name, "_")[[1]][2]
    if (field_id %in% names(var_type)) {
      if (var_type[[field_id]] %in% c("Categorical multiple", "Categorical single")) {
        data[[col_name]] <- as.factor(data[[col_name]])
        coding_map <- codings_showcase[codings_showcase$coding_id == var_coding[[field_id]],]
        # 分类变量 label
        attr(data[[col_name]],"labels") <- setNames(coding_map$coding, coding_map$meaning)
        return(data[[col_name]])
      } else if (var_type[[field_id]] %in%  c("Integer", "Continuous")){
        return(as.numeric(data[[col_name]]))
      } else if (var_type[[field_id]] %in%  c("Date") && grepl("^\\d+$", data[[col_name]][1])){
        return(as.Date(data[[col_name]],origin = "1960-01-01"))
      } else if (var_type[[field_id]] %in%  c("Date") && !grepl("^\\d+$", data[[col_name]][1])){
        return(as.Date(data[[col_name]], format = "%d%b%Y"))
      } else if (var_type[[field_id]] %in%  c("Time") && grepl("^\\d+$", data[[col_name]][1])){
        return(as.POSIXct(data[[col_name]], origin="1960-01-01 00:00:00", tz="UTC"))
      } else if (var_type[[field_id]] %in%  c("Time") && !grepl("^\\d+$", data[[col_name]][1])){
        return(as.POSIXct(data[[col_name]], format = "%d%b%Y %H:%M:%S"))
      } else {
        return(data[[col_name]])
      }
    } else if (field_id %in% "eid"){
      return(as.numeric(data[[col_name]]))
    }
    return(data[[col_name]]) # 如果不满足条件，返回原列
  })

  # 2. 添加变量标签（精确匹配）
  var_labels <- setNames(
    as.list(paste0(data_showcase$Field,
                   ifelse(is.na(data_showcase$Units), "",
                          paste0(", ", data_showcase$Units)))),
    data_showcase$FieldID
  )

  data[] <- lapply(names(data), function(col_name) {
    field_id <- strsplit(col_name, "_")[[1]][2]

    if (field_id %in% names(var_labels)) {
      attr(data[[col_name]],"label") <- var_labels[[field_id]]
      return(data[[col_name]])
    } else if (field_id %in% "eid") {
      attr(data[[col_name]],"label") <- "Encoded anonymised participant ID"
      return(data[[col_name]])
    } else {
      attr(data[[col_name]],"label") <- attr(data[[col_name]],"label")
      return(data[[col_name]])
    }
  })
  return(data)
}




#' 生成提取 ukb 数据的 sas 代码
#'
#' 生成提取 ukb 数据的 sas 代码。需要的 ukb 数据的 field id list，然后生成 sas 代码
#'
#' @param field_list 所需的 ukb 变量的 list
#' @param output_dir_prefix 生成 sas 脚本路径前缀
#'
#' @returns
#' @export
#'
#' @examples
format_sas_code <- function(field_list,output_dir_prefix){
  # 提取用户需要的 filed 的详细信息
  Dictionary_Showcase <- read_csv(paste0(system.file(package = 'myRpkg'),"/extdata/Data_Dictionary_Showcase.csv"))
  Dictionary_Showcase <- Dictionary_Showcase[Dictionary_Showcase$FieldID %in% field_list, c("FieldID","Field","Field_zh","Notes_zh","ValueType","Units","Stability","Instances","Array")]
  write_xlsx(x = Dictionary_Showcase, file = paste0(output_dir_prefix,"_showcase.xlsx"))


  # 读取 UKB_variable_dictionary 文件
  data_showcase <- read_csv(paste0(system.file(package = 'myRpkg'),"/extdata/UKB_variable_dictionary.csv"))

  # 排除掉数据框里根本没有的变量
  data_showcase <- data_showcase[!is.na(data_showcase$Start_pos),]

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

  # 生成变量类型
  data_showcase$code <- paste0("@",data_showcase$Start_pos," ",data_showcase$Variable_name," ",data_showcase$Variable_Type)
  ukb_20201126 <- data_showcase$code[data_showcase$Download_date == "2020-11-26"]
  ukb_20201222 <- data_showcase$code[data_showcase$Download_date == "2020-12-22"]
  ukb_20211013 <- data_showcase$code[data_showcase$Download_date == "2021-10-13"]
  ukb_20220705 <- data_showcase$code[data_showcase$Download_date == "2022-07-05"]
  ukb_20250412 <- data_showcase$code[data_showcase$Download_date == "2025-04-12"]

  # 打印 meta 信息
  # 生成提取数据的 SAS 代码
  print("正在生成提取数据的 SAS 代码")
  # 重复的变量
  data_duplicated <- data_showcase[,c("UDI","Download_date","Count")]
  data_duplicated <- data_duplicated[!data_duplicated$UDI == "eid",]
  print("重复的变量包括如下：")
  data_duplicated <- data_duplicated[duplicated(data_duplicated$UDI) | duplicated(data_duplicated$UDI, fromLast = TRUE),]
  print(data_duplicated[order(data_duplicated$UDI), ])

  # 定义模板
  sas_template <- "
  filename fsjeeno 'E:/rawdata/UKB_Data/UKB_data_20201126/ukb44656.sd2';
  data raw_sjeeno;
    infile fsjeeno RECFM=V LRECL=137496;
    input {ukb_20201126}
  ;
  run;


  filename fsrxlso 'E:/rawdata/UKB_Data/UKB_data_20201222/ukb44921.sd2';
  data raw_srxlso;
    infile fsrxlso RECFM=V LRECL=15741;
    input {ukb_20201222}
  ;
  run;

  filename fjujmir 'E:/rawdata/UKB_Data/UKB_data_20211013/ukb48833.sd2';
  data raw_jujmir;
    infile fjujmir RECFM=V LRECL=19790;
    input {ukb_20211013}
  ;
  run;

  filename fexucrt 'E:/rawdata/UKB_Data/UKB_data_20220705/ukb52673.sd2';
  data raw_exucrt;
    infile fexucrt RECFM=V LRECL=34662;
    input {ukb_20220705}
  ;
  run;

  filename fdxzlqh 'E:/rawdata/UKB_Data/UKB_data_20250412/metabolism.sd2';
  data raw_dxzlqh;
    infile fdxzlqh RECFM=V LRECL=137496;
    input {ukb_20250412}
  ;
  run;

  /* 先对每个数据集按n_eid排序 */
  proc sort data=raw_sjeeno; by n_eid; run;
  proc sort data=raw_srxlso; by n_eid; run;
  proc sort data=raw_jujmir; by n_eid; run;
  proc sort data=raw_exucrt; by n_eid; run;
  proc sort data=raw_dxzlqh; by n_eid; run;

  /* 合并数据集 */
  data merged_data;
    merge raw_sjeeno(in=a)
          raw_srxlso(in=b)
          raw_jujmir(in=c)
          raw_exucrt(in=d)
          raw_dxzlqh(in=e);
    by n_eid;
    /* 保留所有n_eid */
    if a or b or c or d or e;
  run;


  /* 导出为 CSV */
  PROC EXPORT DATA=merged_data  /* 要导出的数据集 */
      OUTFILE='E:/rawdata/UKB_Data/{prefix}_data.csv'  /* 输出路径 */
      DBMS=CSV REPLACE;  /* 指定格式为 CSV，REPLACE 表示覆盖已有文件 */
  RUN;"


  # 插入到模板中
  final_code <- glue(sas_template,
                     ukb_20201126 = paste(ukb_20201126, collapse = "\n  "),
                     ukb_20201222 = paste(ukb_20201222, collapse = "\n  "),
                     ukb_20211013 = paste(ukb_20211013, collapse = "\n  "),
                     ukb_20220705 = paste(ukb_20220705, collapse = "\n  "),
                     ukb_20250412 = paste(ukb_20250412, collapse = "\n  "),
                     prefix = sub(".*/", "", output_dir_prefix))

  # 输出为SAS文件
  writeLines(final_code, paste0(output_dir_prefix,".sas"))
}





#' rename_ukb_data <- function(data, field_list) {
#'   sapply(names(data), function(col_name) {
#'     # 按下划线分割当前列名
#'     x <- unlist(strsplit(col_name, "_"))
#'
#'     # 提取第二部分作为字段名（如果有）
#'     key <- if (length(x) >= 2) x[2] else x[1]
#'
#'     # 查找 field_list 中对应的中文名
#'     if (key %in% names(field_list) && field_list[[key]] != "") {
#'       paste0(field_list[[key]],
#'              ifelse(is.na(x[3]), "", x[3]),
#'              ifelse(is.na(x[4]), "", x[4]))
#'     } else {
#'       col_name  # 找不到就返回原列名
#'     }
#'   })
#' }






#' 合并两个数据框
#'
#' 定义合并函数，自动去除前一个df中与新df重复的变量
#'
#' @param df1 第一个数据框
#' @param df2 第二个数据框
#' @param by 通过哪个变量合并
#'
#' @returns
#' @export
#'
#' @examples
smart_merge <- function(df1, df2, by) {
  # 找到除合并键以外的重复列
  common_cols <- intersect(names(df1), names(df2))
  common_cols <- setdiff(common_cols, by)

  # 去掉 df1 中的重复列
  df1 <- df1 %>% select(-all_of(common_cols))

  # 合并
  full_join(df1, df2, by = by)
}






#' 提取 ukb 数据
#'
#' 从 ukb 数据库中提取所需 fields 的 csv 文件和提取的 fields 的 xlsx 文件
#'
#' @param field_list 所需变量的 field id list
#' @param ukb_data_dir ukb 原始数据的路径；默认 "~/rawdata/"
#' @param output_dir_prefix 原 csv 数据和 field_list xlsx 的路径及文件前缀
#'
#' @returns
#' @export
#'
#' @examples
extract_ukb_data <- function(field_list,ukb_data_dir="~/rawdata/",output_dir_prefix=""){
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
}





#' 生成 field id 的 list
#'
#' 根据用户的英文变量，寻找 field id，并生成 R 脚本文件
#'
#' @param input_vec 用户输入的变量项量
#' @param ukb_data_dir  ukb 原始数据的路径；默认 "~/rawdata/"
#' @param output_dir_prefix 前处理数据的 source 的 R 脚本文件路径前缀
#'
#' @returns res$field_list 和 res$code 是 source 的代码 和 res$add_code 新添加处理的变量
#' @export
#'
#' @examples
generate_fieldids_code <- function(input_vec, ukb_data_dir="~/rawdata/", output_dir_prefix="") {
  # 用户可不传入 eid
  input_vec <- c("eid",input_vec)

  # 获取所有非.R文件路径（递归搜索）
  dir_path <- paste0(ukb_data_dir,"/UKB_Data/vars/")
  file_list <- dir(dir_path, pattern = "[^.R]$")
  var_names <- sapply(strsplit(file_list, "\\."), function(x) x[3])
  file_var <- setNames(as.list(file_list), var_names)

  # 变量和 fieldid的列表
  var_field_list <- list()

  for (element in input_vec) {
    vars <- var_names[grepl(element, var_names)]   # 0或多个
    if (length(vars) == 0) {
      var_field_list[element] <- list(NULL)
      next  # 继续下一个 element
    }

    for (var in vars) {
      var_field_list[[var]] <- readLines(paste0(dir_path,file_var[var]))
    }
  }

  # 初始化结果
  field_list <- c()
  output_str <- ""
  add_str <- ""
  for (var in names(var_field_list)) {
    val <- var_field_list[[var]]
    if (is.null(val)) {
      field_list <- c(field_list, var)
    } else {
      field_list <- c(field_list, val)
      # 新增需要处理的code
      add_str <- paste0(add_str,readLines(paste0(dir_path,file_var[[var]],".R"), n = 1), "\n")
      add_str <- paste0(add_str,"source(","\"",dir_path,file_var[[var]],".R","\"",")", "\n")

      # 判断是否保存
      if (!missing(output_dir_prefix) & nchar(output_dir_prefix) > 0){
        # 并且有这么一个代码文件
        if (file.exists(paste0(output_dir_prefix,".R"))) {
          output_str <- readChar(paste0(output_dir_prefix,".R"), file.info(paste0(output_dir_prefix,".R"))$size)
        }
        # 将var对应的文件第一行输出到一个字符里保存
        output_str <- paste0(output_str,readLines(paste0(dir_path,file_var[[var]],".R"), n = 1), "\n")
        output_str <- paste0(output_str,"source(","\"",dir_path,file_var[[var]],".R","\"",")")
        writeLines(output_str, paste0(output_dir_prefix,".R"))   # 写入
        writeLines(unique(readLines(paste0(output_dir_prefix,".R"))), paste0(output_dir_prefix,".R"))   # 读取去重后输出
        output_str <- readChar(paste0(output_dir_prefix,".R"), file.info(paste0(output_dir_prefix,".R"))$size)  # 输出
      } else {
        output_str <- paste0(output_str,readLines(paste0(dir_path,file_var[[var]],".R"), n = 1), "\n")
        output_str <- paste0(output_str,"source(","\"",dir_path,file_var[[var]],".R","\"",")", "\n")
      }
    }
  }
  # 导出结果
  return(list(field_list = field_list, code = output_str, add_code = add_str))
}





#' 前处理 ukb 数据的 pipeline
#'
#' 提取数据、保存数据、前处理数据、改变变量名等等。相应会输出 field_list xlsx、csv 原始文件、Rdata 文件
#'
#' @param input_vec 需要提取的变量 list
#' @param ukb_data_dir  ukb 原始数据的路径；默认 "~/rawdata/"
#' @param output_dir_prefix 输出内容的路径及前缀
#'
#' @returns
#' @export
#'
#' @examples
preprocess_ukb_pipline <- function(input_vec,ukb_data_dir="~/rawdata/",output_dir_prefix="") {
  # 用户提供field list，找到filed——list和代码
  print(search())
  print("step1 generate_fieldids_code")
  print(Sys.time())
  res <- generate_fieldids_code(input_vec = input_vec,
                                ukb_data_dir = ukb_data_dir,
                                output_dir_prefix = output_dir_prefix)

  # 利用代码提取到环境中（提取后是否保存rawdata）
  print("step2 extract_ukb_data")
  print(Sys.time())
  all <- extract_ukb_data(res$field_list,
                          ukb_data_dir = ukb_data_dir,
                          output_dir_prefix = output_dir_prefix)

  # 处理数据
  print("step3 process_ukb_data")
  print(Sys.time())
  all <- process_ukb_data(data = all)


  # 如果需要保存则保存
  if (!missing(output_dir_prefix) & nchar(output_dir_prefix) > 0){
    # 并且有这么一个代码文件
    if (file.exists(paste0(output_dir_prefix,".Rdata"))) {
      # 加载这个文件
      all_new <- all
      load(paste0(output_dir_prefix,".Rdata"))
      # 找到除合并键以外的重复列
      common_cols <- intersect(names(all_new), names(all))
      common_cols <- setdiff(common_cols, "n_eid")
      # 去掉 原始 all 中的重复列
      all <- all %>% select(-all_of(common_cols))
      # 合并
      all <- full_join(all, all_new, by = "n_eid")
      # 前处理
      print("step4 pasrse")
      print(Sys.time())
      eval(parse(text = gsub("\\.R\"\\)", ".R\", local = TRUE)", res$add_code)))
      # 保存
      save(all,file = paste0(output_dir_prefix,".Rdata"))
    } else {
      # 不存在这个文件就创建一个
      print("step4 pasrse")
      print(Sys.time())
      eval(parse(text = gsub("\\.R\"\\)", ".R\", local = TRUE)", res$code)))
      # 保存
      save(all,file = paste0(output_dir_prefix,".Rdata"))
    }
  } else {
    # 返回 数据框
    print("step4 pasrse")
    print(Sys.time())
    eval(parse(text = gsub("\\.R\"\\)", ".R\", local = TRUE)", res$code)))
    return(all)
  }
  return(all)
}






# x 数值向量（需处理的数据列）
# 离群值定义方法："quantile"（分位数）、"fixed"（固定阈值）或 "iqr"（三倍IQR）

# 处理动作："cap"（封顶，默认）或 "na"（替换为NA）

# lower 下限值（分位数法时为下分位数，固定阈值法时为下限值，IQR法时不使用）
# upper 上限值（分位数法时为上分位数，固定阈值法时为上限值，IQR法时不使用）
# k IQR法的倍数（默认3）
# na.rm 是否忽略缺失值（默认TRUE）


#' 处理离群值
#'
#' 提供三种寻找离群值的方法，以及两种处理离群值的方法
#'
#' @param x 需要处理的变量
#' @param method 处理离群值的方法
#' @param action 离群值是要如何处理
#' @param lower 分位数处理离群值时的下分位，或是固定阈值时的下阈值
#' @param upper 分位数处理离群值时的上分位，或是固定阈值时的上阈值
#' @param k iqr 处理离群值时的 k 倍 iqr
#' @param na.rm 是否处理 na；默认 TRUE
#'
#' @returns
#' @export
#'
#' @examples
handle_outliers <- function(x,
                            method = c("quantile", "fixed", "iqr"),
                            action = c("cap", "na"),
                            lower = 0.025,
                            upper = 0.975,
                            k = 3,
                            na.rm = TRUE) {

  # 参数验证
  method <- match.arg(method)
  action <- match.arg(action)
  if (!is.numeric(x)) stop("输入x必须是数值向量")
  if (method == "iqr" && k <= 0) stop("k必须为正数")

  # 计算离群值边界
  bounds <- switch(method,
                   "quantile" = {
                     c(quantile(x, probs = lower, na.rm = na.rm),
                       quantile(x, probs = upper, na.rm = na.rm))
                   },
                   "fixed" = {
                     c(lower, upper)
                   },
                   "iqr" = {
                     q <- quantile(x, probs = c(0.25, 0.75), na.rm = na.rm)
                     iqr <- q[2] - q[1]
                     c(q[1] - k * iqr, q[2] + k * iqr)
                   }
  )

  # 执行处理
  if (action == "cap") {
    x[x < bounds[1] & !is.na(x)] <- bounds[1]
    x[x > bounds[2] & !is.na(x)] <- bounds[2]
  } else {
    x[x < bounds[1] | x > bounds[2]] <- NA
  }

  return(x)
}
