# 提取模型结果
#' Title
#'
#' @param x
#' @param outcome
#' @param model
#' @param results
#'
#' @return
#' @export
#'
#' @examples
extract_model_results_conf <- function(x, outcome, model, results) {
  # 提取模型系数
  model_summary <- broom.mixed::tidy(model, conf.int = TRUE)
  model_summary <- cbind(outcome, model_summary)
  model_summary <- model_summary[grepl(x, model_summary$term), ]
  names(model_summary)[names(model_summary)=='term'] <- 'exposure'

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


# 提取模型结果
#' Title
#'
#' @param x
#' @param outcome
#' @param model
#' @param results
#'
#' @return
#' @export
#'
#' @examples
extract_model_results_tidy <- function(x, outcome, model, results) {
  # 提取模型系数
  model_summary <- broom.mixed::tidy(model, conf.int = TRUE)
  model_summary <- cbind(outcome, model_summary)
  model_summary <- model_summary[grepl(x, model_summary$term), ]
  names(model_summary)[names(model_summary)=='term'] <- 'exposure'

  if (is.data.frame(results)) {
    results <- bind_rows(results, model_summary)
  } else {
    results <- data.frame()
    results <- bind_rows(results, model_summary)
  }

  # 返回结果
  return(results)
}



# 统一数据框的变量类型
#' Title
#'
#' @param target_df
#' @param reference_df
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





# 定义一个通用的分位数切割函数
#' Title
#'
#' @param dataframe
#' @param var_name
#' @param n
#'
#' @return
#' @export
#'
#' @examples
quartile_cut <- function(dataframe, var_name, n) {
  # 检测 dataframe
  if (!is.data.frame(df)) {
    stop("Error: The input df must be a data frame.")
  }

  # 处理 var_name
  result <- try({
    if (is.character(var_name)) {
      var_name <- var_name
    }
  }, silent = TRUE)
  if (inherits(result, "try-error")) {
    var_name <- deparse(substitute(var_name))
  }

  # 检查 var_name 是否存在于 dataframe
  if (!exists(var_name, where = dataframe)) {
    stop(paste("Object", var_name, "not found in dataframe"))
  }

  # 检查 变量是否为数值
  dataframe[[paste0(var_name, n)]] <- cut(dataframe[[var_name]],
                                          breaks = quantile(dataframe[[var_name]],
                                                            probs = seq(0, 1, 1/n),na.rm = T),
                                          include.lowest = TRUE,
                                          labels = as.character(1:n))
  return(dataframe)
}




#' Title
#'
#' @param x
#' @param file
#' @param sheetName
#' @param row_height
#' @param auto_width
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



#' Title
#'
#' @param data
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
    filed_id <-  strsplit(col_name, "_")[[1]][2]
    if (filed_id %in% names(var_type)) {
      if (var_type[[filed_id]] %in% c("Categorical multiple", "Categorical single")) {
        data[[col_name]] <- as.factor(data[[col_name]])
        coding_map <- codings_showcase[codings_showcase$coding_id == var_coding[[filed_id]],]
        # 分类变量 label
        data[[col_name]] <- set_labels(
          data[[col_name]],
          labels = setNames(coding_map$coding, coding_map$meaning)
        )
        return(data[[col_name]])
      } else if (var_type[[filed_id]] %in%  c("Integer", "Continuous")){
        return(as.numeric(data[[col_name]]))
      } else if (var_type[[filed_id]] %in%  c("Date")){
        return(as.Date(data[[col_name]],origin = "1960-01-01"))
      } else if (var_type[[filed_id]] %in%  c("Time")){
        return(as.POSIXct(data[[col_name]], origin="1960-01-01 00:00:00", tz="UTC"))
      } else {
        return(data[[col_name]])
      }
    } else if (filed_id == "eid"){
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

  label(data) <- lapply(names(data), function(col_name) {
    filed_id <- strsplit(col_name, "_")[[1]][2]

    if (filed_id %in% names(var_labels)) {
      var_labels[[filed_id]]
    } else if (filed_id == "eid") {
      "Encoded anonymised participant ID"
    } else {
      attr(data[[col_name]],"label")
    }
  })
  return(data)
}


#' Title
#'
#' @param field_list
#' @param output_dir_prefix
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






#' Title
#'
#' @param data
#' @param field_list
#'
#' @returns
#' @export
#'
#' @examples
rename_ukb_data <- function(data, field_list) {
  sapply(names(data), function(col_name) {
    # 按下划线分割当前列名
    x <- unlist(strsplit(col_name, "_"))

    # 提取第二部分作为字段名（如果有）
    key <- if (length(x) >= 2) x[2] else x[1]

    # 查找 field_list 中对应的中文名
    if (key %in% names(field_list) && field_list[[key]] != "") {
      paste0(field_list[[key]],
             ifelse(is.na(x[3]), "", x[3]),
             ifelse(is.na(x[4]), "", x[4]))
    } else {
      col_name  # 找不到就返回原列名
    }
  })
}
