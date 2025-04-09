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



# 自动调整列宽导出xlsx
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
    x,                 # 要导出的数据框
    file,              # 文件路径
    sheetName = "Sheet1",  # 工作表名
    row_height = 18,   # 行高（默认18像素）
    auto_width = TRUE, # 是否自动调整列宽
    ...                # 其他透传给write.xlsx的参数（如startCol, borders等）
) {
  # 创建Workbook对象并写入数据
  wb <- createWorkbook()
  addWorksheet(wb, sheetName = sheetName)
  writeData(wb, sheet = sheetName, x = x, ...)  # 透传所有额外参数

  # 自动调整行高（含表头）
  setRowHeights(
    wb, sheet = sheetName,
    rows = 1:(nrow(x) + 1),
    heights = row_height
  )

  # 自动调整列宽（基于内容）
  if (auto_width) {
    setColWidths(
      wb, sheet = sheetName,
      cols = 1:ncol(x),
      widths = "auto"
    )
  }

  # 保存文件
  saveWorkbook(wb, file = file, overwrite = TRUE)
}






# process_ukb_data <- function(data){
#   # 0. 读取 var_dict 文件
#   var_dict <- read_csv(paste0(system.file(package = 'myRpkg'),"/extdata/UKB_variable_dictionary.csv"),
#                        col_types = cols(
#                          Column = col_character(),
#                          UDI = col_character(),
#                          UDI_url = col_character(),
#                          Count = col_integer(),
#                          Type = col_character(),
#                          Description = col_character(),
#                          Description_url = col_character(),
#                          Download_date = col_character()
#                        ))
#
#
#   # 1. 更改变量类型
#   coding_dir <- paste0(system.file(package = 'myRpkg'),"/extdata/codings")
#
#   var_type <- setNames(as.list(var_dict$Type), paste0("n_", gsub("[-.]", "_", var_dict$UDI)))
#   var_description <- setNames(as.list(var_dict$Description), paste0("n_", gsub("[-.]", "_", var_dict$UDI)))
#
#   data[] <- lapply(names(data), function(col_name) {
#     if (col_name %in% names(var_type)) {
#       if (var_type[[col_name]] %in% c("Categorical (multiple)", "Categorical (single)")) {
#         print(col_name)
#         coding_num <- str_extract(var_description[[col_name]], "(?<=data-coding)\\d+")    # 提取数据编码编号
#         print(coding_num)
#         coding_file <- file.path(coding_dir, paste0("data-coding", coding_num, ".tsv"))   # 拼接文件路径
#         print(coding_file)
#         coding_map <- read_tsv(coding_file, show_col_types = FALSE) %>%  # 读取数据编码文件
#           mutate(coding = as.character(coding))  # 确保类型匹配
#
#         # factor
#         data[[col_name]] <- as.factor(data[[col_name]])
#
#         # 更改名字
#         data[[col_name]] <- set_labels(
#           data[[col_name]],
#           labels = setNames(coding_map$coding, coding_map$meaning)
#         )
#
#         return(data[[col_name]])
#       } else if (var_type[[col_name]] %in%  c("Sequence", "Integer", "Continuous")){
#         return(as.numeric(data[[col_name]]))
#       } else {
#         return(data[[col_name]])
#       }
#     }
#     return(data[[col_name]]) # 如果不满足条件，返回原列
#   })
#
#   # 2. 添加变量标签（精确匹配）
#   var_labels <- setNames(as.list(var_dict$Description), paste0("n_", gsub("[-.]", "_", var_dict$UDI)))
#   label(data) <- lapply(names(data), function(x) {
#     if (x %in% names(var_labels)) var_labels[[x]] else ""
#   })
#   return(data)
# }
#







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
    filed_id <-  strsplit(col_name, "_")[[1]][2]

    if (filed_id %in% names(var_labels)) var_labels[[filed_id]] else ""
  })

  label(data) <- lapply(names(data), function(col_name) {
    filed_id <- strsplit(col_name, "_")[[1]][2]

    if (filed_id %in% names(var_labels)) {
      var_labels[[filed_id]]  # 如果存在于var_labels中
    } else if (filed_id == "eid") {   # 添加第一个else if条件
      # 处理condition1为TRUE的情况
      "Encoded anonymised participant ID"
    } else {
      ""  # 默认情况
    }
  })
  return(data)
}

