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
      attr(data[[col_name]],"labels") <- setNames(coding_map$meaning, coding_map$coding)
      return(data[[col_name]])
    } else if (var_type[[field_id]] %in%  c("Integer", "Continuous")){
      return(as.numeric(data[[col_name]]))
    } else if (var_type[[field_id]] %in% c("Date") && grepl("^\\d+$", na.omit(data[[col_name]])[1])) {
      return(as.Date(data[[col_name]], origin = "1960-01-01"))  # 纯数字 → 天数偏移
    } else if (var_type[[field_id]] %in% c("Date") && grepl("^\\d{4}-\\d{2}-\\d{2}$", na.omit(data[[col_name]])[1])) {
      return(as.Date(data[[col_name]]))  # YYYY-MM-DD → 直接转换
    } else if (var_type[[field_id]] %in% c("Date") && !grepl("^\\d+$", na.omit(data[[col_name]])[1])) {
      return(as.Date(data[[col_name]], format = "%d%b%Y"))  # 字符格式 → 按 %d%b%Y 解析, 上一个先满足！！
    } else if (var_type[[field_id]] %in%  c("Time") && grepl("^\\d+$", na.omit(data[[col_name]])[1])){
      return(as.POSIXct(data[[col_name]], origin="1960-01-01 00:00:00", tz="UTC"))
    } else if (var_type[[field_id]] %in%  c("Time") && !grepl("^\\d+$", na.omit(data[[col_name]])[1])){
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
