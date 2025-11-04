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
