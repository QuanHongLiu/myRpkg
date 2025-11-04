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
