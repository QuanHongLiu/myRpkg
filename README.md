### Download package

``` r
# 下载并安装 R 包
devtools::install_github("QuanHongLiu/myRpkg")

# 获取自己计算机的 token 发给作者 
my_token()

# 等待作者确认使用权限后重新安装 R 包
detach("package:myRpkg", unload = TRUE)
devtools::install_github("QuanHongLiu/myRpkg")
```
