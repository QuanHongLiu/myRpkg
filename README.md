### 安装私有 R 包（通过 GitHub PAT）

#### **步骤 1：获取 GitHub 个人访问令牌（PAT）**

1.  登录 GitHub → 点击右上角头像 → **Settings** → **Developer settings** → **Personal access tokens** → **Tokens (classic)**。\
2.  点击 **Generate new token** → 选择 **repo** 权限（勾选所有子权限）。\
3.  生成后**复制 Token**（只会显示一次，务必保存）。

#### **步骤 2：在 R 中配置 PAT**

``` r
devtools::install_github(
  repo = "QuanHongLiu/myRpkg",  # 替换为你的私有仓库路径
  auth_token = "你的GitHub_PAT"    # 替换为你的 Token
)
```
