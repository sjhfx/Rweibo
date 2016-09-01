# Rweibo
This package provides an interface to sina weibo using OAuth 2.0 and cookies.

这个R包是根据李舰开发的[Rweibo](https://r-forge.r-project.org/R/?group_id=1054)修改而成，原作者已经很久没有更新，在新的R软件环境中会报错，例如：Error in oldport != 0

[数据化分析](http://sjhfx.cc)对源代码做了一些修改，迁移到GitHub上进行维护。

## 安装

```R
# install.packages("devtools")
devtools::install_github("sjhfx/Rweibo")
```

## 使用示例

```R
library(Rweibo)
registerApp(app_name, app_key, app_secret)
roauth = createOAuth(app_name, access_name)
post_weibo = statuses.update(roauth, status = "我在使用Rweibo发微博 @数据化分析")
repost_weibo = statuses.repost(roauth, id = "4012457850732123", status = "使用Rweibo转发")
comment_weibo = comments.create(roauth, id = "4012457850732123", comment = "使用Rweibo评论")
# 搜索微博
web1 <- web.search.content(sword = "数据化分析", page = 5)
```

## 关注微信（isjhfx）
![image](https://github.com/sjhfx/rwda/raw/master/images/isjhfx.jpg)