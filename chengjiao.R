options(warn = -1)
##设定工作目录
setwd( "E:/R_work/lianjia")
setInternet2() 
library(RCurl)
library(XML)
library(stringr)
library(plyr)


# default options
defaultOptions <-  curlOptions(
  httpheader = list(
    from = "",
    'user-agent'   = str_c(R.version$platform,
                           R.version$version.string,
                           sep=", ")),
  followlocation = TRUE,
  maxredirs      = 20,
  connecttimeout = 50,
  timeout        = 300,
  cookiefile     = "RCurlCookies.txt",
  cainfo = system.file("CurlSSL","cacert.pem", package = "RCurl"))
options(RCurlOptions = defaultOptions)

##baseurl <- 'https://wh.lianjia.com/chengjiao/'
loop2 <- 1
getPageUrls <- function(baseurl)
{ 
  firsturl <-baseurl
  EncodedUrl <- getURL(firsturl,.encoding="utf-8")
  parseddoc <- htmlParse(EncodedUrl,encoding="UTF-8")
  pagepath <- '//div[@class = "page-box house-lst-page-box"]'
  total_pages <-  xpathSApply(parseddoc,pagepath,xmlGetAttr,"page-data")
  total_pages <- max(as.numeric(unlist(str_extract_all(total_pages,"[[:digit:]]{1,}"))))
  print(str_c("当前区域的URLS条数为：",total_pages))
  
  if(is.infinite(total_pages)==TRUE)
  {
    print(str_c("取区域URLS失败的次数为：",loop2))
    loop2 <<- loop2 + 1
    getPageUrls(baseurl)
  }
  loop2 <<- 1
  urls_list <- list()
  if(total_pages >= 2)
  {
    addurl <- str_c('pg',seq(2,total_pages,1),'/')
    urls_list <- as.list(str_c(baseurl,addurl))
  }
  urls_list <- append(urls_list, list(firsturl), 0)
  return(urls_list)
}

##getPageUrls("3711057140337")
title_path <- "//div[@class='title']/a"
houseInfo_path <- "//div[@class='address']/div[@class='houseInfo']"
dealdate_path <- "//div[@class='address']/div[@class='dealDate']"
totalPrice_path <- "//div[@class='address']/div[@class='totalPrice']"
positionInfo_path <- "//div[@class='flood']/div[@class='positionInfo']"
unitPrice_path <- "//div[@class='flood']/div[@class='unitPrice']"
guapaijia_path <- "//div[@class='info']/div[@class='dealCycleeInfo']"
cycletime_path <- "//div[@class='info']/div[@class='dealCycleeInfo']"


##先写表头信息
header <- data.frame('小区','户型','面积(平米)','朝向','装修情况','是否有电梯','成交时间','总价(万)','楼层','单价（元/平方米）',
                     '挂牌价（万）','成交周期（天）')

##成交房把信息写入文件中
time <- str_replace_all( as.character( Sys.time()),":","-")
filename <- str_c("chenjiao_info ",time,".csv")

write.table(header, filename, sep=","
            ,append = TRUE,quote = FALSE,row.names = FALSE,col.names = FALSE)

loop <- 1
write_chengjiao <- function(pageurl)
{ 
  
  if(url.exists(pageurl) == FALSE)
  {
    print(str_c(pageurl," is not exist"))
  }
  else
  {
    print(str_c("pageurl:",pageurl))
    getpageurl <- getURL(pageurl,.encoding="utf-8")
    parsed_doc <- htmlParse(getpageurl,encoding="UTF-8") 
    title <- xpathSApply(parsed_doc,title_path,xmlValue)
    title_matrix <- str_split_fixed(title,pattern = "[:space:]",n=3)
    xiaoqu <- title_matrix[,1]
    huxian <- title_matrix[,2]
    mianji <- str_extract( title_matrix[,3],pattern = "[0-9.]{1,}")
    houseInfo <- xpathSApply(parsed_doc,houseInfo_path,xmlValue)
    houseInfo_matrix <-str_split_fixed(houseInfo,pattern = "\\|",n=3)
    chaoxiang <- houseInfo_matrix[,1]
    zhuangxiu <-trimws(houseInfo_matrix[,2])
    zhuangxiu <- str_replace(zhuangxiu,pattern = "'\\<U\\+00A0\\>'","")
    dianti <- houseInfo_matrix[,3]
    dealdate <- xpathSApply(parsed_doc,dealdate_path,xmlValue)
    totalPrice <- xpathSApply(parsed_doc,totalPrice_path,xmlValue)
    totalPrice <- str_extract(totalPrice,pattern = "[0-9.]{1,}")
    positionInfo <- xpathSApply(parsed_doc,positionInfo_path,xmlValue)
    unitPrice <- xpathSApply(parsed_doc,unitPrice_path,xmlValue)
    unitPrice <- str_extract(unitPrice,pattern = "[0-9.]{1,}")
    guapaijia <-  str_extract( xpathSApply(parsed_doc,guapaijia_path,xmlValue),'[0-9]{1,}万')
    guapaijia <- str_extract(guapaijia,pattern = "[0-9.]{1,}")
    cycletime <-  str_extract( xpathSApply(parsed_doc,cycletime_path,xmlValue),'[0-9]{1,}天')
    cycletime <- str_extract(cycletime,pattern = "[0-9.]{1,}")
    ##合并各属性信息
    chengjiao_info <- cbind(xiaoqu,huxian,mianji,chaoxiang,zhuangxiu,dianti,dealdate,totalPrice,
                            positionInfo,unitPrice,guapaijia,cycletime)
    print(str_c("写入的记录条数为：",nrow(chengjiao_info)))
    if(nrow(chengjiao_info)==0)
    { 
      print(str_c("取当前页面失败的次数为：",loop))
      loop <<- loop +1
      write_chengjiao(pageurl)
    }
    write.table(chengjiao_info,filename, sep=","
                ,append = TRUE,quote = FALSE,row.names = FALSE,col.names = FALSE)
    Sys.sleep(0)
    loop <<- 1
    
  }
}
 url1 <- "https://wh.lianjia.com/chengjiao/jiangan/"
 url2 <- "https://wh.lianjia.com/chengjiao/jianghan/"
 url3 <- "https://wh.lianjia.com/chengjiao/qiaokou/"
 url4 <- "https://wh.lianjia.com/chengjiao/dongxihu/"
 url5 <- "https://wh.lianjia.com/chengjiao/wuchang/"
 url6 <- "https://wh.lianjia.com/chengjiao/qingshan/"
 url7 <- "https://wh.lianjia.com/chengjiao/hongshan/"
 url8 <- "https://wh.lianjia.com/chengjiao/hanyang/"
 url9 <- "https://wh.lianjia.com/chengjiao/donghugaoxin/"
 url10 <-"https://wh.lianjia.com/chengjiao/jiangxia/"
 urls <-c(url1,url2,url3,url4,url5,url6,url7,url8,url9,url10)

  
 for(i in 1:10)
 {
  baseurl <-  urls[i]
  urls_list <- getPageUrls(baseurl)
  l_ply(urls_list,write_chengjiao)
}


