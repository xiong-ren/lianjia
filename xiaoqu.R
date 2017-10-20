options(warn = -1)
##设定工作目录
setwd( "E:/R_work/lianjia")
library(RCurl)
library(XML)
library(stringr)
library(plyr)

# default options
defaultOptions <-  curlOptions(
  httpheader = list(
    from = "renxiong0307@qq.com",
    'user-agent'   = str_c(R.version$platform,
                           R.version$version.string,
                           sep=", ")),
  followlocation = TRUE,
  maxredirs      = 10,
  connecttimeout = 10,
  timeout        = 300,
  cookiefile     = "RCurlCookies.txt",
  cainfo = system.file("CurlSSL","cacert.pem", package = "RCurl"))
options(RCurlOptions = defaultOptions)
# Options 
#options(RCurlOptions = list())



url <- "https://wh.lianjia.com/sitemap/"
##url <- "https://sz.lianjia.com/sitemap/"

##指定网页编码为UTF-8
EncodedUrl <- getURL(url,.encoding="utf-8")
#选择UTF-8进行网页的解析
parsed_doc <- htmlParse(EncodedUrl,encoding="UTF-8") 
##构建武汉小区的所在区域的XPATH路径
xpath <- "//div[@class='div_con'][position()=3]/ul/li/a"
##函数功能为提供各区域的名称及URL地址
get_region_xiaoqu <- function(x)
{ 
  ##提取的URL没有前缀https要补全
  urls   <- paste('https:',xmlGetAttr(x,'href'),sep='')
  region <- xmlValue(x)
  y <- c(region,urls)
  return(y)
}

##小区编码
get_data_housecode <- function(x)
{
  data_housecode <- xmlGetAttr(node = x,name= 'data-housecode')
  return (data_housecode)
}

regions <- t(xpathSApply(parsed_doc,xpath,get_region_xiaoqu))


##构造提取各小区基本信息的的XPATH路径
housecode_path <- "//div[@class='content']/div[@class ='leftContent']/ul[@class='listContent']
/li"

##小区名称
xiaoqu_path <- "//div[@class='content']/div[@class ='leftContent']/ul[@class='listContent']
/li/div[@class='info']/div[@class='title']/a"

##网签数据
chengjiao_path <- "//div[@class='content']/div[@class ='leftContent']/ul[@class='listContent']
/li/div[@class='info']/div[@class='houseInfo']/a[position()=1]"

##出租数据
zufang_path <- "//div[@class='content']/div[@class ='leftContent']/ul[@class='listContent']
/li/div[@class='info']/div[@class='houseInfo']/a[last()]"

##区域
district_path <- "//div[@class='content']/div[@class ='leftContent']/ul[@class='listContent']
/li/div[@class='info']/div[@class='positionInfo']/a[position()=1]"

##区域范围
bizcircle_path <- "//div[@class='content']/div[@class ='leftContent']/ul[@class='listContent']
/li/div[@class='info']/div[@class='positionInfo']/a[last()]"

##建成年代
fin_year_path<- "//div[@class='content']/div[@class ='leftContent']/ul[@class='listContent']
/li/div[@class='info']/div[@class='positionInfo']"

##单价
price_path <- "//div[@class='content']/div[@class ='leftContent']/ul[@class='listContent']
/li/div[@class='xiaoquListItemRight']/div[@class='xiaoquListItemPrice']/div[@class='totalPrice']/span"

##单价描述
price_desc_path <- "//div[@class='content']/div[@class ='leftContent']/ul[@class='listContent']
/li/div[@class='xiaoquListItemRight']/div[@class='xiaoquListItemPrice']
/div[@class='priceDesc']"

##在售情况
sellCount_path <- "//div[@class='content']/div[@class ='leftContent']/ul[@class='listContent']
/li/div[@class='xiaoquListItemRight']/div[@class='xiaoquListItemSellCount']/a/span"

getPageUrls <- function(url)
{ 
  EncodedUrl <- getURL(url,.encoding="utf-8")
  baseurl <- htmlParse(EncodedUrl,encoding="UTF-8")
  pagepath <- '//div[@class = "page-box house-lst-page-box"]'
  total_pages <-  xpathSApply(baseurl,pagepath,xmlGetAttr,"page-data")
  total_pages <- max(as.numeric(unlist(str_extract_all(total_pages,"[[:digit:]]{1,}"))))
  print(total_pages)
  addurl <- str_c('pg',seq(2,total_pages,1),'/')
  urls_list <- as.list(str_c(url,addurl))
  urls_list <- append(urls_list, list(url), 0)
  return(urls_list)
}



##先写表头信息
header <- data.frame('房屋编码','小区名称','网签','出租','区域','商圈',
                     '建成年代','单价','在售房源')

##把小区信息写入文件中
time <- str_replace_all( as.character( Sys.time()),":","-")
filename <- str_c("xiaoqu_info ",time,".csv")

write.table(header, filename, sep=","
            ,append = TRUE,quote = FALSE,row.names = FALSE,col.names = FALSE)

write_xiaoqu <- function(pageurl)
{ 
  print(pageurl)
  pageurl <- getURL(pageurl,.encoding="utf-8")
  xiaoquparsed_doc <- htmlParse(pageurl,encoding="UTF-8") 
  housecode <- xpathSApply(xiaoquparsed_doc,housecode_path,get_data_housecode)
  xiaoqu <- xpathSApply(xiaoquparsed_doc,xiaoqu_path,xmlValue)
  chengjiao <- xpathSApply(xiaoquparsed_doc,chengjiao_path,xmlValue)
  zufang <- xpathSApply(xiaoquparsed_doc,zufang_path,xmlValue)
  district <- xpathSApply(xiaoquparsed_doc,district_path,xmlValue)
  bizcircle <- xpathSApply(xiaoquparsed_doc,bizcircle_path,xmlValue)
  fin_year <- str_extract(xpathSApply(xiaoquparsed_doc,fin_year_path,xmlValue),'[0-9]{4}')
  price <- xpathSApply(xiaoquparsed_doc,price_path,xmlValue)
  price_desc <- xpathSApply(xiaoquparsed_doc,price_desc_path,xmlValue)
  sellCount <- xpathSApply(xiaoquparsed_doc,sellCount_path,xmlValue)
  ##合并各属性信息
  xiaoqu_info <- cbind(housecode,xiaoqu,chengjiao,zufang,
                       district,bizcircle,fin_year,price,sellCount)
  
  write.table(xiaoqu_info,filename, sep=","
              ,append = TRUE,quote = FALSE,row.names = FALSE,col.names = FALSE)
  Sys.sleep(0)
}

for(i in 1:length(regions[,2]))
{
print(regions[i,2])
urls_list <- getPageUrls(regions[i,2])
l_ply(urls_list,write_xiaoqu)
}









