library(XML)
library(httr)
library(xml2)
library(stringr)




#----------------------------------------------------------------------------------------
saveReport_Text <- function(starpage,endpage){
  
  Sys.setlocale("LC_ALL", "cht")
  
  part1_url <-"http://www.moneydj.com/KMDJ/Common/ListNewArticles.aspx?index1=" 
  part2_url <- "&svc=NW&a=X0100001"
  
  MoneyDJForum_1 = data.frame()
  
  for(i in starpage:endpage){
    
    testurl <- paste(part1_url,i,part2_url,sep="")
    url <- testurl
    doc <- read_html(url)
    
    timexpath <- "//*[@id='ctl00_ctl00_MainContent_Contents_ArticleGridList1_gvList']//td[1]"
    #  [1] "\r\n            08/12 17:46\r\n        "
    time <- xml_text(xml_find_all(doc,timexpath))
    #  [1] "08/1217:46" "08/1216:16" "08/1210:19" "08/1208:44" "08/1208:44" "08/1116:18"
    time <- gsub(pattern="[[:space:]]",replacement="",x= time)   #time
    #  [1] "08/12_17:46" "08/12_16:16" "08/12_10:19" "08/12_08:44" "08/12_08:44"
    time <- paste("2016-", substr(time,1,2),"-", substr(time,4,5), sep="")
    
    titlexpath <- "//*[@id='ctl00_ctl00_MainContent_Contents_ArticleGridList1_gvList']//td/a"
    title <- xml_text(xml_find_all(doc, titlexpath))              #title
    title <- as.data.frame(title)
    time <- as.data.frame(time)
    
    MoneyDJForum <- cbind(title,time )
    MoneyDJForum_1 = rbind(MoneyDJForum_1, data.frame(MoneyDJForum))
    
    file_name <- paste("./temp/",paste("newsPage_",i,sep=""),".txt",sep="")
    write.csv(MoneyDJForum,file=file_name)
  }

  write.csv(MoneyDJForum_1, file="MoneyDJForum_1.csv")
  return(MoneyDJForum_1)
  
}


#----------------------------------------------------------------------------------------
Report_TextMining <- function(Report_Text){

  Text <- Report_Text
  
    
  return(0)
}

