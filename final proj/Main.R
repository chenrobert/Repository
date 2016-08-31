rm(list=ls(all=TRUE))


library(jiebaRD)
library(jiebaR)       # 斷詞利器
library(NLP)
library(tm)           # 文字詞彙矩陣運算
library(slam)         # 稀疏矩陣運算
library(RColorBrewer)
library(wordcloud)    # 文字雲
library(topicmodels)  # 主題模型
library(plyr)

source('fTWII.R')
source('fReport.R')
source('fCheck.R')

#systemset-------------------------------------------------------------------------
Sys.setlocale("LC_ALL", "cht")
#variable--------------------------------------------------------------------------


#begin=============================================================================
#get data--------------------------------------------------------------------------
TWII_Value = saveTWII_Value()
Report_Text = saveReport_Text(1,5)

#新增20160830
dateText = data.frame()
dateText[1,1] = Report_Text[1,2]
j=1

for(i in 1 : 100){
  if(dateText[j,1] == Report_Text[i,2])
    dateText[j,2] = paste(dateText[j,2], Report_Text[i,1], sep="")
  else{
    j = j+1
    dateText[j,1] = Report_Text[i,2]
    dateText[j,2] = paste(dateText[j,2], Report_Text[i,1], sep="")
  }
}

dataTemp = dateText
dataTemp[,2] = str_replace(string=dataTemp[,2], pattern="向上", replacement="1")
dataTemp[,2] = str_replace(string=dataTemp[,2], pattern="挑戰", replacement="1")
dataTemp[,2] = str_replace(string=dataTemp[,2], pattern="收紅", replacement="1")
dataTemp[,2] = str_replace(string=dataTemp[,2], pattern="多頭", replacement="1")
dataTemp[,2] = str_replace(string=dataTemp[,2], pattern="支撐", replacement="1")
dataTemp[,2] = str_replace(string=dataTemp[,2], pattern="買氣", replacement="1")
dataTemp[,2] = str_replace(string=dataTemp[,2], pattern="買氣", replacement="1")
dataTemp[,2] = str_replace(string=dataTemp[,2], pattern="強力", replacement="1")
dataTemp[,2] = str_replace(string=dataTemp[,2], pattern="增強", replacement="1")
dataTemp[,2] = str_replace(string=dataTemp[,2], pattern="升溫", replacement="1")
dataTemp[,2] = str_replace(string=dataTemp[,2], pattern="穩居", replacement="1")
dataTemp[,2] = str_replace(string=dataTemp[,2], pattern="續旺", replacement="1")
dataTemp[,2] = str_replace(string=dataTemp[,2], pattern="偏多", replacement="1")
dataTemp[,2] = str_replace(string=dataTemp[,2], pattern="增溫", replacement="1")
dataTemp[,2] = str_replace(string=dataTemp[,2], pattern="上攻", replacement="1")
dataTemp[,2] = str_replace(string=dataTemp[,2], pattern="站上", replacement="1")
dataTemp[,2] = str_replace(string=dataTemp[,2], pattern="創高", replacement="1")
for(i in 1:nrow(dateText)){
  dateText[i,3] = nchar(dateText[i,2]) - nchar(dataTemp[i,2])
}

dataTemp = dateText
dataTemp[,2] = str_replace(string=dataTemp[,2], pattern="修正", replacement="1")
dataTemp[,2] = str_replace(string=dataTemp[,2], pattern="偏弱", replacement="1")
dataTemp[,2] = str_replace(string=dataTemp[,2], pattern="謹慎", replacement="1")
dataTemp[,2] = str_replace(string=dataTemp[,2], pattern="回檔", replacement="1")
dataTemp[,2] = str_replace(string=dataTemp[,2], pattern="減弱", replacement="1")
dataTemp[,2] = str_replace(string=dataTemp[,2], pattern="保守", replacement="1")
dataTemp[,2] = str_replace(string=dataTemp[,2], pattern="弱勢", replacement="1")
dataTemp[,2] = str_replace(string=dataTemp[,2], pattern="負向", replacement="1")
dataTemp[,2] = str_replace(string=dataTemp[,2], pattern="連賣", replacement="1")
dataTemp[,2] = str_replace(string=dataTemp[,2], pattern="跌破", replacement="1")
dataTemp[,2] = str_replace(string=dataTemp[,2], pattern="向下", replacement="1")
dataTemp[,2] = str_replace(string=dataTemp[,2], pattern="過熱", replacement="1")
dataTemp[,2] = str_replace(string=dataTemp[,2], pattern="看淡", replacement="1")
dataTemp[,2] = str_replace(string=dataTemp[,2], pattern="轉弱", replacement="1")
dataTemp[,2] = str_replace(string=dataTemp[,2], pattern="壓抑", replacement="1")
dataTemp[,2] = str_replace(string=dataTemp[,2], pattern="下挫", replacement="1")
for(i in 1:nrow(dateText)){
  dateText[i,4] = nchar(dateText[i,2]) - nchar(dataTemp[i,2])
}

for(i in 1:nrow(dateText)){
  if(dateText[i,3] - dateText[i,4]>0)
    dateText[i,5] = 1
  else if(dateText[i,3] - dateText[i,4]<0)
    dateText[i,5] = -1
  else
    dateText[i,5] = 0
}


#Change---------------------------------------------------------------------------
TWII_Result = TWII_Change(TWII_Value)                     #漲:1,平:0,跌:-1

#(將TWII結果讀進dateText，因為dateText最剛開始的日期TWII沒有，所以從3開始)
#change from 1
for(i in 1:nrow(dateText)){
  for(j in 1 :nrow(TWII_Result)){
    if(as.character(dateText[i,1]) == index(TWII_Result[j,1]))
      dateText[i,6] = TWII_Result[j,4]
  }
  
}

#檢定如果前一天是樂觀/悲觀，股票會漲/跌
#如果前一天樂觀(1)且股票漲(1)，或是前一天悲觀(-1)且股票跌(-1)，就給1，其餘情況給0

testStat = data.frame()
j = 1
for(i in 1:nrow(dateText)){
  if(dateText[i,5]!=0 && dateText[i,5] == dateText[i,6] ){
    testStat[j,1] = 1
    j = j+1
  }
  else if(dateText[i,5]!=0 && dateText[i,5] != dateText[i,6]){
    testStat[j,1] = 0
    j = j+1
  }
}

t.test(testStat[,1],mu=0.5)
#H1:前一天的市場情緒會造成隔天股價同向或反向變動 H0:前一天市場情緒無關隔天
#test結果不顯著，mean of x在信賴區間內，所以H0




#Reporet_Result = Report_TextMining(Report_Text)            
orgPath = "./temp"
text = Corpus(DirSource(orgPath), list(language = NA))
text <- tm_map(text, removePunctuation)
text <- tm_map(text, removeNumbers)
text <- tm_map(text, function(word)
{ gsub("[A-Za-z0-9]", "", word) })

mixseg = worker()
mat <- matrix( unlist(text) )
totalSegment = data.frame()

for( j in 1:length(mat) )
{
  for( i in 1:length(mat[j,]) )
  {
    Report_Text = segment(as.character(mat[j,i]), jiebar=mixseg)
  }
  totalSegment = rbind(totalSegment, data.frame(Report_Text))
}

# define text array that you want
# delete text length < 2

delidx = which( nchar(as.vector(totalSegment[,1])) < 2 )
countText = totalSegment[-delidx,]
countResult = count(countText)[,1]
countFreq = count(countText)[,2] / sum(count(countText)[,2])
wordcloud(countResult, countFreq, min.freq = 1, random.order = F, ordered.colors = T, 
          colors = rainbow(length(countResult)))




#analysis-------------------------------------------------------------------------
#resultValue = Analysis(TWII_Result,Reporet_Text)
#show------------------------------------------------------------------------------




