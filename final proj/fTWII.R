library("quantmod")
library("zoo")
library("xts")

#----------------------------------------------------------------------------------------
saveTWII_Value <-function(){
  
  #getSymbols("^TWII",src="yahoo")
  #barChart(TWII)
  #chartSeries(TWII,theme="white")
  #chartSeries(TWII["2007-05::2014-05"],theme="white")
  
  
  getSymbols("^TWII", from ="2016-05-15", to = Sys.Date(), src="yahoo")
  write.csv(TWII, "TWII.csv")
  
  return(TWII)
}

#----------------------------------------------------------------------------------------
TWII_Change <-function(TWII_Value){
  
  sample1 <- as.xts(TWII_Value)
  sample2 <- as.xts(TWII_Value)
  
  StartData = as.Date('2016-05-15')
  EndData = Sys.Date()
  
  CountData <- StartData
  PreviousData <- CountData - 1
  
  Status <- 1
  
  
  while(CountData <= EndData){
    
    PreviousData = CountData 
    CountData=CountData+1
    
    tryCatch({
      sample1[as.character(CountData),'TWII.Close'] = 
        as.integer(sample2[as.character(CountData),'TWII.Close']) - 
        as.integer(sample2[as.character(PreviousData),'TWII.Close'])
      
    },error = function(e){
      sample1[as.character(CountData),'TWII.Close'] = 
        as.integer(sample1[as.character(CountData),'TWII.Close'])-
        as.integer(sample1[as.character(CountData),'TWII.Close'])
    })
    
    tryCatch({
      if( as.integer(sample1[as.character(CountData),'TWII.Close']) == 0 ){
        Status=0
      }
      else if( as.integer(sample1[as.character(CountData),'TWII.Close']) > 0 ){
        Status=1
      }
      else if( as.integer(sample1[as.character(CountData),'TWII.Close']) < 0 ){
        Status=-1
      }
    },error = function(e){
      Status=0
    })
    
    sample1[as.character(CountData),'TWII.Close'] = Status
    
  }

  return(sample1)
}