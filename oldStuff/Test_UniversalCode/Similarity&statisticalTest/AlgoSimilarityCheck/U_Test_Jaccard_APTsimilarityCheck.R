library("fmsb")
library("rjson")
library("ggplot2")
library("tidyverse")
library("dplyr")
library("plyr")
library("stringr")
library("data.table")


FinalDataFrame <- read.csv("./1_DATAusedFolder/FinalDataFrame_Apr2024.csv")

##########################################################################

#Similarities for Tactics

Tactic_s_PercentageDF <- function(updateDate, scrappingDate){
  
  APTnames <- FinalDataFrame
  APTnames <- APTnames %>% filter(Update_Date == updateDate) %>% filter(Latest_ScrapDate_For_Given_Update == scrappingDate) %>% filter(PageType == "Groups")
  APTnames <- data.frame(APT_names = APTnames$Title_Name[!duplicated(APTnames$Title_Name)]) 
  
  
  testing <- FinalDataFrame %>% filter(Update_Date == updateDate) %>% filter(Latest_ScrapDate_For_Given_Update == scrappingDate) %>% filter(PageType == "Groups") %>% filter(Tables_TableType == "Techniques Used")
  
  ALl_Techniques <- FinalDataFrame %>% filter(Update_Date == updateDate) %>% filter(Latest_ScrapDate_For_Given_Update == scrappingDate) %>% filter(PageType == "Techniques")
  ALl_Techniques <- data.frame(cbind(Title_ID = ALl_Techniques$Title_ID, SumUpTable_Tactic = ALl_Techniques$SumUpTable_Tactic, SumUpTable_Tactics = ALl_Techniques$SumUpTable_Tactics))
  ALl_Techniques <- ALl_Techniques[!duplicated(ALl_Techniques), ]
  
  col_combine <- c("SumUpTable_Tactic", "SumUpTable_Tactics")    
  Data_new1 <- cbind(Tactic_s = na.omit(unlist(ALl_Techniques[ , col_combine])), ALl_Techniques[ , ! colnames(ALl_Techniques) %in% col_combine, drop = FALSE])
  
  ALl_Techniques <- cbind(ALl_Techniques, Tactic_s = Data_new1$Tactic_s)
  ALl_Techniques <- subset(ALl_Techniques, select = - c(SumUpTable_Tactic, SumUpTable_Tactics))
  ALl_Techniques <- ALl_Techniques[!duplicated(ALl_Techniques), ]
  
  
  Tactic_sList <- list()
  # i = 0
  for(TechID1 in testing$Tables_ID){
    if(TechID1 %in% ALl_Techniques$Title_ID){
      # print(i)
      # print(length(Tactic_sList))
      # print(TechID1)
      IndexCol <- which(ALl_Techniques$Title_ID == TechID1)
      # print(IndexCol)
      
      Tactic_sList <- append(Tactic_sList, ALl_Techniques$Tactic_s[IndexCol])
      # i = i + 1
      # if(!(length(Tactic_sList) == i)){
      #   print(TechID1)
      #   print(IndexCol)
      #   print(i)
      #   print(length(Tactic_sList))
      #   break
      # }
    }
  }
  
  testing <- testing[ , colSums(is.na(testing))==0]
  testing$Tactic_s <- Tactic_sList
  
  
  test <- data.frame(cbind(APTname = testing$Title_Name, APTtechniques = testing$Tactic_s))
  
  Tactic_s_PercentDataFrame <- data.frame(matrix(nrow = length(APTnames$APT_names), ncol = length(APTnames$APT_names)))
  colnames(Tactic_s_PercentDataFrame) <- APTnames$APT_names
  rownames(Tactic_s_PercentDataFrame) <- APTnames$APT_names
  
  #using Jaccard Similarity Coefficient
  for(APT1 in APTnames$APT_names){
    APT1DF <- filter(test, APTname %in% APT1) 
    
    for(APT2 in APTnames$APT_names){
      APT2DF <- filter(test, APTname %in% APT2)
      
      # doing jaccar here
      intersec <- length(intersect(APT1DF$APTtechniques, APT2DF$APTtechniques))
      union <- length(union(APT1DF$APTtechniques, APT2DF$APTtechniques))
      JaccardSim<- intersec / union
      
      # turn it in percentage
      JaccardSim_InPercent <- JaccardSim * 100
      
      Tactic_s_PercentDataFrame[APT1, APT2] <- JaccardSim_InPercent
    }
    
  }
  
  return(Tactic_s_PercentDataFrame)
}

##########################################################################

#Similarities for Domain

Domain_PercentageDF <- function(updateDate, scrappingDate){
  
  APTnames <- FinalDataFrame
  APTnames <- APTnames %>% filter(Update_Date == updateDate) %>% filter(Latest_ScrapDate_For_Given_Update == scrappingDate) %>% filter(PageType == "Groups")
  APTnames <- data.frame(APT_names = APTnames$Title_Name[!duplicated(APTnames$Title_Name)]) 
  
  testing <- FinalDataFrame
  
  testing <- testing %>% filter(Update_Date == updateDate) %>% filter(Latest_ScrapDate_For_Given_Update == scrappingDate) %>% filter(PageType == "Groups") %>% filter(Tables_TableType == "Techniques Used")
  
  testing <- Filter(function(x)!all(is.na(x)), testing)
  
  test <- data.frame(cbind(APTname = testing$Title_Name, APTtechniques = testing$Tables_Domain))
  
  
  Domain_PercentDataFrame <- data.frame(matrix(nrow = length(APTnames$APT_names), ncol = length(APTnames$APT_names)))
  colnames(Domain_PercentDataFrame) <- APTnames$APT_names
  rownames(Domain_PercentDataFrame) <- APTnames$APT_names
  
  #using Jaccard Similarity Coefficient
  for(APT1 in APTnames$APT_names){
    APT1DF <- filter(test, APTname %in% APT1) 
    
    for(APT2 in APTnames$APT_names){
      APT2DF <- filter(test, APTname %in% APT2)
      
      # doing jaccar here
      intersec <- length(intersect(APT1DF$APTtechniques, APT2DF$APTtechniques))
      union <- length(union(APT1DF$APTtechniques, APT2DF$APTtechniques))
      JaccardSim<- intersec / union
      
      # turn it in percentage
      JaccardSim_InPercent <- JaccardSim * 100
      
      Domain_PercentDataFrame[APT1, APT2] <- JaccardSim_InPercent
    }
    
  }
  
  return(Domain_PercentDataFrame)
}

##############################################################################

#Similarities for techniques

Techniques_PercentageDF <- function(updateDate, scrappingDate){
  
  APTnames <- FinalDataFrame
  APTnames <- APTnames %>% filter(Update_Date == updateDate) %>% filter(Latest_ScrapDate_For_Given_Update == scrappingDate) %>% filter(PageType == "Groups")
  APTnames <- data.frame(APT_names = APTnames$Title_Name[!duplicated(APTnames$Title_Name)]) 
  
  testing <- FinalDataFrame

  testing <- testing %>% filter(Update_Date == updateDate) %>% filter(Latest_ScrapDate_For_Given_Update == scrappingDate) %>% filter(PageType == "Groups") %>% filter(Tables_TableType == "Techniques Used")
  
  testing <- Filter(function(x)!all(is.na(x)), testing)
  
  test <- data.frame(cbind(APTname = testing$Title_Name, APTtechniques = testing$Tables_ID))
  
  
  Tech_PercentDataFrame <- data.frame(matrix(nrow = length(APTnames$APT_names), ncol = length(APTnames$APT_names)))
  colnames(Tech_PercentDataFrame) <- APTnames$APT_names
  rownames(Tech_PercentDataFrame) <- APTnames$APT_names
  
  #using Jaccard Similarity Coefficient
  for(APT1 in APTnames$APT_names){
    APT1DF <- filter(test, APTname %in% APT1) 
    
    for(APT2 in APTnames$APT_names){
      APT2DF <- filter(test, APTname %in% APT2)
      
      # doing jaccar here
      intersec <- length(intersect(APT1DF$APTtechniques, APT2DF$APTtechniques))
      union <- length(union(APT1DF$APTtechniques, APT2DF$APTtechniques))
      JaccardSim<- intersec / union
      
      # turn it in percentage
      JaccardSim_InPercent <- JaccardSim * 100
      
      Tech_PercentDataFrame[APT1, APT2] <- JaccardSim_InPercent
    }
    
  }
  
  return(Tech_PercentDataFrame)
}


##############################################################################

#Similarities for softwares
Software_PercentageDF <- function(updateDate, scrappingDate){
  
  APTnames <- FinalDataFrame
  APTnames <- APTnames %>% filter(Update_Date == updateDate) %>% filter(Latest_ScrapDate_For_Given_Update == scrappingDate) %>% filter(PageType == "Groups")
  APTnames <- data.frame(APT_names = APTnames$Title_Name[!duplicated(APTnames$Title_Name)]) 
  
  testing <- FinalDataFrame
  
  testing <- testing %>% filter(Update_Date == updateDate) %>% filter(Latest_ScrapDate_For_Given_Update == scrappingDate) %>% filter(PageType == "Groups") %>% filter(Tables_TableType == "Software")
  
  testing <- Filter(function(x)!all(is.na(x)), testing)
  
  test <- data.frame(cbind(APTname = testing$Title_Name, APTtechniques = testing$Tables_ID))
  
  
  Soft_PercentDataFrame <- data.frame(matrix(nrow = length(APTnames$APT_names), ncol = length(APTnames$APT_names)))
  colnames(Soft_PercentDataFrame) <- APTnames$APT_names
  rownames(Soft_PercentDataFrame) <- APTnames$APT_names
  
  #using Jaccard Similarity Coefficient
  for(APT1 in APTnames$APT_names){
    APT1DF <- filter(test, APTname %in% APT1) 
    
    for(APT2 in APTnames$APT_names){
      APT2DF <- filter(test, APTname %in% APT2)
      
      # doing jaccar here
      intersec <- length(intersect(APT1DF$APTtechniques, APT2DF$APTtechniques))
      union <- length(union(APT1DF$APTtechniques, APT2DF$APTtechniques))
      JaccardSim<- intersec / union
      
      # turn it in percentage
      JaccardSim_InPercent <- JaccardSim * 100
      
      Soft_PercentDataFrame[APT1, APT2] <- JaccardSim_InPercent
    }
    
  }
  return(Soft_PercentDataFrame)
}

##############################################################################

UPDATE_DATE <- "Data_Apr2024"
SCRAPPING_DATE <- "24-04-2024"

Tactics_Percent_Table <- Tactic_s_PercentageDF(UPDATE_DATE, SCRAPPING_DATE)
Domain_Percent_Table <- Domain_PercentageDF(UPDATE_DATE, SCRAPPING_DATE)
Techniques_Percent_Table <- Techniques_PercentageDF(UPDATE_DATE, SCRAPPING_DATE)
Softwares_Percent_Table <- Software_PercentageDF(UPDATE_DATE, SCRAPPING_DATE)



Total = (Techniques_Percent_Table + Softwares_Percent_Table) / 2



# write.csv(Total, "/Users/morzen/Desktop/Jaccard'sCorrelationTech.csv", row.names = FALSE)



##################
#test reduce pool of APT
###########

testing1 <- Techniques_Percent_Table

testing1 <- testing1[, which(colSums(testing1 > 50 & testing1 < 100) > 0)]








testing <- FinalDataFrame

testing <- testing %>% filter(Update_Date == UPDATE_DATE) %>% filter(Latest_ScrapDate_For_Given_Update == SCRAPPING_DATE) %>% filter(PageType == "Groups") %>% filter(Tables_TableType == "Techniques Used")

test <- data.frame(cbind(APTname = testing$Title_Name, APTtechniques = testing$Tables_ID))

APT1DF <- filter(test, APTname %in% "admin@338")
APT2DF <- filter(test, APTname %in% "Ajax Security Team")

intersec <- length(intersect(APT1DF$APTtechniques, APT2DF$APTtechniques))
union <- length(union(APT1DF$APTtechniques, APT2DF$APTtechniques))
JaccardSim<- intersec / union


