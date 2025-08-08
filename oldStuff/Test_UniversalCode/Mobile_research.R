library("fmsb")
library("rjson")
library("ggplot2")
library("tidyverse")
library("dplyr")
library("plyr")
library("stringr")
library("data.table")

FinalDataFrame <- read.csv("./1_DATAusedFolder/FinalDataFrame_Apr2024.csv")

#################################################



#################################################

Mobile <- filter(FinalDataFrame, Latest_ScrapDate_For_Given_Update %in% "12-06-2023") %>% filter(PageType == "Groups") %>% filter(Tables_Domain == "Mobile")
#remove column filled with Na
Mobile <- Mobile[,colSums(is.na(Mobile))<nrow(Mobile)]


TechData_IDandTactic <- filter(FinalDataFrame, PageType %in% "Techniques")
TechData_IDandTactic <- cbind(DataTypeID = TechData_IDandTactic$DataTypeID, Title_ID = TechData_IDandTactic$Title_ID, SumUpTable_ID = TechData_IDandTactic$SumUpTable_ID, SumUpTable_Tactic = TechData_IDandTactic$SumUpTable_Tactic, SumUpTable_Tactics = TechData_IDandTactic$SumUpTable_Tactics,Title_Domain = TechData_IDandTactic$Title_Domain, Title_Name = TechData_IDandTactic$Title_Name)
TechData_IDandTactic <- data.frame(TechData_IDandTactic[!duplicated(TechData_IDandTactic), ])
TechData_IDandTactic <- TechData_IDandTactic %>% mutate(Tactic_s = coalesce(SumUpTable_Tactic, SumUpTable_Tactics))
TechData_IDandTactic <- within(TechData_IDandTactic, rm(SumUpTable_Tactic, SumUpTable_Tactics))


Mobile_tactic_List <- c()
for(Mobile_technique in Mobile$Tables_ID){
  for(technique in TechData_IDandTactic$Title_ID){
    if(Mobile_technique == technique){
      TempRow <- TechData_IDandTactic[TechData_IDandTactic$Title_ID == Mobile_technique, ] 
      Mobile_tactic_List <- append(Mobile_tactic_List, TempRow$Tactic_s)
      
    }
    
  }
  
}


Mobile <- cbind(Mobile, Tactic_s = Mobile_tactic_List)

#Small list of all individual tactics without duplicate
TacticsNoduplicate <- data.frame(TacticNoDub = FinalDataFrame$Title_Tactic[!duplicated(FinalDataFrame$Title_Tactic)]) %>% drop_na(TacticNoDub)

