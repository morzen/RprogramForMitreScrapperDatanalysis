library("fmsb")
library("rjson")
library("ggplot2")
library("tidyverse")
library("dplyr")
library("stringr")

exit <- function() { invokeRestart("abort") } 

# Mac path
MacPwd <- "/Users/morzen/Desktop/MitreAttackAPTscrapper/results/24-03-2023"
# TacticsData <- fromJSON(file=paste(MacPwd,"/","Rformat_Tactics24-03-2023_12-13-48_Mitre_APT.json", sep = ""))
TechData <- fromJSON(file=paste(MacPwd,"/","Rformat_Techniques24-03-2023_12-13-48_Mitre_APT.json", sep = ""))
# SoftData <- fromJSON(file=paste(MacPwd,"/","Rformat_Software24-03-2023_12-13-48_Mitre_APT.json", sep = ""))
GroupsData <- fromJSON(file=paste(MacPwd,"/","Rformat_Groups24-03-2023_12-13-48_Mitre_APT.json", sep = ""))


TechDataDFcreation <- function(TechData) {
  TechID <- list()
  TechTactics <- list()
  for (Tech in TechData){
    TechID <- append(TechID, Tech$"SumUpTable"$ID)
    # print(names(Tech$"SumUpTable"))
    
    if ("Tactic" %in% names(Tech$"SumUpTable")){
      TechTactics <- append(TechTactics, Tech$"SumUpTable"$"Tactic")
    }
    else if("Tactics" %in% names(Tech$"SumUpTable")){
      TechTactics <- append(TechTactics, Tech$"SumUpTable"$"Tactics")
    }else{
      message("going to the else")
    }
  }
  
  
  TechDataDF <- do.call(rbind,
            Map(
              data.frame,
              TechID = c(TechID),
              TechTactics = c(TechTactics)
            ))
  
  return(TechDataDF)
}


TurnComboTacticToList <- function(TacticList) {
  
  for (tactics in TacticList) {
    # print(tactic)
    CheckComma <- grepl(",", tactics)
    if (CheckComma == TRUE){
      tactic_s <- strsplit(tactics, ',')
      for (element in tactic_s) {
        for (thing in element){
          thing <- str_trim(thing)
          
        }
        
      }
      # print(typeof(tactic))
    }
    
  }
  
}

V2 <- function(TacticList) {
  
  for(i in 1:length(TacticList)){
    CheckComma <- grepl(",", TacticList[i])
    if (CheckComma == TRUE){
      TacticList[i] <- strsplit(TacticList[i], ',')
      # print(TacticList[i])
      
      for (element in TacticList[i]){
        for (thing in element){
          print(thing)
        }
        
      }
      
    }
  }
    
}


TechDataDF <- TechDataDFcreation(TechData)
NoDuplicateTechID = TechDataDF$TechID[!duplicated(TechDataDF$TechID)]
NoDuplicateTechTactics = TechDataDF$TechTactics[!duplicated(TechDataDF$TechTactics)]

NoDuplicateTechID <- as.data.frame(NoDuplicateTechID)
NoDuplicateTechTactics <- as.data.frame(NoDuplicateTechTactics)

NoDuplicateTechTacticsNoComboc <-   NoDuplicateTechTactics[!grepl(",", NoDuplicateTechTactics$NoDuplicateTechTactics),]
NoDuplicateTechTacticsNoComboc <- as.data.frame(NoDuplicateTechTacticsNoComboc)

TurnComboTacticToList(TechDataDF$TechTactics)
V2(TechDataDF$TechTactics)
