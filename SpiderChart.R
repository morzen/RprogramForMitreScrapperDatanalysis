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
  
  for(i in 1:length(TacticList)){
    CheckComma <- grepl(",", TacticList[i])
    if (CheckComma == TRUE){
      TacticList[i] <- str_trim(TacticList[i], "left")#remove first space
      # print(TacticList[i])
      
      TacticList[i] <- list(strsplit(as.character(TacticList[i]), split = ", "))
      # print(TacticList[i])
      
    }
  }
  
  return(TacticList)
}


TechDataDF <- TechDataDFcreation(TechData)
NoDuplicateTechID = TechDataDF$TechID[!duplicated(TechDataDF$TechID)]
NoDuplicateTechTactics = TechDataDF$TechTactics[!duplicated(TechDataDF$TechTactics)]

NoDuplicateTechID <- as.data.frame(NoDuplicateTechID)
NoDuplicateTechTactics <- as.data.frame(NoDuplicateTechTactics)

NoDuplicateTechTacticsNoComboc <-   NoDuplicateTechTactics[!grepl(",", NoDuplicateTechTactics$NoDuplicateTechTactics),]
NoDuplicateTechTacticsNoComboc <- as.data.frame(NoDuplicateTechTacticsNoComboc)

TechDataDF$TechTactics <- TurnComboTacticToList(TechDataDF$TechTactics)




# SpiderGraph <- function(APTnameOrID){
IDlist <- list()
Namelist <- list()
# print(names(GroupsData)) to get keys
for (element in names(GroupsData)){
  IDlist <- append(IDlist, element)
  Namelist <- append(Namelist, GroupsData[[element]][["title"]][["Name"]])
}

IDNameDF <- do.call(rbind,
                    Map(
                      data.frame,
                      IDlist = c(IDlist),
                      Namelist = c(Namelist)
                    ))

# print(IDNameDF[IDNameDF$Namelist == 'admin@338',]$IDlist) #return name
# print(IDNameDF[IDNameDF$IDlist == 'G0018',]$Namelist) #return ID


APTnameOrID <- "admin@338"

if(str_detect(APTnameOrID, "G[0-9]{4}") == TRUE){
  if(APTnameOrID %in% IDlist){
    ID <- APTnameOrID
    for (element in GroupsData){
      APT <- element
    }
  }
}else if (str_detect(APTnameOrID, "G[0-9]{4}") == FALSE){
  if(APTnameOrID %in% Namelist){
    ID <- IDNameDF[IDNameDF$Namelist == 'admin@338',]$IDlist
    # APT <- GroupsData[ID]
    for (element in GroupsData){
      APT <- element
    }
  }
}else{
  print("you did not enter an ID or name")
}

APTtechID <- APT[["HeadersAndTables"]][["Techniques Used"]][["ID"]]
APTtechIDList <- list()
for (element in APTtechID){
  APTtechIDList <- append(APTtechIDList,element)
}

print(APTtechIDList)
# }


SpiderGraph("G0018")



