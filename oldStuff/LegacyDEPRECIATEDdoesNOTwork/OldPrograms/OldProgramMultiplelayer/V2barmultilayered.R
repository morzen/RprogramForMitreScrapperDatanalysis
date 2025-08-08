library("fmsb")
library("rjson")
library("ggplot2")
library("tidyverse")
library("dplyr")
library("stringr")

exit <- function() { invokeRestart("abort") } 


#PathToResult <- read.csv("/Users/morzen/Desktop/work/RprogramForMitreScrapperDatanalysis/PathList.csv")[["PathList"]]
PathToResult <- read.csv("/Users/morzen/Desktop/work/RprogramForMitreScrapperDatanalysis/OldProgramMultiplelayer/PathList2.csv")[["PathList"]]

FilesInPath <- list.files(path = PathToResult, pattern = "Rformat_", full.names = TRUE, recursive = TRUE)

fullDataSet <- list()

for(Pwd in FilesInPath){
  
  VersionOfMitreUpdate <- gsub('/','', str_extract(Pwd, "\\/Data_[a-z,A-Z]{3}[0-9]{4}\\/"))
  
  ScrapDate <- gsub('/','', str_extract(Pwd, "\\/([0-9]{1,2}-[0-9]{1,2}-[0-9]{1,4})\\/"))
  
  if(grepl("Rformat_Tactics|Rformat_Techniques|Rformat_Software|Rformat_Groups", Pwd) == TRUE){
    
    datatype <- str_extract(Pwd, "Tactics|Techniques|Software|Groups")
    
    fullDataSet[[VersionOfMitreUpdate]][[ScrapDate]][[datatype]] <- append(fullDataSet[[VersionOfMitreUpdate]][[ScrapDate]][[datatype]], fromJSON(file=Pwd))
    
  }
  
}

# TacticsData <- fullDataSet[["Data_Oct2022"]][["24-03-2023"]][["TacticsData"]]
# TechData <- fullDataSet[["Data_Oct2022"]][["24-03-2023"]][["Techniques"]]
# SoftData <- fullDataSet[["Data_Oct2022"]][["24-03-2023"]][["Software"]]
# GroupsData <- fullDataSet[["Data_Oct2022"]][["24-03-2023"]][["Groups"]]



TechDataDFcreation <- function(TechData){
  TempList <- list()
  
  for (Tech in names(TechData)){
    TempstringTechID <- TechData[[Tech]][["SumUpTable"]][["ID"]]
    TempstringTechID <- c(str_trim(TempstringTechID, "left"))
    TempList[["TechID"]] <- append(TempList[["TechID"]], TempstringTechID)
    # print(names(Tech$"SumUpTable"))
    
    if ("Tactic" %in% names(TechData[[Tech]][["SumUpTable"]])){
      TempstringTactics <- TechData[[Tech]][["SumUpTable"]][["Tactic"]]
      TempstringTactics <- c(str_trim(TempstringTactics, "left"))
      TempList[["TechTactics"]] <- append(TempList[["TechTactics"]], TempstringTactics)
    }
    else if("Tactics" %in% names(TechData[[Tech]][["SumUpTable"]])){
      TempstringTactics <- TechData[[Tech]][["SumUpTable"]][["Tactics"]]
      TempstringTactics <- c(str_trim(TempstringTactics, "left"))
      TempList[["TechTactics"]] <- append(TempList[["TechTactics"]], TempstringTactics)
    }else{
      message("going to the else")
    }
    
    TempstringDomain <- TechData[[Tech]][["title"]][["Domain"]]
    TempstringDomain <- c(str_trim(TempstringDomain, "left"))
    TempList[["TechDomain"]] <- append(TempList[["TechDomain"]], TempstringDomain)
  }
  
  
  TechDataDF <- data.frame(TempList)
  TechDataDF <- TechDataDF[!duplicated(TechDataDF), ]#remove duplicate row
  rownames(TechDataDF) <- 1:nrow(TechDataDF) 
  #view(TechDataDF)
  
  #############
  #turning combo tactics to list
  #############
  for(i in 1:length(TechDataDF$TechTactics)){
    CheckComma <- grepl(",", TechDataDF$TechTactics[i])
    if (CheckComma == TRUE){
      TechDataDF$TechTactics[i] <- str_trim(TechDataDF$TechTactics[i], "left")#remove first space
      # print(TechDataDF$TechTactics[i])
      
      TechDataDF$TechTactics[i] <- list(strsplit(as.character(TechDataDF$TechTactics[i]), split = ", "))
      # print(TechDataDF$TechTactics[i])
      
    }
  }
 ###############
  
  return(TechDataDF)
}


GroupDataDFcreation <- function(GroupsData){
  TempList <- list()
  for(Group in names(GroupsData)){
    Name <- c(str_trim(GroupsData[[Group]][["title"]][["Name"]], "left"))
    ID <- c(str_trim(GroupsData[[Group]][["title"]][["ID"]], "left"))
    
    for(table in names(GroupsData[[Group]][["HeadersAndTables"]])){
      if(table == "Techniques Used"){
        
        for(i in 1:nrow(data.frame(GroupsData[[Group]][["HeadersAndTables"]][["Techniques Used"]]))){
          TempList[["Name"]] <-append(TempList[["Name"]], Name)
          TempList[["ID"]] <-append(TempList[["ID"]], ID)
          TempList[["Domain"]] <-append(TempList[["Domain"]], GroupsData[[Group]][["HeadersAndTables"]][["Techniques Used"]][["Domain"]][[i]])
          TempList[["TechID"]] <-append(TempList[["TechID"]], GroupsData[[Group]][["HeadersAndTables"]][["Techniques Used"]][["ID"]][[i]])
        }
        
      }else if(!("Techniques Used" %in% names(GroupsData[[Group]][["HeadersAndTables"]]))){
        string <- "This APT do not possess a Techniques Used table"
        TempList[["Name"]] <-append(TempList[["Name"]], Name)
        TempList[["ID"]] <-append(TempList[["ID"]], ID)
        TempList[["Domain"]] <-append(TempList[["Domain"]],string)
        TempList[["TechID"]] <-append(TempList[["TechID"]], string)
      }
    }
  }
  GroupDataDF <- data.frame(TempList)
  GroupDataDD <- GroupDataDF[!duplicated(GroupDataDF), ]#remove duplicate row

  return(GroupDataDF)
}


FusionGroupDataDF_with_TechDataDF <- function(x, y){
  GroupDataDF <- x
  TechDataDF <- y
  
  TempRowlist <- list()
  for(ID in GroupDataDF[["TechID"]]){
    
    if(ID == "This APT do not possess a Techniques Used table"){
      
      TempRowlist[["TechID2"]] <- append(TempRowlist[["TechID2"]], ID)
      TempRowlist[["TechTactics"]] <- append(TempRowlist[["TechTactics"]], ID)
      TempRowlist[["TechDomain"]] <- append(TempRowlist[["TechDomain"]], ID)
    }else{
      TempRow <- TechDataDF[TechDataDF$TechID == ID,]
      
      TempRowlist[["TechID2"]] <- append(TempRowlist[["TechID2"]], TempRow[["TechID"]])
      TempRowlist[["TechTactics"]] <- append(TempRowlist[["TechTactics"]], TempRow[["TechTactics"]])
      TempRowlist[["TechDomain"]] <- append(TempRowlist[["TechDomain"]], TempRow[["TechDomain"]])
      
    }
  }

  GroupDataDF[["TechID2"]] <- TempRowlist[["TechID2"]]
  GroupDataDF[["TechTactics"]] <- TempRowlist[["TechTactics"]]
  GroupDataDF[["TechDomain"]] <- TempRowlist[["TechDomain"]]
  
  return(GroupDataDF)
  
  
}




NoDuplicateTacticsFunc <- function(x){
  TechDataDF <- x
  Tacticlist <- list()
  for(i in 1:length(TechDataDF$TechTactics)){
    if (typeof(TechDataDF$TechTactics[[i]]) == "character"){
      Tacticlist <- append(Tacticlist, TechDataDF$TechTactics[[i]])
    }
  }
  NoDuplicateTactics <- Tacticlist[!duplicated(Tacticlist)]
  NoDuplicateTactics <- do.call(rbind,
                                Map(
                                  data.frame,
                                  Tactics = c(NoDuplicateTactics)
                                ))
  NoDuplicateTactics <-   NoDuplicateTactics[!grepl(",", NoDuplicateTactics$Tactics),]
  NoDuplicateTactics <- as.data.frame(NoDuplicateTactics)
  
  return(NoDuplicateTactics)
}


TacticCounter <- function(x, y){
  givendataset <- x
  NodubTactics <- y
  
  givendataset <- givendataset[!duplicated(givendataset$TechID), ]# remove duplicate techniques
  
  
  result <- list()
  for(tacticLookup in NodubTactics$NoDuplicateTactics){
    i <- 0
    for(Tactic in  givendataset$TechTactics){
      if(tacticLookup == Tactic){
        i <- i + 1
      }else if(typeof(Tactic) == "list" & tacticLookup %in% Tactic[[1]]){ 
        i <- i +1 
      }
    }
    result[['count']] <- append(result[['count']], i)
  }
  
  result <- data.frame(result)
  rownames(result) <- NodubTactics$NoDuplicateTactics
  
  return(result)
}

applying_Domain_Filter <- function(x, y){
  givendataset <- x
  DomainFilter <- y
  
  if(length(DomainFilter) != 0){
    givendataset <- filter(givendataset, TechDomain %in% DomainFilter)#filter domain
  }
  
  
  return(givendataset)
}

FindcorrespondingIDorName <- function(x, y){
  APTnameOrID <- x
  GroupsData <- y
  
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
  
  if(str_detect(APTnameOrID, "G[0-9]{4}") == TRUE){
    if(APTnameOrID %in% IDlist){
      
      # print(GroupsData[[APTnameOrID]][["title"]][["Name"]]) #to check if name correspond to ID
      Name <- GroupsData[[APTnameOrID]][["title"]][["Name"]]
      IDandName <- IDandName <- paste(APTnameOrID, Name)
      
    }
  }else if (str_detect(APTnameOrID, "G[0-9]{4}") == FALSE){
    if(APTnameOrID %in% Namelist){
      
      # print(IDNameDF[IDNameDF$Namelist == APTnameOrID,]$IDlist) #to check if name correspond to ID
      ID <- IDNameDF[IDNameDF$Namelist == APTnameOrID,]$IDlist
      IDandName <- paste(ID, APTnameOrID)
    }
  }else{
    print("you did not enter an ID or name")
  }
  
  return(IDandName)
}

APTlookup_TacticCount <- function(x, y, z){
  APTlookup <- x
  GroupDataDF <- y
  TacticList <- z
  
  APTtacticCount <- list()
  
  for(APTlookupName in APTlookup){
    DataAptSelectedOnly <- filter(GroupDataDF, Name %in% APTlookupName)
    TempDF <- TacticCounter(DataAptSelectedOnly, TacticList)
    IDandName <- FindcorrespondingIDorName(APTlookupName, GroupsData)
    APTtacticCount[IDandName] <- TempDF
  }

  APTtacticCount <- data.frame(c(APTtacticCount), check.names=FALSE)
  rownames(APTtacticCount) <- TacticList$NoDuplicateTactics
  #print(names(APTtacticCount))
  return(APTtacticCount)
}

CreatingFinalDF <- function(x, y, z, NoDuplicatedTacticDF){
  TechDataDF_TacticCount <- x
  GroupDataDF_TacticCount <- y
  APTlookup_TacticCount <- z
  
  
  FinalDF<-list()
  FinalDF["Tactics"] <- NoDuplicatedTacticDF
  FinalDF["Total_TechCount"] <- TechDataDF_TacticCount
  FinalDF["Groups_TechCount"] <- GroupDataDF_TacticCount
  FinalDF<-data.frame(FinalDF, check.names=FALSE)
  
  for(element in names(APTlookup_TacticCount)){
    FinalDF[element] <- APTlookup_TacticCount[,element]
  }
  
  return(FinalDF)
}


BarCharFunc <- function(x){
  AptTacticsNumberOftechForEachTactic <- x
  view(AptTacticsNumberOftechForEachTactic)
  AptTacticsNumberOftechForEachTactic <- AptTacticsNumberOftechForEachTactic %>% tidyr::pivot_longer(cols = Total_TechCount:colnames(AptTacticsNumberOftechForEachTactic)[ncol(AptTacticsNumberOftechForEachTactic)])
  view(AptTacticsNumberOftechForEachTactic)
 
  ggplot(AptTacticsNumberOftechForEachTactic, aes(x = Tactics, y = value, fill = name))+
    geom_bar(stat = 'identity', position = 'dodge')+
    theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1))+
    geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25)
  
  
}

run1st_Program <- function(x){
  fullDataSet <- x
  
  TechData <- fullDataSet[["Data_Apr2023"]][["05-05-2023"]][["Techniques"]]
  GroupsData <- fullDataSet[["Data_Apr2023"]][["05-05-2023"]][["Groups"]]

  DomainFilter <- list("Mobile")

  APTlookup <- list("APT29", "Lazarus Group", "APT33", "Sandworm Team", "Windshift", "Dark Caracal")
  
  TechDataDF <- TechDataDFcreation(TechData)#raw data from TechData
  GroupDataDF <- GroupDataDFcreation(GroupsData)#raw data from GroupsData
  GroupDataDF <- FusionGroupDataDF_with_TechDataDF(GroupDataDF, TechDataDF)#Fusing the data of TechDataDF & GroupDataDF
  
  NoDuplicatedTacticDF <- NoDuplicateTacticsFunc(TechDataDF)
  
  TechDataDF <- applying_Domain_Filter(TechDataDF, DomainFilter)#Applying domain filter
  GroupDataDF <- applying_Domain_Filter(GroupDataDF, DomainFilter)#Applying domain filter
  
  
  
  TechDataDF_TacticCount <- TacticCounter(TechDataDF, NoDuplicatedTacticDF)
  GroupDataDF_TacticCount <- TacticCounter(GroupDataDF, NoDuplicatedTacticDF)
  APTlookup_TacticCount <- APTlookup_TacticCount(APTlookup, GroupDataDF, NoDuplicatedTacticDF)
  
  FinalDF <- CreatingFinalDF(TechDataDF_TacticCount, GroupDataDF_TacticCount, APTlookup_TacticCount, NoDuplicatedTacticDF)
  
  BarCharFunc(FinalDF)
}


run1st_Program(fullDataSet)


# tacticLookup <- "Collection"
# Tactic <- list(c("Collection", "Credential Access", "Impact", "Defense Evasion"))
# if(tacticLookup %in% Tactic[[1]]){
#   print("yes")
# }else{
#   print("no")
# }
# print(Tactic[[1]][1])
# 
