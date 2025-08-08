library("fmsb")
library("rjson")
library("ggplot2")
library("tidyverse")
library("stringr")
library("dplyr")


FinalDataFrame <- read.csv("./1_DATAusedFolder/FinalDataFrame_Apr2024.csv")

###############################################
#Start barchart prog
###############################################
AddTacticsColToGroupData_TechIDandTactics <- function(x, y){
  GroupData_TechIDandTactic <- x
  TechData_TechIDandTactic <- y
  
  GroupTacticList <- list()
  for(Group_techniqueID in GroupData_TechIDandTactic$TechniquesID){
    IndexForTechniqueIn_TechData_TechIDandTactic <- which(TechData_TechIDandTactic$TechniquesID == Group_techniqueID)
    GroupTacticList <- append(GroupTacticList, TechData_TechIDandTactic$Tactic_s[IndexForTechniqueIn_TechData_TechIDandTactic])
  }
  
  GroupData_TechIDandTactic$Tactic_s <- GroupTacticList
  
  return(GroupData_TechIDandTactic)
  
}

TacticCounter <- function(x, y){
  givendataset <- x
  NodubTactics <- y
  
  givendataset <- givendataset[!duplicated(givendataset$TechniquesID), ]# remove duplicate techniques
  
  #turn tactic combo to list
  for(index in nrow(givendataset$Tactic_s)){
    if("," %in% givendataset$Tactic_s[index]){
      givendataset$Tactic_s[index] <- strsplit(givendataset$Tactic_s[index], ",")
    }
  }
  
  
  result <- list()
  for(tacticLookup in NodubTactics$TacticNoDub){
    i <- 0
    for(Tactic in  givendataset$Tactic_s){
      if(tacticLookup == Tactic){
        i <- i + 1
      }else if(typeof(Tactic) == "list" & tacticLookup %in% Tactic[[1]]){ 
        i <- i +1 
      }
    }
    result[['count']] <- append(result[['count']], i)
  }
  
  result <- data.frame(result)
  rownames(result) <- NodubTactics$TacticNoDub
  
  return(result)
}


APTlookUp_Count_Func <- function(x, y, z){
  APTlookup <- x
  NodubTactics <- y
  GroupData_TechIDandTactic <- z
  
  APTtacticCount <- list()
  APT_ID_N_Name <- data.frame()
  for(APTtoLook in APTlookup){
    
    if(str_detect(APTtoLook, "G[0-9]{4}") == TRUE){
      singleAPT <- filter(GroupData_TechIDandTactic, GroupsID %in% APTtoLook)
      
      APT_ID_N_Name <- cbind(singleAPT$Name, singleAPT$GroupsID)
      APT_ID_N_Name <- APT_ID_N_Name[!duplicated(APT_ID_N_Name), ]
      
      if(length(APT_ID_N_Name) == 2){
        APT_ID_N_Name <- paste(APT_ID_N_Name[1], APT_ID_N_Name[2], sep="_")
      }
      else{
        print("APT_ID_N_Name of wrong length should be length 2")
      }
      
    }else if (str_detect(APTtoLook, "G[0-9]{4}") == FALSE){
      singleAPT <- filter(GroupData_TechIDandTactic, Name %in% APTtoLook)
      
      APT_ID_N_Name <- cbind(singleAPT$Name, singleAPT$GroupsID)
      APT_ID_N_Name <- APT_ID_N_Name[!duplicated(APT_ID_N_Name), ]
      
      if(length(APT_ID_N_Name) == 2){
        APT_ID_N_Name <- paste(APT_ID_N_Name[1], APT_ID_N_Name[2], sep="_")
      }
      else{
        print("APT_ID_N_Name of wrong length should be length 2")
      }
      
    }else{
      print("you did not enter an GroupID or GroupName")
    }
    
    
    givendataset <- singleAPT
    givendataset <- givendataset[!duplicated(givendataset$TechniquesID), ]# remove duplicate techniques
    
    #turn tactic combo to list
    for(index in nrow(givendataset$Tactic_s)){
      if("," %in% givendataset$Tactic_s[index]){
        givendataset$Tactic_s[index] <- strsplit(givendataset$Tactic_s[index], ",")
      }
    }
    
    
    result <- list()
    for(tacticLookup in NodubTactics$TacticNoDub){
      i <- 0
      for(Tactic in  givendataset$Tactic_s){
        if(tacticLookup == Tactic){
          i <- i + 1
        }else if(typeof(Tactic) == "list" & tacticLookup %in% Tactic[[1]]){ 
          i <- i +1 
        }
      }
      result[['count']] <- append(result[['count']], i)
    }
    
    result <- data.frame(result)
    rownames(result) <- NodubTactics$TacticNoDub
    
    APTtacticCount[APT_ID_N_Name] <- result
    
  }
  
  APTtacticCount <- data.frame(APTtacticCount)
  row.names(APTtacticCount) <- NodubTactics$TacticNoDub
  
  
  return(APTtacticCount)
}

BarCharFunc <- function(x){
  AptTacticsNumberOftechForEachTactic <- x
  
  AptTacticsNumberOftechForEachTactic <- cbind(Tactics = rownames(AptTacticsNumberOftechForEachTactic), AptTacticsNumberOftechForEachTactic)
  
  AptTacticsNumberOftechForEachTactic <- AptTacticsNumberOftechForEachTactic %>% tidyr::pivot_longer(cols = Total_TechniquesPage:colnames(AptTacticsNumberOftechForEachTactic)[ncol(AptTacticsNumberOftechForEachTactic)])
  
  
  ggplot(AptTacticsNumberOftechForEachTactic, aes(x = Tactics, y = value, fill = name))+
    geom_bar(stat = 'identity', position = 'dodge')+
    theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1))+
    geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25)
  
}

mainFunc <- function(x, y){
  
  DomainFilter <- x
  APTlookup <- y
  #to change the data just enter the desired date below
  #ScrapDate <- "30-03-2023"
  #belows date will be the most recent date
  ScrapDate <- format( max(as.Date(FinalDataFrame$Latest_ScrapDate_For_Given_Update, tryFormats = c("%d-%m-%Y"))), "%d-%m-%Y")
  
  TechData <- FinalDataFrame %>% filter(Latest_ScrapDate_For_Given_Update == ScrapDate) %>% filter(PageType == "Techniques")
  GroupsData <- FinalDataFrame %>% filter(Latest_ScrapDate_For_Given_Update == ScrapDate) %>% filter(PageType == "Groups")
  TacticDF <- FinalDataFrame %>% filter(Latest_ScrapDate_For_Given_Update == ScrapDate) %>% filter(PageType == "Tactics")

  #Small list of all individual tactics without duplicate
  TacticsNoduplicate <- data.frame(TacticNoDub = FinalDataFrame$Title_Tactic[!duplicated(FinalDataFrame$Title_Tactic)]) %>% drop_na(TacticNoDub)
  
  
  TechData_TechIDandTactic <- data.frame(cbind(TechniquesID = TechData$Title_ID, Tactic_s = TechData$SumUpTable_Tactic, Tatics = TechData$SumUpTable_Tactics, Title_Domain = TechData$Title_Domain)) %>% distinct()
  #All techniques known by miter attack and it s associated tactics
  # TechData_TechIDandTactic <- TechData_TechIDandTactic %>% mutate(Tactic_s = coalesce(Tactic_s, Tatics)) %>% select(TechniquesID, Tactic_s, Title_Domain)
  TechData_TechIDandTactic <- TechData_TechIDandTactic %>% 
    mutate(Tactic_s = coalesce(Tactic_s, Tatics)) %>% 
    dplyr::select(TechniquesID, Tactic_s, Title_Domain)
  
  #remove duplicate techniques
  TechData_TechIDandTactic <- TechData_TechIDandTactic[!duplicated(TechData_TechIDandTactic$TechniquesID), ]
  
  
  GroupData_TechIDandTactic <- GroupsData %>% filter(Tables_TableType == "Techniques Used")
  GroupData_TechIDandTactic <- data.frame(cbind(Name = GroupData_TechIDandTactic$Title_Name , GroupsID = GroupData_TechIDandTactic$Title_ID, TechniquesID = GroupData_TechIDandTactic$Tables_ID, Tables_Domain = GroupData_TechIDandTactic$Tables_Domain))
  GroupData_TechIDandTactic <- GroupData_TechIDandTactic[!duplicated(GroupData_TechIDandTactic), ]
  GroupData_TechIDandTactic <- AddTacticsColToGroupData_TechIDandTactics(GroupData_TechIDandTactic, TechData_TechIDandTactic)
  
  #Domain Filtering
  if(length(DomainFilter) > 0){
    #print("DOMAIN")
    TechData_TechIDandTactic <- filter(TechData_TechIDandTactic, Title_Domain %in% DomainFilter)
    GroupData_TechIDandTactic <- filter(GroupData_TechIDandTactic, Tables_Domain %in% DomainFilter)
  }
  
  
  Total_TechniquesPage_Count <- TacticCounter(TechData_TechIDandTactic, TacticsNoduplicate)
  Total_GroupsPage_Count <- TacticCounter(GroupData_TechIDandTactic, TacticsNoduplicate)
  
  #APT LookUp
  if(length(APTlookup) > 0){
    #print("APTlookUp")
    APTs_Count <- APTlookUp_Count_Func(APTlookup, TacticsNoduplicate, GroupData_TechIDandTactic)
    FinalDF <- cbind(Total_TechniquesPage = Total_TechniquesPage_Count$count, Total_GroupsPage = Total_GroupsPage_Count$count, APTs_Count = APTs_Count)
    row.names(FinalDF) <- TacticsNoduplicate$TacticNoDub
  }else{
    FinalDF <- cbind(Total_TechniquesPage = Total_TechniquesPage_Count$count, Total_GroupsPage = Total_GroupsPage_Count$count)
    row.names(FinalDF) <- TacticsNoduplicate$TacticNoDub
  }
  
  
  
  return(data.frame(FinalDF))
}

DomainFilter <- list()
APTlookup <- list("APT3")

ClassifiedCountDF <- mainFunc(DomainFilter, APTlookup)

BarCharFunc(ClassifiedCountDF)




#TESTING




