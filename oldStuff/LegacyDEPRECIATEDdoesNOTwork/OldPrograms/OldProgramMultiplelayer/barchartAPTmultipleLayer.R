library("fmsb")
library("rjson")
library("ggplot2")
library("tidyverse")
library("dplyr")
library("stringr")

exit <- function() { invokeRestart("abort") } 

#"/home/morzen/Desktop/RprogramForMitreScrapperDatanalysis/PathList.csv"
#"/Users/morzen/Desktop/work/RprogramForMitreScrapperDatanalysis/PathList.csv"
PathToResult <- read.csv("/Users/morzen/Desktop/work/RprogramForMitreScrapperDatanalysis/PathList.csv")[["PathList"]]

FilesInPath <- list.files(path = PathToResult, pattern = "Rformat_", full.names = TRUE, recursive = TRUE)

fullDataSet <- list()

for(Pwd in FilesInPath){
  
  VersionOfMitreUpdate <- gsub('/','', str_extract(Pwd, "\\/Data_[a-z,A-Z]{3}[0-9]{4}\\/"))
  
  ScrapDate <- gsub('/','', str_extract(Pwd, "\\/([0-9]{1,2}-[0-9]{1,2}-[0-9]{1,4})\\/"))
  
  if(grepl("Rformat_Tactics|Rformat_Techniques|Rformat_Software|Rformat_Groups", Pwd) == TRUE){
    
    datatype <- str_extract(Pwd, "Tactics|Techniques|Software|Groups")
    
    fullDataSet[[VersionOfMitreUpdate]][[ScrapDate]][[datatype]] <- append(fullDataSet[[VersionOfMitreUpdate]][[ScrapDate]][[datatype]], fromJSON(Pwd))
    
  }
  
}

#TacticsData <- fullDataSet[["Data_Oct2022"]][["24-03-2023"]][["TacticsData"]]
TechData <- fullDataSet[["Data_Oct2023"]][["19-01-2024"]][["Techniques"]]
#SoftData <- fullDataSet[["Data_Oct2022"]][["24-03-2023"]][["Software"]]
GroupsData <- fullDataSet[["Data_Oct2023"]][["19-01-2024"]][["Groups"]]



FindcorrespondingIDorName <- function(x){
  APTnameOrID <- x
  
  
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
      IDandName <- paste(APTnameOrID, Name)
      
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



FuncAPTtechIDList <- function(APTnameOrID){
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
 
  
  
  if(str_detect(APTnameOrID, "G[0-9]{4}") == TRUE){
    if(APTnameOrID %in% IDlist){
      
      #print(GroupsData[[APTnameOrID]][["title"]][["Name"]]) #to check if name correspond to ID
      
      ID <- APTnameOrID
      APT <- GroupsData[ID]
      for (element in APT){
        APT <- element
      }
    }
  }else if (str_detect(APTnameOrID, "G[0-9]{4}") == FALSE){
    if(APTnameOrID %in% Namelist){
      
      #print(IDNameDF[IDNameDF$Namelist == APTnameOrID,]$IDlist) #to check if name correspond to ID
      
      ID <- IDNameDF[IDNameDF$Namelist == APTnameOrID,]$IDlist
      APT <- GroupsData[ID]
      for (element in APT){
        APT <- element
      }
    }
  }else{
    print("you did not enter an ID or name")
  }
  
  APTtechID <- names(APT[["HeadersAndTables"]][["Techniques Used"]])
  APTtechIDList <- list()
  
  for (element in APTtechID){
    # print(element)
    APTtechIDList <- append(APTtechIDList,element)
  }
  
  # view(APTtechIDList) ####to check up if techniques correspond
  
  return(APTtechIDList)
}



GetAPTTechList <- function(x){
  GroupsData <- x
  APTTechList <- list()
  for(PageContent in GroupsData){
    APTTechList <- append(APTTechList, names(PageContent$HeadersAndTables$`Techniques Used`))
  }
  return(APTTechList)
}




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
                          TechID = c(str_trim(TechID, "left")),
                          TechTactics = c(str_trim(TechTactics, "left"))
                        ))
  
  
  
  
  TechDataDF %>% distinct(TechDataDF$TechID, .keep_all = TRUE)#remove duplicate for techID column
  
  rownames(TechDataDF) <- 1:nrow(TechDataDF)
  #view(TechDataDF)
  return(TechDataDF)
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



TurnComboTacticToList <- function(TacticList) {
  
  for(i in 1:length(TacticList)){
    CheckComma <- grepl(",", TacticList[i])
    # print(CheckComma)
    # print(TacticList)
    if(CheckComma == TRUE){
      TacticList[i] <- str_trim(TacticList[i], "left")#remove first space
      # print(TacticList[i])
      
      TacticList[i] <- list(strsplit(as.character(TacticList[i]), split = ", "))
      # print(TacticList[i])
      
    }
  }
  
  return(TacticList)
}


AssossAPTidWithTechTactics <- function(x, y){
  APTGroupTechList <- x
  TechdataDF <- y
  
  # APTGroupTechList <- APTtechIDListWithTactics
  # TechdataDF <- TechdataDF
  
  TacticList <- list()
  for(i in 1:length(APTGroupTechList)){
    for(j in 1:length(TechdataDF$TechID)){
      if(APTGroupTechList[i] == TechdataDF$TechID[j]){
        TacticList <- append(TacticList, TechdataDF$TechTactics[j])
      }else{
        next
      }
      
    }
  }
  APTtechIDListWithTactics <- do.call(rbind,
                                      Map(
                                        data.frame,
                                        APTTechID = c(APTGroupTechList),
                                        APTTechTactics = c(TacticList)
                                      ))
  
  return(APTtechIDListWithTactics)
}

APTTechCounterForTactics <- function(X, Y){
  TacticList <- X
  TacticsForAPTtechniques <- Y 
  
  # TacticList <- NoDuplicateTacticsFunc
  # TacticsForAPTtechniques <- APTtechIDListWithTactics$APTTechTactics
  
  # for(i in 1:length(TacticsForAPTtechniques)){
  #   CheckComma <- grepl(",", TacticsForAPTtechniques[i])
  #   # print(CheckComma)
  #   # print(TacticsForAPTtechniques)
  #   if(CheckComma == TRUE){
  #     TacticsForAPTtechniques[i] <- str_trim(TacticsForAPTtechniques[i], "left")#remove first space
  #     # print(TacticsForAPTtechniques[i])
  # 
  #     TacticsForAPTtechniques[i] <- list(strsplit(as.character(TacticsForAPTtechniques[i]), split = ", "))
  #     # print(TacticsForAPTtechniques[i])
  # 
  #   }
  # }
  
  for(i in 1:length(TacticsForAPTtechniques)){
    # print(typeof(TacticsForAPTtechniques[i][[1]]))
    
    if(typeof(TacticsForAPTtechniques[i][[1]]) == "list"){
      #print(TacticsForAPTtechniques[i][[1]][[1]])
      tempSTR <-paste(TacticsForAPTtechniques[i][[1]][[1]], collapse = ", ")
      TacticsForAPTtechniques[i] <- list(strsplit(as.character(tempSTR), split = ", "))
    }
    
    if(typeof(TacticsForAPTtechniques[i][[1]]) == "character"){
      TacticsForAPTtechniques[i] <-TacticsForAPTtechniques[i][[1]]
    }
  }
  

  Tactics <- list()
  Counter <- list()
  for(Tactic_s in TacticList){
    i <- 0
    for(Tactic2count in TacticsForAPTtechniques){
      # print(typeof(Tactic2count))
      
      if(typeof(Tactic2count) == "character"){
        if(Tactic2count == Tactic_s){
          i <- i + 1
        }
      }else if(typeof(Tactic2count) == "list"){
        # print(typeof(Tactic2count))
        # print(Tactic2count)
        # print("Persistence" %in% c(Tactic2count))
        # print(grepl("Persistence", Tactic2count, fixed = TRUE))
        if(grepl(Tactic_s, Tactic2count, fixed = TRUE)){
          # print("went it")
          i <- i + 1
        }
      }else{
        print("another type of variable appeared other than char or list")
      }
      
    }
    
    # print(Tactic_s)
    # print(i)
    
    Tactics <- append(Tactics, Tactic_s)
    Counter <- append(Counter, i)
    
  }
  
  AptNumberTechForTactic <- do.call(rbind,
                                    Map(
                                      data.frame,
                                      Tactics = c(Tactics),
                                      Count = c(Counter)
                                    ))
  
  return(AptNumberTechForTactic)
}


APTtacticCount <- function(x){
  
  APTtoLookup <-x
  
  TechDataDF <- TechDataDFcreation(TechData)
  NoDuplicateTechID = TechDataDF$TechID[!duplicated(TechDataDF$TechID)]
  NoDuplicateTechTactics = TechDataDF$TechTactics[!duplicated(TechDataDF$TechTactics)]
  
  NoDuplicateTechID <- as.data.frame(NoDuplicateTechID)
  NoDuplicateTechTactics <- as.data.frame(NoDuplicateTechTactics)
  
  NoDuplicateTechTacticsNoComboc <-   NoDuplicateTechTactics[!grepl(",", NoDuplicateTechTactics$NoDuplicateTechTactics),]
  NoDuplicateTechTacticsNoComboc <- as.data.frame(NoDuplicateTechTacticsNoComboc)
  
  FinalDataDFstart <- do.call(rbind,
                              Map(
                                data.frame,
                                Tactics = c(NoDuplicateTechTacticsNoComboc)
                              ))
  
  TechDataDF$TechTactics <- TurnComboTacticToList(TechDataDF$TechTactics)
  
  for(element in APTtoLookup){
    
    correspondingIDorName <- FindcorrespondingIDorName(element)
    # print(correspondingIDorName)
    # print(element)
    APTtechIDList <- FuncAPTtechIDList(element)
    
    APTtechListWithTactics <- filter(TechDataDF, TechID %in% APTtechIDList)
    #view(TechDataDF)
    #print(APTtechIDList)
    #print(APTtechListWithTactics)
    
    AptNumberTechForTactic <- APTTechCounterForTactics(NoDuplicateTechTacticsNoComboc$NoDuplicateTechTacticsNoComboc, APTtechListWithTactics$TechTactics)
    #print(AptNumberTechForTactic)
    FinalDataDFstart[correspondingIDorName] <- AptNumberTechForTactic$Count
    
    
  }
  rownames(FinalDataDFstart) <- FinalDataDFstart$Tactics
  FinalDataDFstart <- select(FinalDataDFstart, -c(Tactics))
  
  return(FinalDataDFstart)
}

addColumnToOtherDataframe <- function(x, y){
  TotaltacticsCount <- x
  ApttacticsCount <- y
  
  for(name in colnames(ApttacticsCount)){
    TotaltacticsCount[name] <- ApttacticsCount[name]
  }
  
  return(TotaltacticsCount)
}


BarCharFunc <- function(x){
  AptTacticsNumberOftechForEachTactic <- x
  

  AptTacticsNumberOftechForEachTactic <- AptTacticsNumberOftechForEachTactic %>% tidyr::pivot_longer(cols = Count:colnames(AptTacticsNumberOftechForEachTactic)[ncol(AptTacticsNumberOftechForEachTactic)])

  View(AptTacticsNumberOftechForEachTactic)
  ggplot(AptTacticsNumberOftechForEachTactic, aes(x = Tactics, y = value, fill = name))+
    geom_bar(stat = 'identity', position = 'dodge')+
    theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1))+
    geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25)

}


TotalTechTacticCounter <- function(x, y){
  NoDuplicateTacticsFunc <- x
  TechdataDF <- y
  
  TotalTechTacticCount <- list()
  for (Tactics in NoDuplicateTacticsFunc$NoDuplicateTactics) {
    i <- 0
    for(TechTactic in TechdataDF$TechTactics){
      if(Tactics == TechTactic){
        i <- i + 1
        
      }
    }
    TotalTechTacticCount <- append(TotalTechTacticCount, i)
  }
  
  TotalTechTacticCount <- as.data.frame(t(as.data.frame(TotalTechTacticCount)))
  return(TotalTechTacticCount)
}





runrprog <- function(x){
  APTtoLookup <- x
  APTlistcountatactics <- APTtacticCount(APTtoLookup)
  
  TechdataDF <- TechDataDFcreation(TechData)
  NoDuplicateTacticsFunc <- NoDuplicateTacticsFunc(TechdataDF)
  
  
  APTtechIDListWithTactics <- GetAPTTechList(GroupsData)
  APTtechIDListWithTactics <- AssossAPTidWithTechTactics(APTtechIDListWithTactics, TechdataDF)
  
  #view(APTtechIDListWithTactics)
  APTtechIDListWithTactics <- APTtechIDListWithTactics[!duplicated(APTtechIDListWithTactics$APTTechID), ]# remove duplicate techniques
  #view(APTtechIDListWithTactics)
  
  AptTacticsNumberOftechForEachTactic <- APTTechCounterForTactics(NoDuplicateTacticsFunc$NoDuplicateTactics, APTtechIDListWithTactics$APTTechTactics)
  AptTacticsNumberOftechForEachTactic <- addColumnToOtherDataframe(AptTacticsNumberOftechForEachTactic, APTlistcountatactics)
  #view(AptTacticsNumberOftechForEachTactic)
  
  BarCharFunc(AptTacticsNumberOftechForEachTactic)
  #return(AptTacticsNumberOftechForEachTactic)
}




APTtoLookup <- list("APT29", "APT33")
#runrprog(APTtoLookup)
########################
#testing

APTlistcountatactics <- APTtacticCount(APTtoLookup)

TechdataDF <- TechDataDFcreation(TechData)
NoDuplicateTacticsFunc <- NoDuplicateTacticsFunc(TechdataDF)


APTtechIDListWithTactics <- GetAPTTechList(GroupsData)
APTtechIDListWithTactics <- AssossAPTidWithTechTactics(APTtechIDListWithTactics, TechdataDF)

#view(APTtechIDListWithTactics)
APTtechIDListWithTactics <- APTtechIDListWithTactics[!duplicated(APTtechIDListWithTactics$APTTechID), ]# remove duplicate techniques
#view(APTtechIDListWithTactics)

AptTacticsNumberOftechForEachTactic <- APTTechCounterForTactics(NoDuplicateTacticsFunc$NoDuplicateTactics, APTtechIDListWithTactics$APTTechTactics)

totalTechTacticCount <- TotalTechTacticCounter(NoDuplicateTacticsFunc, TechdataDF)

AptTacticsNumberOftechForEachTactic <- data.frame(AptTacticsNumberOftechForEachTactic, totatl_TechTacticCount = totalTechTacticCount$V1)

AptTacticsNumberOftechForEachTactic <- addColumnToOtherDataframe(AptTacticsNumberOftechForEachTactic, APTlistcountatactics)
#view(AptTacticsNumberOftechForEachTactic)



BarCharFunc(AptTacticsNumberOftechForEachTactic)
#return(AptTacticsNumberOftechForEachTactic)






