library("fmsb")
library("rjson")
library("ggplot2")
library("tidyverse")
library("dplyr")
library("stringr")

exit <- function() { invokeRestart("abort") } 

# kali path /home/morzen/Desktop/work/MitreAttackAPTscrapper/results/24-03-2023
# Mac path /Users/morzen/Desktop/MitreAttackAPTscrapper/results/24-03-2023
Pwd <- "/Users/morzen/Desktop/MitreAttackAPTscrapper/results/24-03-2023"
# TacticsData <- fromJSON(file=paste(Pwd,"/","Rformat_Tactics24-03-2023_12-13-48_Mitre_APT.json", sep = ""))
TechData <- fromJSON(file=paste(Pwd,"/","Rformat_Techniques24-03-2023_12-13-48_Mitre_APT.json", sep = ""))
# SoftData <- fromJSON(file=paste(Pwd,"/","Rformat_Software24-03-2023_12-13-48_Mitre_APT.json", sep = ""))
GroupsData <- fromJSON(file=paste(Pwd,"/","Rformat_Groups24-03-2023_12-13-48_Mitre_APT.json", sep = ""))


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


# TechDataDF <- TechDataDFcreation(TechData)
# NoDuplicateTechID = TechDataDF$TechID[!duplicated(TechDataDF$TechID)]
# NoDuplicateTechTactics = TechDataDF$TechTactics[!duplicated(TechDataDF$TechTactics)]
# 
# NoDuplicateTechID <- as.data.frame(NoDuplicateTechID)
# NoDuplicateTechTactics <- as.data.frame(NoDuplicateTechTactics)
# 
# NoDuplicateTechTacticsNoComboc <-   NoDuplicateTechTactics[!grepl(",", NoDuplicateTechTactics$NoDuplicateTechTactics),]
# NoDuplicateTechTacticsNoComboc <- as.data.frame(NoDuplicateTechTacticsNoComboc)
# 
# TechDataDF$TechTactics <- TurnComboTacticToList(TechDataDF$TechTactics)




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
  
  APTtechID <- APT[["HeadersAndTables"]][["Techniques Used"]][["ID"]]
  APTtechIDList <- list()
  for (element in APTtechID){
    # print(element)
    APTtechIDList <- append(APTtechIDList,element)
  }
  
  # view(APTtechIDList) ####to check up if techniques correspond
  return(APTtechIDList)
}


# APTtechIDList <- FuncAPTtechIDList("G0018")

# TechDataDF All techniques and the attributed tactics
# NoDuplciateTechTacticsNoComboc al existing tactics which will go on branch of spider graph
# APT techIDlist list of techniques for a given APT




# APTtechListWithTactics <- filter(TechDataDF, TechID %in% APTtechIDList)
# df_new <- TechDataDF[TechDataDF$TechID %in% APTtechIDList,]. #also work



# print(TechDataDF$TechTactics)
# print(APTtechIDList)

APTTechCounterForTactics <- function(X, Y){
  TacticList <- X
  TacticsForAPTtechniques <- Y 
  
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


RadargraphFunct <- function(AptNumberTechForTactic){
  # AptNumberTechForTactic <- test
  AllDataframeValues <- unlist(AptNumberTechForTactic)
  HighestOccurence <- max(unlist(AllDataframeValues))
  
  
  data <- t(AptNumberTechForTactic)
  
  
  data <- rbind(HighestOccurence,  0, data)
  
  # colnames(data) <- colnames(data)
  # rownames(data) <- row.names(data)
  
  radarchart(data.frame(data), axistype = 3, 
             plwd = 1, plty = 1
  )
  data2 <- rownames(data)
  data2 <- data2[-c(1,2)]
  # print(rownames(data))
  legend(x=1.5, y=1,
         legend = data2,
         bty = "n", pch=20 ,
         col = 1:length(data2),
         text.col = "black", cex=1.2, pt.cex=2.5)
  
}



RunProg2 <- function(x){
  listAptToLookup <- x
  
  TechDataDF <- TechDataDFcreation(TechData)
  NoDuplicateTechID = TechDataDF$TechID[!duplicated(TechDataDF$TechID)]
  NoDuplicateTechTactics = TechDataDF$TechTactics[!duplicated(TechDataDF$TechTactics)]
  
  NoDuplicateTechID <- as.data.frame(NoDuplicateTechID)
  NoDuplicateTechTactics <- as.data.frame(NoDuplicateTechTactics)
  
  NoDuplicateTechTacticsNoComboc <-   NoDuplicateTechTactics[!grepl(",", NoDuplicateTechTactics$NoDuplicateTechTactics),]
  NoDuplicateTechTacticsNoComboc <- as.data.frame(NoDuplicateTechTacticsNoComboc)
  
  #print(NoDuplicateTechTacticsNoComboc)
  FinalDataDFstart <- do.call(rbind,
                        Map(
                          data.frame,
                          Tactics = c(NoDuplicateTechTacticsNoComboc)
                        ))
  
  TechDataDF$TechTactics <- TurnComboTacticToList(TechDataDF$TechTactics)
  
  
  for(element in listAptToLookup){
    
    correspondingIDorName <- FindcorrespondingIDorName(element)
    # print(correspondingIDorName)
    # print(element)
    APTtechIDList <- FuncAPTtechIDList(element)
    
    APTtechListWithTactics <- filter(TechDataDF, TechID %in% APTtechIDList)
    
    AptNumberTechForTactic <- APTTechCounterForTactics(NoDuplicateTechTacticsNoComboc$NoDuplicateTechTacticsNoComboc, APTtechListWithTactics$TechTactics)
    #print(AptNumberTechForTactic)
    FinalDataDFstart[correspondingIDorName] <- AptNumberTechForTactic$Count
    
    
  }
  rownames(FinalDataDFstart) <- FinalDataDFstart$Tactics
  FinalDataDFstart <- select(FinalDataDFstart, -c(Tactics))
  # print(FinalDataDFstart)
  # RadargraphFunct(FinalDataDFstart)
  RadargraphFunct(FinalDataDFstart)
}

APTtoLookup <- list("Aoqin Dragon", "admin@338", "APT29")

RunProg2(APTtoLookup)


