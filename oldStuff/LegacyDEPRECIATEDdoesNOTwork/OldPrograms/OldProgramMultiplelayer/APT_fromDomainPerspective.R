library("fmsb")
library("rjson")
library("ggplot2")
library("tidyverse")
library("dplyr")
library("stringr")
library(data.table)

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
#TacticsData <- fullDataSet[["Data_Oct2022"]][["24-03-2023"]][["TacticsData"]]
#TechData <- fullDataSet[["Data_Oct2022"]][["24-03-2023"]][["Techniques"]]
#SoftData <- fullDataSet[["Data_Oct2022"]][["24-03-2023"]][["Software"]]
#GroupsData <- fullDataSet[["Data_Oct2022"]][["24-03-2023"]][["Groups"]]



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


run1st_Program <- function(x){
  fullDataSet <- x
  # TechData <- fullDataSet[["Data_Oct2022"]][["24-03-2023"]][["Techniques"]]
  # GroupsData <- fullDataSet[["Data_Oct2022"]][["24-03-2023"]][["Groups"]]
  
  TechData <- fullDataSet[["Data_Apr2023"]][["05-05-2023"]][["Techniques"]]
  GroupsData <- fullDataSet[["Data_Apr2023"]][["05-05-2023"]][["Groups"]]
  
  TechDataDF <- TechDataDFcreation(TechData)#raw data from TechData
  GroupDataDF <- GroupDataDFcreation(GroupsData)#raw data from GroupsData
  GroupDataDF <- FusionGroupDataDF_with_TechDataDF(GroupDataDF, TechDataDF)#Fusing the data of TechDataDF & GroupDataDF
  
  return(GroupDataDF)
}

GroupDataDF <- run1st_Program(fullDataSet)


APTlist <- GroupDataDF[!duplicated(GroupDataDF[c("Name", "ID")]), ]
APTlist <- APTlist[,as.vector(c("Name", "ID"))] #remove all column except choosen one
row.names(APTlist) <- 1:nrow(APTlist)#renumber the rows

DomainNoDuplicateList <- setNames(data.frame(GroupDataDF[!duplicated(GroupDataDF$Domain), ]$Domain), c("Domain"))
TacticsNoDuplicateList <- NoDuplicateTacticsFunc(GroupDataDF)


result <- list()
for(APT in APTlist$Name){
  TempDomainList <- list()
  for(DomainName in DomainNoDuplicateList$Domain){
    i <- 0
    for(DomainToCompare in filter(GroupDataDF, Name %in% APT)$Domain){
      if(DomainName == DomainToCompare){
        i <-i+1
      }
    }
    TempDomainList[[DomainName]] <- i
  }
  #print(TempDomainList)
  

  
  TempTacticList <- list()
  for(TacticName in TacticsNoDuplicateList$NoDuplicateTactics){
    j <- 0
    for(TacticToCompare in filter(GroupDataDF, Name %in% APT)$TechTactics){
      if(TacticName == TacticToCompare){
        j <-j+1
      }
      else if(typeof(TacticToCompare) == "list" & TacticName %in% TacticToCompare[[1]]){
        j <-j+1
      }
    }
    TempTacticList[[TacticName]] <- j
  }
  #print(TempTacticList)
  
  result[[APT]][["Domain"]] <- TempDomainList
  result[[APT]][["Tactics"]] <- TempTacticList
}


####
#part 2
####
test <- data.table()

for(apt in names(result)){
  # print(apt)
  domain <- t(result[[apt]][["Domain"]])
  tactics <- t(result[[apt]][["Tactics"]])
  new_value <- cbind(domain, tactics)
  
  test <- rbindlist(list(test, new_value))
  
}

# this make no sense i have to t() it twice to wake it for it to accept renaming 
# the rownames
colnames(test) = colnames(new_value)
test <- t(test)
test <- t(test)
rownames(test) = c(names(result))
test <- data.frame(test)


DomainAnalysis <- filter(test, ICS > 0 |Mobile > 0) #look for APT with at least one technique in ICS or Mobile because 
#only 4 APT with no tech in Enterprise

#Rare tactics Analysis As per requested and deduced from the bar chart
#Tactics with least amout of techniques are
#Evasion | Impact | Impair.Process.Control | Inhibit.Response.Function
TacticsAnalysis<- filter(test, Impact > 0 | Evasion > 0 | Impair.Process.Control > 0 | Inhibit.Response.Function > 0)




