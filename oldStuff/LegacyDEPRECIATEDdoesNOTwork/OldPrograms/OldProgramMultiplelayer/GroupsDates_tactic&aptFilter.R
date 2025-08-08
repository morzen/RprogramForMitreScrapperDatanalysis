library("plyr")
library("rjson")
library("ggplot2")
library("tidyverse")
library("dplyr")


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
    
    fullDataSet[[VersionOfMitreUpdate]][[ScrapDate]][[datatype]] <- append(fullDataSet[[VersionOfMitreUpdate]][[ScrapDate]][[datatype]], fromJSON(file=Pwd))
    
  }
  
}

#TacticsData <- fullDataSet[["Data_Oct2022"]][["24-03-2023"]][["TacticsData"]]
TechData <- fullDataSet[["Data_Oct2022"]][["24-03-2023"]][["Techniques"]]
#SoftData <- fullDataSet[["Data_Oct2022"]][["24-03-2023"]][["Software"]]
GroupsData <- fullDataSet[["Data_Oct2022"]][["24-03-2023"]][["Groups"]]

########################
# GET everything for when no filter
########################
TechNameAndDatesTodatafame<- function(TechData) {
  ID <- list()
  Created <- list()
  Last_Modified <- list()
  
  
  for (Tech in TechData) {
    title <- Tech$title
    SumUpTable <- Tech$SumUpTable
    
    TechID <- title$ID
    creationDate <- SumUpTable$Created
    LastModifiedDate <- SumUpTable$'Last Modified'
    
    ID <- append(ID, TechID)
    Created <- append(Created, creationDate)
    Last_Modified <- append(Last_Modified, LastModifiedDate)
    
  }
  
  TechDates <-
    do.call(rbind,
            Map(
              data.frame,
              TechID = c(ID),
              Created = c(Created),
              Last_Modified = c(Last_Modified)
            ))
  
  return(TechDates)
}



########################
# GET TACTICS FOR TECHNIQUES PART
########################
TechDataDFcreation <- function(TechData) {
  
  templist <- list()
  
  TechID <- list()
  TechTactics <- list()
  
  for (Tech in names(TechData)){
    templist[["TechID"]] <- append(templist[["TechID"]], TechData[[Tech]][["SumUpTable"]][["ID"]])
    
    if ("Tactic" %in% names(TechData[[Tech]][["SumUpTable"]])){
      templist[["TechTactics"]] <- append(templist[["TechTactics"]], TechData[[Tech]][["SumUpTable"]][["Tactic"]])
    }
    else if("Tactics" %in% names(TechData[[Tech]][["SumUpTable"]])){
      templist[["TechTactics"]] <- append(templist[["TechTactics"]], TechData[[Tech]][["SumUpTable"]][["Tactics"]])
    }else{
      message("going to the else")
    }
    
    templist[["TechDomain"]] <- append(templist[["TechDomain"]], TechData[[Tech]][["title"]][["Domain"]])
    
  }
  
  
  TechDataDF <- do.call(rbind,
                        Map(
                          data.frame,
                          TechID = c(str_trim(templist[["TechID"]], "left")),
                          TechTactics = c(str_trim(templist[["TechTactics"]], "left")),
                          TechDomain = c(str_trim(templist[["TechDomain"]], "left"))
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


getAlltechAndTacticsAssociated <- function(x){
  TechData <- x
  TechDataDF <- TechDataDFcreation(TechData)
  NoDuplicateTechID = TechDataDF$TechID[!duplicated(TechDataDF$TechID)]
  NoDuplicateTechTactics = TechDataDF$TechTactics[!duplicated(TechDataDF$TechTactics)]
  
  NoDuplicateTechID <- as.data.frame(NoDuplicateTechID)
  NoDuplicateTechTactics <- as.data.frame(NoDuplicateTechTactics)
  
  NoDuplicateTechTacticsNoComboc <-   NoDuplicateTechTactics[!grepl(",", NoDuplicateTechTactics$NoDuplicateTechTactics),]
  NoDuplicateTechTacticsNoComboc <- as.data.frame(NoDuplicateTechTacticsNoComboc)
  
 
  TechDataDF$TechTactics <- TurnComboTacticToList(TechDataDF$TechTactics)
  
  return(TechDataDF)
  
}

########################
# tools for filtering
########################

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


########################
# filtering PART
########################


filterChoosenAPTtech <- function(x, y){
  APTnameOrID <- x
  TechDataDF <- y
  
  tempDF <- data.frame()
  
  for(element in APTnameOrID){
    
    correspondingIDorName <- FindcorrespondingIDorName(element)
    # print(correspondingIDorName)
    # print(element)
    APTtechIDList <- FuncAPTtechIDList(element)
    
    APTtechListWithTactics <- filter(TechDataDF, TechID %in% APTtechIDList)
    rownames(APTtechListWithTactics) <- 1:nrow(APTtechListWithTactics)
    APTtechListWithTactics <- cbind(APTtechListWithTactics, APTidAndName=correspondingIDorName)
    APTtechListWithTactics <- APTtechListWithTactics %>%
      select(APTidAndName, everything())
   
    
    tempDF <- rbind(tempDF,APTtechListWithTactics)
  }
  return(tempDF)
}


filterChoosenTactics <- function(x, y){
  TacticName <- x
  TechDataDF <- y
  
  tempDF <- data.frame()
  
  for(element in TacticName){
    tempVar <- data.frame()
    tempVar <- TechDataDF %>% filter(grepl(element, TechDataDF$TechTactics))
    
    tempDF <- rbind(tempDF,tempVar)
  }
  
  tempDF = tempDF[!duplicated(tempDF$TechID),]
  rownames(tempDF) <- 1:nrow(tempDF)
 
  return(tempDF)
}
  
########################
# Get Date for techniques
########################

GetDatesForTechniques <- function(x){
  FilteredOutputData <- x
  
  Created <- list()
  LastModif <- list()
  
  for(element in FilteredOutputData[["TechID"]]){
    datesCreated <- paste(TechData[[element]][["SumUpTable"]][["Created"]])
    datesLastModif <- paste(TechData[[element]][["SumUpTable"]][["Last Modified"]])

    Created <- append(Created, datesCreated)
    LastModif <- append(LastModif, datesLastModif)
  }
  
  FilteredOutputData$Created = paste(Created)
  FilteredOutputData$Last_Modified = paste(LastModif)
  return(FilteredOutputData)
}


########################
# making the graph
########################

makethegraph <- function(x){
  data <- x
  #print(typeof(x$Created))
  Created_ChronologicalOrder <- as.Date(data$Created, format = "%d %B %Y")
  LastMod_ChronologicalOrder <- as.Date(data$Last_Modified, format = "%d %B %Y")
  
  ggplot(data)+
    geom_point(aes(x=factor(Created_ChronologicalOrder), y=factor(TechID)),colour="black")+
    geom_point(aes(x= factor(LastMod_ChronologicalOrder), y=factor(TechID)),colour="red")+
    theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1))
  
}



runProg <- function(a, b, c){
  APTtoLookup <- a
  TacticsLookup <- b
  Domain_filtering <- c
  AllTechWithTacticAssociated <- getAlltechAndTacticsAssociated(TechData)
  #filtering domain right below
  AllTechWithTacticAssociated <- AllTechWithTacticAssociated[grepl(Domain_filtering, AllTechWithTacticAssociated$TechDomain),]
  view(AllTechWithTacticAssociated)
  
  if(length(a) == 0 & length(b) == 0){#no filter
    print(1)
    TechDates <- TechNameAndDatesTodatafame(TechData)
    Created_ChronologicalOrder <- as.Date(TechDates$Created, format = "%d %B %Y")
    LastMod_ChronologicalOrder <- as.Date(TechDates$Last_Modified, format = "%d %B %Y")
    TechDates$Created = Created_ChronologicalOrder
    TechDates$Last_Modified = LastMod_ChronologicalOrder
    
    outputdata <- TechDates
    
  }else if(length(a) > 0 & length(b) == 0){#filtering by APT name
    print(2)
    
    outputdata <- filterChoosenAPTtech(APTtoLookup, AllTechWithTacticAssociated)
    
  }else if(length(a) == 0 & length(b) > 0){#filtering by tactics
    print(3)
    
    outputdata <- filterChoosenTactics(TacticsLookup, AllTechWithTacticAssociated)
    
  }else if(length(a) > 0 & length(b) > 0){#filtering by both 
    print(4)
    
    outputdata1 <- filterChoosenAPTtech(APTtoLookup, AllTechWithTacticAssociated)
    outputdata2 <- filterChoosenTactics(TacticsLookup, AllTechWithTacticAssociated)
   
    outputdata <- outputdata1[ outputdata1$TechID %in% outputdata2$TechID, ]
    
  }else{
    print("not supposed to happen")
  }
  
  
  DatesForTechniques <- GetDatesForTechniques(outputdata)
  
  #view(DatesForTechniques)
  makethegraph(DatesForTechniques)
}

APTtoLookup <- list()
TacticsLookup <- list("Reconnaissance")
Domain_filtering <- list("")
runProg(APTtoLookup, TacticsLookup, Domain_filtering)



########################
# END
########################
clearAllExceptenvironment <- function(x, Occur) {
  # Clear packages
  #p_unload(dplyr, tidyr, stringr) # Clear specific packages
  p_unload(all)  # Easier: clears all add-ons
  detach("package:datasets", unload = TRUE)  # For base
  
  # Clear console
  cat("\014")  # ctrl+L
}
clearAllExceptenvironment()


