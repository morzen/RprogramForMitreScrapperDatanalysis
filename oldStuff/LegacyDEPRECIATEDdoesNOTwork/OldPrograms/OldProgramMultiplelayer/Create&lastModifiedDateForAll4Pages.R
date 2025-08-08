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




TacticsData <- fullDataSet[["Data_Oct2022"]][["24-03-2023"]][["TacticsData"]]
TechData <- fullDataSet[["Data_Oct2022"]][["24-03-2023"]][["Techniques"]]
SoftData <- fullDataSet[["Data_Oct2022"]][["24-03-2023"]][["Software"]]
GroupsData <- fullDataSet[["Data_Oct2022"]][["24-03-2023"]][["Groups"]]


########################
# TACTICS PART
########################
TacticsNameAndDatesTodatafame<- function(TacticsData) {
  TacticsDatalist <- list()
  
  for (Tactic in names(TacticsData)) {
    # print(Tactic)
    # print(TacticsData[[Tactic]][["title"]][["KillchainStageName"]])
    # print(TacticsData[[Tactic]][["SumUpTable"]][["Created"]])
    # print(TacticsData[[Tactic]][["SumUpTable"]][["Last Modified"]])
    
    TacticsDatalist[["ID"]] <- append(TacticsDatalist[["ID"]], Tactic)
    TacticsDatalist[["Domain"]] <- append(TacticsDatalist[["Domain"]], TacticsData[[Tactic]][["title"]][["Domain"]])
    TacticsDatalist[["Name"]] <- append(TacticsDatalist[["Name"]], TacticsData[[Tactic]][["title"]][["KillchainStageName"]])
    TacticsDatalist[["Created"]] <- append(TacticsDatalist[["Created"]], TacticsData[[Tactic]][["SumUpTable"]][["Created"]])
    TacticsDatalist[["Last_Modified"]] <- append(TacticsDatalist[["Last_Modified"]], TacticsData[[Tactic]][["SumUpTable"]][["Last Modified"]])
    
  }

  TacticsDates <- as.data.frame(TacticsDatalist)
  
  return(TacticsDates)
} 

TacticsDates <- TacticsNameAndDatesTodatafame(TacticsData)
#filtering domain, remove if you don't want the filtering
TacticsDates <- TacticsDates[grepl("Enterprise", TacticsDates$Domain),]
Created_ChronologicalOrder <- as.Date(TacticsDates$Created, format = "%d %B %Y")
LastMod_ChronologicalOrder <- as.Date(TacticsDates$Last_Modified, format = "%d %B %Y")

ggplot(TacticsDates)+
  geom_point(aes(x=factor(Created_ChronologicalOrder), y=factor(TacticsDates$Name)),colour="black")+
  geom_point(aes(x= factor(LastMod_ChronologicalOrder), y=factor(TacticsDates$Name)),colour="red")+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1))


########################
# TECHNIQUES PART
########################
TechNameAndDatesTodatafame<- function(TechData) {
  
  
  TechDatalist <- list()
  
  for (Tech in names(TechData)) {
    # print(Tech)
    # print(TechData[[Tech]][["title"]][["KillchainStageName"]])
    # print(TechData[[Tech]][["SumUpTable"]][["Created"]])
    # print(TechData[[Tech]][["SumUpTable"]][["Last Modified"]])
    
    TechDatalist[["ID"]] <- append(TechDatalist[["ID"]], Tech)
    TechDatalist[["Domain"]] <- append(TechDatalist[["Domain"]], TechData[[Tech]][["title"]][["Domain"]])
    TechDatalist[["Name"]] <- append(TechDatalist[["Name"]], TechData[[Tech]][["title"]][["KillchainStageName"]])
    TechDatalist[["Created"]] <- append(TechDatalist[["Created"]], TechData[[Tech]][["SumUpTable"]][["Created"]])
    TechDatalist[["Last_Modified"]] <- append(TechDatalist[["Last_Modified"]], TechData[[Tech]][["SumUpTable"]][["Last Modified"]])
    
  }
  
  TechDates <- as.data.frame(TechDatalist)
  
  return(TechDates)
} 
  

TechDates <- TechNameAndDatesTodatafame(TechData)
#filtering domain, remove if you don't want the filtering
TechDates <- TechDates[grepl("Enterprise", TechDates$Domain),]

Created_ChronologicalOrder <- as.Date(TechDates$Created, format = "%d %B %Y")
LastMod_ChronologicalOrder <- as.Date(TechDates$Last_Modified, format = "%d %B %Y")

ggplot(TechDates)+
  geom_point(aes(x=factor(Created_ChronologicalOrder), y=factor(.data[["ID"]])),colour="black")+
  geom_point(aes(x= factor(LastMod_ChronologicalOrder), y=factor(.data[["ID"]])),colour="red")+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1))

ggplot(TechDates)+
  geom_point(aes(x=factor(.data[["ID"]]), y=factor(Created_ChronologicalOrder)),colour="black")+
  geom_point(aes(x=factor(.data[["ID"]]), y=factor(LastMod_ChronologicalOrder)),colour="red")+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1))


########################
# SOFTWARE PART
########################

SoftNameAndDatesTodatafame<- function(SoftData) {
  Name <- list()
  Created <- list()
  Last_Modified <- list()
  
  
  for (Soft in SoftData) {
    title <- Soft$title
    SumUpTable <- Soft$SumUpTable
    
    SoftName <- title$Name
    creationDate <- SumUpTable$Created
    LastModifiedDate <- SumUpTable$'Last Modified'
    
    Name <- append(Name, SoftName)
    Created <- append(Created, creationDate)
    Last_Modified <- append(Last_Modified, LastModifiedDate)
    
  }
  
  SoftDates <-
    do.call(rbind,
            Map(
              data.frame,
              Name = c(Name),
              Created = c(Created),
              Last_Modified = c(Last_Modified)
            ))
  
  return(SoftDates)
}

SoftDates <- SoftNameAndDatesTodatafame(SoftData)
Created_ChronologicalOrder <- as.Date(SoftDates$Created, format = "%d %B %Y")
LastMod_ChronologicalOrder <- as.Date(SoftDates$Last_Modified, format = "%d %B %Y")

ggplot(SoftDates)+
  geom_point(aes(x=factor(Created_ChronologicalOrder), y=factor(SoftDates$Name)),colour="black")+
  geom_point(aes(x= factor(LastMod_ChronologicalOrder), y=factor(SoftDates$Name)),colour="red")+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1))



########################
# GROUP PART
########################
GroupsNameAndDatesTodatafame<- function(GroupsData) {
  Name <- list()
  Created <- list()
  Last_Modified <- list()
  
  
  for (APT in GroupsData) {
    title <- APT$title
    SumUpTable <- APT$SumUpTable
    
    AptName <- title$Name
    creationDate <- SumUpTable$Created
    LastModifiedDate <- SumUpTable$'Last Modified'
    
    Name <- append(Name, AptName)
    Created <- append(Created, creationDate)
    Last_Modified <- append(Last_Modified, LastModifiedDate)
    
  }
  
  GroupsDates <-
    do.call(rbind,
            Map(
              data.frame,
              Name = c(Name),
              Created = c(Created),
              Last_Modified = c(Last_Modified)
            ))
  
  return(GroupsDates)
} 

GroupsDates <- GroupsNameAndDatesTodatafame(GroupsData)
Created_ChronologicalOrder <- as.Date(GroupsDates$Created, format = " %d %B %Y")
LastMod_ChronologicalOrder <- as.Date(GroupsDates$Last_Modified, format = " %d %B %Y")

ggplot(GroupsDates)+
  geom_point(aes(x=factor(Created_ChronologicalOrder), y=factor(GroupsDates$Name)),colour="black")+
  geom_point(aes(x= factor(LastMod_ChronologicalOrder), y=factor(GroupsDates$Name)),colour="red")+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1))


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


