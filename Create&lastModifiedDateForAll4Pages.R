library("plyr")
library("rjson")
library("ggplot2")
library("tidyverse")
library("dplyr")

# kali path /home/morzen/Desktop/work/MitreAttackAPTscrapper/results/24-03-2023
# Mac path /Users/morzen/Desktop/MitreAttackAPTscrapper/results/24-03-2023

Pwd <- "/Users/morzen/Desktop/MitreAttackAPTscrapper/results/Data_Oct2022/24-03-2023"

TacticsData <- fromJSON(file=paste(Pwd,"/","Rformat_Tactics24-03-2023_12-13-48_Mitre_APT.json", sep = ""))
TechData <- fromJSON(file=paste(Pwd,"/","Rformat_Techniques24-03-2023_12-13-48_Mitre_APT.json", sep = ""))
SoftData <- fromJSON(file=paste(Pwd,"/","Rformat_Software24-03-2023_12-13-48_Mitre_APT.json", sep = ""))
GroupsData <- fromJSON(file=paste(Pwd,"/","Rformat_Groups24-03-2023_12-13-48_Mitre_APT.json", sep = ""))



########################
# tessttt below
########################


########################
# Tesstttt above
########################


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
  
  
#   ID <- list()
#   Created <- list()
#   Last_Modified <- list()
#   
#   
#   for (Tech in TechData) {
#     title <- Tech$title
#     SumUpTable <- Tech$SumUpTable
#     
#     TechID <- title$ID
#     creationDate <- SumUpTable$Created
#     LastModifiedDate <- SumUpTable$'Last Modified'
#     
#     ID <- append(ID, TechID)
#     Created <- append(Created, creationDate)
#     Last_Modified <- append(Last_Modified, LastModifiedDate)
#     
#   }
#   
#   TechDates <-
#     do.call(rbind,
#             Map(
#               data.frame,
#               ID = c(ID),
#               Created = c(Created),
#               Last_Modified = c(Last_Modified)
#             ))
#   
#   return(TechDates)
# }

TechDates <- TechNameAndDatesTodatafame(TechData)
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


