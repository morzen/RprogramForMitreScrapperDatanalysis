
library("plyr")
library("rjson")
library("ggplot2")
library("tidyverse")
library("dplyr")

# Mac path
MacPwd <- "/Users/morzen/Desktop/MitreAttackAPTscrapper/results/24-03-2023"
TacticsData <- fromJSON(file=paste(MacPwd,"/","Rformat_Tactics24-03-2023_12-13-48_Mitre_APT.json", sep = ""))
TechData <- fromJSON(file=paste(MacPwd,"/","Rformat_Techniques24-03-2023_12-13-48_Mitre_APT.json", sep = ""))
SoftData <- fromJSON(file=paste(MacPwd,"/","Rformat_Software24-03-2023_12-13-48_Mitre_APT.json", sep = ""))
GroupsData <- fromJSON(file=paste(MacPwd,"/","Rformat_Groups24-03-2023_12-13-48_Mitre_APT.json", sep = ""))


# Windows path
#WinPwd <-
#TacticsData <- 
#TechData <-
#SoftData <-
#GroupsData3 <-


########################
# TACTICS PART
########################
TacticsNameAndDatesTodatafame<- function(TacticsData) {
  Name <- list()
  Created <- list()
  Last_Modified <- list()
  
  
  for (Tactic in TacticsData) {
    title <- Tactic$title
    SumUpTable <- Tactic$SumUpTable
    
    TacticName <- title$KillchainStageName
    creationDate <- SumUpTable$Created
    LastModifiedDate <- SumUpTable$'Last Modified'
    
    Name <- append(Name, TacticName)
    Created <- append(Created, creationDate)
    Last_Modified <- append(Last_Modified, LastModifiedDate)
    
  }
  
  TacticsDates <-
    do.call(rbind,
            Map(
              data.frame,
              Name = c(Name),
              Created = c(Created),
              Last_Modified = c(Last_Modified)
            ))
  
  return(TacticsDates)
} 

TacticsDates <- TacticsNameAndDatesTodatafame(TacticsData)
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
              ID = c(ID),
              Created = c(Created),
              Last_Modified = c(Last_Modified)
            ))
  
  return(TechDates)
}

TechDates <- TechNameAndDatesTodatafame(TechData)
Created_ChronologicalOrder <- as.Date(TechDates$Created, format = "%d %B %Y")
LastMod_ChronologicalOrder <- as.Date(TechDates$Last_Modified, format = "%d %B %Y")

ggplot(TechDates)+
  geom_point(aes(x=factor(Created_ChronologicalOrder), y=factor(TechDates$ID)),colour="black")+
  geom_point(aes(x= factor(LastMod_ChronologicalOrder), y=factor(TechDates$ID)),colour="red")+
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


