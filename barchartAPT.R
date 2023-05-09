library("fmsb")
library("rjson")
library("ggplot2")
library("tidyverse")
library("dplyr")
library("stringr")

exit <- function() { invokeRestart("abort") } 


# kali path /home/morzen/Desktop/work/MitreAttackAPTscrapper/results/24-03-2023
# Mac path /Users/morzen/Desktop/MitreAttackAPTscrapper/results/24-03-2023
Pwd <- "/Users/morzen/Desktop/MitreAttackAPTscrapper/results/05-05-2023"
# TacticsData <- fromJSON(file=paste(Pwd,"/","Rformat_Tactics24-03-2023_12-13-48_Mitre_APT.json", sep = ""))
TechData <- fromJSON(file=paste(Pwd,"/","Rformat_Techniques05-05-2023_13-04-49_Mitre_APT.json", sep = ""))
# SoftData <- fromJSON(file=paste(Pwd,"/","Rformat_Software24-03-2023_12-13-48_Mitre_APT.json", sep = ""))
GroupsData <- fromJSON(file=paste(Pwd,"/","Rformat_Groups05-05-2023_13-04-49_Mitre_APT.json", sep = ""))




GetAPTTechList <- function(x){
  GroupsData <- x
  APTTechList <- list()
  
  for(PageContent in GroupsData){
    APTTechList <- append(APTTechList, PageContent$HeadersAndTables$`Techniques Used`$ID)
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
  
  for(i in 1:length(TacticsForAPTtechniques)){
    CheckComma <- grepl(",", TacticsForAPTtechniques[i])
    if (CheckComma == TRUE){
      TacticsForAPTtechniques[i] <- str_trim(TacticsForAPTtechniques[i], "left")#remove first space
      # print(TacticsForAPTtechniques[i])

      TacticsForAPTtechniques[i] <- list(strsplit(as.character(TacticsForAPTtechniques[i]), split = ", "))
      # print(TacticsForAPTtechniques[i])

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

BarCharFunc <- function(x){
  AptTacticsNumberOftechForEachTactic <- x
  # Create the data for the chart
  H <- c(AptTacticsNumberOftechForEachTactic$Count)
  M <- c(AptTacticsNumberOftechForEachTactic$Tactics)
  
  par(mar=c(11,4,4,4))
  my_bar <- barplot(height=H,
                    names.arg=M, las=2 )
  
  text(my_bar, paste("n: ", H, sep="") ,cex=1) 
  
  
  
  p<-ggplot(data=AptTacticsNumberOftechForEachTactic, aes(x=Tactics, y=Count))+
    geom_bar(stat="identity", fill="steelblue")+
    geom_text(aes(label=Count), vjust=-0.3, size=3.5)+
    theme_minimal()+
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
  p
}


runrprog <- function(){
  
  TechdataDF <- TechDataDFcreation(TechData)
  NoDuplicateTacticsFunc <- NoDuplicateTacticsFunc(TechdataDF)
  
  
  APTtechIDListWithTactics <- GetAPTTechList(GroupsData)
  APTtechIDListWithTactics <- AssossAPTidWithTechTactics(APTtechIDListWithTactics, TechdataDF)
  
  
  
  AptTacticsNumberOftechForEachTactic <- APTTechCounterForTactics(NoDuplicateTacticsFunc$NoDuplicateTactics, APTtechIDListWithTactics$APTTechTactics)
  
  BarCharFunc(AptTacticsNumberOftechForEachTactic)
  
}

runrprog()
