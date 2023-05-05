library("rjson")
library("ggplot2")
library("tidyverse")
library("dplyr")




Pwd <- "/Users/morzen/Desktop/MitreAttackAPTscrapper/results/05-05-2023"
#TacticsData <- fromJSON(file=paste(Pwd,"/","Rformat_Tactics24-03-2023_12-13-48_Mitre_APT.json", sep = ""))
TechData <- fromJSON(file=paste(Pwd,"/","Rformat_Techniques05-05-2023_13-04-49_Mitre_APT.json", sep = ""))
#SoftData <- fromJSON(file=paste(Pwd,"/","Rformat_Software24-03-2023_12-13-48_Mitre_APT.json", sep = ""))
GroupsData <- fromJSON(file=paste(Pwd,"/","Rformat_Groups05-05-2023_13-04-49_Mitre_APT.json", sep = ""))

Techlist <- list()

print(typeof(TechData))

findTactics <- function(x, y){
  ID <- x
  Techdata <- y
  print(ID)
  
  Techlist <- list()
  
  for(element in names(Techdata)){
    Techlist <- append(Techlist, element)
  }
  
  if(ID %in% Techlist){
    
    if('Tactics' %in% names(Techdata[ID]['SumUpTable'])){
      Tactic_s <- Techdata[ID]['SumUpTable']['Tactics']
    }else if('Tactic' %in% names(Techdata[ID]['SumUpTable'])){
      Tactic_s <- Techdata[ID]['SumUpTable']['Tactic']
    }else{
      Tactic_s <- 'no tactic/s found'
    }
    
    return(Tactic_s)
  }
  
}

Create_GroupIDandName_DF <- function(x){
  
  GroupsfileData <- x
  
  TechAndSoftDict <- list()
  TechDict <- list()
  SoftDict <- list()
  
  for(page in GroupsfileData){
    APTID <- GroupsfileData[page]["title"]['ID']
    APTname <- GroupsfileData[page]["title"]['Name']
    
    for(tables in GroupsfileData[page]["HeadersAndTables"]){
      #  
      if(tables == "Techniques Used"){
        
        for( i in len(GroupsfileData[page]["HeadersAndTables"][tables]["ID"])){
          
          ID <- GroupsfileData[page]["HeadersAndTables"][tables]["ID"][i]
          Name <- GroupsfileData[page]["HeadersAndTables"][tables]["Name"][i]
          
          TechDict["APTID"] <- append(TechDict["APTID"], APTID)
          TechDict["APTname"] <- append(TechDict["APTname"], APTname)
          TechDict["ID"] <- append(TechDict["ID"], ID)
          TechDict["Name"] <- append(TechDict["Name"], Name)
          
          TechAndSoftDict["APTID"] <- append(TechAndSoftDict["APTID"], APTID)
          TechAndSoftDict["APTname"] <- append(TechAndSoftDict["APTname"], APTname)
          TechAndSoftDict["ID"] <- append(TechAndSoftDict["ID"], ID)
          TechAndSoftDict["Name"] <- append(TechAndSoftDict["Name"], Name)
          
          Tactic <- findTactics(ID, TechData)
          TechDict["Tactic_s"] <- append(TechDict["Tactic_s"], Tactic)
          TechAndSoftDict["Tactic_s"] <- append(TechAndSoftDict["Tactic_s"], Tactic)
        }
        
        
      }else if(tables == "Software"){
        
        for( i in len(GroupsfileData[page]["HeadersAndTables"][tables]["ID"])){
          
        } 
        
        
        
      }else if(!("Techniques Used" %in% names(GroupsfileData[page]["HeadersAndTables"]))){
        
        
      }else if(!("Software" %in% names(GroupsfileData[page]["HeadersAndTables"]))){
        
        
      }else if(!("Techniques Used" %in% names(GroupsfileData[page]["HeadersAndTables"])) & !("Software" %in% names(GroupsfileData[page]["HeadersAndTables"]))){
        
        
      }else{
        print("else")
      }
      
      #  
    }
    
    
  }
  
}



##################################################################################################
# Looking at occurence for tech with Tactics classification (can handle with soft lack of tactics message)
##################################################################################################

GroupIDandName <- fromJSON(file=paste("/Users/morzen/Desktop/MitreAttackAPTscrapper/DataAnalysisAndformatingPythonScrip/results/GroupIDandName.json", sep = ""))


#NoDuplicateAPTname = json_Techdata_frame2$APTname[!duplicated(json_Techdata_frame2$APTname)]
#NoDuplicateID = json_Techdata_frame2$ID[!duplicated(json_Techdata_frame2$ID)]

GroupIDandNamedf <- as.data.frame(GroupIDandName)
NoDuplicateTACTICS = GroupIDandNamedf$Tactic_s[!duplicated(GroupIDandNamedf$Tactic_s)]
NoDuplicateTACTICS <- as.data.frame(NoDuplicateTACTICS)

#ID and Sodtware for gruops
Make_geomPoint_APTY_TacticBias <- function(x, Occur, RequestedTactic) {
  Firstdataframe <- as.data.frame(x)
  
  ID<-Firstdataframe$ID
  APTname<-Firstdataframe$APTname
  Tactic_s <- Firstdataframe$Tactic_s
  
  GeomPointFunctionData <-data.frame(ID, APTname, Tactic_s)
  
  #supposed to remove every techniques whose occurence is under 10
  GeomPointFunctionData1 <- GeomPointFunctionData %>% group_by(ID) %>% filter(n()>Occur)
  
  #supposed to classify techniques on x axis from biggest occurence to smallest
  GeomPointFunctionData2 <- data.frame(GeomPointFunctionData1)
  GeomPointFunctionData2 <- GeomPointFunctionData2 %>% filter(grepl(RequestedTactic, GeomPointFunctionData2$Tactic_s))
  GeomPointFunctionData2 <- GeomPointFunctionData2 %>% add_count(ID) %>% arrange(desc(n))
  
  # comment the line below to remove the only string tactics option and have tactics combination
  GeomPointFunctionData2 <- GeomPointFunctionData2[!grepl(",", GeomPointFunctionData2$Tactic_s),]
  
  
  DataForYaxis <- GeomPointFunctionData %>% add_count(APTname) %>% arrange(desc(n))
  
  listnoduplicateY = DataForYaxis$APTname[!duplicated(DataForYaxis$APTname)]
  listnoduplicateX = GeomPointFunctionData2$ID[!duplicated(GeomPointFunctionData2$ID)]
  
  #not sorted just filtered
  # ggplot(GeomPointFunctionData1, aes(ID, APTname)) +
  #   geom_point() +
  #   theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1))
  
  #X axis sorted
  # ggplot(GeomPointFunctionData2, aes(x=factor(ID, level=c(listnoduplicateX)), y=APTname)) +
  #   geom_point() +
  #   theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1))
  
  #Y axis sorted
  # ggplot(GeomPointFunctionData2, aes(x=ID, y=factor(APTname, level=c(listnoduplicateY)))) +
  #   geom_point() +
  #   theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1))
  
  #both sorted
  ggplot(GeomPointFunctionData2, aes(x=factor(ID, level=c(listnoduplicateX)), y=factor(APTname, level=c(listnoduplicateY)))) +
    geom_point() +
    theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1))
}

Make_geomPoint_APTX_TacticBias <- function(x, Occur, RequestedTactic) { 
  # x <- GroupIDandName
  # Occur <- 0
  # RequestedTactic <- "Discovery"
  
  Firstdataframe <- as.data.frame(x)
  
  ID<-Firstdataframe$ID
  APTname<-Firstdataframe$APTname
  Tactic_s <- Firstdataframe$Tactic_s
  
  GeomPointFunctionData <-data.frame(ID, APTname, Tactic_s)
  
  #supposed to remove every techniques whose occurence is under Occur
  GeomPointFunctionData1 <- GeomPointFunctionData %>% group_by(ID) %>% filter(n()>Occur)
  
  #supposed to classify techniques on x axis from biggest occurence to smallest
  GeomPointFunctionData2 <- data.frame(GeomPointFunctionData1)
  GeomPointFunctionData2 <- GeomPointFunctionData2 %>% filter(grepl(RequestedTactic, GeomPointFunctionData2$Tactic_s))
  GeomPointFunctionData2 <- GeomPointFunctionData2 %>% add_count(ID) %>% arrange(desc(n))
  
  # comment the line below to remove the only string tactics option and have tactics combination
  GeomPointFunctionData2 <- GeomPointFunctionData2[!grepl(",", GeomPointFunctionData2$Tactic_s),]
  
  
  DataForXaxis <- GeomPointFunctionData %>% add_count(APTname) %>% arrange(desc(n))
  
  
  listnoduplicateX = DataForXaxis$APTname[!duplicated(DataForXaxis$APTname)]
  listnoduplicateY = GeomPointFunctionData2$ID[!duplicated(GeomPointFunctionData2$ID)]
  
  #nothing sorted just filtered
  # ggplot(GeomPointFunctionData1, aes(ID, APTname)) +
  #   geom_point() +
  #   theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1))
  
  #X axis sorted
  # ggplot(GeomPointFunctionData2, aes(x=factor(APTname, level=c(listnoduplicateX)), y=ID)) +
  #   geom_point() +
  #   theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1))
  
  #Y axis sorted
  # ggplot(GeomPointFunctionData2, aes(x=APTname, y=factor(ID, level=c(listnoduplicateY)))) +
  #   geom_point() +
  #   theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1))
  
  #both sorted
  ggplot(GeomPointFunctionData2, aes(x=factor(APTname, level=c(listnoduplicateX)), y=factor(ID, level=c(listnoduplicateY)))) +
    geom_point() +
    theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1))
}

#the int is for the number of occurence of techniques not apts
Make_geomPoint_APTY_TacticBias(GroupIDandName, 5, '')

Make_geomPoint_APTX_TacticBias(GroupIDandName, 2, '')


clearAllExceptenvironment <- function(x, Occur) {
  # Clear packages
  #p_unload(dplyr, tidyr, stringr) # Clear specific packages
  p_unload(all)  # Easier: clears all add-ons
  detach("package:datasets", unload = TRUE)  # For base
  
  # Clear console
  cat("\014")  # ctrl+L
}
clearAllExceptenvironment()


