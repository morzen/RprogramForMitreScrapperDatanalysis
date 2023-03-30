library("rjson")
library("ggplot2")
library("tidyverse")
library("dplyr")


GroupIDandName <- fromJSON(file=paste("/Users/morzen/Desktop/MitreAttackAPTscrapper/DataAnalysisAndformatingPythonScrip/results/GroupIDandName.json", sep = ""))


##################################################################################################
# Looking at occurence for tech soft and both alike
##################################################################################################
#NoDuplicateAPTname = json_Techdata_frame2$APTname[!duplicated(json_Techdata_frame2$APTname)]
#NoDuplicateID = json_Techdata_frame2$ID[!duplicated(json_Techdata_frame2$ID)]

#ID and Sodtware for gruops
Make_geomPoint_APTY <- function(x, Occur) { 
  Firstdataframe <- as.data.frame(x)
  
  ID<-Firstdataframe$ID
  APTname<-Firstdataframe$APTname
  GeomPointFunctionData <-data.frame(ID, APTname)
  
  #supposed to remove every techniques whose occurence is under 10
  GeomPointFunctionData1 <- GeomPointFunctionData %>% group_by(ID) %>% filter(n()>Occur)
  
  #supposed to classify techniques on x axis from biggest occurence to smallest
  GeomPointFunctionData2 <- data.frame(GeomPointFunctionData1)
  GeomPointFunctionData2 <- GeomPointFunctionData2 %>% add_count(ID) %>% arrange(desc(n))
  
  
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

Make_geomPoint_APTX <- function(x, Occur) { 
  Firstdataframe <- as.data.frame(x)
  
  ID<-Firstdataframe$ID
  APTname<-Firstdataframe$APTname
  GeomPointFunctionData <-data.frame(ID, APTname)
  
  #supposed to remove every techniques whose occurence is under 10
  GeomPointFunctionData1 <- GeomPointFunctionData %>% group_by(ID) %>% filter(n()>Occur)
  
  #supposed to classify techniques on x axis from biggest occurence to smallest
  GeomPointFunctionData2 <- data.frame(GeomPointFunctionData1)
  GeomPointFunctionData2 <- GeomPointFunctionData2 %>% add_count(ID) %>% arrange(desc(n))
  
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
Make_geomPoint_APTY(GroupIDandName, 10)

Make_geomPoint_APTX(GroupIDandName, 1)



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
  Firstdataframe <- as.data.frame(x)
  
  ID<-Firstdataframe$ID
  APTname<-Firstdataframe$APTname
  Tactics <- Firstdataframe$Tactic_s
  
  GeomPointFunctionData <-data.frame(ID, APTname)
  
  #supposed to remove every techniques whose occurence is under 10
  GeomPointFunctionData1 <- GeomPointFunctionData %>% group_by(ID) %>% filter(n()>Occur)
  
  #supposed to classify techniques on x axis from biggest occurence to smallest
  GeomPointFunctionData2 <- data.frame(GeomPointFunctionData1)
  GeomPointFunctionData2 <- GeomPointFunctionData2 %>% filter(grepl(RequestedTactic, GeomPointFunctionData2$Tactic_s))
  GeomPointFunctionData2 <- GeomPointFunctionData2 %>% add_count(ID) %>% arrange(desc(n))
  
  
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
Make_geomPoint_APTY_TacticBias(GroupIDandName, 0, 'Discovery')

Make_geomPoint_APTX_TacticBias(GroupIDandName, 0, 'Discovery')


clearAllExceptenvironment <- function(x, Occur) {
  # Clear packages
  #p_unload(dplyr, tidyr, stringr) # Clear specific packages
  p_unload(all)  # Easier: clears all add-ons
  detach("package:datasets", unload = TRUE)  # For base
  
  # Clear console
  cat("\014")  # ctrl+L
}
clearAllExceptenvironment()

