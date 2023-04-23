library("rjson")
library("ggplot2")
library("tidyverse")
library("dplyr")

# Mac path
MacPwd <- "/Users/morzen/Desktop/MitreAttackAPTscrapper/results/21-03-2023"
TacticsData <- fromJSON(file=paste(MacPwd,"/","Rformat_Tactics21-03-2023_23-51-34_Mitre_APT.json", sep = ""))
TechData <- fromJSON(file=paste(MacPwd,"/","Rformat_Techniques21-03-2023_11-15-50_Mitre_APT.json", sep = ""))
SoftData <- fromJSON(file=paste(MacPwd,"/","Rformat_Software21-03-2023_11-15-50_Mitre_APT.json", sep = ""))
GroupsData <- fromJSON(file=paste(MacPwd,"/","Rformat_Groups21-03-2023_11-15-50_Mitre_APT.json", sep = ""))


# Windows path
#WinPwd <-
#TacticsData <- 
#TechData <-
#SoftData <-
#GroupsData3 <-



DataExtraction <- function(x) { 
  list1 <- list()
  list2 <- list()
  
  for (items in x){
    print(items)
    
    for (item1 in items$HeadersAndTables$`Techniques Used`$ID){
      print(item1)
      list1[[length(list1)+1]] = item1
    }
    
    for (item2 in items$HeadersAndTables$`Techniques Used`$Name){
      print(item2)
      list2[[length(list2)+1]] = item2
    }
    
  }
  print(length(list1))
  print(length(list2))
  dataframe <- as.data.frame(list1, list2)
  return (dataframe)
}

#DataExtraction(GroupsData)
IDtests <- DataExtraction(GroupsData)





GroupIDandName <- fromJSON(file=paste("/Users/morzen/Desktop/MitreAttackAPTscrapper/DataAnalysisAndformatingPythonScrip/results/GroupIDandName.json", sep = ""))

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
Make_geomPoint_APTY(TotalData1, 10)

Make_geomPoint_APTX(TotalData1, 1)







clearAllExceptenvironment <- function(x, Occur) {
  # Clear packages
  #p_unload(dplyr, tidyr, stringr) # Clear specific packages
  p_unload(all)  # Easier: clears all add-ons
  detach("package:datasets", unload = TRUE)  # For base
  
  # Clear console
  cat("\014")  # ctrl+L
}
clearAllExceptenvironment()

