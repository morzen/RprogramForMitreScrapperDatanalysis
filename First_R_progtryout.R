library("rjson")
library("ggplot2")
library("tidyverse")
library("dplyr")

#TotalData1 <- fromJSON(file="/Users/morzen/Desktop/work/RprogramForMitreScrapperDatanalysis/results/Rformat_TOTAL16-02-2023_00-12-41_Mitre_APT.json")
#TechData2 <- fromJSON(file="/Users/morzen/Desktop/work/RprogramForMitreScrapperDatanalysis/results/Rformat_TECH16-02-2023_00-12-41_Mitre_APT.json")
#SoftData3 <- fromJSON(file="/Users/morzen/Desktop/work/RprogramForMitreScrapperDatanalysis/results/Rformat_SOFT16-02-2023_00-12-41_Mitre_APT.json")


TotalData1 <- fromJSON(file="/home/morzen/Desktop/RprogramForMitreScrapperDatanalysis/results/Rformat_TOTAL16-02-2023_00-12-41_Mitre_APT.json")
TechData2 <- fromJSON(file="/home/morzen/Desktop/RprogramForMitreScrapperDatanalysis/results/Rformat_TECH16-02-2023_00-12-41_Mitre_APT.json")
SoftData3 <- fromJSON(file="/home/morzen/Desktop/RprogramForMitreScrapperDatanalysis/results/Rformat_SOFT16-02-2023_00-12-41_Mitre_APT.json")

#NoDubAPTname = json_Techdata_frame2$APTname[!duplicated(json_Techdata_frame2$APTname)]
#NoDubID = json_Techdata_frame2$ID[!duplicated(json_Techdata_frame2$ID)]


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
Make_geomPoint_APTY(TotalData1, 1)

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

