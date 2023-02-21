library("rjson")
library("ggplot2")
library("tidyverse")
library("dplyr")

TotalData1 <- fromJSON(file="/Users/morzen/Desktop/work/RprogramForMitreScrapperDatanalysis/results/Rformat_TOTAL16-02-2023_00-12-41_Mitre_APT.json")
TechData2 <- fromJSON(file="/Users/morzen/Desktop/work/RprogramForMitreScrapperDatanalysis/results/Rformat_TECH16-02-2023_00-12-41_Mitre_APT.json")
SoftData3 <- fromJSON(file="/Users/morzen/Desktop/work/RprogramForMitreScrapperDatanalysis/results/Rformat_SOFT16-02-2023_00-12-41_Mitre_APT.json")

#NoDubAPTname = json_Techdata_frame2$APTname[!duplicated(json_Techdata_frame2$APTname)]
#NoDubID = json_Techdata_frame2$ID[!duplicated(json_Techdata_frame2$ID)]


Make_geomPoint <- function(x, Occur) { 
  Firstdataframe <- as.data.frame(x)
  
  ID<-Firstdataframe$ID
  APTname<-Firstdataframe$APTname
  GeomPointFunctionData1 <-data.frame(ID, APTname)
  
  #supposed to remove every techniques whose occurence is under 10
  GeomPointFunctionData1 <- GeomPointFunctionData1 %>% group_by(ID) %>% filter(n()>Occur)
  
  #graph before classifying occurence on x axis
  # ggplot(GeomPointFunctionData1, aes(ID, APTname)) +
  #   geom_point() +
  #   theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1))
  
  
  #supposed to classify techniques on x axis from biggest occurence to smallest
  GeomPointFunctionData2 <- data.frame(GeomPointFunctionData1)
  GeomPointFunctionData2 <- GeomPointFunctionData2 %>% add_count(ID) %>% arrange(desc(n))
  
  
  listnoduplicate = GeomPointFunctionData2$ID[!duplicated(GeomPointFunctionData2$ID)]
  ggplot(GeomPointFunctionData2, aes(x=factor(ID, level=c(listnoduplicate)), y=APTname)) +
    geom_point() +
    theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1))
}


Make_geomPoint(SoftData3, 2)

# Clear packages
p_unload(dplyr, tidyr, stringr) # Clear specific packages
p_unload(all)  # Easier: clears all add-ons
detach("package:datasets", unload = TRUE)  # For base

# Clear console
cat("\014")  # ctrl+L

