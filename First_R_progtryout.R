
library("rjson")
library("ggplot2")
library("tidyverse")
library("dplyr")

# TotalData1 <- fromJSON(file="/home/morzen/Desktop/RprogramForMitreScrapperDatanalysis/results/Rformat_TOTAL16-02-2023_00-12-41_Mitre_APT.json")
TechData2 <- fromJSON(file="/home/morzen/Desktop/RprogramForMitreScrapperDatanalysis/results/Rformat_TECH16-02-2023_00-12-41_Mitre_APT.json")
SoftData3 <- fromJSON(file="/home/morzen/Desktop/RprogramForMitreScrapperDatanalysis/results/Rformat_SOFT16-02-2023_00-12-41_Mitre_APT.json")

json_Totaldata_frame1 <- as.data.frame(TotalData1)
json_Techdata_frame2 <- as.data.frame(TechData2)
json_Softdata_frame3 <- as.data.frame(SoftData3)


#plot(json_data_frame$APTname, json_data_frame$ID)
#reorder(APTname, ID), ID)
#reorder(ID, APTname), APTname)
ggplot(json_Totaldata_frame1, aes(reorder(ID, APTname), APTname)) +
  geom_point() +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1))

#plot(json_data_frame$APTname, json_data_frame$ID)
#reorder(APTname, ID), ID)
#reorder(ID, APTname), APTname)
ggplot(json_Techdata_frame2, aes(reorder(ID, APTname), APTname)) +
  geom_point() +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1))

#plot(json_data_frame$APTname, json_data_frame$ID)
#reorder(APTname, ID), ID)
#reorder(ID, APTname), APTname)
ggplot(json_Softdata_frame3, aes(reorder(ID, APTname), APTname)) +
  geom_point() +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1))








#NoDubAPTname = json_Techdata_frame2$APTname[!duplicated(json_Techdata_frame2$APTname)]
#NoDubID = json_Techdata_frame2$ID[!duplicated(json_Techdata_frame2$ID)]

ID<-json_Techdata_frame2$ID
APTname<-json_Techdata_frame2$APTname
testTechData<-data.frame(ID, APTname)

testTechData2 <- testTechData %>% group_by(ID) %>% filter(n()>4)

# ggplot(testTechData2, aes(ID, APTname)) +
#   geom_point() +
#   theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1))

testTechData3 <- data.frame(testTechData2)
testTechData3 <- testTechData3 %>% add_count(ID) %>% arrange(desc(n))

ggplot(testTechData3, aes(x=factor(ID), y=APTname)) +
  geom_point() +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1))



# Clear packages
p_unload(dplyr, tidyr, stringr) # Clear specific packages
p_unload(all)  # Easier: clears all add-ons
detach("package:datasets", unload = TRUE)  # For base

# Clear console
cat("\014")  # ctrl+L

