# Load the package required to read JSON files.
#install.packages("rjson") # Optional
library("rjson")
library("tidyverse")
library("ggplot2")

TotalData1 <- fromJSON(file="/Users/morzen/Desktop/work/RprogramForMitreScrapperDatanalysis/results/Rformat_TOTAL16-02-2023_00-12-41_Mitre_APT.json")
TechData2 <- fromJSON(file="/Users/morzen/Desktop/work/RprogramForMitreScrapperDatanalysis/results/Rformat_TECH16-02-2023_00-12-41_Mitre_APT.json")
SoftData3 <- fromJSON(file="/Users/morzen/Desktop/work/RprogramForMitreScrapperDatanalysis/results/Rformat_SOFT16-02-2023_00-12-41_Mitre_APT.json")

json_Totaldata_frame1 <- as.data.frame(TotalData1)
json_Techdata_frame2 <- as.data.frame(TechData2)
json_Softdata_frame3 <- as.data.frame(SoftData3)



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



# Clear packages
p_unload(dplyr, tidyr, stringr) # Clear specific packages
p_unload(all)  # Easier: clears all add-ons
detach("package:datasets", unload = TRUE)  # For base

# Clear console
cat("\014")  # ctrl+L

