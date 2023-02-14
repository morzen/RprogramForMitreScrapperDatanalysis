# Load the package required to read JSON files.
install.packages("rjson") # Optional
library("rjson")
library(ggplot2)
# Give the input file name to the function.
TotalData1 <- fromJSON(file="/home/kali/Desktop/MitreAptScrapper/MitreAttackAPTscrapper/results/14-02-2023/Rformat_TOTAL14-02-2023_11-01-04_Mitre_APT.json")
# Print the result.
json_Totaldata_frame1 <- as.data.frame(TotalData1)
#head(json_data_frame)

#plot(json_data_frame$APTname, json_data_frame$ID)

ggplot(json_Totaldata_frame1, aes(reorder(ID, APTname), APTname)) +
  geom_point() +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1))

# Give the input file name to the function.
TechData2 <- fromJSON(file="/home/kali/Desktop/MitreAptScrapper/MitreAttackAPTscrapper/results/14-02-2023/Rformat_TECH14-02-2023_11-01-04_Mitre_APT.json")
# Print the result.
json_Techdata_frame2 <- as.data.frame(TechData2)
#head(json_data_frame2)

#plot(json_data_frame$APTname, json_data_frame$ID)
#reorder(APTname, ID), ID)
#reorder(ID, APTname), APTname)
ggplot(json_Techdata_frame2, aes(reorder(ID, APTname), APTname)) +
  geom_point() +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1))

# Give the input file name to the function.
SoftData3 <- fromJSON(file="/home/kali/Desktop/MitreAptScrapper/MitreAttackAPTscrapper/results/14-02-2023/Rformat_SOFT14-02-2023_11-01-04_Mitre_APT.json")
# Print the result.
json_Softdata_frame3 <- as.data.frame(SoftData3)
#head(json_data_frame3)

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

