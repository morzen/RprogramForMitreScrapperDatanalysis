library("fmsb")
library("rjson")
library("ggplot2")
library("tidyverse")
library("dplyr")
library("plyr")
library("stringr")
library("data.table")
library("bnlearn")


FinalDataFrame <- read.csv("./1_DATAusedFolder/FinalDataFrame_Apr2024.csv")
  
################################################################################
# Scatterplot
################################################################################

CurrentDf <- FinalDataFrame %>% filter(Update_Date == "Data_Apr2024")

df <- CurrentDf

####################################
# Data handling and encoding
####################################
#OLD VERSION NOT FULL DATA

GroupsDF <- df %>%
  filter(PageType == "Groups") %>%
  filter(Tables_TableType %in% c("Techniques Used", "Software"))

TechDF <- df %>%
  filter(PageType == "Techniques")

# Filter Groups and Techniques data
#FULL DATA
# GroupsDF <- df %>% filter(PageType == "Groups") %>% filter(Tables_TableType %in% c("Techniques Used", "Software"))
# 
# TechDF <- df %>%
#   filter(PageType == "Techniques")

# Merge Tactic_s columns
TechDF$Tactic_s <- ifelse(is.na(TechDF$SumUpTable_Tactic), TechDF$SumUpTable_Tactics, TechDF$SumUpTable_Tactic)

# Drop NAs
# GroupsDF <- GroupsDF %>% select(where(~ !all(is.na(.))))
# TechDF <- TechDF %>% select(where(~ !all(is.na(.))))
GroupsDF <- GroupsDF[rowSums(is.na(GroupsDF)) != ncol(GroupsDF), ]
GroupsDF <- GroupsDF[, colSums(is.na(GroupsDF)) != nrow(GroupsDF)]

TechDF <- TechDF[rowSums(is.na(TechDF)) != ncol(TechDF), ]
TechDF <- TechDF[, colSums(is.na(TechDF)) != nrow(TechDF)]

# Extract unique Technique_ID and Tactic_s combinations
TacticDF <- data.frame(cbind(SumUpTable_ID = TechDF$SumUpTable_ID,
                             Tactic_s = TechDF$Tactic_s))

TacticDF <- TacticDF[!duplicated(TacticDF), ]

# Initialize an empty list to store Tactic_s values
TacticList <- list()

# Loop through Tables_ID values in GroupsDF
for (ID in GroupsDF$Tables_ID) {
  if (ID %in% TacticDF$SumUpTable_ID) {
    IndexForIDlist <- which(TacticDF$SumUpTable_ID == ID)
    
    if(!(length(IndexForIDlist) == 1)){
      print(IndexForIDlist)
      break
    }
    
    value <- TacticDF$Tactic_s[IndexForIDlist]
    TacticList <- append(TacticList, list(value))
    
  } else {
    value <- NA
    TacticList <- append(TacticList, list(value))
  }
}

# Add Tactic_s values to GroupsDF
GroupsDF$Tactic_s <- TacticList



# df <- do.call(rbind,
#               Map(
#                 data.frame,
#                 APTname = GroupsDF$Title_Name,
#                 Table_Types = GroupsDF$Tables_TableType,
#                 Techniques_N_Software = GroupsDF$Tables_ID,
#                 Tactic_s= GroupsDF$Tactic_s,
#                 Domain = GroupsDF$Tables_Domain
#               ))

df <- do.call(rbind,
              Map(
                data.frame,
                APTname = GroupsDF$Title_Name,
                Techniques_N_Software = GroupsDF$Tables_ID,
                Tactic_s= GroupsDF$Tactic_s,
                Domain = GroupsDF$Tables_Domain
              ))
row.names(df) <- seq_len(nrow(df))


df <- df %>%
  separate_rows(Tactic_s, sep = ", ")

###################################################################

####################################################################
#ID and Sodtware for gruops
Make_geomPoint_APTY_TacticBias <- function(x, Occur, RequestedDomain, RequestedTactic) {
  Firstdataframe <- as.data.frame(x)
  
  ID <- Firstdataframe$Techniques_N_Software
  APTname <- Firstdataframe$APTname
  Domain <- Firstdataframe$Domain
  Tactic_s <- Firstdataframe$Tactic_s
  
  
  GeomPointFunctionData <-data.frame(ID, APTname, Domain, Tactic_s)
  
  #supposed to remove every techniques whose occurence is under 10
  GeomPointFunctionData1 <- GeomPointFunctionData %>% group_by(ID) %>% filter(n()>Occur)
  
  #supposed to classify techniques on x axis from biggest occurence to smallest
  GeomPointFunctionData2 <- data.frame(GeomPointFunctionData1)
  GeomPointFunctionData2 <- GeomPointFunctionData2 %>% filter(grepl(RequestedDomain, GeomPointFunctionData2$Domain))
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

Make_geomPoint_APTX_TacticBias <- function(x, Occur, RequestedDomain, RequestedTactic) { 
  # x <- GroupIDandName
  # Occur <- 0
  # RequestedTactic <- "Discovery"
  
  Firstdataframe <- as.data.frame(x)
  
  ID <- Firstdataframe$Techniques_N_Software
  APTname <- Firstdataframe$APTname
  Domain <- Firstdataframe$Domain
  Tactic_s <- Firstdataframe$Tactic_s
  
  
  GeomPointFunctionData <-data.frame(ID, APTname, Domain, Tactic_s)
  
  #supposed to remove every techniques whose occurence is under 10
  GeomPointFunctionData1 <- GeomPointFunctionData %>% group_by(ID) %>% filter(n()>Occur)
  
  #supposed to classify techniques on x axis from biggest occurence to smallest
  GeomPointFunctionData2 <- data.frame(GeomPointFunctionData1)
  GeomPointFunctionData2 <- GeomPointFunctionData2 %>% filter(grepl(RequestedDomain, GeomPointFunctionData2$Domain))
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



#THERE ARE SETTINGS IN THE FUNCTION TO MAKE IT NOT SORTED OR SORTED ON 
#CHOSEN AXIS
#the int is for the number of occurence of techniques not apts so No of occurence of technique > x
Make_geomPoint_APTY_TacticBias(df, 10, '', '')

Make_geomPoint_APTX_TacticBias(df, 10, '', '')

