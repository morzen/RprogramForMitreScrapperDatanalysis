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
# BaysNetwork 
################################################################################
df <- FinalDataFrame

####################################
# Data handling and encoding
####################################
# Filter Groups and Techniques data
GroupsDF <- df %>%
  filter(Update_Date == "Data_Apr2023", PageType == "Groups") %>%
  filter(Tables_TableType %in% c("Techniques Used", "Software"))

TechDF <- df %>%
  filter(Update_Date == "Data_Apr2023", PageType == "Techniques")

# Merge Tactic_s columns
TechDF$Tactic_s <- ifelse(is.na(TechDF$SumUpTable_Tactic), TechDF$SumUpTable_Tactics, TechDF$SumUpTable_Tactic)

# Drop NAs
GroupsDF <- GroupsDF %>% select(where(~ !all(is.na(.))))
TechDF <- TechDF %>% select(where(~ !all(is.na(.))))

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



df <- do.call(rbind,
              Map(
                data.frame,
                APTname = GroupsDF$Title_Name,
                Table_Types = GroupsDF$Tables_TableType,
                Techniques_N_Software = GroupsDF$Tables_ID,
                Tactic_s= GroupsDF$Tactic_s,
                Domain = GroupsDF$Tables_Domain
              ))
row.names(df) <- seq_len(nrow(df))


#################
#counters column

#totalcounter column
tempDF <- df %>%
  group_by(APTname) %>%
  mutate(TotalCounter = paste0(APTname, "_", 1:length(APTname))) %>%
  ungroup()



#totalcounter column
APTnamesNoDuplicate <- unique(df$APTname)
tempDF$ValueCounter <- df$APTname

for(name in APTnamesNoDuplicate){
  j <- 1
  for(i in 1:length(tempDF$ValueCounter)){
    
    if(name == tempDF$ValueCounter[i]){
      tempDF$ValueCounter[i] <- paste(tempDF$ValueCounter[i], j, sep = "_") 
      j <- j + 1
    }
  }
}

rownames(tempDF) <- tempDF$ValueCounter
#################

###################################
# data handling finished next encoding
###################################

# Encoding APTname (APTname)
df <- df %>%
  mutate(APTName_UniqueID = match(APTname, unique(APTname)))

# Encoding Table_Types (Table_Types)
df <- df %>%
  mutate(Table_Types_UniqueID = match(Table_Types, unique(Table_Types)))

# Encoding Techniques_N_Software (Techniques_N_Software)
df <- df %>%
  mutate(Technique_UniqueID = match(Techniques_N_Software, unique(Techniques_N_Software)))

# Encoding Domains (Domain)
df <- df %>%
  mutate(Domain_UniqueID = match(Domain, unique(Domain)))

# Encoding Tactics (Tactic_s)
df <- df %>%
  mutate(Tactic_s_UniqueID = match(Tactic_s, unique(Tactic_s)))


#function usefull to see associated number with encoding
GetAssosiactedNumber <- function(x, y, z){
  df <- x
  StringToSearch <- y
  ColumnType <- z
  
  
  APTassosDF <- data.frame(cbind(APTname = df$APTname, APTName_UniqueID = df$APTName_UniqueID))
  APTassosDF <- APTassosDF[!duplicated(APTassosDF), ]
  
  Table_Types_assosDF <-  data.frame(cbind(Table_Types = df$Table_Types, Table_Types_UniqueID = df$Table_Types_UniqueID))
  Table_Types_assosDF <- Table_Types_assosDF[!duplicated(Table_Types_assosDF), ]
  
  Techniques_N_Software_assosDF <-  data.frame(cbind(Techniques_N_Software = df$Techniques_N_Software, Technique_UniqueID = df$Technique_UniqueID))
  Techniques_N_Software_assosDF <- Techniques_N_Software_assosDF[!duplicated(Techniques_N_Software_assosDF), ]
  
  Tactic_s_assosDF <-  data.frame(cbind(Tactic_s = df$Tactic_s, Tactic_s_UniqueID = df$Tactic_s_UniqueID))
  Tactic_s_assosDF <- Tactic_s_assosDF[!duplicated(Tactic_s_assosDF), ]
  
  Domain_assosDF <-  data.frame(cbind(Domain = df$Domain, Domain_UniqueID = df$Domain_UniqueID))
  Domain_assosDF <- Domain_assosDF[!duplicated(Domain_assosDF), ]
  
  
  
  if(ColumnType == "APTName"){
    
    IndexForSearch <- which(APTassosDF$APTname == StringToSearch)
    AssosiactedNumberForEncoding <- as.integer(APTassosDF$APTName_UniqueID[IndexForSearch])
    
  }else if(ColumnType == "TableType_ID"){
    
    IndexForSearch <- which(Table_Types_assosDF$Table_Types == StringToSearch)
    AssosiactedNumberForEncoding <- as.integer(Table_Types_assosDF$Table_Types_UniqueID[IndexForSearch])
    
  }else if(ColumnType == "TechniqueNsoft_ID"){
    
    IndexForSearch <- which(Techniques_N_Software_assosDF$Techniques_N_Software == StringToSearch)
    AssosiactedNumberForEncoding <- as.integer(Techniques_N_Software_assosDF$Technique_UniqueID[IndexForSearch])
    
  }else if(ColumnType == "Tactic_ID"){
    
    IndexForSearch <- which(Tactic_s_assosDF$Tactic_s == StringToSearch)
    AssosiactedNumberForEncoding <- as.integer(Tactic_s_assosDF$Tactic_s_UniqueID[IndexForSearch])
    
  }else if(ColumnType == "Domain_ID"){
    
    IndexForSearch <- which(Domain_assosDF$Domain == StringToSearch)
    AssosiactedNumberForEncoding <- as.integer(Domain_assosDF$Domain_UniqueID[IndexForSearch])
    
  }else{
    print("entered unknown ColumnType name")
    AssosiactedNumberForEncoding <- "Nothing Found Error"
  }
  
  # print(AssosiactedNumberForEncoding)
  # print(typeof(AssosiactedNumberForEncoding))
  
  return(AssosiactedNumberForEncoding)
}


####################################
# after encoding Building model i think
####################################



# # Create an empty Bayesian network
# bn <- empty.graph(nodes = c("APTName", "TableType_ID", "TechniqueNsoft_ID", "Domain_ID", "Tactic_ID"))
# 
# # Define the arcs between nodes
# bn <- set.arc(bn, "APTName", "TableType_ID")
# bn <- set.arc(bn, "TableType_ID", "TechniqueNsoft_ID")
# bn <- set.arc(bn, "TechniqueNsoft_ID", "Tactic_ID")
# bn <- set.arc(bn, "TechniqueNsoft_ID", "Domain_ID")
# 
# plot(bn)


data_for_bn <- data.frame(
  APTName = df$APTName_UniqueID,
  TableType_ID = df$Table_Types_UniqueID,
  TechniqueNsoft_ID = df$Technique_UniqueID,
  Domain_ID = df$Domain_UniqueID,
  Tactic_ID = df$Tactic_s_UniqueID
)



##############################################################################




####################
##the line below reduce amount of data between rows
rownames(data_for_bn) <- df$ValueCounter
data_for_bn <- data_for_bn[1: 100,]
#
####################


########## 1 just selected data
standardized_data <- scale(data_for_bn)
##########

########## 2 groups data minus ofc NaN s
encoded_df <- as.data.frame(lapply(GroupsDF, function(column) {
  if (is.factor(column) || is.character(column)) {
    as.numeric(factor(column))
  } else {
    column
  }
}))

#the line below reduce amount of data between rows
rownames(encoded_df) <- df$ValueCounter
encoded_df <- encoded_df[1: 100,]
standardized_data <- scale(encoded_df)

standardized_data <- standardized_data[, !apply(is.na(standardized_data), 2, any)]
##########

########## 3 all final dataframe minus ofc NaN s
encoded_df <- as.data.frame(lapply(FinalDataFrame, function(column) {
  if (is.factor(column) || is.character(column)) {
    as.numeric(factor(column))
  } else {
    column
  }
}))

#the line below reduce amount of data between rows
rownames(encoded_df) <- df$ValueCounter
encoded_df <- encoded_df[1: 100,]
standardized_data <- scale(encoded_df)

standardized_data <- standardized_data[, !apply(is.na(standardized_data), 2, any)]
##########




pca_result <- prcomp(standardized_data, center = TRUE, scale. = TRUE)

variance_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
transformed_data <- predict(pca_result, newdata = standardized_data)


plot(pca_result)

# Create the biplot
biplot(pca_result)

# Adding a regression line
abline(lm(pca_result$x[, 1] ~ pca_result$x[, 2]), col = "red")




#############################
# testing
# this https://www.youtube.com/watch?v=0Jp4gsfOLMs

pca <- prcomp(t(standardized_data), scale=TRUE)


pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
barplot(pca.var.per, main="SCREE PLOT", xlab="Principal Component", ylab="Percent Variation")



pca.data <- data.frame(Sample=rownames(pca$x),
                       X=pca$x[,1],
                       Y=pca$x[,2])


ggplot(data=pca.data, aes(x=X, y=Y, label=Sample)) +
  geom_text() +
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle("My PCA Graph")

loading_scores <- pca$rotation[,1]

Data_scores <- abs(loading_scores)
Data_scores_ranked <- sort(Data_scores, decreasing=TRUE)
top_10_Data <- names(Data_scores_ranked[1:10])




#############################
# testing
#https://www.youtube.com/watch?v=DngS4LNNzc8
#https://www.youtube.com/watch?v=mNpBrHwOCt4


library(MASS)
library(factoextra)



###############################
# testing MCA

library(FactoMineR)
library(vcd)
library(factoextra)

tempDF2 <- tempDF[1: 20,]
NamesForRow <- tempDF2$ValueCounter

tempDF2 <- tempDF2[,-c(6, 7)]
rownames(tempDF2) <- NamesForRow

temp3 <- tempDF2
temp3[is.na(temp3)] <- "ThisIsAMissingValue"
rownames(temp3) <- NamesForRow





arthritis_mca <- MCA(tempDF2, 
                     ncp = 2, 
                     graph = FALSE)


fviz_mca_biplot(arthritis_mca, 
                repel = TRUE, 
                ggtheme = theme_minimal())




###############################
# testing FAMD


arthiris_famd <- FAMD(temp3,
                      ncp = 2, 
                      graph = TRUE)


fviz_famd_var(tempDF2, "quanti.var", repel = TRUE,
              col.var = "black")




###############################
# testing MCA

library(FactoMineR)

res.famd <- FAMD(temp3,
                 graph = FALSE,
                 ncp = 2)





fviz_famd_var(res.famd, "quanti.var", repel = TRUE,
              col.var = "black")

fviz_famd_var(res.famd, "quanti.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)





# Load the FactoMineR package
library(FactoMineR)

# Convert your categorical variables to factors
categorical_variables <- c("APTname", "Table_Types", "Tactic_s", "Domain")
temp3 <- mutate(temp3, across(categorical_variables, as.factor))
# Create a data frame containing your categorical variables
data_frame <- temp3[, categorical_variables]
# Perform the FAMD analysis
res.famd <- FAMD(data_frame,
                 graph = FALSE,
                 ncp = 2)
# Visualize the results of the FAMD analysis
fviz_famd(res.famd)


