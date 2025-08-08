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

Selected_MitreUpddateVersion <- "Data_Apr2024"

################################################################################
# Create Binary Matrix 
################################################################################
df <- FinalDataFrame

GroupsDF <- df %>%
  filter(Update_Date == Selected_MitreUpddateVersion, PageType == "Groups") 

TechDF <- df %>%
  filter(PageType %in% c("Techniques", "Software"))

#remove Na s full row
GroupsDF <- GroupsDF[rowSums(is.na(GroupsDF)) != ncol(GroupsDF), ]
GroupsDF <- GroupsDF[, colSums(is.na(GroupsDF)) != nrow(GroupsDF)]

TechDF <- TechDF[rowSums(is.na(TechDF)) != ncol(TechDF), ]
TechDF <- TechDF[, colSums(is.na(TechDF)) != nrow(TechDF)]


listTechNsoftIDs <- unique(TechDF$DataTypeID)

ListGroups <- unique(GroupsDF$Title_Name)


BinaryMatrix <- matrix(0, nrow = length(ListGroups), ncol = length(listTechNsoftIDs), 
                   dimnames = list(ListGroups, listTechNsoftIDs))


for(APTgroup in ListGroups){
  APTsubset <- subset(GroupsDF, Title_Name == APTgroup & Tables_TableType %in% c("Techniques Used", "Software"))
  
  for(TechNsoftIDs in listTechNsoftIDs){
    if(TechNsoftIDs %in% APTsubset$Tables_IDorName){
      BinaryMatrix[APTgroup, TechNsoftIDs] <- 1
    }
  }
}


################################################################################
# BaysNetwork 
################################################################################

# Get the dimensions of the matrix
# n_rows <- nrow(BinaryMatrix)
# n_cols <- ncol(BinaryMatrix)
# 
# # Create an edge list from the binary matrix (from rows to other rows)
# edges <- data.frame()
# 
# # Loop through each column and create directed edges between rows
# for (col in 1:n_cols) {
#   # Find the rows where this column has a 1
#   rows_with_edges <- which(BinaryMatrix[, col] == 1)
#   
#   # Create directed edges between rows (if multiple rows share a column)
#   if (length(rows_with_edges) > 1) {
#     for (i in 1:(length(rows_with_edges) - 1)) {
#       for (j in (i + 1):length(rows_with_edges)) {
#         # Use row names instead of row numbers
#         edges <- rbind(edges, c(from = rownames(BinaryMatrix)[rows_with_edges[i]], 
#                                 to = rownames(BinaryMatrix)[rows_with_edges[j]]))
#       }
#     }
#   }
# }
# 
# # Ensure column names for the edge data frame
# colnames(edges) <- c("from", "to")
# 
# # Create the DAG (directed graph) using the row names as node labels
# g <- graph_from_data_frame(edges, directed = TRUE, vertices = data.frame(name = rownames(BinaryMatrix)))
# 
# # Plot the DAG with row names as vertex labels
# plot(g, vertex.label = V(g)$name)













binary_data <- as.data.frame(t(BinaryMatrix))

# Create a random structure or define your DAG
# You can learn the structure from the data using hill-climbing algorithm or define it manually

# Option 1: Learn the structure from the binary matrix using hill-climbing
bn_structure <- hc(binary_data)  # maxp limits the maximum number of parents


# Option 2: Define the structure manually (e.g., from prior knowledge)
# bn_structure <- model2network("[Node1][Node2|Node1][Node3|Node2]") # Example of manual structure

# Plot the learned structure
plot(bn_structure)

# Convert each column of binary data into a factor (discrete)
binary_data[] <- lapply(binary_data, factor)


# Fit the Bayesian network using Maximum Likelihood Estimation (MLE)
bn_fitted <- bn.fit(bn_structure, binary_data, method = "mle")
# Fit the Bayesian network using Bayesian Parameter Estimation (BPE)
bn_fitted <- bn.fit(bn_structure, binary_data, method = "bayes")







