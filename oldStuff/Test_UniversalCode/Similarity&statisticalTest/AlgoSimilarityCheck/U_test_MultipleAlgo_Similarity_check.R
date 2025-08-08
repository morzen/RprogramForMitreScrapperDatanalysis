library("bnlearn")
library("proxy")
library("tm")
library("tidyverse")
library("corrr")
library("Matrix")
library("recommenderlab")

set.seed(4444)

FinalDataFrame <- read.csv("./1_DATAusedFolder/FinalDataFrame_Apr2024.csv")

df <- FinalDataFrame

####################################
# Data handling 
####################################
#OLD VERSION NOT FULL DATA

GroupsDF <- df %>%
  filter(Update_Date == "Data_Apr2024", PageType == "Groups") %>%
  filter(Tables_TableType %in% c("Techniques Used", "Software"))

TechDF <- df %>%
  filter(Update_Date == "Data_Apr2024", PageType == "Techniques")

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
                Techniques_N_Software = GroupsDF$Tables_ID,
                Tactic_s= GroupsDF$Tactic_s,
                Domain = GroupsDF$Tables_Domain
              ))
row.names(df) <- seq_len(nrow(df))


df <- df %>%
  separate_rows(Tactic_s, sep = ", ")
APTnames <- unique(df$APTname)


####################################
# main code
####################################

#Cosine similarity (COS)
Cosine_similarity <- function(APT1subset, APT2subset){
  # APT1subset <- APTnames
  # APT2subset <- APTnames
  APT1subset <- APT1subset[, !names(APT1subset) %in% c("APTname")]
  APT2subset <- APT2subset[, !names(APT2subset) %in% c("APTname")]
  
  # Create a corpus for APT1subset and APT2subset
  corpus_df1 <- Corpus(VectorSource(APT1subset))
  corpus_df2 <- Corpus(VectorSource(APT2subset))
  
  # Create Document-Term Matrices (DTMs) for both data frames
  dtm_df1 <- DocumentTermMatrix(corpus_df1)
  dtm_df2 <- DocumentTermMatrix(corpus_df2)
  
  # Convert the DTMs to matrices
  mat_df1 <- as.matrix(dtm_df1)
  mat_df2 <- as.matrix(dtm_df2)
  
  # Calculate cosine similarity between the two data frames
  cosine_similarity <- proxy::simil(x = mat_df1, y = mat_df2, method = "cosine")
  
  # Calculate the avergae cosine similarity between the 2 entire dataframes in percentage
  average_cos_similarity_percentage <- mean(1 - cosine_similarity) * 100
  
  
  
  return(average_cos_similarity_percentage)
}

#Pearson's correlation (COR)

Pearson_correlation_similarity <- function(APT1subset, APT2subset){
  APT1subset <- APT1subset[, !names(APT1subset) %in% c("APTname")]
  APT2subset <- APT2subset[, !names(APT2subset) %in% c("APTname")]
  
  encode_text <- function(column) {
    if (is.factor(column) || is.character(column)) {
      unique_values <- unique(column)
      numerical_codes <- seq_along(unique_values)
      encoding <- setNames(numerical_codes, unique_values)
      return(encoding[column])
    } else {
      return(column)  # No encoding for numerical columns
    }
  }
  
  
  df1_encoded <- as.data.frame(lapply(APT1subset, encode_text))
  df2_encoded <- as.data.frame(lapply(APT2subset, encode_text))
  
  # Determine the number of rows in each dataframe
  num_rows_df1 <- nrow(df1_encoded)
  num_rows_df2 <- nrow(df2_encoded)
  
  # Initialize a matrix to store the correlation values
  correlation_values <- matrix(0, nrow = num_rows_df1, ncol = num_rows_df2)
  
  # Calculate Pearson's correlation for each pair of rows using proxy::simil
  for (i in 1:num_rows_df1) {
    for (j in 1:num_rows_df2) {
      correlation_values[i, j] <- proxy::simil(x = df1_encoded[i, , drop = FALSE], y = df2_encoded[j, , drop = FALSE], method = "pearson")
    }
  }
  
  # Calculate the average correlation, considering valid correlations only
  valid_correlations <- correlation_values[correlation_values != 0]  # Exclude zero correlations
  average_correlation <- mean(valid_correlations, na.rm = TRUE)
  
  # Calculate the average correlation as a percentage
  average_Pearson_correlation_percentage_similarity <- (1 - abs(average_correlation)) * 100
  
  
  return(average_Pearson_correlation_percentage_similarity)
}
  
#Constrained pearson's correlation (CPC)
Constrained_Pearson_correlation_similarity <- function(APT1subset, APT2subset){
  APT1subset <- APT1subset[, !names(APT1subset) %in% c("APTname")]
  APT2subset <- APT2subset[, !names(APT2subset) %in% c("APTname")]
  
  # encode_text <- function(column) {
  #   if (is.factor(column) || is.character(column)) {
  #     unique_values <- unique(column)
  #     numerical_codes <- seq_along(unique_values)
  #     encoding <- setNames(numerical_codes, unique_values)
  #     return(encoding[column])
  #   } else {
  #     return(column)  # No encoding for numerical columns
  #   }
  # }
  
  # Calculate CPC between df1 and df2

  # cpc <- correlate(APT1subset, APT2subset)
  # 
  #return(average_Constrained__Pearson_correlation_percentage_similarity)
}

#Jaccard similarity

Jaccards_similarity <- function(APT1subset, APT2subset){
  APT1subset <- APT1subset[, !names(APT1subset) %in% c("APTname")]
  APT2subset <- APT2subset[, !names(APT2subset) %in% c("APTname")]
  
  intersec <- length(intersect(APT1subset, APT2subset))
  union <- length(union(APT1subset, APT2subset))
  JaccardSim<- intersec / union
  
  # turn it in percentage
  JaccardSim_InPercent <- JaccardSim * 100
  return(JaccardSim_InPercent)
}

#Mean square distance (MSD)

Mean_square_distance_similarity <- function(APT1subset, APT2subset){
  APT1subset <- APT1subset[, !names(APT1subset) %in% c("APTname")]
  APT2subset <- APT2subset[, !names(APT2subset) %in% c("APTname")]
  
  encode_text <- function(column) {
    if (is.factor(column) || is.character(column)) {
      unique_values <- unique(column)
      numerical_codes <- seq_along(unique_values)
      encoding <- setNames(numerical_codes, unique_values)
      return(encoding[column])
    } else {
      return(column)  # No encoding for numerical columns
    }
  }
  
  
  df1_encoded <- as.data.frame(lapply(APT1subset, encode_text))
  df2_encoded <- as.data.frame(lapply(APT2subset, encode_text))
  
  
  num_rows <- min(nrow(df1_encoded), nrow(df2_encoded))
  df1 <- df1_encoded[1:num_rows, ]
  df2 <- df2_encoded[1:num_rows, ]
  
  # Calculate the squared differences between the two dataframes
  squared_diff <- (as.matrix(df1) - as.matrix(df2))^2
  
  # Calculate the mean of the squared differences (MSD)
  msd <- mean(squared_diff, na.rm = TRUE)
  
  # Calculate the maximum possible MSD based on the number of unique words
  unique_words_df1 <- unique(unlist(strsplit(tolower(df1_encoded), " ")))
  unique_words_df2 <- unique(unlist(strsplit(tolower(df2_encoded), " ")))
  
  max_possible_msd <- sum((length(unique_words_df1) + length(unique_words_df2))^2)
  
  
  msd_percentage_similarity <- 100 * (1 - (msd / max_possible_msd))
  
  return(msd_percentage_similarity)
}

#Jaccard mean square distance (JMSD)

Jaccard_mean_square_distance_similarity <- function(APT1subset, APT2subset){
  APT1subset <- APT1subset[, !names(APT1subset) %in% c("APTname")]
  APT2subset <- APT2subset[, !names(APT2subset) %in% c("APTname")]
  

  jaccard_similarity <- function(text1, text2) {
    set1 <- unique(unlist(strsplit(tolower(text1), " ")))
    set2 <- unique(unlist(strsplit(tolower(text2), " ")))
# 
    intersect_length <- length(intersect(set1, set2))
    union_length <- length(union(set1, set2))
    return(intersect_length / union_length)
  }
  
  # Calculate Jaccard Mean Square Distance (JMSD) for the entire data frames
  jmsd <- mean(1 - sapply(1:nrow(APT1subset), function(i) {
    jaccard_similarity(APT1subset[i, ], APT2subset[i, ])
  })^2)
  
  # Invert and express as a percentage
  jmsd_percentage_similarity <- 100 * (1 - jmsd)

  return(jmsd_percentage_similarity)
}

#Weighted regularized matrix factorization (WRMF)

#seem to be inadequate for my data more made for recommandation

#Factored item similarity models (FISM)

#seem to be inadequate for my data more made for recommandation


################################################################################
#main
################################################################################
Pearson_results_PercentDataFrame <- data.frame(matrix(nrow = length(APTnames), ncol = length(APTnames)))
colnames(Pearson_results_PercentDataFrame) <- APTnames
rownames(Pearson_results_PercentDataFrame) <- APTnames

Jaccard_results_PercentDataFrame <- data.frame(matrix(nrow = length(APTnames), ncol = length(APTnames)))
colnames(Jaccard_results_PercentDataFrame) <- APTnames
rownames(Jaccard_results_PercentDataFrame) <- APTnames

Mean_square_distance_results_PercentDataFrame <- data.frame(matrix(nrow = length(APTnames), ncol = length(APTnames)))
colnames(Mean_square_distance_results_PercentDataFrame) <- APTnames
rownames(Mean_square_distance_results_PercentDataFrame) <- APTnames

Jaccard_mean_square_distance_results_PercentDataFrame <- data.frame(matrix(nrow = length(APTnames), ncol = length(APTnames)))
colnames(Jaccard_mean_square_distance_results_PercentDataFrame) <- APTnames
rownames(Jaccard_mean_square_distance_results_PercentDataFrame) <- APTnames


for(APT1 in APTnames){
  APT1subset <- subset(df, APTname == APT1)


  for(APT2 in APTnames){
    APT2subset <- subset(df, APTname == APT2)


    cat(APT1,":", APT2, " _ ", which(APTnames == APT1), ":", which(APTnames == APT2), "on ", length(APTnames), "\n")

    # Cosine_results <- Cosine_similarity(APT1subset, APT2subset)

    Pearson_results <- Pearson_correlation_similarity(APT1subset, APT2subset)
    # cat("Pearson: ", Pearson_results, "\n")
    Pearson_results_PercentDataFrame[APT1, APT2] <- Pearson_results

    # this one is not crashing
    Jaccard_results <- Jaccards_similarity(APT1subset, APT2subset)
    # cat("Jaccard: ", Jaccard_results, "\n")
    Jaccard_results_PercentDataFrame[APT1, APT2] <- Jaccard_results

    # this one is not crashing
    Mean_square_distance_results <- Mean_square_distance_similarity(APT1subset, APT2subset)
    # cat("Mean_square_distance: ", Mean_square_distance_results, "\n")
    Mean_square_distance_results_PercentDataFrame[APT1, APT2] <- Mean_square_distance_results

    # this one is not crashing
    Jaccard_mean_square_distance_results <- Jaccard_mean_square_distance_similarity(APT1subset, APT2subset)
    # cat("Jaccard_mean_square_distance: ", Jaccard_mean_square_distance_results, "\n")
    Jaccard_mean_square_distance_results_PercentDataFrame[APT1, APT2] <- Jaccard_mean_square_distance_results

    # cat("------","\n")
    # break
  }

  # break
}




################################################################################
#testing area
################################################################################

# Define functions to be executed concurrently
function1 <- function(APTnames) {
  # Function 1 code
  for(APT1 in APTnames){
    APT1subset <- subset(df, APTname == APT1)
    
    for(APT2 in APTnames){
      APT2subset <- subset(df, APTname == APT2)
      cat(APT1, which(APTnames == APT1),":", which(APTnames == APT2), APT2, "on ", length(APTnames), "\n")
      Pearson_results <- Pearson_correlation_similarity(APT1subset, APT2subset)
      # cat("Pearson: ", Pearson_results, "\n")
      Pearson_results_PercentDataFrame[APT1, APT2] <- Pearson_results
      # break
    }
    # break
  }
  return(Pearson_results_PercentDataFrame)
}

function2 <- function(APTnames) {
  for(APT1 in APTnames){
    APT1subset <- subset(df, APTname == APT1)
    
    for(APT2 in APTnames){
      APT2subset <- subset(df, APTname == APT2)
      cat(APT1, which(APTnames == APT1),":", which(APTnames == APT2), APT2, "on ", length(APTnames), "\n")
      # this one is not crashing
      Jaccard_results <- Jaccards_similarity(APT1subset, APT2subset)
      # cat("Jaccard: ", Jaccard_results, "\n")
      Jaccard_results_PercentDataFrame[APT1, APT2] <- Jaccard_results
      # break
    }
    # break
  }
  return(Jaccard_results_PercentDataFrame)
}

function3 <- function(APTnames) {
  for(APT1 in APTnames){
    APT1subset <- subset(df, APTname == APT1)
    
    for(APT2 in APTnames){
      APT2subset <- subset(df, APTname == APT2)
      cat(APT1, which(APTnames == APT1),":", which(APTnames == APT2), APT2, "on ", length(APTnames), "\n")
      # this one is not crashing
      Mean_square_distance_results <- Mean_square_distance_similarity(APT1subset, APT2subset)
      # cat("Mean_square_distance: ", Mean_square_distance_results, "\n")
      Mean_square_distance_results_PercentDataFrame[APT1, APT2] <- Mean_square_distance_results
      # break
    }
    # break
  }
  return(Mean_square_distance_results_PercentDataFrame)
}

function4 <- function(x) {
  APTnames <- x
  for(APT1 in APTnames){
    APT1subset <- subset(df, APTname == APT1)
    
    for(APT2 in APTnames){
      APT2subset <- subset(df, APTname == APT2)
      # cat(APT1, which(APTnames == APT1),":", which(APTnames == APT2), APT2, "on ", length(APTnames), "\n")
      # this one is not crashing
      Jaccard_mean_square_distance_results <- Jaccard_mean_square_distance_similarity(APT1subset, APT2subset)
      # cat("Jaccard_mean_square_distance: ", Jaccard_mean_square_distance_results, "\n")
      Jaccard_mean_square_distance_results_PercentDataFrame[APT1, APT2] <- Jaccard_mean_square_distance_results
      
      
      # break
    }
    # break
  }
  
  return(Jaccard_mean_square_distance_results_PercentDataFrame)
}




library(parallel)

# # Define the number of CPU cores to use
# num_cores <- detectCores()
# 
# # Create a cluster object with specified number of CPU cores
# cl <- makeCluster(num_cores)



#super long i saved the results in csvformat
Pearson_results_PercentDataFrame <- function1(APTnames)
#write.csv(Pearson_results_PercentDataFrame, "/Users/morzen/Desktop/Pearson_results_PercentDataFrame.csv", row.names = FALSE)

#quicker
Jaccard_results_PercentDataFrame <- function2(APTnames)
Mean_square_distance_results_PercentDataFrame <- function3(APTnames)
Jaccard_mean_square_distance_results_PercentDataFrame <- function4(APTnames)



