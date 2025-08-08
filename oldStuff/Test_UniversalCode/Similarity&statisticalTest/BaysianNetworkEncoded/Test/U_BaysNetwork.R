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

###################################
# data handling finished next encoding
###################################

#function usefull to see associated number with encoding
GetAssosiactedNumber <- function(x, y, z, afterz){
  df <- x
  StringToSearch <- y
  ColumnType <- z
  dfID <- afterz
  
  
  APTassosDF <- data.frame(cbind(APTname = df$APTname, APTName_UniqueID = dfID$APTname))
  APTassosDF <- data.frame(APTassosDF[!duplicated(APTassosDF), ])
  
  Table_Types_assosDF <-  data.frame(cbind(Table_Types = df$Table_Types, Table_Types_UniqueID = dfID$Table_Types))
  Table_Types_assosDF <- Table_Types_assosDF[!duplicated(Table_Types_assosDF), ]
  
  Techniques_N_Software_assosDF <-  data.frame(cbind(Techniques_N_Software = df$Techniques_N_Software, Technique_UniqueID = dfID$Techniques_N_Software))
  Techniques_N_Software_assosDF <- Techniques_N_Software_assosDF[!duplicated(Techniques_N_Software_assosDF), ]
  
  Tactic_s_assosDF <-  data.frame(cbind(Tactic_s = df$Tactic_s, Tactic_s_UniqueID = dfID$Tactic_s))
  Tactic_s_assosDF <- Tactic_s_assosDF[!duplicated(Tactic_s_assosDF), ]
  
  Domain_assosDF <-  data.frame(cbind(Domain = df$Domain, Domain_UniqueID = dfID$Domain))
  Domain_assosDF <- Domain_assosDF[!duplicated(Domain_assosDF), ]
  
 
  
  if(ColumnType == "APTName"){
    
    IndexForSearch <- which(APTassosDF$APTname == StringToSearch)
    AssosiactedNumberForEncoding <- as.integer(APTassosDF$APTName_UniqueID[IndexForSearch])
    
  }else if(ColumnType == "TableType_ID"){
    
    IndexForSearch <- which(Table_Types_assosDF$Table_Types == StringToSearch)
    AssosiactedNumberForEncoding <- as.integer(Table_Types_assosDF$Table_Types_UniqueID[IndexForSearch])
    
  }else if(ColumnType == "Techniques_N_Software_ID"){
    
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


# # Encoding APTname (APTname)
# df <- df %>%
#   mutate(APTName_UniqueID = match(APTname, unique(APTname)))
# 
# # Encoding Table_Types (Table_Types)
# df <- df %>%
#   mutate(Table_Types_UniqueID = match(Table_Types, unique(Table_Types)))
# 
# # Encoding Techniques_N_Software (Techniques_N_Software)
# df <- df %>%
#   mutate(Technique_UniqueID = match(Techniques_N_Software, unique(Techniques_N_Software)))
# 
# # Encoding Domains (Domain)
# df <- df %>%
#   mutate(Domain_UniqueID = match(Domain, unique(Domain)))
# 
# # Encoding Tactics (Tactic_s)
# df <- df %>%
#   mutate(Tactic_s_UniqueID = match(Tactic_s, unique(Tactic_s)))

################################################
#test
dfID <- as.data.frame(lapply(df, function(column) {
  if (is.factor(column) || is.character(column)) {
    as.numeric(factor(column))
  } else {
    column
  }
}))

##############################################


####################################
# after encoding Building model i think
####################################


### Create an automatic version of this 
# Create an empty Bayesian network
# bn <- empty.graph(nodes = c("APTName", "TableType_ID", "TechniqueNsoft_ID", "Domain_ID", "Tactic_ID"))
# 
# # Define the arcs between nodes
# bn <- set.arc(bn, "APTName", "TableType_ID")
# bn <- set.arc(bn, "TableType_ID", "TechniqueNsoft_ID")
# bn <- set.arc(bn, "TechniqueNsoft_ID", "Tactic_ID")
# bn <- set.arc(bn, "TechniqueNsoft_ID", "Domain_ID")
# 
# plot(bn)

################################################
#test
# Assuming 'df' is your original dataframe
library(missForest)

# Impute missing values using MissForest
imputed_data <- missForest(dfID)

# Convert imputed data to a dataframe
imputed_df <- data.frame(imputed_data$ximp)


# Learn the Bayesian network structure using the imputed data
library(bnlearn)
learned_structure <- hc(imputed_df, maxp = 3)

# Plot the learned structure
plot(learned_structure)

bn <-learned_structure
##############################################


data_for_bn <- data.frame(
  APTname = dfID$APTname,
  Table_Types = dfID$Table_Types,
  Techniques_N_Software = dfID$Techniques_N_Software,
  Domain = dfID$Domain,
  Tactic_s = dfID$Tactic_s
)



##############################################################################
#test
 
#not entierely sure what is happenning here


categorical_cols <- c("APTname", "Table_Types", "Techniques_N_Software", "Domain", "Tactic_s")

data_for_bn <- data_for_bn %>%
  mutate_at(vars(all_of(categorical_cols)), factor)




# Define your network structure (if not done already)
# Define the nodes and edges based on your problem

# Fit the Bayesian network using maximum likelihood estimation (mle)
bn_fit <- bn.fit(bn, data_for_bn, method = "mle")


##############################################################################


##########################
# Query
##########################



Selected_APTName <- GetAssosiactedNumber(df, "APT33", "APTName", dfID)

Selected_TableType_ID <- GetAssosiactedNumber(df, "Technique Used", "TableType_ID", dfID)

Selected_Technique_ID <- GetAssosiactedNumber(df, "T1071.001", "Techniques_N_Software_ID", dfID)

Selected_Domain_ID <- GetAssosiactedNumber(df, "Enterprise", "Domain_ID", dfID)

Selected_Tactic_ID <- GetAssosiactedNumber(df, "Command and Control", "Tactic_ID", dfID)

#"APTname", "Table_Types", "Techniques_N_Software", "Domain", "Tactic_s"
#event is what you wanna calculate the probability of this event happenning
#the evidence is/are the conditions for the evaluation of the probability of the event happenning
cpquery(bn_fit, event = (APTname == Selected_APTName), evidence = ((Techniques_N_Software == Selected_Technique_ID) & (Domain == Selected_Domain_ID) & (Tactic_s == Selected_Tactic_ID)))





#THE ABOVE SORT OF WORK IN AN INTERESTING WAY but found not use for it yet

##########################
# test. NOT WORKING AFTER THAT POINT
##########################





# Assuming you have already defined and learned the Bayesian network

# Compare two APT groups based on their attributes
compare_apts <- function(network, apt1_data, apt2_data) {
  # Make inference using the network
  inference_result_apt1 <- predict(network, apt1_data)
  inference_result_apt2 <- predict(network, apt2_data)
  
  # Compare the posterior probabilities or likelihoods
  similarity_score <- compare_scores(inference_result_apt1, inference_result_apt2)
  
  return(similarity_score)
}

# Function to compare likelihood scores
compare_scores <- function(score1, score2) {
  # You can define your own comparison logic here
  # For example, you might calculate the absolute difference between scores
  return(abs(score1 - score2))
}




for(APT1id in dfID$APTname){
  APT1DF <- filter(dfID, APTname %in% APT1id)
  
  for(APT2id in dfID$APTname){
    APT2DF <- filter(dfID, APTname %in% APT2id)
    
    apt1_data <- as.list(APT1DF)
    apt2_data <- as.list(APT2DF)
    
    comparison_results <- compare_apts(bn_fit, apt1_data, apt2_data)
    
    print(comparison_results)
    break
  }
  
}



inference_result_apt1 <- predict(bn_fit, apt1_data, method = "bayes")
inference_result_apt2 <- predict(bn_fit, apt2_data)

# Compare the posterior probabilities or likelihoods
similarity_score <- compare_scores(inference_result_apt1, inference_result_apt2)


