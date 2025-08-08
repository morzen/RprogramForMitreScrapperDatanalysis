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
FinalDataFrame <- read.csv("./TestFinal_df.csv")

################################################################################
# BaysNetwork 
################################################################################
df <- FinalDataFrame

####################################
# Data handling and encoding
####################################
#OLD VERSION NOT FULL DATA

GroupsDF <- df %>%
  filter(Update_Date == "Data_Apr2024", PageType == "Groups") %>%
  filter(Tables_TableType %in% c("Techniques Used", "Software"))

TechDF <- df %>%
  filter(Update_Date == "Data_Apr2024", PageType == "Techniques")

SoftDF <- df %>%
  filter(Update_Date == "Data_Apr2024", PageType == "Software")

# Filter Groups and Techniques data
#FULL DATA
# GroupsDF <- df %>% filter(PageType == "Groups") %>% filter(Tables_TableType %in% c("Techniques Used", "Software"))
# 
# TechDF <- df %>%
#   filter(Update_Date == "Data_Apr2024", PageType == "Techniques")

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

###################################
# data handling finished next encoding
###################################

dfID <- as.data.frame(lapply(df, function(column) {
  if (is.factor(column) || is.character(column)) {
    as.numeric(factor(column))
  } else {
    column
  }
}))

##############################################


#df for the baysian network
# Handle missing values by converting them to a specific level (e.g., -1)
data_for_bn <- dfID  # Create a copy of your data frame
data_for_bn[is.na(data_for_bn)] <- -1

data_for_bn$APTname <- factor(data_for_bn$APTname)
# data_for_bn$Table_Types <- factor(data_for_bn$Table_Types)
data_for_bn$Techniques_N_Software <- factor(data_for_bn$Techniques_N_Software)
data_for_bn$Domain <- factor(data_for_bn$Domain)
data_for_bn$Tactic_s <- factor(data_for_bn$Tactic_s)

#dropping tactic and domain, reppaetition because associated with techniques anyway
data_for_bn <- subset(data_for_bn, select = -c(Tactic_s, Domain))


comparison <- do.call(rbind,
                         Map(
                           data.frame,
                           APTnameOrigin=df$APTname,
                           APTnameEncoded=data_for_bn$APTname,
                           TechOrigin=df$Techniques_N_Software,
                           TechEncoded=data_for_bn$Techniques_N_Software,
                           TacticOrigin=df$Tactic_s,
                           DomainOrigin=df$Domain
                         ))
# ################################################


# library(missForest)
# 
# 
# imputed_data <- missForest(data_for_bn)
# 
# 
# imputed_df <- data.frame(imputed_data$ximp)

imputed_df <-data_for_bn

library(bnlearn)
# learned_structure <- hc(imputed_df, maxp = 4)
#hc not hierachical clustering but hill climbing
#pnal : Pearson's Negative Affinity Linkage to cluster
learned_structure <- hc(imputed_df, score = "pnal")
plot(learned_structure)

bn <-learned_structure
###################
#tried to represent the whole data frame in structured way


# dfID2 <- as.data.frame(lapply(FinalDataFrame, function(column) {
#   if (is.factor(column) || is.character(column)) {
#     as.numeric(factor(column))
#   } else {
#     column
#   }
# }))
# 
# 
# library(igraph)
# learned_structure <- hc(dfID2, score = "nal-g")
# g <- as.igraph(learned_structure)
# plot(g, layout = layout.reingold.tilford(g))
# 
# layout <- layout.reingold.tilford(g, circular = FALSE)
# 
# # # Manually adjust layout for better spacing
# # layout[, 1] <- layout[, 1] * 1.5  # Adjust x-coordinates for horizontal spacing
# # layout[, 2] <- layout[, 2] * 1.5  # Adjust y-coordinates for vertical spacing
# 
# 
# plot(g, layout = layout, 
#      vertex.label = V(g)$name, 
#      vertex.size = 5,
#      vertex.shape = "circle", 
#      vertex.color = "lightblue",
#      edge.arrow.size = 0.2, 
#      edge.color = "gray",
#      vertex.label.cex = 0.5,        # Adjust label size
#      vertex.label.dist = 1.5,       # Distance between label and node
#      vertex.label.degree = -pi/2,   # Label placement angle
#      vertex.label.family = "serif", # Font family for better readability
#      vertex.label.font = 0.5,         # Bold font for labels
#      asp = 0.                      # Aspect ratio to spread nodes horizontally
# )



# layout <- layout.reingold.tilford(g, root = V(g)[1])
# plot(g, layout = layout, vertex.label = V(g)$name, vertex.size = 20,
#      vertex.shape = "rectangle", vertex.color = "lightblue",
#      edge.arrow.size = 0.5, edge.color = "gray")


#####################

# ##############################################

# # Create the Bayesian network Manually
# bn <- empty.graph(nodes = c("APTname", "Table_Types", "Techniques_N_Software", "Domain", "Tactic_s"))
# 
# 
# # Define the arcs between nodes (as you did previously)
# bn <- set.arc(bn, "APTname", "Table_Types")
# bn <- set.arc(bn, "Table_Types", "Techniques_N_Software")
# bn <- set.arc(bn, "Techniques_N_Software", "Tactic_s")
# bn <- set.arc(bn, "Techniques_N_Software", "Domain")
# plot(bn)

# ##############################################


# Fit the Bayesian network
#maximum likelihood mle
bn_fit <- bn.fit(bn, data_for_bn, method = "mle")
#interesting to 
#View(bn_fit)
# AND 
#View(bn_fit[["Techniques_N_Software"]][["prob"]])



#################################################
#To visualise tables of probabilities in the Bayesian network
#################################################

levels_APTname <- levels(factor(df$APTname))
levels_Tech <- levels(factor(df$Techniques_N_Software))


#test node APTname alone
testing_APTname_node_encoded <- data.frame(bn_fit[["APTname"]][["prob"]])
testing_APTname_node_decoded <- testing_APTname_node_encoded

testing_APTname_node_decoded$APTname <- levels_APTname[testing_APTname_node_decoded$Var1]

#test node tech
testing_encoded <- data.frame(bn_fit[["Techniques_N_Software"]][["prob"]])
testing_decoded <- testing_encoded

# Revert the numerical encoding
testing_decoded$APTname <- levels_APTname[testing_decoded$APTname]
testing_decoded$Techniques_N_Software <- levels_Tech[testing_decoded$Techniques_N_Software]
##############################################


# categorical_cols <- c("APTname", "Table_Types", "Techniques_N_Software", "Domain", "Tactic_s")
# 
# data_for_bn <- data_for_bn %>%
#   mutate_at(vars(all_of(categorical_cols)), factor)
# 



# Define your network structure (if not done already)
# Define the nodes and edges based on your problem

# Fit the Bayesian network using maximum likelihood estimation (mle)
#bn_fit <- bn.fit(bn, data_for_bn, method = "mle")



save(bn_fit, file = "./Test_UniversalCode/Similarity&statisticalTest/BaysianNetworkEncoded/BayesNetworkData/bn_fit.rda")

write.csv(df, "./Test_UniversalCode/Similarity&statisticalTest/BaysianNetworkEncoded/BayesNetworkData/df.csv", row.names = FALSE)
write.csv(data_for_bn, "./Test_UniversalCode/Similarity&statisticalTest/BaysianNetworkEncoded/BayesNetworkData/data_for_bn.csv", row.names = FALSE)
##############################################################################

