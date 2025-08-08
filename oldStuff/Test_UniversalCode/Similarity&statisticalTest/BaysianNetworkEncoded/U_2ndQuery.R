library("bnlearn")
library(dplyr)
library(tibble)

pwd <- "./Test_UniversalCode/Similarity&statisticalTest/BaysianNetworkEncoded/BayesNetworkData/"

# load("/Users/morzen/Desktop/work/RprogramForMitreScrapperDatanalysis/Test_UniversalCode/Similarity&statisticalTest/BayesNetworkData/ManuFactorbayesian_networkV4_FULLDATA_newDAG.rda")
load(paste0(pwd, "bn_fit.rda"))
# three variabvle below not used in program but good for visualizing 
df <- read.csv(paste0(pwd, "df.csv"))
data_for_bn <- read.csv(paste0(pwd, "data_for_bn.csv"))


##########################
# Subtsetting of MITRE dataset
##########################
# Function to divide the list by an integer and keep the subset
divide_list <- function(input_list, divisor) {
  # Create an index for the elements to keep
  indices_to_keep <- seq(1, length(input_list), by = divisor)
  
  # Subset the list using the calculated indices
  result_list <- input_list[indices_to_keep]
  
  return(result_list)
}


Divisor <- 2
APTnamesNodplic <- df[!duplicated(df$APTname), ]$APTname
APTdict <- list()
for(APT in APTnamesNodplic){
  tempdf <- df %>%
    filter(APTname == APT)
  
  TTPlist <- list(unique(tempdf$Techniques_N_Software))
  
  #shuffle
  TTPlist <- sample(TTPlist[[1]])

  #divid
  TTPlist <- divide_list(TTPlist, Divisor)

  APTdict[[APT]] <- TTPlist
  
}




##########################
# Multiple QUERY.  custom or from MITRE datast
##########################
df_NumericalEncodingLink <- cbind(df, Encoded_APTname = data_for_bn$APTname, Encoded_TechNsoftID = data_for_bn$Techniques_N_Software)
DF_techNsoft_encoded = cbind(NotEncoded = df_NumericalEncodingLink$Techniques_N_Software, Encoded = df_NumericalEncodingLink$Encoded_TechNsoftID)
DF_techNsoft_encoded <- data.frame(unique(DF_techNsoft_encoded))





# Read the CSV file
Costum_dict_APT_TPP_df <- read.csv("/Users/morzen/Desktop/CTIATTACKreport_2.csv")

# Remove empty rows
Costum_dict_APT_TPP_df <- Costum_dict_APT_TPP_df[rowSums(Costum_dict_APT_TPP_df == "") != ncol(Costum_dict_APT_TPP_df), ]

# Clean the ListTechNSoft column
Costum_dict_APT_TPP_df$ListTechNSoft <- gsub('\"', '', Costum_dict_APT_TPP_df$ListTechNSoft)

# Split the ListTechNSoft column into a list of vectors
Costum_dict_APT_TPP_df$ListTechNSoft <- lapply(strsplit(Costum_dict_APT_TPP_df$ListTechNSoft, ",\\s*"), trimws)

test_dict = list()
for(element in Costum_dict_APT_TPP_df$Names){
  row_numbers <- which(Costum_dict_APT_TPP_df[["Names"]] == element)

  test_dict[element] = Costum_dict_APT_TPP_df$ListTechNSoft[row_numbers]
}




test_dict_apt_name <- names(test_dict)
# Filter the list
TestDictAPTs_with_MITRE_data <- APTdict[names(APTdict) %in% test_dict_apt_name]



# #for costum manually inputed APTs uncomment below
# Costum_dict_APT_TPP <- list(
#   "Lazarus Group" = as.factor(c("T1189", "T1203", "T1547.012", "T1574.002", "T1547.012", "T1140", "T1574.002", "T1027.001", "T1027.002", "T1027.002", "T1003.001", "T1057", "T1082", "T1083", "T1113", "T1071.001", "T1132.002", "T1573.001", "T1041")),
#   "andariel" = as.factor(c("T1584.006", "T1583.003", "T1566.001", "T1204.002", "T1059.007", "T1036.005", "T1027.003", "T1497.001", "T1049", "T1057", "T1113", "T1071.001", "T1095", "T1573.001", "T1041", "T1486")),
#   "Sandworm" = as.factor(c("T0847", "T0807", "T0871", "T0853", "T0872", "T0809", "T0855", "T0831"))
# )

#Costum dataset below
# Costum_dict_APT_TPP <- test_dict

#if you want from mitre dataset uncomment below
# Costum_dict_APT_TPP <- APTdict

# if you want apt from custom dataset with mitre attack data uncoment bloew
Costum_dict_APT_TPP <- TestDictAPTs_with_MITRE_data

result_query_dict <- list()
for(Key_element in names(Costum_dict_APT_TPP)){
  ListOfID_TechNsoft <- Costum_dict_APT_TPP[[Key_element]]


  
  #if Na it means MITRE doesn't have the technique associate with any APTs
  translated_values <- DF_techNsoft_encoded$Encoded[match(ListOfID_TechNsoft, DF_techNsoft_encoded$NotEncoded)]
  
  evidence <- list(
    Techniques_N_Software = na.omit(factor(unique(translated_values)))
    # Tactic_s = factor(APT2DF$Tactic_s)
    # Domain = factor(APT2DF$Domain)
  )
  
  evidence2 <- list()
  evidence2$Techniques_N_Software <- DF_techNsoft_encoded$Encoded %in% translated_values
  
  APTidNumber <- data_for_bn[!duplicated(data_for_bn$APTname), ]$APTname
  
  ResultsSingleQuery_lw <- list()
  
  # evidence_columns <- c("Table_Types", "Techniques_N_Software", "Tactic_s", "Domain")
  #evidence_columns <- c("Techniques_N_Software")#, "Tactic_s", "Domain")
  for(APT1id in APTidNumber){
    
    APT1DF <- subset(data_for_bn, APTname %in% APT1id)
    # Run the cpquery with the "lw" method using the loaded Bayesian network
    #likelihood weighting
    result_lw <- cpquery(bn_fit, event = APTname == APT1id, evidence = evidence, method = "lw")#, n = 1000)
    
    ResultsSingleQuery_lw[APT1id] <- result_lw
    
  }
  
  lw_result <- do.call(rbind,
                       Map(
                         data.frame,
                         APTname = unique(df$APTname),
                         results = ResultsSingleQuery_lw
                       ))
  rownames(lw_result) <- seq(nrow(lw_result))
  lw_result <- lw_result[order(-lw_result$results), ]
  
  result_single_query_dict <- setNames(as.list(lw_result$results), lw_result$APTname)
  result_query_dict[[Key_element]] <- result_single_query_dict
}





##############
#Ratio analysis
##############



topX = 5

sucess = 0
failure = 0
for(APTquery in names(result_query_dict)){
  print(APTquery)
  queryresult <- result_query_dict[[APTquery]]
  
  queryresult <- queryresult[1:topX]

  if(APTquery %in% names(queryresult)){
    sucess <- sucess + 1
  }else{
    failure <- failure + 1
  }
}

cat("Settings: ", "TopX: ", topX, "sets Divided by: ", Divisor, "\n sucess rate: ",
    sucess, "failure rate: ", failure, "Total: ", length(result_query_dict), "\n Sucess rate: ",
    sucess, "/", length(result_query_dict), " in percentage: ", (sucess/length(result_query_dict))*100)




Costum_dict_APT_TPP_df <- as.data.frame(Costum_dict_APT_TPP_df)
