


remove_random_elements <- function(int_vec, percent_to_remove) {
  
  
  # Input validation
  stopifnot(is.integer(int_vec),
            percent_to_remove >= 0,
            percent_to_remove <= 100)
  
  # Early returns for boundary cases
  if (percent_to_remove == 0) return(int_vec)          # Keep 100% (original)
  if (percent_to_remove == 100) return(int_vec[0])     # Keep 0% (empty vector)
  if(percent_to_remove > 100) return(print("can't remove more data than 100% of the data"))
  if(percent_to_remove < 0) return(print("can't be fuller than the original data"))
  
  # Calculate exact number of elements to keep
  n_total <- length(int_vec)
  n_to_keep <- round(n_total * (1 - percent_to_remove/100))
  
  # Random sampling (handles n_to_keep = 0 automatically)
  int_vec[sample(n_total, n_to_keep)] 
}






library("bnlearn")
library(dplyr)

pwd <- "./dataforBayesiaNetwork/"

# load("/Users/morzen/Desktop/work/RprogramForMitreScrapperDatanalysis/Test_UniversalCode/Similarity&statisticalTest/BayesNetworkData/ManuFactorbayesian_networkV4_FULLDATA_newDAG.rda")
load(paste0(pwd, "bn_fit.rda"))
# three variabvle below not used in program but good for visualizing
comparison <- read.csv(paste0(pwd, "comparison_Encodedtest_df.csv"))
data_for_bn <- read.csv(paste0(pwd, "data_for_bn.csv"))




# ListOfID_Tech = c("T0847", "T0807")
# ListOfID_Soft = c("S0009", "S0002")
# ListOfID_CAPEC = c("CAPEC-150", "CAPEC-98")
# ListOfID_CWE = c("CWE-522", "CWE-1021")
# ListOfID_CVE = c("CVE-2024-26167", "CVE-2023-36920")
# ListOfID_Pulse = c("65f1c06e8876e33699dbf7f4", "62f28e895658ab6b557f73c9")
# 
# #remove NA
# 
# Tech_translated_values <- na.omit(factor(unique(comparison$techniques_Encoded[match(ListOfID_Tech, comparison$techniques_Origin)])))
# Soft_translated_values <- na.omit(factor(unique(comparison$software_Encoded[match(ListOfID_Soft, comparison$software_Origin)])))
# CAPEC_translated_values <- na.omit(factor(unique(comparison$CAPEC_ID_Encoded[match(ListOfID_CAPEC, comparison$CAPEC_ID_Origin)])))
# CWE_translated_values <- na.omit(factor(unique(comparison$CWE_ID_Encoded[match(ListOfID_CWE, comparison$CWE_ID_Origin)])))
# CVE_translated_values <- na.omit(factor(unique(comparison$CVE_ID_Encoded[match(ListOfID_CVE, comparison$CVE_ID_Origin)])))
# Pulse_translated_values <- na.omit(factor(unique(comparison$Pulse_ID_Encoded[match(ListOfID_Pulse, comparison$Pulse_ID_Origin)])))




name <- "APT29"
PercentageToRemove <- 0


encoded_value <- unique(comparison$APTnameEncoded[comparison$APTnameOrigin == name])
test <- subset(data_for_bn, APTname == encoded_value)

Tech_translated_values <- unique(test$techniques_str[!test$techniques_str %in% c(-1, NA)])
Tech_translated_values <- remove_random_elements(Tech_translated_values, PercentageToRemove)
Tech_translated_values <- factor(Tech_translated_values)

Soft_translated_values <- test$software_str[!test$software_str %in% c(-1, NA)]
Soft_translated_values <- remove_random_elements(Soft_translated_values, PercentageToRemove)
Soft_translated_values <- factor(Soft_translated_values)

CAPEC_translated_values <- test$CAPEC_ID[!test$CAPEC_ID %in% c(-1, NA)]
CAPEC_translated_values <- remove_random_elements(CAPEC_translated_values, PercentageToRemove)
CAPEC_translated_values <- factor(CAPEC_translated_values)

CWE_translated_values <- test$CWE_ID[!test$CWE_ID %in% c(-1, NA)]
CWE_translated_values <- remove_random_elements(CWE_translated_values, PercentageToRemove)
CWE_translated_values <- factor(CWE_translated_values)

CVE_translated_values <- test$CVE_ID[!test$CVE_ID %in% c(-1, NA)]
CVE_translated_values <- remove_random_elements(CVE_translated_values, PercentageToRemove)
CVE_translated_values <- factor(CVE_translated_values)

Pulse_translated_values <- test$Pulse_ID[!test$Pulse_ID %in% c(-1, NA)]
Pulse_translated_values <- remove_random_elements(Pulse_translated_values, PercentageToRemove)
Pulse_translated_values <- factor(Pulse_translated_values)

evidence <- list(
  techniques_str = Tech_translated_values,
  software_str = Soft_translated_values,
  CAPEC_ID = CAPEC_translated_values,
  CWE_ID = CWE_translated_values,
  CVE_ID = CVE_translated_values,
  Pulse_ID = Pulse_translated_values
)


APTidNumber <- data_for_bn[!duplicated(data_for_bn$APTname), ]$APTname

ResultsSingleQuery_ls <- list()
for(APT1id in APTidNumber){

  APT1DF <- subset(data_for_bn, APTname %in% APT1id)
  # Run the cpquery with the "lw" method using the loaded Bayesian network
  #likelihood weighting

  #logic sampling
  result_ls <- cpquery(
    bn_fit,
    event = (APTname == APT1id),
    evidence = (
      techniques_str %in% Tech_translated_values &  # Use %in% for sets
        software_str %in% Soft_translated_values &
        CAPEC_ID %in% CAPEC_translated_values &
        CWE_ID %in% CWE_translated_values &
        CVE_ID %in% CVE_translated_values &
        Pulse_ID %in% Pulse_translated_values
    ),
    method = "ls"
  )


  ResultsSingleQuery_ls[APT1id] <- result_ls

}


ls_result <- do.call(rbind,
                     Map(
                       function(res, idx) {
                         data.frame(results = res, row_id = idx)
                       },
                       ResultsSingleQuery_ls,
                       seq_along(ResultsSingleQuery_ls)  # Use numeric indices
                     ))

ls_result <- ls_result %>%
  left_join(
    comparison %>%
      select(APTnameEncoded, APTnameOrigin) %>%
      distinct(),  # Ensure no duplicates (if needed)
    by = c("row_id" = "APTnameEncoded")
  ) %>% select(-row_id, name = APTnameOrigin)










###############################################################################
#crosscomparison table
###############################################################################




library("bnlearn")
library(dplyr)

pwd <- "./dataforBayesiaNetwork/"

# load("/Users/morzen/Desktop/work/RprogramForMitreScrapperDatanalysis/Test_UniversalCode/Similarity&statisticalTest/BayesNetworkData/ManuFactorbayesian_networkV4_FULLDATA_newDAG.rda")
load(paste0(pwd, "bn_fit.rda"))
# three variabvle below not used in program but good for visualizing
comparison <- read.csv(paste0(pwd, "comparison_Encodedtest_df.csv"))
data_for_bn <- read.csv(paste0(pwd, "data_for_bn.csv"))

PercentageToRemove <- 0
APTidNumber <- data_for_bn[!duplicated(data_for_bn$APTname), ]$APTname


ResultDf <- matrix(NA, 
                        nrow = length(APTidNumber),
                        ncol = length(APTidNumber),
                        dimnames = list(APTidNumber, APTidNumber))

# Convert to data frame (will keep numeric names)
ResultDf <- as.data.frame(ResultDf)


i <- 0
for(encoded_value in colnames(ResultDf)){
  print(encoded_value)
  
  test <- subset(data_for_bn, APTname == encoded_value)
  #print(head(test))
  
  Tech_translated_values <- unique(test$techniques_str[!test$techniques_str %in% c(-1, NA)])
  Tech_translated_values <- remove_random_elements(Tech_translated_values, PercentageToRemove)
  Tech_translated_values <- factor(Tech_translated_values)
  
  Soft_translated_values <- test$software_str[!test$software_str %in% c(-1, NA)]
  Soft_translated_values <- remove_random_elements(Soft_translated_values, PercentageToRemove)
  Soft_translated_values <- factor(Soft_translated_values)
  
  CAPEC_translated_values <- test$CAPEC_ID[!test$CAPEC_ID %in% c(-1, NA)]
  CAPEC_translated_values <- remove_random_elements(CAPEC_translated_values, PercentageToRemove)
  CAPEC_translated_values <- factor(CAPEC_translated_values)
  
  CWE_translated_values <- test$CWE_ID[!test$CWE_ID %in% c(-1, NA)]
  CWE_translated_values <- remove_random_elements(CWE_translated_values, PercentageToRemove)
  CWE_translated_values <- factor(CWE_translated_values)
  
  CVE_translated_values <- test$CVE_ID[!test$CVE_ID %in% c(-1, NA)]
  CVE_translated_values <- remove_random_elements(CVE_translated_values, PercentageToRemove)
  CVE_translated_values <- factor(CVE_translated_values)
  
  Pulse_translated_values <- test$Pulse_ID[!test$Pulse_ID %in% c(-1, NA)]
  Pulse_translated_values <- remove_random_elements(Pulse_translated_values, PercentageToRemove)
  Pulse_translated_values <- factor(Pulse_translated_values)
  
  evidence <- list(
    techniques_str = Tech_translated_values,
    software_str = Soft_translated_values,
    CAPEC_ID = CAPEC_translated_values,
    CWE_ID = CWE_translated_values,
    CVE_ID = CVE_translated_values,
    Pulse_ID = Pulse_translated_values
  )

  #print(head(evidence))
  
  
  for(APT1id in rownames(ResultDf)){
    result_ls <- cpquery(
      bn_fit,
      event = (APTname == APT1id),
      evidence = (
        techniques_str %in% Tech_translated_values &
          software_str %in% Soft_translated_values &
          CAPEC_ID %in% CAPEC_translated_values &
          CWE_ID %in% CWE_translated_values &
          CVE_ID %in% CVE_translated_values &
          Pulse_ID %in% Pulse_translated_values
      ),
      method = "ls",
      n = 100000  # Increase sample size for better estimates
    )
    
    if(APT1id == APT1id && encoded_value == APT1id){
      print(paste0("result: ", result_ls))
    }
    ResultDf[APT1id, encoded_value] <- result_ls
  }
  
  
  
  if(i == 10){
    break
  }
  
  i <- i + 1
  
}




############################################
####################################################################################################################################



library("bnlearn")
library(dplyr)

pwd <- "./dataforBayesiaNetwork/"

# load("/Users/morzen/Desktop/work/RprogramForMitreScrapperDatanalysis/Test_UniversalCode/Similarity&statisticalTest/BayesNetworkData/ManuFactorbayesian_networkV4_FULLDATA_newDAG.rda")
load(paste0(pwd, "bn_fit.rda"))
# three variabvle below not used in program but good for visualizing
comparison <- read.csv(paste0(pwd, "comparison_Encodedtest_df.csv"))
data_for_bn <- read.csv(paste0(pwd, "data_for_bn.csv"))
# 1. Get the ENCODED value for APT29 (must match network's factor levels)
apt29_encoded <- which(levels(bn_fit$APTname$levels) == "97")[1]  # Use your target APT





name <- "APT29"
PercentageToRemove <- 0


encoded_value <- unique(comparison$APTnameEncoded[comparison$APTnameOrigin == name])
test <- subset(data_for_bn, APTname == encoded_value)

Tech_translated_values <- unique(test$techniques_str[!test$techniques_str %in% c(-1, NA)])
Tech_translated_values <- remove_random_elements(Tech_translated_values, PercentageToRemove)
Tech_translated_values <- factor(Tech_translated_values)

Soft_translated_values <- unique(test$software_str[!test$software_str %in% c(-1, NA)])
Soft_translated_values <- remove_random_elements(Soft_translated_values, PercentageToRemove)
Soft_translated_values <- factor(Soft_translated_values)

CAPEC_translated_values <- unique(test$CAPEC_ID[!test$CAPEC_ID %in% c(-1, NA)])
CAPEC_translated_values <- remove_random_elements(CAPEC_translated_values, PercentageToRemove)
CAPEC_translated_values <- factor(CAPEC_translated_values)

CWE_translated_values <- unique(test$CWE_ID[!test$CWE_ID %in% c(-1, NA)])
CWE_translated_values <- remove_random_elements(CWE_translated_values, PercentageToRemove)
CWE_translated_values <- factor(CWE_translated_values)

CVE_translated_values <- unique(test$CVE_ID[!test$CVE_ID %in% c(-1, NA)])
CVE_translated_values <- remove_random_elements(CVE_translated_values, PercentageToRemove)
CVE_translated_values <- factor(CVE_translated_values)

Pulse_translated_values <- unique(test$Pulse_ID[!test$Pulse_ID %in% c(-1, NA)])
Pulse_translated_values <- remove_random_elements(Pulse_translated_values, PercentageToRemove)
Pulse_translated_values <- factor(Pulse_translated_values)

evidence <- list(
  techniques_str = Tech_translated_values,
  software_str = Soft_translated_values,
  CAPEC_ID = CAPEC_translated_values,
  CWE_ID = CWE_translated_values,
  CVE_ID = CVE_translated_values,
  Pulse_ID = Pulse_translated_values
)






# 3. Execute LW query
result <- cpquery(
  bn_fit,
  event = (APTname == apt29_encoded),
  evidence = evidence,
  method = "lw",
  n = 100000
)

print(result)

