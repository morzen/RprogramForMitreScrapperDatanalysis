library("fmsb")
library("rjson")
library("ggplot2")
library("tidyverse")
library("dplyr")
library("plyr")
library("stringr")
library("data.table")
library("bnlearn")

FinalDataFrame <- read.csv("./TestFinal_df.csv")
FinalDataFrame <- subset(FinalDataFrame, select = -c(id))
################################################################################
# BaysNetwork 
################################################################################
df <- FinalDataFrame


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

data_for_bn$name <- factor(data_for_bn$name)
data_for_bn$techniques_str <- factor(data_for_bn$techniques_str)
data_for_bn$software_str <- factor(data_for_bn$software_str)
data_for_bn$CAPEC_ID <- factor(data_for_bn$CAPEC_ID)
data_for_bn$CWE_ID <- factor(data_for_bn$CWE_ID)
data_for_bn$CVE_ID <- factor(data_for_bn$CVE_ID)
data_for_bn$Pulse_ID <- factor(data_for_bn$Pulse_ID)



comparison <- do.call(rbind,
                      Map(
                        data.frame,
                        APTnameOrigin=df$name,
                        APTnameEncoded=data_for_bn$name,
                        techniques_Origin=df$techniques_str,
                        techniques_Encoded=data_for_bn$techniques_str,
                        software_Origin=df$software_str,
                        software_Encoded=data_for_bn$software_str,
                        CAPEC_ID_Origin=df$CAPEC_ID,
                        CAPEC_ID_Encoded=data_for_bn$CAPEC_ID,
                        CWE_ID_Origin=df$CWE_ID,
                        CWE_ID_Encoded=data_for_bn$CWE_ID,
                        CVE_ID_Origin=df$CVE_ID,
                        CVE_ID_Encoded=data_for_bn$CVE_ID,
                        Pulse_ID_Origin=df$Pulse_ID,
                        Pulse_ID_Encoded=data_for_bn$Pulse_ID
                      ))
# ################################################
write.csv(data_for_bn, file = "data_for_bn_Encodedtest_df.csv", row.names = FALSE)
write.csv(comparison, file = "comparison_Encodedtest_df.csv", row.names = FALSE)

data_for_bn <- read.csv("./data_for_bn_Encodedtest_df.csv")
comparison <- read.csv("./comparison_Encodedtest_df.csv")



imputed_df <-data_for_bn

imputed_df <- as.data.frame(lapply(imputed_df, function(x) {
  if(is.integer(x)) as.factor(x) else x
}))

# Verify the conversion
str(imputed_df)


# learned_structure <- hc(imputed_df, maxp = 4)
#hc not hierachical clustering but hill climbing

gc()

#bic works but the dag isn't great
#learned_structure <- hc(imputed_df, score = "bic")
#bde is the best so far
learned_structure <- hc(imputed_df, score = "bde")
#pnal doesn't work #pnal : Pearson's Negative Affinity Linkage to cluster
# learned_structure <- hc(imputed_df, score = "pnal")
plot(learned_structure)

bn <-learned_structure

# Fit the Bayesian network
#maximum likelihood mle
bn_fit <- bn.fit(bn, imputed_df, method = "mle")




save(bn_fit, file = "./Test_UniversalCode/Similarity&statisticalTest/BaysianNetworkEncoded/BayesNetworkData/bn_fit.rda")

write.csv(df, "./Test_UniversalCode/Similarity&statisticalTest/BaysianNetworkEncoded/BayesNetworkData/df.csv", row.names = FALSE)
write.csv(data_for_bn, "./Test_UniversalCode/Similarity&statisticalTest/BaysianNetworkEncoded/BayesNetworkData/data_for_bn.csv", row.names = FALSE)
##############################################################################

