library("bnlearn")

pwd <- "./Test_UniversalCode/Similarity&statisticalTest/BaysianNetworkEncoded/BayesNetworkData/"

# load("/Users/morzen/Desktop/work/RprogramForMitreScrapperDatanalysis/Test_UniversalCode/Similarity&statisticalTest/BayesNetworkData/ManuFactorbayesian_networkV4_FULLDATA_newDAG.rda")
load(paste0(pwd, "bn_fit.rda"))
# three variabvle below not used in program but good for visualizing 
df <- read.csv(paste0(pwd, "df.csv"))
data_for_bn <- read.csv(paste0(pwd, "data_for_bn.csv"))

##########################
# SINGLE QUERY
##########################
df_NumericalEncodingLink <- cbind(df, Encoded_APTname = data_for_bn$APTname, Encoded_TechNsoftID = data_for_bn$Techniques_N_Software)
DF_techNsoft_encoded = cbind(NotEncoded = df_NumericalEncodingLink$Techniques_N_Software, Encoded = df_NumericalEncodingLink$Encoded_TechNsoftID)
DF_techNsoft_encoded <- data.frame(unique(DF_techNsoft_encoded))


#EXPECTED Lazarus Group from this repport: https://securelist.com/unveiling-lazarus-new-campaign/110888/
# ListOfID_TechNsoft = as.factor(c("T1189", "T1203", "T1547.012", "T1574.002", "T1547.012", "T1140", "T1574.002", "T1027.001", "T1027.002", "T1027.002", "T1003.001", "T1057", "T1082", "T1083", "T1113", "T1071.001", "T1132.002", "T1573.001", "T1041"))
#EXPECTED andariel from this repport: https://securelist.com/andariel-evolves-to-target-south-korea-with-ransomware/102811/
# ListOfID_TechNsoft = as.factor(c("T1584.006", "T1583.003", "T1566.001", "T1204.002", "T1059.007", "T1036.005", "T1027.003", "T1497.001", "T1049", "T1057", "T1113", "T1071.001", "T1095", "T1573.001", "T1041", "T1486"))
#EXPECTED Sandworm from this repport: https://cloud.google.com/blog/topics/threat-intelligence/sandworm-disrupts-power-ukraine-operational-technology/
ListOfID_TechNsoft = as.factor(c("T0847", "T0807", "T0871", "T0853", "T0872", "T0809", "T0855", "T0831"))

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
ResultsSingleQuery_ls <- list()


# evidence_columns <- c("Table_Types", "Techniques_N_Software", "Tactic_s", "Domain")
#evidence_columns <- c("Techniques_N_Software")#, "Tactic_s", "Domain")
for(APT1id in APTidNumber){
  
  APT1DF <- subset(data_for_bn, APTname %in% APT1id)
  # Run the cpquery with the "lw" method using the loaded Bayesian network
  #likelihood weighting
  result_lw <- cpquery(bn_fit, event = APTname == APT1id, evidence = evidence, method = "lw")#, n = 1000)
  
  #logic sampling
  result_ls <-  cpquery(bn_fit,
                        event = APTname == APT1id,
                        evidence = Techniques_N_Software == evidence2$Techniques_N_Software,
                        method = "ls")#, n = 1000)
  
  ResultsSingleQuery_lw[APT1id] <- result_lw
  ResultsSingleQuery_ls[APT1id] <- result_ls
  
}

lw_result <- do.call(rbind,
                     Map(
                       data.frame,
                       APTname = unique(df$APTname),
                       results = ResultsSingleQuery_lw
                     ))
rownames(lw_result) <- seq(nrow(lw_result))

ls_result <- do.call(rbind,
                     Map(
                       data.frame,
                       APTname = unique(df$APTname),
                       results = ResultsSingleQuery_ls
                     ))
rownames(ls_result) <- seq(nrow(ls_result))


##########################
# Normal bayesian network query with Likelihood weighting LW and logic sampling LS
# cross comparison matrix
##########################

APTidNumber <- data_for_bn[!duplicated(data_for_bn$APTname), ]$APTname

ResultTable <- data.frame(matrix(nrow = length(APTidNumber), ncol = length(APTidNumber)))
colnames(ResultTable) <- APTidNumber
rownames(ResultTable) <- APTidNumber

ResultTable_lw <- ResultTable
ResultTable_ls <- ResultTable

# evidence_columns <- c("Table_Types", "Techniques_N_Software", "Tactic_s", "Domain")
#evidence_columns <- c("Techniques_N_Software")#, "Tactic_s", "Domain")
for(APT1id in APTidNumber){
  APT1DF <- subset(data_for_bn, APTname %in% APT1id)
  
  for(APT2id in APTidNumber){
    APT2DF <- subset(data_for_bn, APTname %in% APT2id)
    # Create a list of evidence
    # evidence <- list(
    #   Table_Types = factor(APT2DF$Table_Types),
    #   Techniques_N_Software = factor(APT2DF$Techniques_N_Software),
    #   Tactic_s = factor(APT2DF$Tactic_s),
    #   Domain = factor(APT2DF$Domain)
    # )
    evidence <- list(
      Techniques_N_Software = factor(APT2DF$Techniques_N_Software)#,
      # Tactic_s = factor(APT2DF$Tactic_s)
      # Domain = factor(APT2DF$Domain)
    )
    
    # Run the cpquery with the "lw" method using the loaded Bayesian network
    #likelihood weighting
    result_lw <- cpquery(bn_fit, event = APTname == APT1id, evidence = evidence, method = "lw")#, n = 1000)
    
    # result_ls <-  cpquery(bn_fit,
    #                       event = APTname == 2,
    #                       evidence = Table_Types == APT2DF$Table_Types &
    #                         Techniques_N_Software == APT2DF$Techniques_N_Software &
    #                         Tactic_s == APT2DF$Tactic_s &
    #                         Domain == APT2DF$Domain,
    #                       method = "ls")#, n = 1000)
    
    #logic sampling
    # print(APT2DF$Techniques_N_Software)
    result_ls <-  cpquery(bn_fit,
                          event = APTname == APT1id,
                          evidence = Techniques_N_Software == APT2DF$Techniques_N_Software,
                          method = "ls")#, n = 1000)
    
    # result_ls <-  cpquery(bn_fit,
    #                       event = APTname == APT1id,
    #                       evidence = Techniques_N_Software == APT2DF$Techniques_N_Software &
    #                         Tactic_s == APT2DF$Tactic_s, #&
    #                         # Domain == APT2DF$Domain,
    #                       method = "ls")#, n = 1000)
    
    # place commands here
    ResultTable_lw[APT1id, APT2id] <- result_lw*100
    ResultTable_ls[APT1id, APT2id] <- result_ls*100
    
    
    
    
    
    
  }
  
}



APTnamesNodplic <- df[!duplicated(df$APTname), ]$APTname
colnames(ResultTable_lw) <- APTnamesNodplic
rownames(ResultTable_lw) <- APTnamesNodplic

colnames(ResultTable_ls) <- APTnamesNodplic
rownames(ResultTable_ls) <- APTnamesNodplic




# write.csv(ResultTable_lw, "/Users/morzen/Desktop/BayesianResultsForMphilTransfer.csv", row.names = FALSE)



##########################
# outlier and ratio of success analysis
##########################
totalListOfRatio_lw <- list()
totalListOfRatio_ls <- list()

totalListoutlier_lw <- list()
totalListoutlier_ls <- list()


ratioFunc <- function(x, k){
  INputDF <- x
  topK <- k
  
  # write.csv(test1, "/Users/morzen/Desktop/likelihood weighting2.csv", row.names = FALSE)
  # write.csv(test2, "/Users/morzen/Desktop/likelihood sampling2.csv", row.names = FALSE)
  
  keys1 <- unique(row.names(INputDF))
  RatioSucessFail1 <- setNames(rep("", length(keys1)), keys1)
  
  listOflist_mostCommonOutLier <- list()
  
  for(i in seq_along(INputDF)) {
    
    
    new_data <- subset(INputDF, select = row.names(INputDF)[i])
    
    
    sorted_indices <- order(new_data[[row.names(INputDF)[i]]], decreasing = TRUE)
    sorted_dataframe <- as.data.frame(new_data[sorted_indices, ])
    
    colnames(sorted_dataframe) <- colnames(new_data)
    row.names(sorted_dataframe) <- row.names(new_data)[sorted_indices]
    
    
    top_X_df <- head(sorted_dataframe, topK)
    
    if(row.names(INputDF)[i] %in% row.names(top_X_df)){
      RatioSucessFail1[row.names(INputDF)[i]] <- TRUE
    }else{
      RatioSucessFail1[row.names(INputDF)[i]] <- FALSE
      listOflist_mostCommonOutLier <- append(listOflist_mostCommonOutLier, row.names(INputDF)[i])
    }
    
    # if(i == 3){
    #   break
    # }
  }
  
  RatioSucessFail1 <- as.logical(RatioSucessFail1)
  true_count <- sum(RatioSucessFail1)
  false_count <- length(RatioSucessFail1) - true_count
  
  print(paste("true: ",true_count,"/",false_count," :false"))
  perctangeSucess <- (true_count / length(RatioSucessFail1)) * 100
  print((true_count / length(RatioSucessFail1)) * 100)
  
  
  return(list(Return1 = perctangeSucess, Return2 = listOflist_mostCommonOutLier))
}



for(i in seq(100)){
  print(i)
  
  APTidNumber <- data_for_bn[!duplicated(data_for_bn$APTname), ]$APTname
  
  ResultTable <- data.frame(matrix(nrow = length(APTidNumber), ncol = length(APTidNumber)))
  colnames(ResultTable) <- APTidNumber
  rownames(ResultTable) <- APTidNumber
  
  ResultTable_lw <- ResultTable
  ResultTable_ls <- ResultTable
  
  # evidence_columns <- c("Table_Types", "Techniques_N_Software", "Tactic_s", "Domain")
  #evidence_columns <- c("Techniques_N_Software")#, "Tactic_s", "Domain")
  for(APT1id in APTidNumber){
    APT1DF <- filter(data_for_bn, APTname %in% APT1id)
    
    for(APT2id in APTidNumber){
      APT2DF <- filter(data_for_bn, APTname %in% APT2id)
      # Create a list of evidence
      # evidence <- list(
      #   Table_Types = factor(APT2DF$Table_Types),
      #   Techniques_N_Software = factor(APT2DF$Techniques_N_Software),
      #   Tactic_s = factor(APT2DF$Tactic_s),
      #   Domain = factor(APT2DF$Domain)
      # )
      evidence <- list(
        Techniques_N_Software = factor(APT2DF$Techniques_N_Software)#,
        # Tactic_s = factor(APT2DF$Tactic_s)
        # Domain = factor(APT2DF$Domain)
      )
      
      # Run the cpquery with the "lw" method using the loaded Bayesian network
      #likelihood weighting
      result_lw <- cpquery(bn_fit, event = APTname == APT1id, evidence = evidence, method = "lw")#, n = 1000)
      
      # result_ls <-  cpquery(bn_fit,
      #                       event = APTname == 2,
      #                       evidence = Table_Types == APT2DF$Table_Types &
      #                         Techniques_N_Software == APT2DF$Techniques_N_Software &
      #                         Tactic_s == APT2DF$Tactic_s &
      #                         Domain == APT2DF$Domain,
      #                       method = "ls")#, n = 1000)
      
      #logic sampling
      result_ls <-  cpquery(bn_fit,
                            event = APTname == APT1id,
                            evidence = Techniques_N_Software == APT2DF$Techniques_N_Software,
                            method = "ls")#, n = 1000)
      
      # result_ls <-  cpquery(bn_fit,
      #                       event = APTname == APT1id,
      #                       evidence = Techniques_N_Software == APT2DF$Techniques_N_Software &
      #                         Tactic_s == APT2DF$Tactic_s, #&
      #                         # Domain == APT2DF$Domain,
      #                       method = "ls")#, n = 1000)
      
      # place commands here
      ResultTable_lw[APT1id, APT2id] <- result_lw*100
      ResultTable_ls[APT1id, APT2id] <- result_ls*100
      
      
      
      
      
      
    }
    
  }
  
  
  
  APTnamesNodplic <- df[!duplicated(df$APTname), ]$APTname
  colnames(ResultTable_lw) <- APTnamesNodplic
  rownames(ResultTable_lw) <- APTnamesNodplic
  
  colnames(ResultTable_ls) <- APTnamesNodplic
  rownames(ResultTable_ls) <- APTnamesNodplic
  ##################################################
  
  
  result1 <- ratioFunc(ResultTable_lw, 5)
  result2 <- ratioFunc(ResultTable_ls, 5)
  
  totalListOfRatio_lw <- append(totalListOfRatio_lw, result1$Return1)
  totalListOfRatio_ls <- append(totalListOfRatio_ls, result2$Return1)
  
  totalListoutlier_lw <- append(totalListoutlier_lw, result1$Return2)
  totalListoutlier_ls <- append(totalListoutlier_ls, result2$Return2)
  
}


# Extract integers from the list
my_integers_lw <- unlist(totalListOfRatio_lw)

avg_lw <- mean(my_integers_lw)


my_integers_ls <- unlist(totalListOfRatio_ls)

avg_ls <- mean(my_integers_ls)

####################################################