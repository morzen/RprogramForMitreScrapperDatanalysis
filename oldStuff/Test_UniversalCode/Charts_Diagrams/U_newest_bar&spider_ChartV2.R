library("fmsb")
library("rjson")
library("ggplot2")
library("tidyverse")
library("dplyr")
library("plyr")
library("stringr")
library("data.table")


FinalDataFrame <- read.csv("./1_DATAusedFolder/FinalDataFrame_Apr2024.csv")

Selected_MitreUpddateVersion <- "Data_Apr2024"

Selected_ScrapDate <- "24-04-2024"

################
#functions
################
Total_Tech_tactics_counter <- function(x, y){
  NoDuplicateTacticsFunc <- x
  TechdataDF <- y
  
  # NoDuplicateTacticsFunc <- NoDuplicateTactics
  # TechdataDF <- Tech_data_DF
  
  for(i in 1:length(TechdataDF$TechTactics)){
    CheckComma <- grepl(",", TechdataDF$TechTactics[i])
    # print(CheckComma)
    # print(TechdataDF$TechTactics)
    if(CheckComma == TRUE){
      TechdataDF$TechTactics[i] <- str_trim(TechdataDF$TechTactics[i], "left")#remove first space
      # print(TechdataDF$TechTactics[i])
      
      TechdataDF$TechTactics[i] <- strsplit(as.character(TechdataDF$TechTactics[i]), split = ", ")
      
      
      # print(TechdataDF$TechTactics[i])
      
    }
  }
  
  
  TotalTechTacticCount <- list()
  for(Tactic in NoDuplicateTactics$NoDuplicateTactics){
    j <- 0
    
    for(i in 1:length(TechdataDF$TechTactics)){
      for(element in TechdataDF$TechTactics[i]){
        if(length(element) > 1){
          for(k in 1:length(element)){
            if(element[k] == Tactic){
              j <- j + 1
            }
          }
        }
        if(length(element) == 1){
          if(element == Tactic){
            j <- j + 1
          }
        }
      }
    }
    TotalTechTacticCount <- append(TotalTechTacticCount, j)
  }
  
  TotalTechTacticCount <- as.data.frame(t(as.data.frame(TotalTechTacticCount)))
  rownames(TotalTechTacticCount) <- NoDuplicateTacticsFunc$NoDuplicateTactics
  
  return(TotalTechTacticCount)
}


tactics_adding_Func <- function(df) {

  GroupsDF <- df %>%
    filter(PageType == "Groups") %>%
    filter(Tables_TableType %in% c("Techniques Used", "Software"))
  
  TechDF <- df %>%
    filter(Update_Date == Selected_MitreUpddateVersion, PageType == "Techniques")
  
  # Merge Tactic_s columns
  TechDF$Tactic_s <- ifelse(is.na(TechDF$SumUpTable_Tactic), TechDF$SumUpTable_Tactics, TechDF$SumUpTable_Tactic)
  
  # Drop NAs
  # GroupsDF <- GroupsDF %>% select(where(~ !all(is.na(.))))
  # TechDF <- TechDF %>% select(where(~ !all(is.na(.))))
  # 
  GroupsDF <- GroupsDF[rowSums(is.na(GroupsDF)) != ncol(GroupsDF), ]
  GroupsDF <- GroupsDF[, colSums(is.na(GroupsDF)) != nrow(GroupsDF)]
  
  TechDF <- TechDF[rowSums(is.na(TechDF)) != ncol(TechDF), ]
  TechDF <- TechDF[, colSums(is.na(TechDF)) != nrow(TechDF)]
  
  # Extract unique Technique_ID and Tactic_s combinations
  TacticDF <- data.frame(cbind(SumUpTable_ID = TechDF$SumUpTable_ID,
                               Tactic_s = TechDF$Tactic_s))
  
  TacticDF <- TacticDF[!duplicated(TacticDF), ]
  
  # Initialize an empty list to store Tactic_s values
  Group_TacticDF <- data.frame(Tactics = character())
  # Loop through Tables_ID values in GroupsDF
  for (ID in GroupsDF$Tables_ID) {
    if (ID %in% TacticDF$SumUpTable_ID) {
      IndexForIDlist <- which(TacticDF$SumUpTable_ID == ID)
      
      if(!(length(IndexForIDlist) == 1)){
        print(IndexForIDlist)
        break
      }
      
      value <- TacticDF$Tactic_s[IndexForIDlist]
      new_row <- data.frame(Tactics = value)
      Group_TacticDF <- rbind(Group_TacticDF, new_row)
      
    } else {
      value <- NA
      new_row <- data.frame(Tactics = value)
      Group_TacticDF <- rbind(Group_TacticDF, new_row)
    }
  }
  
  # Add Tactic_s values to GroupsDF
  GroupsDF$Tactic_s <- Group_TacticDF$Tactics
  
  return(as.data.frame(GroupsDF))
}


ID_Name_lookUp <- function(x, y){
  APTlookup <- x
  df <- y
  
  df <- df %>% filter(PageType == "Groups")
  ID_and_Name_DF <- data.frame(unique(cbind(ID = df$Title_ID,
                                            Name = df$Title_Name)))
  
  ID_and_Name <- c()
  for(APTnameOrID in APTlookup){
    
    if(str_detect(APTnameOrID, "G[0-9]{4}") == TRUE){
      if(APTnameOrID %in% ID_and_Name_DF$ID){
        row_index <- which(ID_and_Name_DF$ID == APTnameOrID)
        Name <- ID_and_Name_DF$Name[row_index]
        IDandName <- paste(APTnameOrID, Name)
        
      }
    }else if (str_detect(APTnameOrID, "G[0-9]{4}") == FALSE){
      if(APTnameOrID %in% ID_and_Name_DF$Name){
        row_index <- which(ID_and_Name_DF$Name == APTnameOrID)
        ID <- ID_and_Name_DF$ID[row_index]
        IDandName <- paste(ID, APTnameOrID)
      }
    }else{
      print("you did not enter an ID or name")
    }
    
    ID_and_Name <- c(ID_and_Name, IDandName)
  }
  
  return(ID_and_Name)
}

APT_lookup_and_Counter <- function(x, y){
  
  APTlookup <- x
  df <- y
  
  
  ID_and_Name_DF <- data.frame(unique(cbind(ID = df$Title_ID,
                                            Name = df$Title_Name)))
  
  ID_and_Name <- c()
  APT_tactic_counter_DF <- data.frame(Rowname = NoDuplicateTactics)
  for(APTnameOrID in APTlookup){
    
    if(str_detect(APTnameOrID, "G[0-9]{4}") == TRUE){
      if(APTnameOrID %in% ID_and_Name_DF$ID){
        row_index <- which(ID_and_Name_DF$ID == APTnameOrID)
        ID <- APTnameOrID
        name <- ID_and_Name_DF$Name[row_index]
        IDandName <- paste(ID, name)
        
      }
    }else if (str_detect(APTnameOrID, "G[0-9]{4}") == FALSE){
      if(APTnameOrID %in% ID_and_Name_DF$Name){
        row_index <- which(ID_and_Name_DF$Name == APTnameOrID)
        ID <- ID_and_Name_DF$ID[row_index]
        name <- APTnameOrID
        IDandName <- paste(ID, name)
      }
    }else{
      print("you did not enter an ID or name")
    }
    
    APT_DF <- df %>% filter(Title_Name == name)
    APT_DF <- APT_DF %>% filter(Tables_TableType == "Techniques Used")
    APT_data_DF <-  unique(data.frame(APTname = APT_DF$Title_Name, TechID = APT_DF$Tables_ID, TechTactics = APT_DF$Tactic_s))
    
    APTcount<- Total_Tech_tactics_counter(NoDuplicateTactics, APT_data_DF)
    APT_tactic_counter_DF[[IDandName]] <- APTcount$V1
    
  }
  rownames(APT_tactic_counter_DF) <- APT_tactic_counter_DF[, 1]
  APT_tactic_counter_DF <- APT_tactic_counter_DF[-1]
  
  return(APT_tactic_counter_DF)
}


barChartFunc <- function(x){
  BarTest <- x

  BarTest <- BarTest %>% 
    tidyr::pivot_longer(cols = AllTechCount:colnames(BarTest)[ncol(BarTest)])
  
  # Convert Tactics to a factor with levels in the desired order
  BarTest$Tactics <- factor(BarTest$Tactics, levels = unique(BarTest$Tactics))
  
  # Create the bar chart
  ggplot(BarTest, aes(x = Tactics, y = value, fill = name)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
    geom_text(aes(label = value), position = position_dodge(width = 0.9), vjust = -0.25) +
    labs(title = "Bar Chart APTs Technique by Tactics", x = "Tactics", y = "Value")
  
}


SpiderGrapgFunc <- function(x){
  df <- FinalDF_count
  AllDataframeValues <- subset(df, select = -1)
  
  data <- t(AllDataframeValues)
  
  AllDataframeValues <- unlist(AllDataframeValues)
  
  # Filter numeric columns
  numeric_df <- df[sapply(df, is.numeric)]
  
  # Get the maximum value in the numeric columns
  HighestOccurence <- max(numeric_df, na.rm = TRUE)
  
  
  
  
  
  data <- rbind(HighestOccurence,  0, data)
  
  # colnames(data) <- colnames(data)
  # rownames(data) <- row.names(data)
  
  radarchart(data.frame(data), axistype = 3, 
             plwd = 1, plty = 1
  )
  data2 <- rownames(data)
  data2 <- data2[-c(1,2)]
  
  legend(x=1, y=1,
         legend = data2,
         bty = "n", pch=20 ,
         col = 1:length(data2),
         text.col = "black", cex=1.2, pt.cex=2.5)
  
  
}


###################
#program
####

FinalDataFrame <- FinalDataFrame %>% filter(Update_Date == Selected_MitreUpddateVersion) %>% filter(Latest_ScrapDate_For_Given_Update == Selected_ScrapDate)
NoDuplicateTactics <- unique(FinalDataFrame$Title_Tactic) %>% na.omit() %>% data.frame() %>% set_names(c("NoDuplicateTactics"))

#Total tech part
Tech_DF <- FinalDataFrame %>% filter(Update_Date == Selected_MitreUpddateVersion, PageType == "Techniques")
Tech_DF$Tactic_s <- ifelse(is.na(Tech_DF$SumUpTable_Tactic), Tech_DF$SumUpTable_Tactics, Tech_DF$SumUpTable_Tactic)
Tech_data_DF <-  unique(data.frame(TechID = Tech_DF$Title_ID, TechTactics = Tech_DF$Tactic_s))

Total_tech_tactic_count <- Total_Tech_tactics_counter(NoDuplicateTactics, Tech_data_DF)

#total group part
GroupDF_with_tactics_associated <- tactics_adding_Func(FinalDataFrame)
Group_DF <- GroupDF_with_tactics_associated %>% filter(Tables_TableType == "Techniques Used")
Group_data_DF <-  unique(data.frame(TechID = Group_DF$Tables_ID, TechTactics = Group_DF$Tactic_s))

Total_Group_tactic_count <- Total_Tech_tactics_counter(NoDuplicateTactics, Group_data_DF)

#APT lookup part
APTlookup <- c("G0016", "G0018", "G0032")

APT_looked_up_Count <- APT_lookup_and_Counter(APTlookup, Group_DF)
###################





FinalDF_count <- cbind(Tactics = NoDuplicateTactics$NoDuplicateTactics, AllTechCount = Total_tech_tactic_count$V1, AllGroupsTechCount = Total_Group_tactic_count$V1, APT_looked_up_Count)

#remove AllTechCount and AllGroupsTechCount
FinalDF_count <- subset(FinalDF_count, select = -c(AllTechCount, AllGroupsTechCount))

barChartFunc(FinalDF_count)


SpiderGrapgFunc(FinalDF_count)

###############################
#testin area
###############################
