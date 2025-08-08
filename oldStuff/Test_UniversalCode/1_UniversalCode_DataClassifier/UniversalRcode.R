library("fmsb")
library("rjson")
library("ggplot2")
library("tidyverse")
library("dplyr")
library("plyr")
library("stringr")
library("data.table")
library("jsonlite")

exit <- function() { invokeRestart("abort") } 


#create One fuull dataset using all json files for every updates created by the srcaping python tool
DataSetCreation <- function(){
  PathToResult <- "/Users/morzen/Desktop/MitreAttackAPTscrapper/results"
  
  FilesInPath <- list.files(path = PathToResult, pattern = "Rformat_", full.names = TRUE, recursive = TRUE)
  
  fullDataSet <- list()
  
  for(Pwd in FilesInPath){
    VersionOfMitreUpdate <- gsub('/','', str_extract(Pwd, "\\/Data_[a-z,A-Z]{3}[0-9]{4}\\/"))
    ScrapDate <- gsub('/','', str_extract(Pwd, "\\/([0-9]{1,2}-[0-9]{1,2}-[0-9]{1,4})\\/"))
    
    if(grepl("Rformat_Tactics|Rformat_Techniques|Rformat_Software|Rformat_Groups", Pwd) == TRUE){
      datatype <- str_extract(Pwd, "Tactics|Techniques|Software|Groups")
      
      # Read the contents of the file
      file_contents <- readLines(Pwd)
      
      # Convert the file contents to a JSON object
      json_data <- fromJSON(paste(file_contents, collapse = ""))
      
      fullDataSet[[VersionOfMitreUpdate]][[ScrapDate]][[datatype]] <- append(fullDataSet[[VersionOfMitreUpdate]][[ScrapDate]][[datatype]], json_data)
    }
  }
  
  return(fullDataSet)
}


#gather all the needed tags for the column name in order to change the full dataset to a dataframe
GettingTags <- function(x){
  fullDataSet <- x
  
  
  TitleTagNoDuplicate <- list()
  SumUpTableNoDuplicate <- list()
  TableType_list <- list()
  TablesTag <- list()
  
  for(Update_Date in names(fullDataSet)){
    Latest_ScrapDate_For_Given_Update <- max(names(fullDataSet[[Update_Date]]))
    for(PageType in names(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]])){
      for(DataTypeID in names(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]])){
        for(Specific_DataType_Page_Elements in names(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]])){
          if(Specific_DataType_Page_Elements == "title"){
            
            for(IndividualTag in names(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]])){
              if(!(IndividualTag %in% TitleTagNoDuplicate)){
                TitleTagNoDuplicate <- append(TitleTagNoDuplicate, IndividualTag)
                
              }
            }
            
          }else if(Specific_DataType_Page_Elements == "URL"){
            next
            
          }else if(Specific_DataType_Page_Elements == "SumUpTable"){
            
            for(IndividualTag in names(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]])){
              if(!(IndividualTag %in% SumUpTableNoDuplicate)){
                SumUpTableNoDuplicate <- append(SumUpTableNoDuplicate, IndividualTag)
                
              }
            }
            
          }else if(Specific_DataType_Page_Elements == "HeadersAndTables"){
            for(TableType in names(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]])){
              
              if(!(TableType %in% TableType_list)){
                TableType_list <- append(TableType_list, TableType)
                
              }
              
              for(IDorName in names(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]][[TableType]])){
                
                for( IndividualTag in names(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]][[TableType]][[IDorName]])){
                  
                  if(!(IndividualTag %in% TablesTag)){
                    TablesTag <- append(TablesTag, IndividualTag)
                    
                  }
                }
              }
            }
            
          }else if(Specific_DataType_Page_Elements == "ScrapDate"){
            next
            
          }else{
            print("page element unknown")
          }
          
          
        }
        # break
      }
      # break
    }
    # break
  }
  
  TitleTagNoDuplicate <- unique(TitleTagNoDuplicate)
  SumUpTableNoDuplicate <- unique(SumUpTableNoDuplicate)
  TablesTag <- unique(TablesTag)
  
  TitleTagNoDuplicate <- t(TitleTagNoDuplicate)
  TitleTagNoDuplicate <- t(TitleTagNoDuplicate)
  
  SumUpTableNoDuplicate <- t(SumUpTableNoDuplicate)
  SumUpTableNoDuplicate <- t(SumUpTableNoDuplicate)
  
  TablesTag <- t(TablesTag)
  TablesTag <- t(TablesTag)
  
  
  Tags <- list(TitleTag = TitleTagNoDuplicate, SumUpTableTag = SumUpTableNoDuplicate, TablesTag = TablesTag, TableTypes = TableType_list)
  
  
  
  return(Tags)
}



############################################################################
#CVEa
############################################################################


findCVE <- function(x, y){
  string <- x
  pattern <- y
  
  
  match_positions <- gregexpr(pattern, string)
  
  matched_patterns <- regmatches(string, match_positions)[[1]]
  
  if(length(matched_patterns) > 1){
    matched_patterns <- paste(matched_patterns, collapse = ", ")
  }
  
  return(as.character(matched_patterns))
}


CVE_column_addingFunc <- function(x){
  endresult <- x 
  
  lengthNrow <- nrow(endresult)
  
  CVE_df <- data.frame(
    MixCVE = rep(NA, lengthNrow),
    CVE_use = rep(NA, lengthNrow),
    CVE_description = rep(NA, lengthNrow)
  )
  
  pattern <- "CVE-\\d{4}-\\d{4,7}|CVE \\d{4}-\\d{4,7}|cve-\\d{4}-\\d{4,7}|cve \\d{4}-\\d{4,7}"
  for(i in seq(1, nrow(endresult))){
    
    if(grepl(pattern, endresult$Tables_Use[i], ignore.case = TRUE) == TRUE){
      # print("Use")
      item <- findCVE(endresult$Tables_Use[i], pattern)
      
      CVE_df[i, "MixCVE"] <- item
      CVE_df[i, "CVE_use"] <- item
      
    }else if(grepl(pattern, endresult$Tables_Description[i], ignore.case = TRUE) == TRUE){
      # print("Description")
      item <- findCVE(endresult$Tables_Description[i], pattern)
      
      CVE_df[i, "MixCVE"] <- item
      CVE_df[i, "CVE_description"] <- item
      
    }
    
  }
  
  df <- cbind(endresult, CVE_df)
  
  
  return(df)
  
}

############################################################################
#MAIN
############################################################################
#here the fulldataset change into a big dataframe

#to reactivate function uncoment the one line below
MainFunc <- function(fullDataSet, tags){
 
  
  fullDataSet <- fullDataSet
  Tags <- tags
  
  fullDataFrame <- data.frame()
  for(Update_Date in names(fullDataSet)){
    #setting up Update variable and latest scrap date for given update
    Latest_ScrapDate_For_Given_Update <- max(names(fullDataSet[[Update_Date]]))
    print(Update_Date)
    print(Latest_ScrapDate_For_Given_Update)
    ############
    
    for(PageType in names(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]])){
      # print(PageType)
      for(DataTypeID in names(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]])){
        # print(DataTypeID) #One page
        
        for(Specific_DataType_Page_Elements in names(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]])){
          #print(Specific_DataType_Page_Elements)
          
          
          if(Specific_DataType_Page_Elements == "title"){
            # print("title")
            
            columns = Tags$TitleTag
            titledf = data.frame(matrix(nrow = 1, ncol = length(columns)))
            colnames(titledf) = columns
            
            for(Tag in colnames(titledf)){
              #print(Tag)
              #print(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]][[Tag]])
              item <- fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]][[Tag]]
              
              #titledf[Tag] <- item
              
              if(length(item) > 1 ){
                item <- list(item)
                
              }else{
                if(is.null(item) == FALSE){
                  titledf[Tag] <- item
                }
                else{
                  titledf[Tag] <- NA
                }
                
              }
              
            }
            colnames(titledf) <- paste("Title", colnames(titledf), sep = "_")
            
          }else if(Specific_DataType_Page_Elements == "URL"){
            # print("URL")
            
            #print(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]])
            WebPageURL <- fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]]
            
          }else if(Specific_DataType_Page_Elements == "SumUpTable"){
            # print("SumUpTable")
            
            
            columns = Tags$SumUpTableTag
            SumUpTabledf = data.frame(matrix(nrow = 1, ncol = length(columns)))
            colnames(SumUpTabledf) = columns
            
            for(Tag in colnames(SumUpTabledf)){
              #print(Tag)
              #print(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]][[Tag]])
              item <- fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]][[Tag]]
              #SumUpTabledf[Tag] <- item
              
              if(is.null(item) == FALSE){
                SumUpTabledf[Tag] <- item
              }
              else{
                SumUpTabledf[Tag] <- NA
              }
              
            }
            colnames(SumUpTabledf) <- paste("SumUpTable", colnames(SumUpTabledf), sep = "_")
            
          }else if(Specific_DataType_Page_Elements == "HeadersAndTables"){
            # print("HeadersAndTables")
            HeadersAndTablesdf <- data.frame()
            
            for(TableType in names(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]])){
              
              
              if(typeof(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]][[TableType]]) == "list"){
                # print("LIST")
                
                for(IDorName in names(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]][[TableType]])){
                  # print(paste("IDorName:", IDorName))
                  SingleItemdf <- data.frame(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]][[TableType]][[IDorName]], check.names = FALSE)
                  
                  columnsForheaders = Tags$TablesTag
                  test1 <- c(as.character(columnsForheaders))
                  test2 <- c(names(SingleItemdf))
                  
                  empty_cols <- c(test1, test2)
                  
                  dup_indices <- duplicated(empty_cols) | duplicated(empty_cols, fromLast = TRUE)
                  
                  empty_cols <- empty_cols[!dup_indices]
                  
                  empty_colsDF = data.frame(matrix(nrow = nrow(SingleItemdf) , ncol = length(empty_cols)))
                  colnames(empty_colsDF) = empty_cols
                  
                  # if( Update_Date == "Data_Oct2022" && IDorName == "T1631"){
                  #   print(PageType)
                  #   print(DataTypeID)
                  #   print(IDorName)
                  #   print(typeof(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]][[TableType]]))
                  # }
                  
                  SingleItemdf <- cbind(SingleItemdf, empty_colsDF)
                  
                  
                  SingleItemdf <- cbind(TableType = TableType, IDorName = IDorName, SingleItemdf)
                  HeadersAndTablesdf <- rbind(HeadersAndTablesdf,SingleItemdf)
                  
                  
                }
                
              }else if(typeof(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]][[TableType]]) == "character"){
                
                # print(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]][[TableType]])
                # print("CHARACTER")
                
                columns = Tags$TablesTag
                SingleItemdf = data.frame(matrix(nrow = 1, ncol = length(columns)))
                colnames(SingleItemdf) = columns
                
                
                for(Tag in colnames(SingleItemdf)){
                  # print(Tag)
                  # print(names(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]]))
                  # print(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]][[TableType]])
                  item <- fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]][[TableType]]
                  
                  # print(item)
                  
                  if(is.null(item) == FALSE){
                    SingleItemdf[Tag] <- item
                  }
                  else{
                    SingleItemdf[Tag] <- NA
                  }
                  
                }
                
                SingleItemdf <- cbind(TableType = TableType, IDorName = IDorName, SingleItemdf)
                HeadersAndTablesdf <- rbind(HeadersAndTablesdf,SingleItemdf)
                
              }else{
                print("THE ELSE of diferent type than char or list")
                # print(DataTypeID)
                # print(typeof(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]][[TableType]]))
                # print(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]][[TableType]])
              }
              
              
            }
            
            colnames(HeadersAndTablesdf) <- paste("Tables", colnames(HeadersAndTablesdf), sep = "_")
            
            
          }else if(Specific_DataType_Page_Elements == "ScrapDate"){
            # print("ScrapDate")
            
            #print(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]])
            ScrapDateInFile <- fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]]
            next
            
          }else{
            print("page element unknown")
          }
          
          # print(Update_Date)
          # print(Latest_ScrapDate_For_Given_Update)
          # print(PageType)
          # print(DataTypeID)
          
        }
        
        
        
        static_info <- do.call(rbind,
                               Map(
                                 data.frame,
                                 Update_Date = Update_Date,
                                 Latest_ScrapDate_For_Given_Update = Latest_ScrapDate_For_Given_Update,
                                 PageType = PageType,
                                 DataTypeID = DataTypeID,
                                 webpageURl = WebPageURL
                               ))
        
        RowNumHeadersAndTablesdf <- nrow(HeadersAndTablesdf)
        
        
        
        static_info <- static_info[rep(seq_len(nrow(static_info)), each = RowNumHeadersAndTablesdf), ]
        rownames(static_info) <- 1:nrow(static_info)
        titledf <- titledf[rep(seq_len(nrow(titledf)), each = RowNumHeadersAndTablesdf), ]
        rownames(titledf) <- 1:nrow(titledf)
        SumUpTabledf <- SumUpTabledf[rep(seq_len(nrow(SumUpTabledf)), each = RowNumHeadersAndTablesdf), ]
        rownames(SumUpTabledf) <- 1:nrow(SumUpTabledf)
        
        
        
        onepageDF <- within(merge(static_info, titledf, by=0, all=TRUE), rm("Row.names"))
        onepageDF <- within(merge(onepageDF, SumUpTabledf, by=0, all=TRUE), rm("Row.names"))
        onepageDF <- within(merge(onepageDF, HeadersAndTablesdf, by=0, all=TRUE), rm("Row.names"))
        
        
        
        fullDataFrame <- rbind(fullDataFrame, onepageDF)
        
        # if(DataTypeID == "G0018" & Update_Date == "Data_Oct2022"){
        #   view(fullDataFrame)
        # }
        # break
      }
      # break
    }
    # break
  }
  
  return(fullDataFrame)
}




fullDataSet <- DataSetCreation()
tags <- GettingTags(fullDataSet)

FinalDataFrame <- MainFunc(fullDataSet, tags)
FinalDataFrame <- CVE_column_addingFunc(FinalDataFrame)

#FinalDataFrame <- read.csv("/Users/morzen/Desktop/work/RprogramForMitreScrapperDatanalysis/1_DATAusedFolder/FinalDataFrame_Oct2024.csv")

write.csv(FinalDataFrame, file = "/Users/morzen/Desktop/FinalDataFrame_Oct2024.csv", row.names = FALSE)

write(toJSON(fullDataSet), file = "/Users/morzen/Desktop/MassiveAPTJson.json")

###########
#testing
###########












# 
# {
#   Mitre ATT&CK update version:{
#     Scrapping date:{
#       Group:{
#         UniqueID:{
#           Title:{
#             Name:,
#             ID:,
#             Associated APT:,
#             Tactic:,
#             Domain:
#           },
#           URL:,
#           SumUPTable:{
#             ID:,
#             Contributors:,
#             Version:,
#             Created:,
#             Last Modified:,
#             Associated Groups:,
#             Type:,
#             Platforms:,
#             Associated Software:,
#             Sub-techniques:,
#             Tactic:,
#             Sub-technique of:,
#             Tactics:,
#             System Requirements:,
#             Permissions Required:,
#             Effective Permissions:,
#             Defense Bypassed:,
#             Supports Remote:,
#             Impact Type:,
#             Tactic Type:,
#             MTC ID:,
#             CAPEC ID:
#           },
#           HeadersAndTable:{
#             Techniques Used:{
#               Domain:,
#               ID:,
#               Name:,
#               Use:
#             },
#             Software:{
#               ID:,
#               Name:,
#               References:,
#               Techniques:
#             },
#             Associated Group Descriptions:{
#               Name:,
#               Description:
#             },
#             Campaigns:{
#               ID:,
#               Name:,
#               First Seen:,
#               Last Seen:,
#               References:,
#               Techniques:,
#               Description:
#             },
#             Groups That Use This Software:{
#               ID:,
#               Name:,
#               References
#             },
#             Associated Software Descriptions:{
#               Name:,
#               Description:
#             },
#             Techniques:{
#               ID:,
#               Name:,
#               Description:
#             },
#             Mitigations:{
#               ID:,
#               Mitigation:,
#               Description:
#             },
#             Detection:{
#               ID:,
#               Data Source:,
#               Data Component:,
#               Detects:
#             },
#             Procedure Examples:{
#               ID:,
#               Name:,
#               Description:
#             },
#             References:
#           },
#           ScrapDate:
#         }
#       },
#       Software:{
#         UniqueID:{
#           UniqueID:{
#             Title:{
#               Name:,
#               ID:,
#               Associated APT:,
#               Tactic:,
#               Domain:
#             },
#             URL:,
#             SumUPTable:{
#               ID:,
#               Contributors:,
#               Version:,
#               Created:,
#               Last Modified:,
#               Associated Groups:,
#               Type:,
#               Platforms:,
#               Associated Software:,
#               Sub-techniques:,
#               Tactic:,
#               Sub-technique of:,
#               Tactics:,
#               System Requirements:,
#               Permissions Required:,
#               Effective Permissions:,
#               Defense Bypassed:,
#               Supports Remote:,
#               Impact Type:,
#               Tactic Type:,
#               MTC ID:,
#               CAPEC ID:
#             },
#             HeadersAndTable:{
#               Techniques Used:{
#                 Domain:,
#                 ID:,
#                 Name:,
#                 Use:
#               },
#               Software:{
#                 ID:,
#                 Name:,
#                 References:,
#                 Techniques:
#               },
#               Associated Group Descriptions:{
#                 Name:,
#                 Description:
#               },
#               Campaigns:{
#                 ID:,
#                 Name:,
#                 First Seen:,
#                 Last Seen:,
#                 References:,
#                 Techniques:,
#                 Description:
#               },
#               Groups That Use This Software:{
#                 ID:,
#                 Name:,
#                 References
#               },
#               Associated Software Descriptions:{
#                 Name:,
#                 Description:
#               },
#               Techniques:{
#                 ID:,
#                 Name:,
#                 Description:
#               },
#               Mitigations:{
#                 ID:,
#                 Mitigation:,
#                 Description:
#               },
#               Detection:{
#                 ID:,
#                 Data Source:,
#                 Data Component:,
#                 Detects:
#               },
#               Procedure Examples:{
#                 ID:,
#                 Name:,
#                 Description:
#               },
#               References:
#             },
#             ScrapDate:
#           }
#         }
#       },
#       Tactics:{
#         UniqueID:{
#           UniqueID:{
#             Title:{
#               Name:,
#               ID:,
#               Associated APT:,
#               Tactic:,
#               Domain:
#             },
#             URL:,
#             SumUPTable:{
#               ID:,
#               Contributors:,
#               Version:,
#               Created:,
#               Last Modified:,
#               Associated Groups:,
#               Type:,
#               Platforms:,
#               Associated Software:,
#               Sub-techniques:,
#               Tactic:,
#               Sub-technique of:,
#               Tactics:,
#               System Requirements:,
#               Permissions Required:,
#               Effective Permissions:,
#               Defense Bypassed:,
#               Supports Remote:,
#               Impact Type:,
#               Tactic Type:,
#               MTC ID:,
#               CAPEC ID:
#             },
#             HeadersAndTable:{
#               Techniques Used:{
#                 Domain:,
#                 ID:,
#                 Name:,
#                 Use:
#               },
#               Software:{
#                 ID:,
#                 Name:,
#                 References:,
#                 Techniques:
#               },
#               Associated Group Descriptions:{
#                 Name:,
#                 Description:
#               },
#               Campaigns:{
#                 ID:,
#                 Name:,
#                 First Seen:,
#                 Last Seen:,
#                 References:,
#                 Techniques:,
#                 Description:
#               },
#               Groups That Use This Software:{
#                 ID:,
#                 Name:,
#                 References
#               },
#               Associated Software Descriptions:{
#                 Name:,
#                 Description:
#               },
#               Techniques:{
#                 ID:,
#                 Name:,
#                 Description:
#               },
#               Mitigations:{
#                 ID:,
#                 Mitigation:,
#                 Description:
#               },
#               Detection:{
#                 ID:,
#                 Data Source:,
#                 Data Component:,
#                 Detects:
#               },
#               Procedure Examples:{
#                 ID:,
#                 Name:,
#                 Description:
#               },
#               References:
#             },
#             ScrapDate:
#           }
#         }
#       },
#       Techniques:{
#         UniqueID:{
#           UniqueID:{
#             Title:{
#               Name:,
#               ID:,
#               Associated APT:,
#               Tactic:,
#               Domain:
#             },
#             URL:,
#             SumUPTable:{
#               ID:,
#               Contributors:,
#               Version:,
#               Created:,
#               Last Modified:,
#               Associated Groups:,
#               Type:,
#               Platforms:,
#               Associated Software:,
#               Sub-techniques:,
#               Tactic:,
#               Sub-technique of:,
#               Tactics:,
#               System Requirements:,
#               Permissions Required:,
#               Effective Permissions:,
#               Defense Bypassed:,
#               Supports Remote:,
#               Impact Type:,
#               Tactic Type:,
#               MTC ID:,
#               CAPEC ID:
#             },
#             HeadersAndTable:{
#               Techniques Used:{
#                 Domain:,
#                 ID:,
#                 Name:,
#                 Use:
#               },
#               Software:{
#                 ID:,
#                 Name:,
#                 References:,
#                 Techniques:
#               },
#               Associated Group Descriptions:{
#                 Name:,
#                 Description:
#               },
#               Campaigns:{
#                 ID:,
#                 Name:,
#                 First Seen:,
#                 Last Seen:,
#                 References:,
#                 Techniques:,
#                 Description:
#               },
#               Groups That Use This Software:{
#                 ID:,
#                 Name:,
#                 References
#               },
#               Associated Software Descriptions:{
#                 Name:,
#                 Description:
#               },
#               Techniques:{
#                 ID:,
#                 Name:,
#                 Description:
#               },
#               Mitigations:{
#                 ID:,
#                 Mitigation:,
#                 Description:
#               },
#               Detection:{
#                 ID:,
#                 Data Source:,
#                 Data Component:,
#                 Detects:
#               },
#               Procedure Examples:{
#                 ID:,
#                 Name:,
#                 Description:
#               },
#               References:
#             },
#             ScrapDate:
#           }
#         }
#       }
#     }
#   }
# }






TitleTagNoDuplicate <- list()
SumUpTableNoDuplicate <- list()
TableType_list <- list()
TablesTag <- list()

for(Update_Date in names(fullDataSet)){
  Latest_ScrapDate_For_Given_Update <- max(names(fullDataSet[[Update_Date]]))
  for(PageType in names(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]])){
    for(DataTypeID in names(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]])){
      for(Specific_DataType_Page_Elements in names(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]])){
        if(Specific_DataType_Page_Elements == "title"){
          
          for(IndividualTag in names(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]])){
            if(!(IndividualTag %in% TitleTagNoDuplicate)){
              TitleTagNoDuplicate <- append(TitleTagNoDuplicate, IndividualTag)
              
            }
          }
          
        }else if(Specific_DataType_Page_Elements == "URL"){
          next
          
        }else if(Specific_DataType_Page_Elements == "SumUpTable"){
          
          for(IndividualTag in names(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]])){
            if(!(IndividualTag %in% SumUpTableNoDuplicate)){
              SumUpTableNoDuplicate <- append(SumUpTableNoDuplicate, IndividualTag)
              
            }
          }
          
        }else if(Specific_DataType_Page_Elements == "HeadersAndTables"){
          for(TableType in names(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]])){
            
            if(!(TableType %in% TableType_list)){
              TableType_list <- append(TableType_list, TableType)
              
            }
            
            for(IDorName in names(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]][[TableType]])){
              
              for( IndividualTag in names(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]][[TableType]][[IDorName]][[1]])){
                
                if(!(IndividualTag %in% TablesTag[[TableType]])){
                  print(TableType)
                  TablesTag[[TableType]] <- c(TablesTag[[TableType]], IndividualTag)
                  
                }
              }
            }
          }
          
        }else if(Specific_DataType_Page_Elements == "ScrapDate"){
          next
          
        }else{
          print("page element unknown")
        }
        
        
      }
      # break
    }
    # break
  }
  # break
}

TitleTagNoDuplicate <- unique(TitleTagNoDuplicate)
SumUpTableNoDuplicate <- unique(SumUpTableNoDuplicate)
TablesTag <- unique(TablesTag)

TitleTagNoDuplicate <- t(TitleTagNoDuplicate)
TitleTagNoDuplicate <- t(TitleTagNoDuplicate)

SumUpTableNoDuplicate <- t(SumUpTableNoDuplicate)
SumUpTableNoDuplicate <- t(SumUpTableNoDuplicate)

TablesTag <- t(TablesTag)
TablesTag <- t(TablesTag)


Tags <- list(TitleTag = TitleTagNoDuplicate, SumUpTableTag = SumUpTableNoDuplicate, TablesTag = TablesTag, TableTypes = TableType_list)

