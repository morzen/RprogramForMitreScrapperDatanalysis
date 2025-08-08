library("fmsb")
library("rjson")
library("ggplot2")
library("tidyverse")
library("dplyr")
library("plyr")
library("stringr")
library("data.table")
library("jsonlite")
library("mongolite")


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
              for(IDorName in names(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]][[TableType]])){
                
                for( IndividualTag in names(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]][[TableType]][[IDorName]])){
                  
                  if(!(IndividualTag %in% TablesTag[[TableType]])){
                    
                    TablesTag[[TableType]] <- append(TablesTag[[TableType]], IndividualTag)
                    
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
      }
    }
  }
  
  TitleTagNoDuplicate <- unique(TitleTagNoDuplicate)
  SumUpTableNoDuplicate <- unique(SumUpTableNoDuplicate)
  # TablesTag <- unique(TablesTag)
  
  TitleTagNoDuplicate <- t(TitleTagNoDuplicate)
  TitleTagNoDuplicate <- t(TitleTagNoDuplicate)
  
  SumUpTableNoDuplicate <- t(SumUpTableNoDuplicate)
  SumUpTableNoDuplicate <- t(SumUpTableNoDuplicate)
  
  # TablesTag <- t(TablesTag)
  # TablesTag <- t(TablesTag)
  
  
  Tags <- list(TitleTag = TitleTagNoDuplicate, SumUpTableTag = SumUpTableNoDuplicate, TablesTag = TablesTag)
  
  return(Tags)
}




fullDataSet <- DataSetCreation()
tags <- GettingTags(fullDataSet)

################################################################################
#MONGO DB
################################################################################



# Connect to MongoDB
mongo_connection <- mongo(collection = "my_collection", 
                          db = "my_database", 
                          url = "mongodb://localhost:27017")
