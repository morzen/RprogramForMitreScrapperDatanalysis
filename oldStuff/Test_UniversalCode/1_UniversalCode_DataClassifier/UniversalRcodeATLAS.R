library("fmsb")
library("rjson")
library("ggplot2")
library("tidyverse")
library("dplyr")
library("plyr")
library("stringr")
library("data.table")

exit <- function() { invokeRestart("abort") } 

#create One fuull dataset using all json files for every updates created by the srcaping python tool
DataSetCreation <- function(){
  PathToResult <- "/Users/morzen/Desktop/work/MITRE_ATLAS_SCRAPPER/results"

  FilesInPath <- list.files(path = PathToResult, pattern = "Rformat_", full.names = TRUE, recursive = TRUE)
  
  fullDataSet <- list()
  
  for(Pwd in FilesInPath){
    
    VersionOfMitreUpdate <- gsub('/','', str_extract(Pwd, "\\/Data_[a-z,A-Z]{3}[0-9]{4}\\/"))
    ScrapDate <- gsub('/','', str_extract(Pwd, "\\/([0-9]{1,2}-[0-9]{1,2}-[0-9]{1,4})\\/"))
    print(Pwd)
    
    if(grepl("Rformat_Tactics|Rformat_Techniques|Rformat_Mitigations|Rformat_Studies", Pwd) == TRUE){
      
      datatype <- str_extract(Pwd, "Tactics|Techniques|Mitigations|Studies")
      
      fullDataSet[[VersionOfMitreUpdate]][[ScrapDate]][[datatype]] <- append(fullDataSet[[VersionOfMitreUpdate]][[ScrapDate]][[datatype]], fromJSON(Pwd))
      
    }
    
  }
  return(fullDataSet)
}



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
            
            for(IndividualTag in fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]]){
              if(!(IndividualTag %in% TitleTagNoDuplicate)){
                TitleTagNoDuplicate <- append(TitleTagNoDuplicate, IndividualTag)
                
              }
            }
            
          }else if(Specific_DataType_Page_Elements == "URL"){
            next
            # print(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]])
            
          }else if(Specific_DataType_Page_Elements == "SumUpTable"){
            
            for(IndividualTag in names(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]])){
              if(!(IndividualTag %in% SumUpTableNoDuplicate)){
                SumUpTableNoDuplicate <- append(SumUpTableNoDuplicate, IndividualTag)
                
              }
            }
            
          }else if(Specific_DataType_Page_Elements == "HeadersAndTables"){
            
            for(TableTitle in names(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]])){
              for(columnName in names(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]][[TableTitle]])){
                if(!(columnName %in% TablesTag)){
                  TablesTag <- append(TablesTag, columnName)
                  
                }
              }
            }
            
            
          }else if(Specific_DataType_Page_Elements == "ScrapDate"){
            next
            # print(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]])
            
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
  
  
  Tags <- list(TitleTag = TitleTagNoDuplicate, SumUpTableTag = SumUpTableNoDuplicate, TablesTag = TablesTag)
    
  return(Tags)
}


# fullDataSet <- DataSetCreation()
# Tags <- GettingTags(fullDataSet)









MainFunc <- function(fullDataSet, tags){
  fullDataSet <- fullDataSet
  Tags <- tags
  
  fullDataFrame <- data.frame()
  for(Update_Date in names(fullDataSet)){
    #setting up Update variable and latest scrap date for given update
    Latest_ScrapDate_For_Given_Update <- max(names(fullDataSet[[Update_Date]]))
    # print(Update_Date)
    # print(Latest_ScrapDate_For_Given_Update)
    ############
    
    for(PageType in names(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]])){
      # print(PageType)
      for(DataTypeID in names(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]])){
        # print(DataTypeID) #One page
        
        for(Specific_DataType_Page_Elements in names(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]])){
          #print(Specific_DataType_Page_Elements)
          
          
          if(Specific_DataType_Page_Elements == "title"){
            Title <- fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]]
            
          }else if(Specific_DataType_Page_Elements == "URL"){
            WebPageURL <- fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]]
            
          }else if(Specific_DataType_Page_Elements == "SumUpTable"){
            
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
            HeadersAndTablesdf <- data.frame()
            
            for(TableType in names(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]])){
              # print(TableType)#Table types
              for(element_content in names(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]][[TableType]])){
                test <- length(fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]][[TableType]][[element_content]])
                
              }
              columns = Tags$TablesTag
              SingleItemdf = data.frame(matrix(nrow = test, ncol = length(columns)))
              colnames(SingleItemdf) = columns
              
              for(Tag in colnames(SingleItemdf)){
                
                item <- fullDataSet[[Update_Date]][[Latest_ScrapDate_For_Given_Update]][[PageType]][[DataTypeID]][[Specific_DataType_Page_Elements]][[TableType]][[Tag]]
                #SingleItemdf[Tag] <- item
                
                if(is.null(item) == FALSE){
                  SingleItemdf[Tag] <- item
                }
                else{
                  SingleItemdf[Tag] <- NA
                }
                
                
              }
              
              SingleItemdf <- cbind(TableType = TableType, SingleItemdf)
              HeadersAndTablesdf <- rbind(HeadersAndTablesdf,SingleItemdf)
              
              
            }
            
            
            
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
                                 webpageURl = WebPageURL,
                                 Title = Title
                                 
                               ))
        
        RowNumHeadersAndTablesdf <- nrow(HeadersAndTablesdf)
        
        
        
        static_info <- static_info[rep(seq_len(nrow(static_info)), each = RowNumHeadersAndTablesdf), ]
        rownames(static_info) <- 1:nrow(static_info)
  
        SumUpTabledf <- SumUpTabledf[rep(seq_len(nrow(SumUpTabledf)), each = RowNumHeadersAndTablesdf), ]
        rownames(SumUpTabledf) <- 1:nrow(SumUpTabledf)
        
        
        
        onepageDF <- within(merge(static_info, SumUpTabledf, by=0, all=TRUE), rm("Row.names"))
        onepageDF <- within(merge(onepageDF, HeadersAndTablesdf, by=0, all=TRUE), rm("Row.names"))
        
        
        
        fullDataFrame <- rbind(fullDataFrame, onepageDF)
        
      }
      
    }
    
  }
  return(fullDataFrame)
}


fullDataSet <- DataSetCreation()
tags <- GettingTags(fullDataSet)

FinalDataFrame <- MainFunc(fullDataSet, tags)

# write.csv(FinalDataFrame, "/Users/morzen/Desktop/work/RprogramForMitreScrapperDatanalysis/Test_UniversalCode/Similarity&statisticalTest/FullDF_MITRE_ATLAS1.csv", row.names = FALSE)
write.csv(FinalDataFrame, "/Users/morzen/Desktop/FullDF_MITRE_ATLAS1.csv", row.names = FALSE)


FinalMITRE_ATLASDataFrame <- read.csv("/Users/morzen/Desktop/FullDF_MITRE_ATLAS1.csv")




