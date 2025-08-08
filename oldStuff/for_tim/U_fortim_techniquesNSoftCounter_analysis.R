library(dplyr)
library(stringr)
library(collapsibleTree)
library(readxl)
library(tidyr)

FinalDataFrame <- read.csv("/Users/morzen/Desktop/work/RprogramForMitreScrapperDatanalysis/Test_UniversalCode/Similarity&statisticalTest/FinalDataFrame_Apr2024.csv")

Selected_MitreUpddateVersion <- "Data_Apr2024"

Selected_ScrapDate <- "24-04-2024"

FinalDataFrame <- FinalDataFrame %>% filter(Update_Date == Selected_MitreUpddateVersion) %>% filter(Latest_ScrapDate_For_Given_Update == Selected_ScrapDate)

remove_empty_columns <- function(data) {
  data[, colSums(is.na(data)) != nrow(data), drop = FALSE]
}

# APTgroups <- FinalDataFrame %>% filter(Update_Date == Selected_MitreUpddateVersion) %>% filter(Latest_ScrapDate_For_Given_Update == Selected_ScrapDate) %>% filter(PageType == "Groups")
# APTgroups <- APTgroups %>% filter(grepl("Techniques Used|Software", Tables_TableType))
# APTgroups <- remove_empty_columns(APTgroups)
# 

###
#tactic fucntion creation test
###

tactics_adding_Func <- function(df) {
  
  GroupsDF <- df %>%
    filter(PageType == "Groups") %>%
    filter(Tables_TableType %in% c("Techniques Used", "Software"))
  
  TechDF <- df %>%
    filter(Update_Date == Selected_MitreUpddateVersion, PageType == "Techniques")
  
  # Merge Tactic_s columns
  TechDF$Tactic_s <- ifelse(is.na(TechDF$SumUpTable_Tactic), TechDF$SumUpTable_Tactics, TechDF$SumUpTable_Tactic)
  
  # Drop NAs
  GroupsDF <- GroupsDF %>% select(where(~ !all(is.na(.))))
  TechDF <- TechDF %>% select(where(~ !all(is.na(.))))
  
  # Extract unique Technique_ID and Tactic_s combinations
  TacticDF <- data.frame(cbind(SumUpTable_ID = TechDF$SumUpTable_ID,
                               Tactic_s = TechDF$Tactic_s))
  
  TacticDF <- TacticDF[!duplicated(TacticDF), ]
  
  
  lengthNrow <- nrow(GroupsDF)
  
  Tactic_s_df <- data.frame(
    Tactic_s = rep(NA, lengthNrow)
  )
  
  # Loop through Tables_ID values in GroupsDF
  for (ID in unique(GroupsDF$Tables_ID)){
    if (ID %in% TacticDF$SumUpTable_ID) {
      IndexForIDlist <- which(TacticDF$SumUpTable_ID == ID)
      IndexForEndPlacement <- which(GroupsDF$Tables_ID == ID)
      
      if(!(length(IndexForIDlist) == 1)){
        print(IndexForIDlist)
        break
      }
      
      value <- TacticDF$Tactic_s[IndexForIDlist]
     
      for(element in IndexForEndPlacement){
        Tactic_s_df$Tactic_s[element] <- value
      }
      
      
    }
  }
  
  # Add Tactic_s values to GroupsDF
  # GroupsDF$Tactic_s <- TacticList
  GroupsDF <- cbind(GroupsDF, Tactic_s = Tactic_s_df$Tactic_s)
  
  
  return(as.data.frame(GroupsDF))
}


lookUp_APT_techniques <- function(x, y){
  Technique_to_lookUp <- x
  APTgroups <- y
  unique_techniques_forAPT <- unique(APTgroups$Tables_IDorName)
  
  if(Technique_to_lookUp %in% unique_techniques_forAPT){
    Selected_Data_rows <- APTgroups[APTgroups$Tables_IDorName == Technique_to_lookUp, ]
    return(Selected_Data_rows)
  }
}

lookUp_APT_tactic <- function(x, y){
  Tactic_to_lookUp <- x
  DF_used_inFunct <- y
  unique_tactics_forAPT <- unique(na.omit(DF_used_inFunct$Tactic_s))

  if(Tactic_to_lookUp %in% unique_tactics_forAPT){
    df_test <- DF_used_inFunct[grepl(Tactic_to_lookUp, DF_used_inFunct$Tactic_s), ]
    return(df_test)
  }
}


lookUp_APT_techniques_with_Occurence <- function(x, y){
  Technique_to_lookUp <- x
  DF_used_inFunct <- y
  unique_techniques_forAPT <- unique(DF_used_inFunct$Techniques_N_Software)
  
  if(Technique_to_lookUp %in% unique_techniques_forAPT){
    Selected_Data_rows <- DF_used_inFunct[DF_used_inFunct$Techniques_N_Software == Technique_to_lookUp, ]
    return(Selected_Data_rows)
  }
}

lookUp_APT_tactic_with_Occurence <- function(x, y){
  Tactic_to_lookUp <- x
  DF_used_inFunct <- y
  unique_tactics_forAPT <- unique(na.omit(DF_used_inFunct$tactics))
  
  if(Tactic_to_lookUp %in% unique_tactics_forAPT){
    df_test <- DF_used_inFunct[grepl(Tactic_to_lookUp, DF_used_inFunct$tactics), ]
    return(df_test)
  }
}


returnOccurence_tech <- function(x){
  APTgroups <- x
  
  unique_techniques_forAPT <- unique(APTgroups$Tables_IDorName)
  df <- do.call(rbind,
                Map(
                  data.frame,
                  TableType = NA,
                  Techniques_N_Software = unique_techniques_forAPT,
                  occurence = NA,
                  Name = NA,
                  domain = NA,
                  tactics = NA,
                  APTs_name = NA
                ))
  
  numberofoccurence <- list()
  
  for(techOrSoftID_in_UniqueAPTTechSoftIDLIST in unique_techniques_forAPT){
    
    Counter_i <- 0
    
    #print(techOrSoftID_in_UniqueAPTTechSoftIDLIST)
    
    for(techOrID_in_APTgroups in APTgroups$Tables_IDorName){
      
      if(identical(techOrSoftID_in_UniqueAPTTechSoftIDLIST, techOrID_in_APTgroups)){
        Counter_i <- Counter_i + 1
      }
      
    }
    
    Selected_Data_rows <- APTgroups[APTgroups$Tables_IDorName == techOrSoftID_in_UniqueAPTTechSoftIDLIST, ]
    
    # numberofoccurence <- append(numberofoccurence, Counter_i)
    # APTlist_thatuse_techOrSoft <- list(unique(Selected_Data_rows$Title_Name))
    # nameofTechOrSoft <- unique(Selected_Data_rows$Tables_Name)
    # TableType <- unique(Selected_Data_rows$Tables_TableType)
    # domainForTech <- unique(Selected_Data_rows$Tables_Domain)
    # TacticForTech <- unique(Selected_Data_rows$Tactic_s)
    
    Indice <- which(df$Techniques_N_Software == techOrSoftID_in_UniqueAPTTechSoftIDLIST)
    
    
    df[Indice, "occurence"] <- append(numberofoccurence, Counter_i)
    
    if(length(unique(Selected_Data_rows$Title_Name)) == 1){
      df[Indice, "APTs_name"] <- unique(Selected_Data_rows$Title_Name)
    }else if(length(unique(Selected_Data_rows$Title_Name) > 1)){
      APTlist <- list(unique(Selected_Data_rows$Title_Name))
      df[Indice, "APTs_name"] <- paste(APTlist[[1]], collapse = ", ")
    }
    
    if(length(unique(Selected_Data_rows$Tables_Name)) == 1){
      df[Indice, "Name"] <- unique(Selected_Data_rows$Tables_Name)
    }else if(length(unique(Selected_Data_rows$Tables_Name) > 1)){
      Tables_Namelist <- list(unique(Selected_Data_rows$Tables_Name))
      df[Indice, "Name"] <- paste(Tables_Namelist[[1]], collapse = ", ")
    }
    
    if(length(unique(Selected_Data_rows$Tables_Name)) == 1){
      df[Indice, "TableType"] <- unique(Selected_Data_rows$Tables_TableType)
    }else if(length(unique(Selected_Data_rows$Tables_TableType) > 1)){
      TableTypelist <- list(unique(Selected_Data_rows$Tables_TableType))
      df[Indice, "TableType"] <- paste(TableTypelist[[1]], collapse = ", ")
    }
    
    if(length(unique(Selected_Data_rows$Tables_Domain)) == 1){
      df[Indice, "domain"] <- unique(Selected_Data_rows$Tables_Domain)
    }else if(length(unique(Selected_Data_rows$Tables_Domain) > 1)){
      Domainlist <- list(unique(Selected_Data_rows$Tables_Domain))
      df[Indice, "domain"] <- paste(Domainlist[[1]], collapse = ", ")
    }
    
    if(length(unique(Selected_Data_rows$Tactic_s)) == 1){
      df[Indice, "tactics"] <- unique(Selected_Data_rows$Tactic_s)
    }else if(length(unique(Selected_Data_rows$Tactic_s) > 1)){
      Tactic_slist <- list(unique(Selected_Data_rows$Tactic_s))
      df[Indice, "tactics"] <- paste(Tactic_slist[[1]], collapse = ", ")
    }
    
  }
  
  return(df)
}

#####################
#possibilities for tactics to lookup

# #Uique tactic and combo tactic
# unique_tactics_Combo_forAPT <- unique(na.omit(APTgroups$Tactic_s))
# 
#unique tactic no combo
UniqueTactics <- FinalDataFrame %>%
  filter(PageType == "Tactics")
UniqueTactics <- unique(UniqueTactics$Title_Tactic)

####################

########
#final
APTgroups <- tactics_adding_Func(FinalDataFrame)
# APTgroups <- APTgroups %>% filter(Update_Date == Selected_MitreUpddateVersion) %>% filter(Latest_ScrapDate_For_Given_Update == Selected_ScrapDate)
occurenceDF <- returnOccurence_tech(APTgroups)


# lookUp_APT_techniques("T1204", APTgroups)
# lookUp_APT_tactic("Reconnaissance", APTgroups)
# 
# lookUp_APT_techniques_with_Occurence("T1204", occurenceDF)
# lookUp_APT_tactic_with_Occurence("Reconnaissance", occurenceDF)
######


############
#test
############


onlycomboTacticdf <- APTgroups %>%
  separate_rows(Tactic_s, sep = ", ")

onlycomboTacticdf <- onlycomboTacticdf %>% filter(Tables_TableType == "Techniques Used")



essentialdata <- data.frame(cbind(Name = onlycomboTacticdf$Title_Name, ID = onlycomboTacticdf$Tables_IDorName, Tactic = onlycomboTacticdf$Tactic_s))


essentialdata <- essentialdata[order(match(essentialdata$Tactic, UniqueTactics)), ]


# write.csv(essentialdata, "/Users/morzen/Desktop/essentialdata.csv", row.names = FALSE)



# collapsibleTree(
#   essentialdata,
#   hierarchy = c("Tactic", "ID", "Name"),
#   width = 1000,
#   height = 1000,
#   zoomable = TRUE
# )
# 
# tree <- collapsibleTree(
#   data,
#   hierarchy = c("Tactic", "ID", "Tactic"),
#   collapsed = TRUE,
#   width = "100%",
#   height = "1000px",
#   zoomable = TRUE
# )

###########################
#TESTING for the testing
##########################
# testtt1 <- as.list(unique(test3$Name))
# testtt2 <- as.list(unique(APTgroups$Title_Name))
# 
# diff_list <- setdiff(testtt2, testtt1)
#turn into json
test3 <- essentialdata

my_dict_test <- list()
for(APTname in unique(test3$Name)){
  print(APTname)
  oneAPT <- test3 %>% filter(Name == APTname)
  
  TempDict <- list()
  for(Tactic_iterator in unique(oneAPT$Tactic)){
    oneAPT_tactic <- oneAPT %>% filter(Tactic == Tactic_iterator)
    
    TempDict[[Tactic_iterator]] <- oneAPT_tactic$ID
  }
  my_dict_test[[APTname]] <- TempDict
}


# jsonlite::write_json(my_dict_test, "/Users/morzen/Desktop/APT_4_collapsableTree.json", pretty = TRUE)
##########################



library(collapsibleTree)

# Example dataframe with hierarchy column
data_df <- essentialdata

# Create collapsible tree
yggdrasil <- collapsibleTree(
  id = "tree",
  data_df, 
  hierarchy = c("Tactic", "ID", "Tactic"),
  inputId = "node",
  collapsed = TRUE,
  width = 1000,
  height = 1000,
  zoomable = TRUE
)

collapsibleTree(
  id = "tree",
  data_df, 
  hierarchy = c("Tactic", "ID", "Name"),
  inputId = "node",
  collapsed = TRUE,
  width = 1000,
  height = 1000,
  zoomable = TRUE
)

# slidingTacticFilter <- function(x, y, z){
#   # Selected item
#   selected_item <- x
#   testdata <- y
#   UniqueTactics <- z
#   
#   if(is.null(selected_item) == TRUE){
#     return(NULL)
#   }else{
#     # Get the index of the selected item in the original list
#     index <- match(selected_item, UniqueTactics)
#     
#     # Create a new list containing all items up to and including the selected item
#     filtered_items <- UniqueTactics[1:index]
#     
#     # List of strings to filter
#     strings_to_filter <- filtered_items
#     
#     # Remove rows containing any of the strings in the list for the "gender" column
#     filtered_data <- data %>%
#       filter(!(Tactic %in% strings_to_filter))
#     
#     
#     return(filtered_data)
#   }
#   
#   
# }


# slideTacticDfresult<- slidingTacticFilter("Execution", data, UniqueTactics)










library(shiny)
library(collapsibleTree)

# Define UI for application that draws a collapsible tree
ui <- fluidPage(
  
  # Application title
  titlePanel("test"),
  
  # Sidebar with a select input for the root node
  sidebarLayout(
    sidebarPanel(
      tags$p("Console Output:"),
      verbatimTextOutput("str_consoletest"),  # Output for console
      
      
      tags$p("selected_node:"),
      verbatimTextOutput("str_nodeclicked")
    ),
    
    # Show a tree diagram with the selected root node
    mainPanel(
      collapsibleTreeOutput("plot", width = "100%", height = "1200px")
    )
  )
)

# Define server logic required to draw a collapsible tree diagram
server <- function(input, output, session) {
  i = 0
  IDselectedList <- list()
  
  output$plot <- renderCollapsibleTree({
    yggdrasil
  })
  
  
  output$str_nodeclicked <- renderPrint({
    i = i + 1
    selected_data <- NULL
    print(i)
    
    nodeInputted <- input$node
    print(nodeInputted)
    
    print("--LastTypeOfNodeClicked--")
    print(names(nodeInputted)[1])
    print("--LastValueClicked--")
    print(nodeInputted[[1]])
    
    if(names(nodeInputted)[1] == "ID"){
      IDselectedList <- append(IDselectedList, nodeInputted[[1]])
      
      selected_data <- data[data$ID %in% IDselectedList, ]
      selected_data <- data[data$Name %in% selected_data[["Name"]], ]
      selected_data <- selected_data[!grepl(nodeInputted[[1]], selected_data[["ID"]]), ]
      
      print("--List--")
      print(IDselectedList)
      
      # data <- selected_data
    }
    
    print("------selected_data-------")
    
    print(selected_data)
    
    print("------depth-------")
    node <- input$node
    print(input)
    print(input$Values)
    depth <- length(unlist(node$path)) - 1
    
    print(depth)
    
  })
  
  output$str_consoletest <- renderPrint(str("i never managed to get the output"))
  
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)


######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################




testDF <- essentialdata


while (TRUE) {
  print(testDF)
  
  UniqueTactic <- unique(testDF$Tactic)
  for (i in seq_along(UniqueTactic)) {
    cat(paste0(i, ". ", UniqueTactic[i], "\n"))
  }
  
  tactic_user_input <- readline(prompt = "Enter Tactic Number: ")
  
  TacticSelected <- UniqueTactic[as.integer(tactic_user_input)]
  
  
  TacticDFselected <- testDF[testDF$Tactic %in% TacticSelected, ]
  
  UniqueTechnique <- unique(TacticDFselected$ID)
  for (i in seq_along(UniqueTechnique)) {
    cat(paste0(i, ". ", UniqueTechnique[i], "\n"))
  }
  
  Tech_user_input <- readline(prompt = "Enter Technique Number: ")
  
  TechniqueSelected <- UniqueTechnique[as.integer(Tech_user_input)]
  
  
  TechniqueDFselected <- testDF[testDF$ID %in% TechniqueSelected, ]
  
  
  
  testDF <- testDF[testDF$Name %in% TechniqueDFselected$Name, ]
  testDF <- subset(testDF, ID != TechniqueSelected)
  
  
  if(length(unique(testDF$Name)) == 1){
    print(testDF)
    
    print(paste0("\n", "likey suspect is: ", unique(testDF$Name)))
    break
  }
    
  else if(tactic_user_input == "quit" || Tech_user_input == "quit" || tactic_user_input == "q" || Tech_user_input == "q"){
    break
  }
  
}



