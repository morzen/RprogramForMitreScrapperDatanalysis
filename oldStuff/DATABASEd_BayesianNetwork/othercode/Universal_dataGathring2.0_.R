library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)

# Define URLs for each domain
urls <- list(
  enterprise = "https://raw.githubusercontent.com/mitre-attack/attack-stix-data/master/enterprise-attack/enterprise-attack.json",
  mobile = "https://raw.githubusercontent.com/mitre-attack/attack-stix-data/master/mobile-attack/mobile-attack.json",
  ics = "https://raw.githubusercontent.com/mitre-attack/attack-stix-data/master/ics-attack/ics-attack.json"
)

# Function to download and save data
download_data <- function(url, domain) {
  response <- GET(url)
  if (status_code(response) == 200) {
    writeBin(content(response, "raw"), paste0(domain, "-attack.json"))
    message(paste("Downloaded:", domain, "-attack.json"))
  } else {
    message(paste("Failed to download:", domain))
  }
}

# Download data for all domains
lapply(names(urls), function(domain) download_data(urls[[domain]], domain))

# Load data for each domain
enterprise_data <- fromJSON("enterprise-attack.json")
mobile_data <- fromJSON("mobile-attack.json")
ics_data <- fromJSON("ics-attack.json")

# Convert to dataframes
enterprise_data_df <- as.data.frame(enterprise_data)
mobile_data_df <- as.data.frame(mobile_data)
ics_data_df <- as.data.frame(ics_data)

# Store dataframes in a named list
dataframes <- list(enterprise = enterprise_data_df, mobile = mobile_data_df, ics = ics_data_df)

# Iterate through the list of dataframes
for (domain in names(dataframes)) {
  # Get the current dataframe
  current_df <- dataframes[[domain]]
  
  # Select the relevant column
  mitre_contents <- select(current_df, objects.x_mitre_contents)
  
  # Remove NULL values
  cleaned_df <- mitre_contents[!sapply(mitre_contents$objects.x_mitre_contents, is.null), ]
  
  # Convert cleaned_df to a dataframe
  cleaned_df <- as.data.frame(cleaned_df)
  
  # Perform the join with the original dataframe
  result_df <- current_df %>%
    left_join(cleaned_df, by = c("objects.id" = "object_ref")) %>%
    # Replace objects.x_mitre_contents with object_modified
    mutate(objects.x_mitre_contents = object_modified) %>%
    # Remove the original object_modified column
    select(-object_modified) %>%
    # Rename the column
    rename(objects.x_mitre_contents_object_modified = objects.x_mitre_contents)
  
  # Update the respective dataframe variable
  assign(paste0(domain, "_data_df"), result_df, envir = .GlobalEnv)
}

FinalColName <- list()
for (domain in names(dataframes)) {
  
  
  givenDF <- enterprise_data_df
  
  # Initialize an empty dataframe to store results
  data_information_result_df <- data.frame(
    column_name = character(),
    OriginalDF_col_index = integer(),
    list_length = character(),
    nested_column_names = character(),
    stringsAsFactors = FALSE
  )
  
  # Iterate through each column in the dataframe
  for (col_name in colnames(givenDF)) {
    if (is.list(givenDF[[col_name]])) {
      # Get the lengths of the lists in the column
      list_lengths <- sapply(givenDF[[col_name]], length)
      
      # Get unique lengths
      unique_lengths <- unique(list_lengths)
      
      # Get column names if the list items are dataframes
      nested_column_names <- sapply(givenDF[[col_name]], function(x) {
        if (is.data.frame(x)) {
          return(colnames(x))  # Return column names as a vector
        } else {
          return(NA)  # Return NA if not a dataframe
        }
      })
      
      # Flatten the nested_column_names and get unique names
      unique_column_names <- unique(na.omit(unlist(nested_column_names)))
      
      # Create a new row for the dataframe
      new_row <- data.frame(
        column_name = col_name,
        OriginalDF_col_index = which(colnames(givenDF) == col_name),  # Get the index of the column in givenDF
        list_length = paste(unique_lengths, collapse = ", "),
        nested_column_names = paste(unique_column_names, collapse = ", "),  # Join unique names
        stringsAsFactors = FALSE
      )
      
      # Append the new row to the result dataframe
      data_information_result_df <- rbind(data_information_result_df, new_row)
    }
  }
  
  
  
  tempDF <- subset(data_information_result_df, nested_column_names != "")
  new_col_name <- list()
  # Iterate through each row in tempDF
  for (i in seq_len(nrow(tempDF))) {
    namelist <- tempDF$nested_column_names[i]
    OG_colname <- tempDF$column_name[i]
    
    # Split the namelist into individual elements
    elements <- unlist(strsplit(namelist, ", "))  # Assuming names are comma-separated
    
    for (element in elements) {
      # Create the new column name and append it to the list
      new_col_name[[OG_colname]] <- c(new_col_name[[OG_colname]], paste0(OG_colname, "_-_", element))
    }
  }
  
  
  all_given_df_colName_nested <- colnames(givenDF)
  
  
  # Replace keys in all_given_df_colName_nested with their associated items
  for (key in names(new_col_name)) {
    # Find the index of the key in the original list
    index <- which(all_given_df_colName_nested == key)
    
    if (length(index) > 0) {
      # Replace the key with its associated items
      all_given_df_colName_nested <- c(
        all_given_df_colName_nested[1:(index - 1)],  # Items before the key
        new_col_name[[key]],                 # Associated items
        all_given_df_colName_nested[(index + 1):length(all_given_df_colName_nested)]  # Items after the key
      )
    }
  }
  
  FinalColName <- c(FinalColName, all_given_df_colName_nested)
}

#all column name necessary
FinalColName <- unique(FinalColName)

# # JUST REMINDER OF NAMES FOR EACH OG DF

# mobile_data_df 
# ics_data_df 

################################################################################


givenDF2 <- enterprise_data_df

NestedCol <- tempDF$column_name[1]
emptyDF_to_fill <- data.frame()

for(row_index in 1:nrow(givenDF2)){
  
  single_rows <- givenDF2[row_index, ]
  
  if(!is.null(single_rows[[NestedCol]][[1]])){
    print(1)
    
    nested_df <- single_rows[[NestedCol]][[1]]
    newCol_name_list <- strsplit(selected_rows$nested_column_names, ",")[[1]]
    newCol_name_list <- trimws(paste0(NestedCol, "_-_", newCol_name_list))
    
    if(length(single_rows[[NestedCol]][[1]]) == 0){
      df_empty_ish <- data.frame(matrix(NA, nrow = 1, ncol = length(newCol_name_list)))
      colnames(df_empty_ish) <- newCol_name_list
      nested_df <- df_empty_ish
    }else{
      colnames(nested_df) <- paste(NestedCol, "_-_", colnames(nested_df), sep = "")
    }

    
    
    for (col in names(nested_df)) {
      if (!col %in% newCol_name_list) {
        nested_df[[col]] <- NA  # Add the missing column and fill with NA
      }
    }
    
    
    nrow_nested_df <- nrow(nested_df)
    duplicated_rows <- single_rows[rep(1, nrow_nested_df), ]
    
    insertion_index <- which(colnames(single_rows) == NestedCol)  # This is the static index where nested_df will be inserted
    
    # Combine the dataframes dynamically
    combined_df <- cbind(
      duplicated_rows[, 1:(insertion_index - 1)],  # All columns before the insertion point
      nested_df,                                    # All columns from nested_df
      duplicated_rows[, insertion_index:ncol(duplicated_rows)]  # All columns after the insertion point
    )
    
    combined_df <- subset(combined_df, select = -which(names(combined_df) == col_name))
    emptyDF_to_fill <- bind_rows(emptyDF_to_fill, combined_df)
    
  }else if(is.null(single_rows[[NestedCol]][[1]])){
    print(2)
    selected_rows <- tempDF %>% filter(column_name == NestedCol)

    newCol_name_list <- strsplit(selected_rows$nested_column_names, ",")[[1]]
    newCol_name_list <- trimws(paste0(NestedCol, "_-_", newCol_name_list))  # Trim whitespace and create new names

    # Create a new dataframe with NA values
    df_empty_ish <- data.frame(matrix(NA, nrow = 1, ncol = length(newCol_name_list)))
    colnames(df_empty_ish) <- newCol_name_list

    insertion_index_2 <- selected_rows$OriginalDF_col_index

    combined_df_2 <- cbind(
      single_rows[, 1:(insertion_index_2 - 1)],  # All columns before the insertion point
      df_empty_ish,                                        # New dataframe with NA values
      single_rows[, insertion_index_2:ncol(single_rows)]  # All columns after the insertion point
    )

    combined_df_2 <- subset(combined_df_2, select = -which(names(combined_df_2) == col_name))
    emptyDF_to_fill <- bind_rows(emptyDF_to_fill, combined_df_2)
  }
}



