
MITRE_Groups_TTPs <- function(arg1) {

  CompleteURL <- paste("mongodb://", arg1, ":27017", sep = "")
  
  #MITRE ATT&CK group part
  # 1. Connect to MongoDB
  con <- mongo(
    collection = "1_MITRE_ATTCK_Data",
    db = "Cyber_necronomicon_phd",
    url = CompleteURL
  )
  
  # 2. Get ALL documents where ID starts with G - no field filtering
  group_docs <- con$find('{"title.ID": {"$regex": "^G"}}')
  
  # 3. Convert directly to JSON (preserves all nested structure)
  json_output <- toJSON(group_docs, pretty = TRUE)
  json_output2 <- fromJSON(json_output, simplifyDataFrame = FALSE)
  
  
  
  json_output2[[1]][["ScrapDate"]]
  #"25-11-2024_14-33-57"
  
  
  keep_elements <- sapply(json_output2, function(x) {
    !is.null(x[["ScrapDate"]]) && x[["ScrapDate"]] == "25-11-2024_14-33-57"
  })
  
  json_output3 <- json_output2[keep_elements]
  
  
  
  newdataset <- list()
  
  for(element in json_output3) {
    # Process Techniques Used - remove empty sublists
    techList <- element$HeadersAndTables$`Techniques Used`
    techList <- names(techList[lengths(techList) > 0])  # Keep only non-empty items
    
    # Process Software - remove empty sublists
    SoftList <- element$HeadersAndTables$Software
    SoftList <- names(SoftList[lengths(SoftList) > 0])  # Keep only non-empty items
    
    # Only create entry if at least one list has content
    
    new_entry <- list(
      id = element$title$ID,  # Including ID in the entry itself
      name = element$title$Name,
      techniques = techList,
      software = SoftList
    )
    
    # Use ID as the list name
    newdataset[[as.character(element$title$ID)]] <- new_entry
    
  }
  
  
  
  # Method that handles varying lengths properly
  flattened_df <- imap_dfr(newdataset, ~ {
    # Convert each element to a tibble, handling varying lengths
    tibble(
      id = .x$id,
      name = .x$name,
      techniques = list(.x$techniques),  # Wrap in list to preserve vectors
      software = list(.x$software),
      .groups = "drop"
    ) %>% 
      mutate(group = .y)  # Add the group name
  }) %>% 
    select(group, everything())
  
  # If you want techniques/software as comma-separated strings:
  flattened_df <- flattened_df %>%
    mutate(
      techniques_str = map_chr(techniques, function(x) {
        if(length(x) > 0) paste(x, collapse = ", ") else NA_character_
      }),
      software_str = map_chr(software, function(x) {
        if(length(x) > 0) paste(x, collapse = ", ") else NA_character_
      })
    )
  
  
  flattened_df <- flattened_df[, !names(flattened_df) %in% c("group", ".groups", "techniques", "software")]
  
  
  # First unnest techniques (keeping software_str as NA where technique exists)
  techniques_part <- flattened_df %>%
    select(id, name, techniques_str) %>%
    separate_rows(techniques_str, sep = ",\\s*") %>%
    mutate(techniques_str = str_trim(techniques_str),
           software_str = NA_character_) %>%
    filter(techniques_str != "")  # Remove empty strings
  
  # Then unnest software (keeping techniques_str as NA where software exists)
  software_part <- flattened_df %>%
    select(id, name, software_str) %>%
    separate_rows(software_str, sep = ",\\s*") %>%
    mutate(software_str = str_trim(software_str),
           techniques_str = NA_character_) %>%
    filter(software_str != "")  # Remove empty strings
  
  # Combine both parts
  Mitre_attack_final_flattened <- bind_rows(techniques_part, software_part) %>%
    arrange(id, name)  # Optional sorting
  
  return(Mitre_attack_final_flattened)
}






#####################################
#get the schema
#####################################
SCHEMA_TTPS_to_IOCs <- function(arg1) {
  
  CompleteURL <- paste("mongodb://", arg1, ":27017", sep = "")
  
  
  ## 1. Improved Connection with Timeout Settings
  con <- mongo(
    collection = "0_IDs_level_abstraction_schema",
    db = "Cyber_necronomicon_phd",
    url = CompleteURL,
    options = ssl_options(weak_cert_validation = TRUE),  # If using SSL
    verbose = FALSE
  )
  
  ## 2. Proper Data Retrieval with Error Handling
  tryCatch({
    # Get all documents as proper nested structure
    all_documents <- con$find(query = '{}')
    
    # Convert to pure list format if it's coming as data frame
    if (is.data.frame(all_documents)) {
      all_documents_list <- jsonlite::fromJSON(
        jsonlite::toJSON(all_documents, auto_unbox = TRUE),
        simplifyDataFrame = FALSE
      )
    } else {
      all_documents_list <- all_documents
    }
    
    # Remove empty elements
    all_documents_list <- purrr::discard(all_documents_list, ~ length(.x) == 0)
    
    # Print success message
    message(sprintf("Successfully retrieved %d documents", length(all_documents_list)))
  }, error = function(e) {
    message("Error retrieving data: ", e$message)
  }, finally = {
    # Always disconnect
    if (exists("con")) rm(con)
    gc()
  })
  
  
  all_documents_list <- all_documents_list[1]#lsit
  all_documents_list <- all_documents_list[[1]]
  
  flatten_to_table <- function(nested_list) {
    # Helper function to recursively flatten and track both path and item numbers
    flatten_recursive <- function(x, depth = 1, parent_name = "", item_num = NA) {
      if (!is.list(x)) {
        # Base case - not a list
        return(data.frame(
          path = parent_name,
          value = as.character(x),
          depth = depth,
          item_num = item_num,
          stringsAsFactors = FALSE
        ))
      }
      
      if (length(x) == 0) {
        # Empty list case
        return(data.frame(
          path = parent_name,
          value = NA_character_,
          depth = depth,
          item_num = item_num,
          stringsAsFactors = FALSE
        ))
      }
      
      # Recursive case
      dfs <- lapply(seq_along(x), function(i) {
        elem_name <- if (is.null(names(x))) i else names(x)[i]
        new_name <- if (parent_name == "") elem_name else paste(parent_name, elem_name, sep = "_")
        
        # Only assign item_num for vectors at the current level
        current_item_num <- if (!is.list(x[[i]]) && length(x[[i]]) > 1) seq_along(x[[i]]) else NA
        
        flatten_recursive(
          x[[i]], 
          depth + 1, 
          new_name,
          item_num = current_item_num
        )
      })
      
      rbindlist(dfs, fill = TRUE)
    }
    
    # Start recursion and convert to data frame
    as.data.frame(flatten_recursive(nested_list))
  }
  
  # Process the data
  flat_data <- flatten_to_table(all_documents_list)
  
  # Now proceed with your existing code:
  max_depth <- max(flat_data$depth, na.rm = TRUE)
  col_names <- paste0("level_", seq_len(max_depth))
  
  # Split paths (using underscore separator to match the function)
  path_df <- map_dfc(seq_len(max_depth), ~ {
    map_chr(flat_data$path, function(p) {
      parts <- strsplit(p, "_", fixed = TRUE)[[1]]
      if (length(parts) >= .x) parts[.x] else NA_character_
    })
  }) %>% set_names(col_names)
  
  # Combine with values - now item_num exists
  result <- bind_cols(path_df, flat_data %>% select(value, item_num))
  
  # For terminal nodes with multiple items, append item number to last level
  
  FullTTP_schema <- result %>%
    mutate(across(last(col_names), ~ ifelse(!is.na(item_num), 
                                            paste(., item_num, sep = "_"), 
                                            .))) %>%
    select(-item_num)
  
  
  FullTTP_schema <- FullTTP_schema %>%
    mutate(
      exploits = ifelse(level_5 == "exploits", value, NA),
      PulseID = ifelse(level_5 == "PulseID", level_6, NA),
      name = ifelse(level_5 == "PulseID", value, NA)
    ) %>%
    select(-level_5, -level_6, -value)  # Remove original columns
  
  
  colnames(FullTTP_schema)[colnames(FullTTP_schema) == "level_1"] <- "ATTCK_ID"
  colnames(FullTTP_schema)[colnames(FullTTP_schema) == "level_2"] <- "CAPEC_ID"
  colnames(FullTTP_schema)[colnames(FullTTP_schema) == "level_3"] <- "CWE_ID"
  colnames(FullTTP_schema)[colnames(FullTTP_schema) == "level_4"] <- "CVE_ID"
  colnames(FullTTP_schema)[colnames(FullTTP_schema) == "PulseID"] <- "Pulse_ID"
  
  FullTTP_schema <- FullTTP_schema[, !(names(FullTTP_schema) %in% c("level_7", "level_8"))]
  
  
  FullTTP_schema <- FullTTP_schema[!(duplicated(FullTTP_schema) | duplicated(FullTTP_schema, fromLast = TRUE)), ]
  
  return(FullTTP_schema)
}






#####LINKING GROUPS AND CVE
# library(mongolite)
# library(jsonlite)
# 
# # Connect to MongoDB
# con <- mongo(
#   collection = "5_OTX_CVE_to_exploits_N_IOCsID",
#   db = "Cyber_necronomicon_phd",
#   url = "mongodb://192.168.0.21:27017"
# )
# 
# # Query for all documents (including those with empty groups)
# all_documents <- con$find(
#   fields = '{"CVE_ID": 1, "OTX_alienvault.pulse_info.pulses.groups": 1, "_id": 0}'
# )
# 
# # Create the dictionary structure
# result_dict <- list()
# for (i in 1:nrow(all_documents)) {
#   cve_id <- all_documents[i, "CVE_ID"]
#   
#   # Safely extract groups (handling cases where path doesn't exist)
#   groups <- tryCatch({
#     grps <- all_documents[i, "OTX_alienvault"]$pulse_info$pulses$groups
#     if (is.null(grps)) list() else grps
#   }, error = function(e) list())
#   
#   # If groups is a list of lists, flatten it
#   if (is.list(groups) && length(groups) == 1 && is.list(groups[[1]])) {
#     groups <- unlist(groups, recursive = FALSE)
#   }
#   
#   result_dict[[cve_id]] <- groups
# }
# 
# result_dict2 <- result_dict[sapply(result_dict, length) > 0]



####################
#Groups linked to IOCs through alienvault
###################
OTX_Groups_to_data <- function(arg1) {

  
  CompleteURL <- paste0("mongodb://", arg1, ":27017")
  con <- mongo(
    collection = "6_OTX_PulseID_IOCs",
    db = "Cyber_necronomicon_phd", 
    url = CompleteURL
  )
  
  # Get all documents and convert to list - FIXED BRACKET ISSUE HERE
  docs_list <- con$find(query = '{}', fields = '{}') %>%
    { lapply(seq_len(nrow(.)), function(i) {
      jsonlite::fromJSON(
        jsonlite::toJSON(.[i, ], auto_unbox = TRUE),
        simplifyVector = FALSE
      )
    }) }
  
  #get one linked to groups
  # docs_list <- con$find(
  #   query = '{"in_group": true}',
  #   fields = '{}'
  # ) %>%
  #   # Convert each row to list, preserving nesting
  #   { lapply(seq_len(nrow(.)), function(i) {
  #     jsonlite::fromJSON(
  #       jsonlite::toJSON(.[i, ], auto_unbox = TRUE),
  #       simplifyVector = FALSE
  #     )
  #   }) }
  
  
  
  # Improved recursive function to search through documents
  find_and_extract <- function(x, id = NULL) {
    # Skip atomic vectors (non-list elements)
    if (!is.list(x)) return(NULL)
    
    # Check if this is a pulse-like object
    if (!is.null(x$id) && !is.null(x$groups)) {
      return(list(
        id = x$id,
        groups = x$groups,
        attack_ids = x$attack_ids %||% list(),
        indicators = x$indicators %||% list()
      ))
    }
    
    # Recursive case: search through all list elements
    results <- list()
    for (item in x) {
      # Only recurse if it's a list (not an atomic vector)
      if (is.list(item)) {
        found <- find_and_extract(item)
        if (!is.null(found)) {
          # Handle both single items and lists of items
          if (!is.null(found$id)) {
            results[[found$id]] <- found
          } else {
            results <- c(results, found)
          }
        }
      }
    }
    
    if (length(results) > 0) results else NULL
  }
  
  # Process documents with error handling
  newdict <- list()
  for (doc in docs_list) {
    tryCatch({
      extracted <- find_and_extract(doc)
      if (!is.null(extracted)) {
        newdict <- c(newdict, extracted)
      }
    }, error = function(e) {
      message("Error processing document: ", e$message)
    })
  }
  
  # Flatten to dataframe
  flattened_data <- map_df(newdict, function(entry) {
    tryCatch({
      # Extract common fields
      group_info <- if (length(entry$groups) > 0) entry$groups[[1]] else list(name = NA, id = NA)
      
      # Create attack_id rows
      attack_rows <- if (length(entry$attack_ids) > 0) {
        tibble(
          id = entry$id,
          groups_name = group_info$name,
          groups_id = group_info$id,
          attack_ids = unlist(entry$attack_ids),
          indicators_id = NA_integer_,
          indicators_indicator = NA_character_,
          indicators_type = NA_character_,
          indicators_created = NA_character_,
          indicators_content = NA_character_,
          indicators_title = NA_character_,
          indicators_description = NA_character_,
          indicators_is_active = NA_integer_
        )
      } else {
        NULL
      }
      
      # Create indicator rows
      indicator_rows <- if (length(entry$indicators) > 0) {
        map_df(entry$indicators, function(ind) {
          tibble(
            id = entry$id,
            groups_name = group_info$name,
            groups_id = group_info$id,
            attack_ids = NA_character_,
            indicators_id = ind$id %||% NA_integer_,
            indicators_indicator = ind$indicator %||% NA_character_,
            indicators_type = ind$type %||% NA_character_,
            indicators_created = ind$created %||% NA_character_,
            indicators_content = ind$content %||% NA_character_,
            indicators_title = ind$title %||% NA_character_,
            indicators_description = ind$description %||% NA_character_,
            indicators_is_active = ind$is_active %||% NA_integer_
          )
        })
      } else {
        NULL
      }
      
      bind_rows(attack_rows, indicator_rows)
    }, error = function(e) {
      message("Error processing entry ", entry$id %||% "unknown", ": ", e$message)
      NULL
    })
  })
  
  # Final processing
  OTX_groups_IOCsetc <- flattened_data %>%
    select(id, groups_name, groups_id, attack_ids,
           indicators_id, indicators_indicator, indicators_type,
           indicators_created, indicators_content, indicators_title,
           indicators_description, indicators_is_active) %>%
    mutate(across(where(is.character), ~na_if(., "Error when retrieving OTX data")))

  # View the result
  return(OTX_groups_IOCsetc)
}


getSoftwares <- function(arg1){
  CompleteURL <- paste("mongodb://", arg1, ":27017", sep = "")
  
  #MITRE ATT&CK group part
  # 1. Connect to MongoDB
  con <- mongo(
    collection = "1_MITRE_ATTCK_Data",
    db = "Cyber_necronomicon_phd",
    url = CompleteURL
  )
  
  # 2. Get ALL documents where ID starts with G - no field filtering
  group_docs <- con$find('{"title.ID": {"$regex": "^S"}}')
  
  # 3. Convert directly to JSON (preserves all nested structure)
  json_output <- toJSON(group_docs, pretty = TRUE)
  json_output2 <- fromJSON(json_output, simplifyDataFrame = FALSE)
  
  
  
  json_output2[[1]][["ScrapDate"]]
  #"25-11-2024_14-33-57"
  
  
  keep_elements <- sapply(json_output2, function(x) {
    !is.null(x[["ScrapDate"]]) && x[["ScrapDate"]] == "25-11-2024_14-33-57"
  })
  
  json_output3 <- json_output2[keep_elements]
  
  
  
  newdataset <- list()
  
  for(element in json_output3) {
    # Process Techniques Used - remove empty sublists
    techList <- element$HeadersAndTables$`Techniques Used`
    techList <- names(techList[lengths(techList) > 0])  # Keep only non-empty items
    
    # Process Software - remove empty sublists
    SoftList <- element$HeadersAndTables$Software
    SoftList <- names(SoftList[lengths(SoftList) > 0])  # Keep only non-empty items
    
    # Only create entry if at least one list has content
    
    new_entry <- list(
      id = element$title$ID,  # Including ID in the entry itself
      name = element$title$Name,
      techniques = techList,
      software = SoftList
    )
    
    # Use ID as the list name
    newdataset[[as.character(element$title$ID)]] <- new_entry
    
  }
  
  
  
  # Method that handles varying lengths properly
  flattened_df <- imap_dfr(newdataset, ~ {
    # Convert each element to a tibble, handling varying lengths
    tibble(
      id = .x$id,
      name = .x$name,
      techniques = list(.x$techniques),  # Wrap in list to preserve vectors
      software = list(.x$software),
      .groups = "drop"
    ) %>% 
      mutate(group = .y)  # Add the group name
  }) %>% 
    select(group, everything())
  
  # If you want techniques/software as comma-separated strings:
  flattened_df <- flattened_df %>%
    mutate(
      techniques_str = map_chr(techniques, function(x) {
        if(length(x) > 0) paste(x, collapse = ", ") else NA_character_
      }),
      software_str = map_chr(software, function(x) {
        if(length(x) > 0) paste(x, collapse = ", ") else NA_character_
      })
    )
  
  return(flattened_df)
}


linkTechNsoft <- function(arg1, arg2){
  
  testMitregroup <- arg1
  testsoftware <- arg2
  
  APTnamesUnique <- unique(testMitregroup$name)
  APTnamesUnique <- APTnamesUnique[!is.na(APTnamesUnique)]
  SoftwareNameUnique <- unique(testMitregroup$software_str)
  SoftwareNameUnique <- SoftwareNameUnique[!is.na(SoftwareNameUnique)]
  
  
  
  
  
  testsoftware <- testsoftware %>% 
    filter(id %in% SoftwareNameUnique)
  
  testsoftware <- subset(testsoftware, select = c(id, techniques))
  
  
  
  newDf <- data.frame()
  
  
  for(APT in APTnamesUnique){
    
    # 1. Filter group data
    tempdf <- testMitregroup %>% 
      filter(name %in% APT)
    
    if (all(is.na(tempdf$software_str))) {
      #print(paste("Empty soft:", APT))
      newDf <- rbind(newDf, tempdf)
    } else {
      #print(APT)
      
      # 2. Get all unique techniques and software from the group
      group_techniques <- tempdf %>% 
        filter(!is.na(techniques_str)) %>% 
        distinct(techniques_str) %>% 
        pull(techniques_str)
      
      group_software <- tempdf %>% 
        filter(!is.na(software_str)) %>% 
        distinct(software_str) %>% 
        pull(software_str)
      
      # 3. Get software techniques and flatten
      tempsoft <- testsoftware %>% 
        filter(id %in% group_software) %>% 
        unnest_longer(techniques)
      
      # 4. Create technique-software mapping (only for techniques in this group)
      tech_software_mapping <- tempsoft %>%
        filter(techniques %in% group_techniques) %>%
        distinct(techniques, software_str = id)
      
      # 5. Create the techniques part of the output
      techniques_part <- tempdf %>%
        filter(!is.na(techniques_str)) %>%
        distinct(id, name, techniques_str) %>%
        left_join(tech_software_mapping, by = c("techniques_str" = "techniques")) %>%
        select(id, name, techniques_str, software_str)
      
      # 6. Create the software-only part (only software not already covered)
      covered_software <- techniques_part %>% 
        filter(!is.na(software_str)) %>% 
        distinct(software_str) %>% 
        pull(software_str)
      
      missing_software <- setdiff(group_software, covered_software)
      
      software_part <- tibble(
        id = unique(tempdf$id),
        name = unique(tempdf$name),
        techniques_str = NA_character_,
        software_str = missing_software
      )
      
      # 7. Combine both parts
      final_df <- bind_rows(techniques_part, software_part) %>%
        arrange(techniques_str, software_str)
      
      newDf <- rbind(newDf, final_df)
    }
  }
  
  
  
  
  return(newDf)
}

######################
#full combitnation
######################

library(mongolite)
library(jsonlite)
library(purrr)
library(tidyverse)
library(tidyr)
library(data.table)
library(dplyr)


#"mongodb://192.168.1.245:27017"
IP <- "192.168.1.245" 

# MitreGroupTTPs <- MITRE_Groups_TTPs(IP)
# 
# SchemaATTCKT_TTP_to_lowestLEvel <- SCHEMA_TTPS_to_IOCs(IP)
# 
# OTXGroupsToData <- OTX_Groups_to_data(IP)
# 
# softwares <- getSoftwares(IP)
# 
# MitreGroupTTPs <- linkTechNsoft(MitreGroupTTPs, softwares)

MitreGroupTTPs <- read.csv("./dataforBayesiaNetwork/MitreGroupTTPs.csv")
SchemaATTCKT_TTP_to_lowestLEvel <- read.csv("./dataforBayesiaNetwork/SchemaATTCKT_TTP_to_lowestLEvel.csv")
OTXGroupsToData <- read.csv("./dataforBayesiaNetwork/OTXGroupsToData.csv")



# write.csv(SchemaATTCKT_TTP_to_lowestLEvel, file = "./dataforBayesiaNetwork/SchemaATTCKT_TTP_to_lowestLevel.csv", row.names = FALSE)
# write.csv(MitreGroupTTPs, file = "./dataforBayesiaNetwork/MitreGroupTTPs.csv", row.names = FALSE)
# write.csv(OTXGroupsToData, file = "./dataforBayesiaNetwork/OTXGroupsToData.csv", row.names = FALSE)
####################################################################################
#link MITRE group and Schema:

# SchemaATTCKT_TTP_to_lowestLEvel <- SchemaATTCKT_TTP_to_lowestLEvel %>%
#   # Group by the key columns that identify duplicates
#   group_by(ATTCK_ID, CAPEC_ID, CWE_ID, CVE_ID) %>%
#   # Filter: Keep only rows where either:
#   # 1. It's the only row in the group (n() == 1), OR
#   # 2. It has at least one non-NA value in (exploits, Pulse_ID, name)
#   filter(
#     n() == 1 |
#       !(is.na(exploits) & is.na(Pulse_ID) & is.na(name))
#   ) %>%
#   ungroup()
# 
# 
# # Now perform the left join to enrich the MitreGroupTTPs data
# enriched_ttp_data <- MitreGroupTTPs %>%
#   left_join(unique_ttp_details,
#             by = c("techniques_str" = "ATTCK_ID"),
#             suffix = c("_group", "_technique"))

# View the results
# head(enriched_ttp_data)

MitreGroupTTPs <- read.csv("./dataforBayesiaNetwork/MitreGroupTTPs.csv")
SchemaATTCKT_TTP_to_lowestLEvel <- read.csv("./dataforBayesiaNetwork/SchemaATTCKT_TTP_to_lowestLEvel.csv")
OTXGroupsToData <- read.csv("./dataforBayesiaNetwork/OTXGroupsToData.csv")


setnames(MitreGroupTTPs, old = c("name", "id"), new = c("APTname", "APTid"))


## Step 1: Clean the SchemaATTCKT data - fixed version
SchemaATTCKT_clean <- SchemaATTCKT_TTP_to_lowestLEvel %>%
  group_by(ATTCK_ID, CAPEC_ID, CWE_ID, CVE_ID) %>%
  filter(n() == 1 | (!is.na(exploits) | !is.na(Pulse_ID) | !is.na(name))) %>%
  ungroup()

setnames(SchemaATTCKT_clean, old = c("name"), new = c("IOCname"))

## Step 2: Perform the join with explicit column handling
enriched_ttp_data <- MitreGroupTTPs %>%
  left_join(
    SchemaATTCKT_clean,
    by = c("techniques_str" = "ATTCK_ID"),
    relationship = "many-to-many"  # Acknowledging the expected relationship
  ) 


## Step 3: View the results
head(enriched_ttp_data)

# enriched_ttp_data$IOCname <- NULL
# enriched_ttp_data$exploits <- NULL
# enriched_ttp_data$APTid <- NULL
enriched_ttp_data <- subset(enriched_ttp_data, select = -c(APTid, exploits, IOCname))


filtered_df <- OTXGroupsToData %>%
  filter(!is.na(groups_name) & 
           (!is.na(attack_ids) | indicators_type == "CVE"))

filtered_df <- filtered_df %>%
  rename_with(~ case_when(
    .x == "groups_name" ~ "APTname",
    .x == "attack_ids" ~ "techniques_str",
    .x == "indicators_indicator" ~ "CVE_ID",
    .x == "id" ~ "Pulse_ID",
    TRUE ~ .x
  )) %>%
  select(APTname, techniques_str, CVE_ID, Pulse_ID)




missing_cols <- setdiff(names(enriched_ttp_data), names(filtered_df))
filtered_df[missing_cols] <- NA

# Reorder columns to match enriched_ttp_data
#filtered_df <- filtered_df[, names(enriched_ttp_data)]



enriched_ttp_data <- rbind(enriched_ttp_data, filtered_df)


write.csv(enriched_ttp_data, file = "./dataforBayesiaNetwork/TestFinal_df.csv", row.names = FALSE)


################################################################################
#TEST 
################################################################################



