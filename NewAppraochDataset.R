library(dplyr)
library(stringr)
library(tidyr)

# Use read.csv for base R. It's a good practice to set stringsAsFactors to FALSE.
FullDataScrap <- read.csv("./oldStuff/1_DATAusedFolder/FinalDataFrame_Apr2024.csv", stringsAsFactors = FALSE)
FinalDF_need_enrichement <- read.csv("./TestFinal_df.csv", stringsAsFactors = FALSE)
FullDataScrap <- as.data.frame(FullDataScrap)
FinalDF_need_enrichement <- as.data.frame(FinalDF_need_enrichement)


TTPdetail_test <- FinalDF_need_enrichement
TTPdetail_test <- TTPdetail_test$techniques_str
TTPdetail_test <- unique(TTPdetail_test)

# First, create a lookup table using the SAME logic as your tactics_adding_Func
technique_tactics_lookup <- FullDataScrap %>%
  filter(PageType == "Techniques") %>%
  mutate(
    # THIS IS THE KEY - exactly like your tactics_adding_Func
    Tactic = ifelse(is.na(SumUpTable_Tactic), SumUpTable_Tactics, SumUpTable_Tactic)
  ) %>%
  filter(!is.na(Tactic)) %>%
  select(Technique_ID = SumUpTable_ID, Tactic) %>%
  distinct()


# Create a dataframe with techniques and their tactics
techniques_with_tactics <- data.frame(
  Technique = TTPdetail_test,
  Tactic = NA
)

# Fill in the tactics
for(i in 1:length(TTPdetail_test)) {
  tech <- TTPdetail_test[i]
  
  # Look for the technique in the lookup table
  matches <- technique_tactics_lookup[technique_tactics_lookup$Technique_ID == tech, "Tactic"]
  
  if(length(matches) > 0) {
    # If multiple tactics (like comma-separated), they're already combined
    techniques_with_tactics$Tactic[i] <- matches[1]
  }
}

# Convert Tactic column to list (splitting by comma when present)
techniques_with_tactics <- techniques_with_tactics %>%
  mutate(
    # Split by comma and trim whitespace
    Tactic_list = str_split(Tactic, ",\\s*")
  )
# Join the tactics to your dataframe by matching techniques_str with Technique
FinalDF_need_enrichement <- FinalDF_need_enrichement %>%
  left_join(techniques_with_tactics %>% select(Technique, Tactic_list), 
            by = c("techniques_str" = "Technique")) %>%
  # Move Tactic_list to the first position
  select(Tactic_list, everything())

# View the result
head(FinalDF_need_enrichement)

# Explode the Tactic_list column so each tactic gets its own row
FinalDF_need_enrichement_exploded <- FinalDF_need_enrichement %>%
  unnest_longer(Tactic_list)

# View the result
write.csv(FinalDF_need_enrichement_exploded, "FinalDF_Tactics_enrichement_exploded.csv", row.names = FALSE)



#####
#start analysis
#####
library(dplyr)
library(stringr)
library(tidyr)

FinalDF_Tactics_enrichement_exploded <- read.csv("./FinalDF_Tactics_enrichement_exploded.csv", stringsAsFactors = FALSE)
FinalDF_Tactics_enrichement_exploded <- as.data.frame(FinalDF_Tactics_enrichement_exploded)





############################
#####
#start analysis
#####
library(dplyr)
library(stringr)
library(tidyr)

FinalDF_Tactics_enrichement_exploded <- read.csv("./FinalDF_Tactics_enrichement_exploded.csv", stringsAsFactors = FALSE)
FinalDF_Tactics_enrichement_exploded <- as.data.frame(FinalDF_Tactics_enrichement_exploded)

# Create attack_path_df from the exploded dataframe, removing NA techniques
attack_path_df <- FinalDF_Tactics_enrichement_exploded %>%
  filter(!is.na(techniques_str)) %>%  # Remove NA techniques
  select(Tactic_list, APTname, techniques_str) %>%
  rename(
    Tactic = Tactic_list,
    Technique = techniques_str
  ) %>%
  distinct()

# Check what we have
print(paste("attack_path_df created with", nrow(attack_path_df), "rows"))
print("First few rows:")
head(attack_path_df)

# Function to build nested attack path structure (NO DEPTH LIMIT, NO MIN APTS)
build_nested_path <- function(current_technique, attack_path_df, 
                              current_stage = NULL, 
                              path_so_far = c(),
                              current_apts = NULL) {
  
  # Get current technique's tactics
  current_tactics <- attack_path_df %>%
    filter(Technique == current_technique) %>%
    pull(Tactic) %>%
    unique() %>%
    paste(collapse = ", ")
  
  # If first iteration, get all APTs with current technique
  if(is.null(current_apts)) {
    current_apts <- attack_path_df %>%
      filter(Technique == current_technique) %>%
      pull(APTname) %>%
      unique()
  } else {
    # Filter existing APT pool to those that ALSO have this technique
    current_apts <- attack_path_df %>%
      filter(APTname %in% current_apts,
             Technique == current_technique) %>%
      pull(APTname) %>%
      unique()
  }
  
  # Calculate current level from path_so_far
  current_level <- length(path_so_far) + 1
  
  # Create node structure
  node <- list(
    technique = current_technique,
    tactics = current_tactics,
    apt_count = length(current_apts),
    score = length(current_apts),
    level = current_level,
    path_so_far = if(current_level == 1) {
      current_technique
    } else {
      paste(c(path_so_far, current_technique), collapse = " → ")
    },
    apTs = current_apts,
    children = list()
  )
  
  # Update path
  path_so_far <- c(path_so_far, current_technique)
  
  # If no APTs left, return node with no children
  if(length(current_apts) == 0) {
    return(node)
  }
  
  # Find next technique candidates among current APT pool
  next_technique_candidates <- attack_path_df %>%
    filter(APTname %in% current_apts,
           !Technique %in% path_so_far,
           !is.na(Technique)) %>%  # Remove NA from candidates
    group_by(Technique, Tactic) %>%
    summarise(
      frequency = n_distinct(APTname),
      .groups = "drop"
    ) %>%
    arrange(desc(frequency))
  
  # Apply kill chain filtering if stage is provided
  if(!is.null(current_stage) && !is.na(current_stage)) {
    kill_chain <- c(
      "Reconnaissance", "Resource Development", "Initial Access", "Execution",
      "Persistence", "Privilege Escalation", "Defense Evasion", "Credential Access",
      "Discovery", "Lateral Movement", "Collection", "Command and Control",
      "Exfiltration", "Impact"
    )
    
    current_idx <- which(kill_chain == current_stage)
    
    if(length(current_idx) > 0) {
      allowed_stages <- kill_chain[current_idx:length(kill_chain)]
      next_technique_candidates <- next_technique_candidates %>%
        filter(Tactic %in% allowed_stages)
    }
  }
  
  # If no valid next techniques, we're done
  if(nrow(next_technique_candidates) == 0) {
    return(node)
  }
  
  # Add children for EACH candidate (no limit)
  for(i in 1:nrow(next_technique_candidates)) {
    next_tech <- next_technique_candidates$Technique[i]
    next_stage <- next_technique_candidates$Tactic[i]
    
    child_node <- build_nested_path(
      next_tech, attack_path_df, next_stage, 
      path_so_far, current_apts
    )
    
    node$children[[next_tech]] <- child_node
  }
  
  return(node)
}

# Function to build all nested paths (NO DEPTH LIMIT, NO MIN APTS)
build_all_nested_paths <- function(attack_path_df) {
  
  # Get ALL possible starting techniques (no minimum, excluding NA)
  starting_points <- attack_path_df %>%
    filter(!is.na(Technique)) %>%  # Remove NA techniques
    group_by(Technique, Tactic) %>%
    summarise(
      start_apt_count = n_distinct(APTname),
      .groups = "drop"
    ) %>%
    filter(start_apt_count >= 1) %>%  # At least 1 APT
    arrange(desc(start_apt_count))
  
  cat(paste0("Building nested paths for ", nrow(starting_points), " starting techniques...\n"))
  cat("No depth limit - will continue until no more options exist\n")
  cat("No minimum APTs - paths can narrow down to single APTs\n\n")
  
  # Build nested structure for each starting technique
  all_paths <- list()
  
  for(i in 1:nrow(starting_points)) {
    tech <- starting_points$Technique[i]
    stage <- starting_points$Tactic[i]
    
    cat(paste0("  Building path ", i, "/", nrow(starting_points), ": ", tech, 
               " (", starting_points$start_apt_count[i], " APTs)\n"))
    
    path_tree <- build_nested_path(tech, attack_path_df, current_stage = stage)
    all_paths[[tech]] <- path_tree
  }
  
  return(all_paths)
}

# RUN IT
cat("\n=== BUILDING NESTED PATH STRUCTURE ===\n")
cat("No depth limit | No minimum APTs | Stops when no more options\n")
cat("================================================================\n")

nested_paths <- build_all_nested_paths(attack_path_df)




# Save as RDS (preserves structure)
saveRDS(nested_paths, "attack_paths_nested_complete.rds")

# Save as JSON
library(jsonlite)
json_output <- toJSON(nested_paths, pretty = TRUE, auto_unbox = TRUE)
write(json_output, "attack_paths_nested_complete.json")

cat("\n=== SAVED ===\n")
cat("- Nested structure: 'attack_paths_nested_complete.rds'\n")
cat("- JSON version: 'attack_paths_nested_complete.json'\n")
