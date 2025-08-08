library("fmsb")
library("rjson")
library("ggplot2")
library("tidyverse")
library("dplyr")
library("plyr")
library("stringr")
library("data.table")
library("bnlearn")

FinalDataFrame <- read.csv("./dataforBayesiaNetwork/TestFinal_df.csv")
################################################################################
# BaysNetwork 
################################################################################
df <- FinalDataFrame


###################################
# data handling finished next encoding
###################################

data_for_bn <- as.data.frame(lapply(df, function(column) {
  if (is.factor(column) || is.character(column)) {
    as.factor(column)  # Keep as factor instead of converting to numeric
  } else {
    column
  }
}))

##############################################
# 
# comparison <- do.call(rbind,
#                       Map(
#                         data.frame,
#                         APTnameOrigin=df$APTname,
#                         APTnameEncoded=data_for_bn$APTname,
#                         techniques_Origin=df$techniques_str,
#                         techniques_Encoded=data_for_bn$techniques_str,
#                         software_Origin=df$software_str,
#                         software_Encoded=data_for_bn$software_str,
#                         CAPEC_ID_Origin=df$CAPEC_ID,
#                         CAPEC_ID_Encoded=data_for_bn$CAPEC_ID,
#                         CWE_ID_Origin=df$CWE_ID,
#                         CWE_ID_Encoded=data_for_bn$CWE_ID,
#                         CVE_ID_Origin=df$CVE_ID,
#                         CVE_ID_Encoded=data_for_bn$CVE_ID,
#                         Pulse_ID_Origin=df$Pulse_ID,
#                         Pulse_ID_Encoded=data_for_bn$Pulse_ID
#                       ))
# 
# write.csv(comparison, file = "./dataforBayesiaNetwork/comparison_Encodedtest_df.csv", row.names = FALSE)

# ################################################
# write.csv(data_for_bn, file = "./dataforBayesiaNetwork/data_for_bn_Encodedtest_df.csv", row.names = FALSE)
# data_for_bn <- read.csv("./dataforBayesiaNetwork/data_for_bn_Encodedtest_df.csv")

comparison <- read.csv("./dataforBayesiaNetwork/comparison_Encodedtest_df.csv")






library(data.table)
library(bnlearn)

# 1. Load data with proper NA handling
raw_data <- fread("./dataforBayesiaNetwork/TestFinal_df.csv", 
                  na.strings = c("", "NA", "NULL", "\\N"))

# 2. Select only the columns you need for the BN
cols_to_keep <- c("APTname", "techniques_str", "software_str") # Add others as needed
data_subset <- raw_data[, ..cols_to_keep]

# 3. Convert to factors while preserving original levels
for (col in cols_to_keep) {
  data_subset[[col]] <- as.factor(data_subset[[col]])
}

# 4. Create a complete-case sample that preserves rare levels
set.seed(123)
sample_size <- min(10000, nrow(data_subset)) # Adjust based on your memory

# Stratified sampling to preserve rare levels
sampled_data <- data_subset %>%
  group_by(across(all_of(cols_to_keep))) %>%
  sample_frac(size = sample_size/nrow(data_subset), replace = FALSE) %>%
  ungroup()

# 5. Ensure we have enough observations for pnal
min_obs <- 50 # Minimum observations needed for pnal to work well
if (nrow(sampled_data) < min_obs) {
  warning("Sample size too small for pnal - consider increasing sample size")
  sampled_data <- data_subset[sample(.N, min_obs), ]
}

# 6. Final data prep
sampled_data <- droplevels(na.omit(sampled_data)) # Now safe to remove NAs

# 7. Run pnal score
if (nrow(sampled_data) > 0) {
  bn_pnal <- hc(sampled_data, score = "pnal", restart = 5, perturb = 3)
  plot(bn_pnal)
} else {
  stop("No valid observations remaining after preprocessing")
}

# 8. Optional: Compare with other scores
bn_bde <- hc(sampled_data, score = "bde")
compare(bn_pnal, bn_bde)



#imputed_df <- imputed_df[sample(nrow(imputed_df), size = round(nrow(imputed_df)/20)), ]


#bic works but the dag isn't great
learned_structure1 <- hc(deduped_df, score = "bic")
#bde is the best so far
learned_structure2 <- hc(deduped_df, score = "bde")
#pnal doesn't work #pnal : Pearson's Negative Affinity Linkage to cluster
learned_structure3 <- hc(deduped_df, score = "pnal")


plot(learned_structure2)

bn <-learned_structure3

# Fit the Bayesian network
#maximum likelihood mle
bn_fit <- bn.fit(bn, imputed_df, method = "mle")




save(bn_fit, file = "./dataforBayesiaNetwork/bn_fit.rda")
save(imputed_df, file = "./dataforBayesiaNetwork/data_for_bn.RData")
##############################################################################



library(data.table)
library(bnlearn)
library(Rgraphviz)

# 1. Load data while preserving relationships
raw_data <- fread("./dataforBayesiaNetwork/TestFinal_df.csv", 
                  na.strings = c("", "NA", "NULL", "\\N"))

# 2. Smarter factor conversion (preserves key relationships)
convert_factors <- function(x, colname) {
  if (is.character(x) || is.factor(x)) {
    x <- as.character(x)
    # Special handling for different column types
    if (colname %in% c("APTname", "techniques_str", "software_str")) {
      # Preserve full values for key variables
      return(as.factor(x))
    } else if (grepl("^CVE", x[1])) {
      return(as.factor(substr(x, 1, 10))) # Keep more CVE info
    } else {
      # For other columns, keep top 30 levels
      top <- names(sort(table(x), decreasing = TRUE)[1:min(30, length(unique(x)))])
      return(factor(ifelse(x %in% top, x, "OTHER"), levels = c(top, "OTHER")))
    }
  }
  return(x)
}

# Apply conversion column-wise
data_clean <- raw_data[, Map(convert_factors, .SD, names(.SD))]
data_clean <- na.omit(data_clean)

# 3. Focus on most connected APTs (better relationships)
apt_connections <- data_clean[, .(count = .N), by = APTname][order(-count)]
top_apts <- apt_connections[1:min(15, .N), APTname]
sampled_data <- data_clean[APTname %in% top_apts]

# 4. Learn structure with relationship-focused parameters
bn_model <- tabu(sampled_data,
                 score = "bde",
                 iss = 10,       # Better for small samples
                 tabu = 100,     # More thorough search
                 max.iter = 2000,
                 maxp = 4)       # Reasonable complexity

# 5. Visualize meaningful relationships
strength <- arc.strength(bn_model, sampled_data)
strong_arcs <- strength[strength$strength > 0.25, ] # Show stronger connections

# Customized plot
graphviz.plot(bn_model,
              layout = "dot",    # Hierarchical layout
              main = "APT Threat Pattern Analysis",
              highlight = list(
                arcs = strong_arcs[, c("from", "to")],
                col = "red",
                lwd = 2
              ),
              render = TRUE)



####################################################

# Using mtcars dataset as example
data(data_clean)

# First 5 rows
cat("First 5 rows:\n")
print(head(data_clean, 5))

# 5 random rows
cat("\n5 random rows:\n")
set.seed(123)
print(data_clean[sample(nrow(data_clean), 5), ])

# Last 5 rows
cat("\nLast 5 rows:\n")
print(tail(data_clean, 5))


#####################################################
library(data.table)
library(bnlearn)
library(Rgraphviz)
library("fmsb")
library("rjson")
library("ggplot2")
library("tidyverse")
library("dplyr")
library("plyr")
library("stringr")
library("data.table")
library("bnlearn")

library(data.table)
library(bnlearn)
library(Rgraphviz)

# 1. Load and clean data
raw_data <- fread("./dataforBayesiaNetwork/TestFinal_df.csv",
                  na.strings = c("", "NA", "NULL", "\\N"),
                  colClasses = "character")

# 2. Enhanced level reduction with droplevels
reduce_levels <- function(x, colname) {
  if (!is.character(x)) return(x)
  
  # Column-specific handling
  if (colname == "APTname") {
    top <- names(sort(table(x), decreasing = TRUE)[1:min(50, length(unique(x)))])
    x[!x %in% top] <- "OTHER_APT"
  } 
  else if (colname == "techniques_str") {
    x <- substr(x, 1, 6) # T1059.001 → T1059.
  }
  else if (colname %in% c("CAPEC_ID", "CWE_ID")) {
    x <- paste0(substr(colname, 1, 3), substr(x, nchar(x)-2, nchar(x))) # CAPEC-100 → CAP100
  }
  else if (grepl("CVE|Pulse", colname)) {
    x <- substr(x, 1, 8) # CVE-2021-34527 → CVE-2021
  }
  else {
    top <- names(sort(table(x), decreasing = TRUE)[1:min(30, length(unique(x)))])
    x[!x %in% top] <- "OTHER"
  }
  
  # Convert to factor and drop unused levels immediately
  droplevels(as.factor(x))
}

# 3. Apply reduction and clean
data_clean <- raw_data[, Map(reduce_levels, .SD, names(.SD))] %>% na.omit()

# 4. Verify no unused levels remain
stopifnot(all(sapply(data_clean, function(x) {
  if (is.factor(x)) all(levels(x) %in% x) else TRUE
})))

# 5. Learn structure with Bayesian scoring
bn_model <- hc(data_clean, 
               score = "bde",  # Bayesian with informed prior
               iss = 10,       # Equivalent sample size
               maxp = 2)       # Limit parent nodes

# 6. Fit with logistic regression (sparse)
bn_fit <- bn.fit(bn_model, data_clean, method = "logistic")

# 7. Custom similarity function with evidence handling
apt_similarity <- function(model, target_apt, evidence) {
  # Process evidence to match model's factor levels
  processed_ev <- lapply(names(evidence), function(col) {
    if (col %in% names(data_clean)) {
      lvls <- levels(data_clean[[col]])
      val <- evidence[[col]]
      # Convert evidence to factor with same levels
      if (is.character(val)) {
        if (val %in% lvls) val else "OTHER"
      } else {
        val
      }
    } else {
      evidence[[col]]
    }
  }) %>% setNames(names(evidence))
  
  # Get posterior distribution
  post <- cpdist(model, nodes = "APTname", 
                 evidence = processed_ev,
                 method = "lw", n = 10000)
  
  # Calculate normalized similarity
  sim_scores <- prop.table(table(post$APTname))
  if (target_apt %in% names(sim_scores)) sim_scores[target_apt] else 0
}

# 8. Visualization of strong relationships
strength <- arc.strength(bn_model, data_clean)
strong_arcs <- strength[strength$strength > quantile(strength$strength, 0.8), ]

graphviz.plot(bn_model,
              layout = "dot",
              main = "APT Threat Relationship Network",
              highlight = list(
                arcs = as.matrix(strong_arcs[, c("from", "to")]),
                col = "red",
                lwd = 2
              ))














