library("fmsb")
library("rjson")
library("ggplot2")
library("tidyverse")
library("dplyr")
library("plyr")
library("stringr")
library("data.table")


FinalDataFrame <- read.csv("/Users/morzen/Desktop/FinalDataFrame_Oct2024.csv")
unique_list <- lapply(FinalDataFrame, unique)

unique_list <- lapply(FinalDataFrame, function(x) unique(na.omit(x)))



# CWEs <- read.csv("/Users/morzen/Downloads/1000.csv")
# data <- read.table("/Users/morzen/Downloads/1000.tsv", header = TRUE, sep = "\t")


library(readxl)

# Read first sheet
df <- read_excel("/Users/morzen/Downloads/699.xls")
df2 <- read.csv("/Users/morzen/Desktop/699Test_fromExcel.csv")

library(readr)

# Export with all safeguards
write_csv(
  df,
  "699Test_fromExcel.csv",
  na = "NA",                  # Consistent NA representation
  append = FALSE,             # Overwrite existing file
  col_names = TRUE,           # Keep column names
  quote = "needed",           # Only quote when necessary
  escape = "double",          # Proper escaping
  eol = "\n"                  # Unix-style line endings
)
