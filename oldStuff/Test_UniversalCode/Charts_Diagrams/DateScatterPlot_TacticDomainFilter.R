library("fmsb")
library("rjson")
library("ggplot2")
library("tidyverse")
library("plyr")
library("stringr")
library("data.table")
library("bnlearn")
library("dplyr")


#FinalDataFrame <- read.csv("./1_DATAusedFolder/FinalDataFrame_Apr2024.csv")


FinalDataFrame <- read.csv("/Users/morzen/Desktop/FinalDataFrame_Oct2024.csv")
################################################################################
# BaysNetwork 
################################################################################
CurrentDf <- FinalDataFrame %>% filter(Update_Date == "Data_Apr2024")

df <- CurrentDf

####################################
# Data handling and encoding
####################################
#OLD VERSION NOT FULL DATA

GroupsDF <- df %>%
  filter(PageType == "Groups") %>%
  filter(Tables_TableType %in% c("Techniques Used", "Software"))

TechDF <- df %>%
  filter(PageType == "Techniques")

# Filter Groups and Techniques data
#FULL DATA
# GroupsDF <- df %>% filter(PageType == "Groups") %>% filter(Tables_TableType %in% c("Techniques Used", "Software"))
# 
# TechDF <- df %>%
#   filter(Update_Date == "Data_Apr2024", PageType == "Techniques")

# Merge Tactic_s columns
TechDF$Tactic_s <- ifelse(is.na(TechDF$SumUpTable_Tactic), TechDF$SumUpTable_Tactics, TechDF$SumUpTable_Tactic)

# Drop NAs
# GroupsDF <- GroupsDF %>% select(where(~ !all(is.na(.))))
# TechDF <- TechDF %>% select(where(~ !all(is.na(.))))

GroupsDF <- GroupsDF[rowSums(is.na(GroupsDF)) != ncol(GroupsDF), ]
GroupsDF <- GroupsDF[, colSums(is.na(GroupsDF)) != nrow(GroupsDF)]

TechDF <- TechDF[rowSums(is.na(TechDF)) != ncol(TechDF), ]
TechDF <- TechDF[, colSums(is.na(TechDF)) != nrow(TechDF)]

# Extract unique Technique_ID and Tactic_s combinations
TacticDF <- data.frame(cbind(SumUpTable_ID = TechDF$SumUpTable_ID,
                             Tactic_s = TechDF$Tactic_s))

TacticDF <- TacticDF[!duplicated(TacticDF), ]

# Initialize an empty list to store Tactic_s values
TacticList <- list()

# Loop through Tables_ID values in GroupsDF
for (ID in GroupsDF$Tables_ID) {
  if (ID %in% TacticDF$SumUpTable_ID) {
    IndexForIDlist <- which(TacticDF$SumUpTable_ID == ID)
    
    if(!(length(IndexForIDlist) == 1)){
      print(IndexForIDlist)
      break
    }
    
    value <- TacticDF$Tactic_s[IndexForIDlist]
    TacticList <- append(TacticList, list(value))
    
  } else {
    value <- NA
    TacticList <- append(TacticList, list(value))
  }
}

# Add Tactic_s values to GroupsDF
GroupsDF$Tactic_s <- TacticList



# df <- do.call(rbind,
#               Map(
#                 data.frame,
#                 APTname = GroupsDF$Title_Name,
#                 Table_Types = GroupsDF$Tables_TableType,
#                 Techniques_N_Software = GroupsDF$Tables_ID,
#                 Tactic_s= GroupsDF$Tactic_s,
#                 Domain = GroupsDF$Tables_Domain
#               ))

df_data <- do.call(rbind,
              Map(
                data.frame,
                APTname = GroupsDF$Title_Name,
                Techniques_N_Software = GroupsDF$Tables_ID,
                Tactic_s= GroupsDF$Tactic_s,
                Domain = GroupsDF$Tables_Domain
              ))
row.names(df) <- seq_len(nrow(df))


df_data <- df_data %>%
  separate_rows(Tactic_s, sep = ", ")

##################################################

# GroupsPage_DF <- df %>%
#   filter(PageType == "Groups") %>% 
#   select(Title_Name, SumUpTable_Created, SumUpTable_Last.Modified) %>%
#   distinct()
# 
# TechPage_DF <- df %>%
#   filter(PageType == "Techniques")%>% 
#   select(Title_ID, SumUpTable_Created, SumUpTable_Last.Modified,) %>%
#   distinct()
# 
# SoftPage_DF <- df %>%
#   filter(PageType == "Software")%>% 
#   select(Title_ID, SumUpTable_Created, SumUpTable_Last.Modified) %>%
#   distinct()
# 
# TacticPage_DF <- df %>%
#   filter(PageType == "Tactics")%>% 
#   select(Title_Tactic, SumUpTable_Created, SumUpTable_Last.Modified, Title_Domain) %>%
#   distinct()

GroupsPage_DF <- df %>%
  filter(PageType == "Groups") %>% 
  dplyr::select(Title_Name, SumUpTable_Created, SumUpTable_Last.Modified) %>%
  distinct()

TechPage_DF <- df %>%
  filter(PageType == "Techniques")%>% 
  dplyr::select(Title_ID, SumUpTable_Created, SumUpTable_Last.Modified) %>%
  dplyr::distinct()

SoftPage_DF <- df %>%
  filter(PageType == "Software")%>% 
  dplyr::select(Title_ID, SumUpTable_Created, SumUpTable_Last.Modified) %>%
  dplyr::distinct()

TacticPage_DF <- df %>%
  filter(PageType == "Tactics")%>% 
  dplyr::select(Title_Tactic, SumUpTable_Created, SumUpTable_Last.Modified, Title_Domain) %>%
  dplyr::distinct()


TechNSoft_pageDf <- combined_df <- rbind(TechPage_DF, SoftPage_DF)

##################################################
merged_df <- df_data

##ADDING GROUP CREATION DATE
# Merging dataframes with different column names for ID
merged_df <- merge(merged_df, GroupsPage_DF, by.x = "APTname", by.y = "Title_Name", all.y = TRUE)
# Renaming the last two columns
colnames(merged_df)[c(ncol(merged_df)-1, ncol(merged_df))] <- c("Group_SumUpTable_Created", "Group_SumUpTable_Last.Modified")

##ADDING TechniqueANDsoftware CREATION DATE
# Merging dataframes with different column names for ID
merged_df <- merge(merged_df, TechNSoft_pageDf, by.x = "Techniques_N_Software", by.y = "Title_ID", all.y = TRUE)
# Renaming the last two columns
colnames(merged_df)[c(ncol(merged_df)-1, ncol(merged_df))] <- c("techNsoft_SumUpTable_Created", "techNsoft_SumUpTable_Last.Modified")

##ADDING TacticsByDomain CREATION DATE
# Merging dataframes with different column names for ID
merged_df <- merge(merged_df, TacticPage_DF, by.x = c("Tactic_s", "Domain"), by.y = c("Title_Tactic", "Title_Domain"), all.x = TRUE)
# Renaming the last two columns
colnames(merged_df)[c(ncol(merged_df)-1, ncol(merged_df))] <- c("TacticNdomain_SumUpTable_Created", "TacticNdomain_SumUpTable_Last.Modified")

## Organise
merged_df <- merged_df[c("APTname", "Group_SumUpTable_Created", "Group_SumUpTable_Last.Modified", "Techniques_N_Software", "techNsoft_SumUpTable_Created", "techNsoft_SumUpTable_Last.Modified",
                          "Tactic_s", "Domain", "TacticNdomain_SumUpTable_Created", "TacticNdomain_SumUpTable_Last.Modified")]
##################################################



###################################################
#APTs
Created_ChronologicalOrder <- as.Date(GroupsPage_DF$SumUpTable_Created, format = "%d %B %Y")
LastMod_ChronologicalOrder <- as.Date(GroupsPage_DF$SumUpTable_Last.Modified, format = "%d %B %Y")

# Create new columns for month-year
GroupsPage_DF$Created_MonthYear <- format(Created_ChronologicalOrder, "%B %Y")
GroupsPage_DF$LastMod_MonthYear <- format(LastMod_ChronologicalOrder, "%B %Y")

# Combine all unique month-year values from both variables
all_month_years_APT <- unique(c(GroupsPage_DF$Created_MonthYear, GroupsPage_DF$LastMod_MonthYear))

# Convert month-year strings to Date objects for sorting
# Add "01" as the day to create a valid date (e.g., "01 April 2024")
all_month_years_dates_APT <- as.Date(paste0("01 ", all_month_years_APT), format = "%d %B %Y")

# Sort the month-year values chronologically
all_month_years_sorted_APT <- all_month_years_APT[order(all_month_years_dates_APT)]

# Convert the MonthYear columns to factors with the correct order
GroupsPage_DF$Created_MonthYear <- factor(GroupsPage_DF$Created_MonthYear, levels = all_month_years_sorted_APT)
GroupsPage_DF$LastMod_MonthYear <- factor(GroupsPage_DF$LastMod_MonthYear, levels = all_month_years_sorted_APT)

# Combine the data for plotting
# Create a new data frame for plotting
plot_data_APT <- data.frame(
  MonthYear = c(GroupsPage_DF$Created_MonthYear, GroupsPage_DF$LastMod_MonthYear),
  Title_Name = c(GroupsPage_DF$Title_Name, GroupsPage_DF$Title_Name),
  Type = rep(c("Created", "Last Modified"), each = nrow(GroupsPage_DF))
)

# Ensure MonthYear is a factor with the correct order
plot_data_APT$MonthYear <- factor(plot_data_APT$MonthYear, levels = all_month_years_sorted_APT)

# Plot the data
ggplot(plot_data_APT) +
  geom_point(aes(x = MonthYear, y = factor(Title_Name), colour = Type)) +
  scale_colour_manual(values = c("Created" = "black", "Last Modified" = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(x = "Month-Year", y = "Title Name", colour = "Type")  # Add axis labels and legend title


# Plot the data
ggplot(plot_data_APT) +
  geom_point(aes(x = factor(Title_Name), y = MonthYear, colour = Type)) +
  scale_colour_manual(values = c("Created" = "black", "Last Modified" = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(x = "Title Name", y = "Month-Year", colour = "Type")  # Add axis labels and legend title






###################################################
#Techniques
Created_ChronologicalOrder <- as.Date(TechPage_DF$SumUpTable_Created, format = "%d %B %Y")
LastMod_ChronologicalOrder <- as.Date(TechPage_DF$SumUpTable_Last.Modified, format = "%d %B %Y")

# Create new columns for month-year
TechPage_DF$Created_MonthYear <- format(Created_ChronologicalOrder, "%B %Y")
TechPage_DF$LastMod_MonthYear <- format(LastMod_ChronologicalOrder, "%B %Y")

# Combine all unique month-year values from both variables
all_month_years_tech <- unique(c(TechPage_DF$Created_MonthYear, TechPage_DF$LastMod_MonthYear))

# Convert month-year strings to Date objects for sorting
# Add "01" as the day to create a valid date (e.g., "01 April 2024")
all_month_years_dates_tech <- as.Date(paste0("01 ", all_month_years_tech), format = "%d %B %Y")

# Sort the month-year values chronologically
all_month_years_sorted_tech <- all_month_years_tech[order(all_month_years_dates_tech)]

# Convert the MonthYear columns to factors with the correct order
TechPage_DF$Created_MonthYear <- factor(TechPage_DF$Created_MonthYear, levels = all_month_years_sorted_tech)
TechPage_DF$LastMod_MonthYear <- factor(TechPage_DF$LastMod_MonthYear, levels = all_month_years_sorted_tech)

# Combine the data for plotting
# Create a new data frame for plotting
plot_data_tech <- data.frame(
  MonthYear = c(TechPage_DF$Created_MonthYear, TechPage_DF$LastMod_MonthYear),
  Title_Name = c(TechPage_DF$Title_ID, TechPage_DF$Title_ID),
  Type = rep(c("Created", "Last Modified"), each = nrow(TechPage_DF))
)

# Ensure MonthYear is a factor with the correct order
plot_data_tech$MonthYear <- factor(plot_data_tech$MonthYear, levels = all_month_years_sorted_tech)

# Plot the data
ggplot(plot_data_tech) +
  geom_point(aes(x = MonthYear, y = factor(Title_Name), colour = Type)) +
  scale_colour_manual(values = c("Created" = "black", "Last Modified" = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(x = "Month-Year", y = "Title Name", colour = "Type")  # Add axis labels and legend title


# Plot the data
ggplot(plot_data_tech) +
  geom_point(aes(x = factor(Title_Name), y = MonthYear, colour = Type)) +
  scale_colour_manual(values = c("Created" = "black", "Last Modified" = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(x = "Title Name", y = "Month-Year", colour = "Type")  # Add axis labels and legend title




###################################################
#Softwares
Created_ChronologicalOrder <- as.Date(SoftPage_DF$SumUpTable_Created, format = "%d %B %Y")
LastMod_ChronologicalOrder <- as.Date(SoftPage_DF$SumUpTable_Last.Modified, format = "%d %B %Y")


# Create new columns for month-year
SoftPage_DF$Created_MonthYear <- format(Created_ChronologicalOrder, "%B %Y")
SoftPage_DF$LastMod_MonthYear <- format(LastMod_ChronologicalOrder, "%B %Y")

# Combine all unique month-year values from both variables
all_month_years_Soft <- unique(c(SoftPage_DF$Created_MonthYear, SoftPage_DF$LastMod_MonthYear))

# Convert month-year strings to Date objects for sorting
# Add "01" as the day to create a valid date (e.g., "01 April 2024")
all_month_years_dates_soft <- as.Date(paste0("01 ", all_month_years_Soft), format = "%d %B %Y")

# Sort the month-year values chronologically
all_month_years_sorted_soft <- all_month_years_Soft[order(all_month_years_dates_soft)]

# Convert the MonthYear columns to factors with the correct order
SoftPage_DF$Created_MonthYear <- factor(SoftPage_DF$Created_MonthYear, levels = all_month_years_sorted_soft)
SoftPage_DF$LastMod_MonthYear <- factor(SoftPage_DF$LastMod_MonthYear, levels = all_month_years_sorted_soft)

# Combine the data for plotting
# Create a new data frame for plotting
plot_data_soft <- data.frame(
  MonthYear = c(SoftPage_DF$Created_MonthYear, SoftPage_DF$LastMod_MonthYear),
  Title_Name = c(SoftPage_DF$Title_ID, SoftPage_DF$Title_ID),
  Type = rep(c("Created", "Last Modified"), each = nrow(SoftPage_DF))
)

# Ensure MonthYear is a factor with the correct order
plot_data_soft$MonthYear <- factor(plot_data_soft$MonthYear, levels = all_month_years_sorted_soft)

# Plot the data
ggplot(plot_data_soft) +
  geom_point(aes(x = MonthYear, y = factor(Title_Name), colour = Type)) +
  scale_colour_manual(values = c("Created" = "black", "Last Modified" = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(x = "Month-Year", y = "Title Name", colour = "Type")  # Add axis labels and legend title


# Plot the data
ggplot(plot_data_soft) +
  geom_point(aes(x = factor(Title_Name), y = MonthYear, colour = Type)) +
  scale_colour_manual(values = c("Created" = "black", "Last Modified" = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(x = "Title Name", y = "Month-Year", colour = "Type")  # Add axis labels and legend title



###################################################
#Tactics
TacticInOrder <- unique(TacticPage_DF$Title_Tactic)
TacticsDates <- TacticPage_DF[grepl("Enterprise", TacticPage_DF$Title_Domain),]
Created_ChronologicalOrder <- as.Date(TacticsDates$SumUpTable_Created, format = "%d %B %Y")
LastMod_ChronologicalOrder <- as.Date(TacticsDates$SumUpTable_Last.Modified, format = "%d %B %Y")

# Create new columns for month-year
TacticsDates$Created_MonthYear <- format(Created_ChronologicalOrder, "%B %Y")
TacticsDates$LastMod_MonthYear <- format(LastMod_ChronologicalOrder, "%B %Y")

# Combine all unique month-year values from both variables
all_month_years_tactic <- unique(c(TacticsDates$Created_MonthYear, TacticsDates$LastMod_MonthYear))

# Convert month-year strings to Date objects for sorting
# Add "01" as the day to create a valid date (e.g., "01 April 2024")
all_month_years_dates_Tactic <- as.Date(paste0("01 ", all_month_years_tactic), format = "%d %B %Y")

# Sort the month-year values chronologically
all_month_years_sorted_Tactic <- all_month_years_tactic[order(all_month_years_dates_Tactic)]

# Convert the MonthYear columns to factors with the correct order
TacticsDates$Created_MonthYear <- factor(TacticsDates$Created_MonthYear, levels = all_month_years_sorted_Tactic)
TacticsDates$LastMod_MonthYear <- factor(TacticsDates$LastMod_MonthYear, levels = all_month_years_sorted_Tactic)

# Combine the data for plotting
# Create a new data frame for plotting
plot_data_Tactic <- data.frame(
  MonthYear = c(TacticsDates$Created_MonthYear, TacticsDates$LastMod_MonthYear),
  Title_Name = c(TacticsDates$Title_Tactic, TacticsDates$Title_Tactic),
  Type = rep(c("Created", "Last Modified"), each = nrow(TacticsDates))
)

# Ensure MonthYear is a factor with the correct order
plot_data_Tactic$MonthYear <- factor(plot_data_Tactic$MonthYear, levels = all_month_years_sorted_Tactic)

# Plot the data
ggplot(plot_data_Tactic) +
  geom_point(aes(x = MonthYear, y = factor(Title_Name), colour = Type)) +
  scale_colour_manual(values = c("Created" = "black", "Last Modified" = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(x = "Month-Year", y = "Title Name", colour = "Type")  # Add axis labels and legend title


# Plot the data
ggplot(plot_data_Tactic) +
  geom_point(aes(x = factor(Title_Name), y = MonthYear, colour = Type)) +
  scale_colour_manual(values = c("Created" = "black", "Last Modified" = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(x = "Title Name", y = "Month-Year", colour = "Type")  # Add axis labels and legend title

#si qu epoint rouge, cree et modifier dans le meme mois


###############################################################################
# Techniques Dates filtered by APT Tactics or Domain
###############################################################################


SelectedAPT <- c("Sandworm Team")
SelectedTactic <- c()
SelectedDomain <- c("Mobile")

filtered_merged_df <- merged_df

if(length(SelectedAPT) != 0){
  filtered_merged_df <- filtered_merged_df %>% filter(APTname %in% SelectedAPT)
}

print(length(SelectedTactic))
if(length(SelectedTactic) != 0){
  filtered_merged_df <- filtered_merged_df %>% filter(Tactic_s %in% SelectedTactic)
}

if(length(SelectedDomain) != 0){
  filtered_merged_df <- filtered_merged_df %>% filter(Domain %in% SelectedDomain)
}





#Techniques
Created_ChronologicalOrder <- as.Date(filtered_merged_df$techNsoft_SumUpTable_Created, format = "%d %B %Y")
LastMod_ChronologicalOrder <- as.Date(filtered_merged_df$techNsoft_SumUpTable_Last.Modified, format = "%d %B %Y")


# Create new columns for month-year
filtered_merged_df$Created_MonthYear <- format(Created_ChronologicalOrder, "%B %Y")
filtered_merged_df$LastMod_MonthYear <- format(LastMod_ChronologicalOrder, "%B %Y")

# Combine all unique month-year values from both variables
all_month_years_filtered_tech <- unique(c(filtered_merged_df$Created_MonthYear, filtered_merged_df$LastMod_MonthYear))

# Convert month-year strings to Date objects for sorting
# Add "01" as the day to create a valid date (e.g., "01 April 2024")
all_month_years_dates_filtered_tech <- as.Date(paste0("01 ", all_month_years_filtered_tech), format = "%d %B %Y")

# Sort the month-year values chronologically
all_month_years_sorted_filtered_tech <- all_month_years_filtered_tech[order(all_month_years_dates_filtered_tech)]

# Convert the MonthYear columns to factors with the correct order
filtered_merged_df$Created_MonthYear <- factor(filtered_merged_df$Created_MonthYear, levels = all_month_years_sorted_filtered_tech)
filtered_merged_df$LastMod_MonthYear <- factor(filtered_merged_df$LastMod_MonthYear, levels = all_month_years_sorted_filtered_tech)

# Combine the data for plotting
# Create a new data frame for plotting
plot_data_filtered_tech <- data.frame(
  MonthYear = c(filtered_merged_df$Created_MonthYear, filtered_merged_df$LastMod_MonthYear),
  Title_Name = c(filtered_merged_df$Techniques_N_Software, filtered_merged_df$Techniques_N_Software),
  Type = rep(c("Created", "Last Modified"), each = nrow(filtered_merged_df))
)

# Ensure MonthYear is a factor with the correct order
plot_data_filtered_tech$MonthYear <- factor(plot_data_filtered_tech$MonthYear, levels = all_month_years_sorted_filtered_tech)

# Plot the data
p <- ggplot(plot_data_filtered_tech) +
  geom_point(aes(x = MonthYear, y = factor(Title_Name), colour = Type)) +
  scale_colour_manual(values = c("Created" = "black", "Last Modified" = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(x = "Month-Year", y = "Title Name", colour = "Type")  # Add axis labels and legend title

print(p)

# Plot the data
p <- ggplot(plot_data_filtered_tech) +
  geom_point(aes(x = factor(Title_Name), y = MonthYear, colour = Type)) +
  scale_colour_manual(values = c("Created" = "black", "Last Modified" = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(x = "Title Name", y = "Month-Year", colour = "Type")  # Add axis labels and legend title

print(p)





###############################################################################
# Software Dates filtered by APT 
###############################################################################


SelectedAPT <- c("Sandworm Team")


filtered_merged_df <- merged_df

if(length(SelectedAPT) != 0){
  filtered_merged_df <- filtered_merged_df %>% filter(APTname %in% SelectedAPT)
}


filtered_merged_df <- filtered_merged_df %>% filter(Domain %in% NA)




#Techniques
Created_ChronologicalOrder <- as.Date(filtered_merged_df$techNsoft_SumUpTable_Created, format = "%d %B %Y")
LastMod_ChronologicalOrder <- as.Date(filtered_merged_df$techNsoft_SumUpTable_Last.Modified, format = "%d %B %Y")


# Create new columns for month-year
filtered_merged_df$Created_MonthYear <- format(Created_ChronologicalOrder, "%B %Y")
filtered_merged_df$LastMod_MonthYear <- format(LastMod_ChronologicalOrder, "%B %Y")

# Combine all unique month-year values from both variables
all_month_years_filtered_tech <- unique(c(filtered_merged_df$Created_MonthYear, filtered_merged_df$LastMod_MonthYear))

# Convert month-year strings to Date objects for sorting
# Add "01" as the day to create a valid date (e.g., "01 April 2024")
all_month_years_dates_filtered_tech <- as.Date(paste0("01 ", all_month_years_filtered_tech), format = "%d %B %Y")

# Sort the month-year values chronologically
all_month_years_sorted_filtered_tech <- all_month_years_filtered_tech[order(all_month_years_dates_filtered_tech)]

# Convert the MonthYear columns to factors with the correct order
filtered_merged_df$Created_MonthYear <- factor(filtered_merged_df$Created_MonthYear, levels = all_month_years_sorted_filtered_tech)
filtered_merged_df$LastMod_MonthYear <- factor(filtered_merged_df$LastMod_MonthYear, levels = all_month_years_sorted_filtered_tech)

# Combine the data for plotting
# Create a new data frame for plotting
plot_data_filtered_tech <- data.frame(
  MonthYear = c(filtered_merged_df$Created_MonthYear, filtered_merged_df$LastMod_MonthYear),
  Title_Name = c(filtered_merged_df$Techniques_N_Software, filtered_merged_df$Techniques_N_Software),
  Type = rep(c("Created", "Last Modified"), each = nrow(filtered_merged_df))
)

# Ensure MonthYear is a factor with the correct order
plot_data_filtered_tech$MonthYear <- factor(plot_data_filtered_tech$MonthYear, levels = all_month_years_sorted_filtered_tech)

# Plot the data
p <- ggplot(plot_data_filtered_tech) +
  geom_point(aes(x = MonthYear, y = factor(Title_Name), colour = Type)) +
  scale_colour_manual(values = c("Created" = "black", "Last Modified" = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(x = "Month-Year", y = "Title Name", colour = "Type")  # Add axis labels and legend title

print(p)

# Plot the data
p <- ggplot(plot_data_filtered_tech) +
  geom_point(aes(x = factor(Title_Name), y = MonthYear, colour = Type)) +
  scale_colour_manual(values = c("Created" = "black", "Last Modified" = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(x = "Title Name", y = "Month-Year", colour = "Type")  # Add axis labels and legend title

print(p)















#zooming for given p


print(p)


# Build the ggplot object to extract data
p_build <- ggplot_build(p)

# Extract the limits for the x and y axes
x_limits <- p_build$data[[1]]$x
y_limits <- p_build$data[[1]]$y

# Print the results
cat("X-axis limits: ", min(x_limits), "to", max(x_limits), "\n")
cat("Y-axis limits: ", min(y_limits), "to", max(y_limits), "\n")

p_zoomed <- p + 
  coord_cartesian(xlim = c(300, 350), ylim = c(1100, 1200)) +  # Set visible area
  ggtitle("Zoomed In Plot with coord_cartesian")

print(p_zoomed)
