#install.packages("gridExtra")
library(gridExtra)

data <- read.csv("./BaseData/TestFinal_df.csv")
data2 <- read.csv("./BaseData/TestFinal_df.csv")



# Get unique combinations for each feature
techniques_unique <- unique(data[c("APTname", "techniques_str")])
software_unique <- unique(data[c("APTname", "software_str")])
cve_unique <- unique(data[c("APTname", "CVE_ID")])

# Remove empty strings
techniques_unique <- techniques_unique[techniques_unique$techniques_str != "", ]
software_unique <- software_unique[software_unique$software_str != "", ]
cve_unique <- cve_unique[cve_unique$CVE_ID != "", ]

# Count frequencies for each APTname
tech_counts <- as.data.frame(table(techniques_unique$APTname))
software_counts <- as.data.frame(table(software_unique$APTname))
cve_counts <- as.data.frame(table(cve_unique$APTname))

# Rename columns
colnames(tech_counts) <- c("APTname", "Count")
colnames(software_counts) <- c("APTname", "Count")
colnames(cve_counts) <- c("APTname", "Count")

# Get overall feature frequencies (not per APT)
techniques_freq <- as.data.frame(table(techniques_unique$techniques_str))
colnames(techniques_freq) <- c("feature", "count")
techniques_freq <- techniques_freq[order(-techniques_freq$count), ]

software_freq <- as.data.frame(table(software_unique$software_str))
colnames(software_freq) <- c("feature", "count")
software_freq <- software_freq[order(-software_freq$count), ]

cve_freq <- as.data.frame(table(cve_unique$CVE_ID))
colnames(cve_freq) <- c("feature", "count")
cve_freq <- cve_freq[order(-cve_freq$count), ]

# Create distribution plots (top 20 features for readability)
plot1 <- ggplot(head(techniques_freq, 20), aes(x = reorder(feature, count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Distribution of Techniques (Overall)", x = "Technique", y = "Count") +
  coord_flip() +
  theme_minimal()

plot2 <- ggplot(head(software_freq, 20), aes(x = reorder(feature, count), y = count)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  labs(title = "Distribution of Software (Overall)", x = "Software", y = "Count") +
  coord_flip() +
  theme_minimal()

plot3 <- ggplot(head(cve_freq, 20), aes(x = reorder(feature, count), y = count)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(title = "Distribution of CVE IDs (Overall)", x = "CVE ID", y = "Count") +
  coord_flip() +
  theme_minimal()

# Arrange all plots on one page
# Create the top 20 version (already have this)
# Create the top 20 version (keep as is with coord_flip)
# Create the top 20 version (already correct with coord_flip)
# Create the top 20 version
top20_page <- grid.arrange(plot1, plot2, plot3, ncol = 1)

# Create individual complete graphs for techniques and software
complete_plot1 <- ggplot(techniques_freq, aes(x = reorder(feature, -count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Complete Distribution of Techniques", x = "Technique", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

complete_plot2 <- ggplot(software_freq, aes(x = reorder(feature, -count), y = count)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  labs(title = "Complete Distribution of Software", x = "Software", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

# Filter CVE data to show only counts >= 10
filteringValue <- 50
cve_filtered <- cve_freq[cve_freq$count >= filteringValue, ]

complete_plot3_filtered <- ggplot(cve_filtered, aes(x = reorder(feature, -count), y = count)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(title = "Distribution of CVE IDs (Count >= 10)", x = "CVE ID", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

# Display all plots
print(top20_page)
print(complete_plot1)
print(complete_plot2)
print(complete_plot3_filtered)

# Show how many CVEs were filtered out
cat("Total unique CVEs:", nrow(cve_freq), "\n")
cat("CVEs with count >= ", filteringValue, ":", nrow(cve_filtered), "\n")
cat("CVEs filtered out (count <", filteringValue, "):", nrow(cve_freq) - nrow(cve_filtered), "\n")





# Use the same frequency tables you already have: techniques_freq, software_freq, cve_freq

plot1_vert <- ggplot(head(techniques_freq, 20), aes(x = reorder(feature, count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Techniques", x = "", y = "Count") +   # shorter titles for side‑by‑side
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

plot2_vert <- ggplot(head(software_freq, 20), aes(x = reorder(feature, count), y = count)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  labs(title = "Software", x = "", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

plot3_vert <- ggplot(head(cve_freq, 20), aes(x = reorder(feature, count), y = count)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(title = "CVE IDs", x = "", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

# Place them left‑to‑right
grid.arrange(plot1_vert, plot2_vert, plot3_vert, ncol = 3)













#################################################################################
#extra features

# Get unique combinations for each feature
CAPEC_unique <- unique(data[c("APTname", "CAPEC_ID")])
CWE_unique <- unique(data[c("APTname", "CWE_ID")])
Pulse_unique <- unique(data[c("APTname", "Pulse_ID")])

# Remove empty strings
CAPEC_unique <- CAPEC_unique[CAPEC_unique$CAPEC_ID != "", ]
CWE_unique <- CWE_unique[CWE_unique$CWE_ID != "", ]
Pulse_unique <- Pulse_unique[Pulse_unique$Pulse_ID != "", ]

# Count frequencies for each APTname
CAPEC_counts <- as.data.frame(table(CAPEC_unique$APTname))
CWE_counts <- as.data.frame(table(CWE_unique$APTname))
Pulse_counts <- as.data.frame(table(Pulse_unique$APTname))

# Rename columns
colnames(CAPEC_counts) <- c("APTname", "Count")
colnames(CWE_counts) <- c("APTname", "Count")
colnames(Pulse_counts) <- c("APTname", "Count")

# Get overall feature frequencies (not per APT)
CAPEC_freq <- as.data.frame(table(CAPEC_unique$CAPEC_ID))
colnames(CAPEC_freq) <- c("feature", "count")
CAPEC_freq <- CAPEC_freq[order(-CAPEC_freq$count), ]

CWE_freq <- as.data.frame(table(CWE_unique$CWE_ID))
colnames(CWE_freq) <- c("feature", "count")
CWE_freq <- CWE_freq[order(-CWE_freq$count), ]

Pulse_freq <- as.data.frame(table(Pulse_unique$Pulse_ID))
colnames(Pulse_freq) <- c("feature", "count")
Pulse_freq <- Pulse_freq[order(-Pulse_freq$count), ]

# Create distribution plots (top 20 features for readability)
plot1 <- ggplot(head(CAPEC_freq, 20), aes(x = reorder(feature, count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Distribution of Capec (Overall)", x = "Capec", y = "Count") +
  coord_flip() +
  theme_minimal()

plot2 <- ggplot(head(CWE_freq, 20), aes(x = reorder(feature, count), y = count)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  labs(title = "Distribution of CWE (Overall)", x = "CWE", y = "Count") +
  coord_flip() +
  theme_minimal()

plot3 <- ggplot(head(Pulse_freq, 20), aes(x = reorder(feature, count), y = count)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(title = "Distribution of Pulse IDs (Overall)", x = "Pulse ID", y = "Count") +
  coord_flip() +
  theme_minimal()

# Arrange all plots on one page
# Create the top 20 version (already have this)
# Create the top 20 version (keep as is with coord_flip)
# Create the top 20 version (already correct with coord_flip)
# Create the top 20 version
top20_page <- grid.arrange(plot1, plot2, plot3, ncol = 1)

# Create individual complete graphs for techniques and software
complete_plot1 <- ggplot(techniques_freq, aes(x = reorder(feature, -count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Complete Distribution of Capec", x = "Capec", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

complete_plot2 <- ggplot(software_freq, aes(x = reorder(feature, -count), y = count)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  labs(title = "Complete Distribution of CWE", x = "CWE", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

# Filter CVE data to show only counts >= 10
filteringValue <- 50
cve_filtered <- cve_freq[cve_freq$count >= filteringValue, ]

complete_plot3_filtered <- ggplot(cve_filtered, aes(x = reorder(feature, -count), y = count)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(title = "Distribution of Pulse IDs (Count >= 10)", x = "Pulse ID", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

# Display all plots
print(top20_page)
print(complete_plot1)
print(complete_plot2)
print(complete_plot3_filtered)

# Show how many CVEs were filtered out
cat("Total unique Pulse id:", nrow(cve_freq), "\n")
cat("Pulse with count >= ", filteringValue, ":", nrow(cve_filtered), "\n")
cat("Pulse filtered out (count <", filteringValue, "):", nrow(cve_freq) - nrow(cve_filtered), "\n")






# Make sure you are using CAPEC_freq, CWE_freq, Pulse_freq (already created)

p_capec_vert <- ggplot(head(CAPEC_freq, 20), aes(x = reorder(feature, count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "CAPEC", x = "", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

p_cwe_vert <- ggplot(head(CWE_freq, 20), aes(x = reorder(feature, count), y = count)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  labs(title = "CWE", x = "", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

p_pulse_vert <- ggplot(head(Pulse_freq, 20), aes(x = reorder(feature, count), y = count)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(title = "Pulse IDs", x = "", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

# Horizontal arrangement (left to right)
grid.arrange(p_capec_vert, p_cwe_vert, p_pulse_vert, ncol = 3)


################################################################################




library(ggplot2)
library(dplyr)

# Add a Type column to each frequency data frame (you already have them)
techniques_freq$Type <- "Techniques"
software_freq$Type   <- "Software"
CAPEC_freq$Type      <- "CAPEC"
CWE_freq$Type        <- "CWE"
cve_freq$Type        <- "CVE"
Pulse_freq$Type      <- "Pulse"

# Combine all into one table
all_freq <- bind_rows(techniques_freq, software_freq, CAPEC_freq,
                      CWE_freq, cve_freq, Pulse_freq)

# Keep top 20 per Type
top20_all <- all_freq %>%
  group_by(Type) %>%
  slice_max(order_by = count, n = 20) %>%
  ungroup()

# Enforce the categories order (left to right)
top20_all$Type <- factor(top20_all$Type,
                         levels = c("Techniques", "Software", "CAPEC",
                                    "CWE", "CVE", "Pulse"))

ggplot(top20_all, aes(x = reorder(feature, -count), y = count, fill = Type)) +   # map fill to Type
  geom_col() +
  facet_wrap(~ Type, nrow = 1, scales = "free_x") +
  scale_fill_manual(values = c(
    "Techniques" = "steelblue",
    "Software"   = "darkorange",
    "CAPEC"      = "firebrick",
    "CWE"        = "darkgreen",
    "CVE"        = "purple",
    "Pulse"      = "goldenrod"
  )) +
  labs(title = "Top 20 Features per Category (ordered largest to smallest)", 
       x = "", y = "Count") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
    strip.text = element_text(face = "bold", size = 9),
    panel.spacing = unit(1, "lines")
  ) +
  guides(fill = "none")   # remove legend (optional)

