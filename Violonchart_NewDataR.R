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

violinchat_comprehensive <- function(..., evidence = NULL, data = FinalDataFrame) {
  
  # Capture all APT names provided
  apt_names <- c(...)
  if (length(apt_names) == 0) {
    stop("At least one APT name must be provided.")
  }
  
  # Helper function to prepare data
  prepare_data <- function(var_name, input_data) {
    input_data %>%
      dplyr::filter(!is.na(!!sym(var_name))) %>%
      dplyr::group_by(APTname, indicator = !!sym(var_name)) %>%
      dplyr::summarise(count = n(), .groups = "drop") %>%
      dplyr::group_by(APTname) %>%
      dplyr::mutate(
        total = sum(count),
        proportion = count / total,
        indicator_type = case_when(
          var_name == "techniques_str" ~ "MITRE Techniques",
          var_name == "software_str"    ~ "Software",
          var_name == "CAPEC_ID"        ~ "CAPEC",
          var_name == "CWE_ID"          ~ "CWE",
          var_name == "CVE_ID"          ~ "CVE",
          var_name == "Pulse_ID"        ~ "Pulse"
        )
      ) %>%
      dplyr::ungroup()
  }
  
  # Prepare APT data (all requested groups)
  apt_data <- data %>%
    dplyr::filter(APTname %in% apt_names)
  
  # Prepare custom evidence data if provided
  if (!is.null(evidence)) {
    custom_rows <- list()
    
    for (type in names(evidence)) {
      if (length(evidence[[type]]) > 0) {
        custom_rows[[type]] <- tibble(
          APTname = "Custom Evidence",
          !!sym(type) := evidence[[type]]
        )
      }
    }
    
    custom_data <- bind_rows(custom_rows) %>%
      mutate(APTname = "Custom Evidence")
    
    combined_raw <- bind_rows(apt_data, custom_data)
  } else {
    combined_raw <- apt_data
  }
  
  # Process all indicator types
  indicator_types <- c("techniques_str", "software_str", "CAPEC_ID", 
                       "CWE_ID", "CVE_ID", "Pulse_ID")
  combined_data <- bind_rows(lapply(indicator_types, function(x) prepare_data(x, combined_raw))) %>%
    mutate(
      indicator_type = factor(
        indicator_type,
        levels = c("MITRE Techniques", "Software", "CAPEC", "CWE", "CVE", "Pulse"),
        ordered = TRUE
      )
    )
  
  # Fix order of APTname groups: user-specified APTs first, then Custom Evidence
  desired_order <- apt_names
  if (!is.null(evidence)) desired_order <- c(desired_order, "Custom Evidence")
  combined_data$APTname <- factor(combined_data$APTname, levels = desired_order)
  
  # Build title
  if (is.null(evidence)) {
    plot_title <- paste("APT Comparison:", paste(apt_names, collapse = ", "))
  } else {
    plot_title <- paste("APT vs Custom Evidence Comparison:", paste(apt_names, collapse = ", "))
  }
  
  # Create plot
  p <- ggplot(combined_data, aes(x = APTname, y = proportion, fill = APTname)) +
    geom_violin(trim = FALSE, alpha = 0.7) +
    geom_boxplot(width = 0.2, fill = "white", outlier.shape = NA) +
    facet_wrap(~indicator_type, scales = "free_y", nrow = 1) +
    scale_fill_brewer(palette = "Set1") +
    labs(
      title = plot_title,
      subtitle = "Distribution of different indicator types",
      x = "",
      y = "Proportion of indicators used",
      fill = "Group"
    ) +
    theme_minimal() +
    theme(
      legend.position = "top",
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(face = "bold"),
      panel.spacing = unit(1, "lines")
    )
  
  print(p)
  return(invisible(combined_data))
}

violinchat_comprehensive("APT29", "Lazarus Group")

violinchat_comprehensive("APT29", "Lazarus Group", "PROMETHIUM", "Sandworm Team", "Windshift") 



evidence <- list(
  techniques_str = c("T1059", "T1071"),
  software_str = c("Metasploit"),
  CVE_ID = c("CVE-2021-44228", "CVE-2021-45046"),
  Pulse_ID = c("676652f78bbff8f3a7dcb4c3")
)

violinchat_comprehensive("APT29", evidence = evidence)
