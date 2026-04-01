
#'---
#' title: "Macrophyte trends in the bays of Lake Minnetonka"
#' author: "Mike Verhoeven, ..."
#' output: 
#'    html_document:
#'       toc: true
#'       theme: default
#'       toc_depth: 3
#'       toc_float:
#'           collapsed: false
#'---

#' 1. pull in plant observation data from PI surveys, clean, and compile those.
#' 2. calculate key metrics from those
#' 3. evaluate the change in those metrics over time
#' 
#' Notes: 
#' I use two common packages and they have slightly different syntax: data.table and dplyr
#' I use google gemini and chat GPT to build codeblcks quite often. These always require carful review, but they can really puch out code quickly!
#' 


# Preface -----------------------------------------------------------------


#' # Document Preamble
knitr::opts_chunk$set(warning = FALSE, message = FALSE) 

strttime <- Sys.time()
getwd()


# ..load libraries ------------------------------------------------------------------

#' ## Libraries

library(data.table) 
# update_dev_pkg()# remotes::install_github("Rdatatable/data.table")
library(ggplot2)
library(sf)
library(vegan)
library(gridExtra)
library(dplyr)
library(tidyr)
library(janitor)
library(knitr)
library(readxl)
library(dplyr)
library(purrr)
library(fs)
library(stringr)
library(lubridate)
library(scales)
library(binom)
library(vegan)


# ..load in functions -------------------------------------------------------
#' ## Functions

f_dowle3natozeros = function(DT, x) {
  # or by number (slightly faster than by name) :
  for (j in x)
    set(DT,which(is.na(DT[[j]])),j,"0")
}


# Load Data -------------------------------------------------

#' ## Data
#' 
#' Garrett has all the data organized in the Region's folder:
#' 
#' "U:\EWR\INDIVIDUAL USER FIILES\R3S AIS Spec\lake plant management\Lake Info_Region 3 South\Minnetonka_Hennepin\Data Analysis\Bay Specific"
#' 
#' 

# Define the root directory
root_path <- "U:/EWR/INDIVIDUAL USER FIILES/R3S AIS Spec/lake plant management/Lake Info_Region 3 South/Minnetonka_Hennepin/Data Analysis/Bay Specific"

# 1. Get a list of all Excel files in the subfolders
# 'recurse = TRUE' handles the folder tree structure
# Use 'regexp' to find files that end specifically with your naming convention
excel_files <- dir_ls(root_path, 
                      recurse = TRUE, 
                      regexp = "_PIData_Combined\\.xlsx$")

# Check how many files were found before running the heavy lifting
print(paste("Found", length(excel_files), "Bay files."))


# ..audit column names ------------------------------------------------------


#' 1 - updated file names to fit format (e.g., broken one =  Carson_St.Louis_PISurveyData_Combined .xlsx)
#' 2 - updated some tab names(e.g., broken one = 6-7and10-2017)
#' 3 - duplicated column names: 
#' [1] "Found duplicate columns in the following locations:"
#' # A tibble: 4 × 3
#' bay                                  survey_date duplicate_column       
#' <chr>                                <chr>       <chr>                  
#' 1 Carson_St.Louis_PIData_Combined.xlsx 8-12-2013   lythrum_salicaria      
#' 2 Grays_PIData_Combined.xlsx           8-9-2022    potamogeton_amplifolius (checked source-- 2nd one should have been P nodosus)
#' 3 Northarm_PIData_Combined.xlsx        8-13-2025   lemna_trisulca         (checked source-- 1st one should have been L salicaria)
#' 4 Northarm_PIData_Combined.xlsx        8-12-2024   spirodela_polyrrhiza (checked source-- 2nd one should have been P nodosus)
#' #' 
#' 
#' 4 - Work through column names checking on the taxonomy and code switchin' to names. 
#' Some fixed in the files, others fixed in R using a renamer


#Function to find duplicates in a single file
audit_bay_columns <- function(file_path) {
  all_sheets <- excel_sheets(file_path)
  survey_sheets <- str_subset(all_sheets, "^\\d{1,2}-\\d{1,2}-\\d{4}$")
  
  map_df(survey_sheets, ~{
    # Read just the header row (faster)
    header <- read_excel(file_path, sheet = .x, n_max = 0, .name_repair = "minimal")
    cols <- names(header)
    
    # Identify which names appear more than once
    dupes <- cols[duplicated(cols)]
    
    if(length(dupes) > 0) {
      tibble(
        bay = basename(file_path),
        survey_date = .x,
        duplicate_column = dupes
      )
    } else {
      NULL # Return nothing if the sheet is clean
    }
  })
}

#Run the audit across all your files
column_audit <- map_df(excel_files, audit_bay_columns)

#View the results
if(nrow(column_audit) > 0) {
  print("Found duplicate columns in the following locations:")
  print(column_audit)
} else {
  print("All clear! No duplicate columns found.")
}



# ..pull in data ------------------------------------------------------------


#Function to read all tabs from a single file and stack them
read_bay_data <- function(file_path) {
  
  # 1. Get ALL sheet names
  all_sheets <- excel_sheets(file_path)
  
  # 2. FILTER: Only keep sheets that match the m-d-YYYY pattern
  # Regex Breakdown: 1-2 digits, hyphen, 1-2 digits, hyphen, 4 digits
  survey_sheets <- str_subset(all_sheets, "^\\d{1,2}-\\d{1,2}-\\d{4}$")
  
  # Optional: Print a warning if sheets were skipped
  skipped <- setdiff(all_sheets, survey_sheets)
  if(length(skipped) > 0) {
    message(paste0("Skipping non-survey tabs in ", basename(file_path), ": ", paste(skipped, collapse = ", ")))
  }
  
  bay_label <- str_remove(basename(file_path), "_PIData_Combined\\.xlsx$")
  
  # 3. Map only over the VALID survey sheets
  map_df(survey_sheets, ~{
    read_excel(file_path, sheet = .x) %>%
      mutate(
        survey_date = mdy(.x), 
        bay_name = bay_label
      ) %>%
      mutate(across(everything(), as.character)) 
  })
}
# 3. Execute the process
# This creates a list where each element is a Bay's full dataset
bay_data_list <- map(excel_files, read_bay_data)

# Optional: If you want one giant dataset for all bays combined:
# all_bays_combined <- bind_rows(bay_data_list)

# Optional: To keep them as separate objects in your environment named by Bay:
names(bay_data_list) <- str_remove(basename(excel_files), "_PIData_Combined.xlsx")
list2env(bay_data_list, envir = .GlobalEnv)



# Create a tall data frame of all columns and which bay they belong to
column_summary <- map_df(bay_data_list, ~tibble(col_name = colnames(.x)), .id = "bay")

# Count how many times each column name appears across all bays
column_counts <- column_summary %>%
  count(col_name) %>%
  arrange(desc(n))

print(column_counts)


column_matrix <- column_summary %>%
  mutate(exists = "Yes") %>%
  pivot_wider(names_from = bay, values_from = exists)

# View it in a pop-up window
View(column_matrix)


# ...renaming columns --------------------------------------------------------

# 1. Your updated name map
rename_map <- c(
  "surveyor"                   = "Surveyors",
  "surveyor_1"                   = "Surveyor",
  "latitude_1"                   = "Latitude",
  "latitude_2"                   = "Lat",
  "longitude_1"                  = "Longitude",
  "longitude_2"                  = "Long",
  "depth_ft"                      = "DepthFt",
  "depth"                      = "Depth",
  "date"                       = "Date",
  "station_number"             = "sta_nbr",
  "station_number_1"             = "Point"
)

# 2. Columns to discard
cols_to_drop <- c("alltaxa", "whole_rake_density", "multipartsurvey", "plant_height")

# 3. Execute the cleaning on the list
bay_data_list <- map(bay_data_list, function(df) {
  df %>%
    # Rename typos/variations first
    rename(any_of(rename_map)) %>%
    # Convert all to lowercase to catch "Depth" vs "depth"
    rename_with(tolower) %>%
    # Remove the unwanted columns
    select(-any_of(cols_to_drop)) %>%
    # Remove those readExcel auto-repair columns (e.g., ...11)
    select(-matches("^\\.\\.\\.\\d+$"))
})


# ..reformat ----------------------------------

# 4. Combine into one master dataframe
minnetonka_full_data <- bind_rows(bay_data_list)

names(minnetonka_full_data)

setDT(minnetonka_full_data)

# 2. Define the core metadata pattern
# This catches variations like surveyor_1, latitude_2, station_number, etc.
meta_pattern <- "^(lake|bay|date|survey_date|surveyor|station|stat|depth|lat|long|point)"

# 3. Get all current names
all_cols <- names(minnetonka_full_data)

# 4. Split and Sort
# Grep identifies the metadata; everything else is a plant
meta_cols <- all_cols[grepl(meta_pattern, all_cols, ignore.case = TRUE)]
plant_cols <- all_cols[!grepl(meta_pattern, all_cols, ignore.case = TRUE)]

# Define a logical "Lead" order for the most important columns
lead_cols <- c("lake", "bay_name", "date", "survey_date", "station_number", "surveyor")

# Find metadata that didn't make the "Lead" list (like lat/long) and sort them
meta_remainder <- sort(setdiff(meta_cols, lead_cols))

# 5. Create the final ordered vector
final_order <- c(
  intersect(lead_cols, all_cols), # Keep only lead cols that actually exist
  meta_remainder,                # Other metadata (alphabetical)
  sort(plant_cols)               # All plants (alphabetical)
)

# 6. Reorder the data.table IN-PLACE (no assignment needed)
setcolorder(minnetonka_full_data, final_order)


# 7. fillmatrix with zeros

f_dowle3natozeros(minnetonka_full_data, plant_cols)


# fwrite(minnetonka_full_data, file = "scripts&data/data/output/minnetonka_combined_data.csv")



# Carmans -----------------------------------------------------------------

#view data 
minnetonka_full_data[bay_name == "Carmans"]

#snip out just the piece of interest
carmans <- minnetonka_full_data[bay_name == "Carmans"]

#survey dates & number of sampled points & surveyor
carmans[ , .("n_points" = .N, surveyors = unique(surveyor_1)) , survey_date ]


#sampling depths per survey
  #first QC and fix depths
  carmans[ , .N , depth ]  #these inlcude ft for sure. any in m?
  
  carmans[  , depth_ft := as.numeric(depth)  , ]
  
  #these look okay?
  
  carmans[ , .(value = summary(depth_ft), metric = names(summary(depth_ft))), survey_date]
  
  carmans[depth_ft >60 ,] # that ain't right!
  
  #drop bad point
  carmans <- carmans[depth_ft<60]
  
  
  
head(carmans)
names(carmans)

# 1. Create a summary of detected species for each row
veg_summary <- carmans %>%
  # Add a unique ID to ensure we can join back perfectly if station_numbers repeat
  mutate(row_id = row_number()) %>% 
  # Pivot species columns into a single long column
  pivot_longer(
    cols = aquatic_moss:zannichellia_palustris, 
    names_to = "species", 
    values_to = "presence"
  ) %>%
  # Convert presence to numeric (in case they are read as characters) 
  # and keep only where species was detected
  filter(as.numeric(presence) > 0) %>%
  # Group by the unique row and collapse species names into a single string
  group_by(row_id) %>%
  summarize(vegetation_detected = paste(species, collapse = ", "))

# 2. Join it back to your original 'carmans' data
carmans <- carmans %>%
  mutate(row_id = row_number()) %>%
  left_join(veg_summary, by = "row_id") %>%
  select(-row_id) # Remove the helper ID

rm(veg_summary)

#sample effort by depth
# Ensure depth is numeric and create the plot
ggplot(carmans, aes(x = as.numeric(depth_ft))) +
  # Create 2-foot bins starting at 0
  geom_histogram(binwidth = 2, boundary = 0, fill = "steelblue", color = "white") +
  # Split the plot into a grid by survey date
  facet_wrap(~survey_date) +
  # Clean up labels
  labs(
    title = "Sampling Effort by Depth Bin",
    subtitle = "Counts of sample points per 5-foot depth intervals",
    x = "Depth (ft)",
    y = "Number of Points Sampled"
  ) +
  theme_minimal()



# prop vegetated by depth -------------------------------------------------



# 1. Prepare the data
plot_data <- carmans %>%
  mutate(
    depth_numeric = as.numeric(depth_ft),
    # Create 2ft bins
    depth_bin = floor(depth_numeric / 2) * 2
  ) %>%
  # Determine if each point is vegetated or not
  rowwise() %>%
  mutate(
    Status = if_else(
      any(c_across(aquatic_moss:zannichellia_palustris) > 0, na.rm = TRUE), 
      "Vegetated", 
      "Non-Vegetated"
    )
  ) %>%
  ungroup()

# 2. Create the plot
ggplot(plot_data, aes(x = depth_bin, fill = Status)) +
  # geom_bar automatically counts the rows for us
  # position = "stack" puts the vegetated count on top of/in front of the rest
  geom_bar(width = 2, just = 0, color = "white", position = "stack") +
  facet_wrap(~survey_date) +
  # Using a high-contrast color palette
  scale_fill_manual(values = c("Non-Vegetated" = "grey80", "Vegetated" = "forestgreen")) +
  labs(
    title = "Vegetated Points vs. Total Sampling Effort",
    subtitle = "Total bar height = Total points sampled | Green segment = Points with vegetation",
    x = "Depth Bin (ft)",
    y = "Number of Points",
    fill = "Point Status"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


#density curves for veg by dpeth

# density curves by depth -------------------------------------------------


# 1. Filter the data to ONLY include points where vegetation was detected
# 2. Ensure depth is numeric
veg_only <- carmans %>%
  filter(!is.na(vegetation_detected)) %>%
  mutate(depth_numeric = as.numeric(depth_ft))

# 3. Plot the density
ggplot(carmans, aes(x = depth_ft, fill = survey_date)) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Distribution of Vegetation",
    subtitle = "Useful for comparing depth occupancy, each curve shares same volume, only distrib varies",
    x = "Depth (ft)",
    y = "Estimated Count",
    fill = "Survey Date"
  ) +
  theme_minimal()




# ..plant occ by zone:  -----------------------------------------------------


# --- DATA PREP ---
plot_data <- carmans %>%
  # 1. Standardize and bin your data
  mutate(
    # Ensure depth and date are numeric/date objects
    depth_numeric = as.numeric(depth_ft),
    # If not already done, make survey_date a proper date/numeric year
    year_numeric = as.numeric(format(as.Date(survey_date), "%Y")),
    
    # 2. Replicate the depth bins from the example image
    depth_bin = case_when(
      depth_numeric < 2 ~ "0-1 ft (Removed)", # We can remove this for the final plot
      depth_numeric >= 2 & depth_numeric < 6 ~ "2-5 ft",
      depth_numeric >= 6 & depth_numeric < 11 ~ "6-10 ft",
      depth_numeric >= 11 & depth_numeric < 16 ~ "11-15 ft",
      depth_numeric >= 16 & depth_numeric < 21 ~ "16-20 ft",
      depth_numeric >= 21 & depth_numeric < 26 ~ "21-25 ft",
      depth_numeric >= 26 & depth_numeric < 30 ~ "26-30 ft",
      TRUE ~ "30+ ft (Removed)"
    ),
    
    # 3. Create a clean presence/absence column
    is_vegetated = if_else(!is.na(vegetation_detected), 1, 0)
  ) %>%
  # 4. Filter to keep only the relevant bins
  filter(depth_bin %in% c("2-5 ft", "6-10 ft", "11-15 ft", "16-20 ft", "21-25 ft", "26-30 ft"))

# --- SUMMARIZE & CALCULATE CONFIDENCE INTERVALS ---
# Define a helper function to calculate the binomial CI
get_binomial_ci <- function(successes, total) {
  # Use an "exact" method (e.g., Clopper-Pearson), which is robust for FOO
  res <- binom.confint(x = successes, n = total, method = "exact")
  return(data.frame(ymin = res$lower, ymax = res$upper))
}

plot_summary <- plot_data %>%
  group_by(depth_bin, year_numeric) %>%
  summarize(
    total_points = n(),
    veg_points = sum(is_vegetated),
    foo = veg_points / total_points, # mean frequency
    .groups = "drop"
  ) %>%
  # Map the helper function to calculate CIs for every row
  rowwise() %>%
  mutate(get_binomial_ci(veg_points, total_points)) %>%
  ungroup() %>%
  # Re-order the depth bins for the facet order
  mutate(depth_bin = factor(depth_bin, levels = c("2-5 ft", "6-10 ft", "11-15 ft", "16-20 ft", "21-25 ft", "26-30 ft")))

# 2. Updated Plot
ggplot(plot_summary, aes(x = factor(year_numeric), y = foo, fill = depth_bin, shape = depth_bin)) +
  # Subtle background tint for each panel
  geom_rect(aes(fill = depth_bin), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.04, inherit.aes = FALSE) +
  
  # Error bars
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0, color = "gray20", size = 0.5) +
  
  # Points with custom shapes (repeated/extended shapes for 6 categories)
  geom_point(color = "black", size = 3, stroke = 0.7) +
  
  facet_grid(. ~ depth_bin, scales = "free_x", space = "free_x") +
  
  # Color and Shape mapping
  scale_y_continuous(labels = label_percent(), limits = c(0, 1.05), breaks = seq(0, 1, 0.2), expand = c(0, 0)) +
  scale_shape_manual(values = c(22, 24, 21, 23, 25, 21)) + # Square, Up-Tri, Circle, Diamond, Down-Tri, Circle
  scale_fill_viridis_d(option = "mako", direction = -1) + # Professional blue-green gradient
  
  labs(
    y = "Frequency of Occurrence",
    x = NULL,
    title = "Vegetation Frequency of Occurrence by Depth",
    subtitle = "Points: Mean FOO | Bars: 95% Clopper-Pearson Binomial Confidence Intervals"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray92"),
    panel.border = element_rect(color = "gray85", fill = NA, size = 0.4),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 9),
    strip.background = element_rect(fill = "gray98", color = "gray85"),
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "none",
    panel.spacing = unit(0.3, "lines")
  )


# ..rake density plot -------------------------------------------------------

# 1. Prepare Abundance Data
# We need to sum or average the species scores for every point first
abundance_summary <- carmans[] %>%
  mutate(
    depth_numeric = as.numeric(depth_ft),
    year_numeric = as.numeric(format(as.Date(survey_date), "%Y")),
    depth_bin = case_when(
      depth_numeric >= 2  & depth_numeric < 6  ~ "2-5 ft",
      depth_numeric >= 6  & depth_numeric < 11 ~ "6-10 ft",
      depth_numeric >= 11 & depth_numeric < 16 ~ "11-15 ft",
      depth_numeric >= 16 & depth_numeric < 21 ~ "16-20 ft",
      depth_numeric >= 21 & depth_numeric < 26 ~ "21-25 ft",
      depth_numeric >= 26 & depth_numeric < 31 ~ "26-30 ft",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(depth_bin)) %>%
  # Convert species columns to numeric ---
  mutate(across(aquatic_moss:zannichellia_palustris, as.numeric)) %>% 
 ---------------------------------------------------
rowwise() %>%
  mutate(total_abundance = sum(c_across(aquatic_moss:zannichellia_palustris), na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(depth_bin, year_numeric) %>%
  summarize(
    mean_abundance = mean(total_abundance, na.rm = TRUE),
    se = sd(total_abundance, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(depth_bin = factor(depth_bin, levels = c(
    "2-5 ft", "6-10 ft", "11-15 ft", "16-20 ft", "21-25 ft", "26-30 ft"
  )))

# 2. The Plot
ggplot(abundance_summary, aes(x = factor(year_numeric), y = mean_abundance, fill = depth_bin, shape = depth_bin)) +
  # Background tint
  geom_rect(aes(fill = depth_bin), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.04, inherit.aes = FALSE) +
  
  # Standard Error bars
  geom_errorbar(aes(ymin = mean_abundance - se, ymax = mean_abundance + se), 
                width = 0, color = "gray20", size = 0.5) +
  
  # Points
  geom_point(color = "black", size = 3.5, stroke = 0.8) +
  
  facet_grid(. ~ depth_bin, scales = "free_x", space = "free_x") +
  
  # Aesthetics
  scale_shape_manual(values = c(22, 24, 21, 23, 25, 21)) +
  scale_fill_viridis_d(option = "mako", direction = -1) +
  
  labs(
    y = "Mean Sample Abundance (Sum of Scores)",
    x = NULL,
    title = "Vegetation Abundance by Depth Bin",
    subtitle = "Points: Mean total abundance per station | Bars: ±1 Standard Error"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray92"),
    panel.border = element_rect(color = "gray85", fill = NA, size = 0.4),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 9),
    strip.background = element_rect(fill = "gray98", color = "gray85"),
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "none",
    panel.spacing = unit(0.3, "lines")
  )


# Dropping M spicatum -----------------------------------------------------

invasive_species <- c(
  "butomus_umbellatus",    # Flowering rush
  "lythrum_salicaria",     # Purple loosestrife
  "myriophyllum_spicatum", # Eurasian watermilfoil
  "potamogeton_crispus",   # Curly-leaf pondweed
  "phragmites_australis",  # Non-native Phragmites
  "typha_angustifolia"     # Narrow-leaf cattail
)

carmans_native <- carmans
  carmans_native[, (invasive_species) := NULL]

veg_summary <- carmans_native %>%
  # Add a unique ID to ensure we can join back perfectly if station_numbers repeat
  mutate(row_id = row_number()) %>% 
  # Pivot species columns into a single long column
  pivot_longer(
    cols = aquatic_moss:zannichellia_palustris, 
    names_to = "species", 
    values_to = "presence"
  ) %>%
  # Convert presence to numeric (in case they are read as characters) 
  # and keep only where species was detected
  filter(as.numeric(presence) > 0) %>%
  # Group by the unique row and collapse species names into a single string
  group_by(row_id) %>%
  summarize(vegetation_detected = paste(species, collapse = ", "))

# 2. Join it back to your original 'carmans_native' data
carmans_native <- carmans_native %>%
  mutate(row_id = row_number()) %>%
  left_join(veg_summary, by = "row_id") %>%
  select(-row_id) # Remove the helper ID

rm(veg_summary)





# ..FOO natives only ------------------------------------------------------


# --- DATA PREP ---
plot_data <- carmans_native %>%
  # 1. Standardize and bin your data
  mutate(
    # Ensure depth and date are numeric/date objects
    depth_numeric = as.numeric(depth_ft),
    # If not already done, make survey_date a proper date/numeric year
    year_numeric = as.numeric(format(as.Date(survey_date), "%Y")),
    
    # 2. Replicate the depth bins from the example image
    depth_bin = case_when(
      depth_numeric < 2 ~ "0-1 ft (Removed)", # We can remove this for the final plot
      depth_numeric >= 2 & depth_numeric < 6 ~ "2-5 ft",
      depth_numeric >= 6 & depth_numeric < 11 ~ "6-10 ft",
      depth_numeric >= 11 & depth_numeric < 16 ~ "11-15 ft",
      depth_numeric >= 16 & depth_numeric < 21 ~ "16-20 ft",
      depth_numeric >= 21 & depth_numeric < 26 ~ "21-25 ft",
      depth_numeric >= 26 & depth_numeric < 30 ~ "26-30 ft",
      TRUE ~ "30+ ft (Removed)"
    ),
    
    # 3. Create a clean presence/absence column
    is_vegetated = if_else(!is.na(vegetation_detected.y), 1, 0)
  ) %>%
  # 4. Filter to keep only the relevant bins
  filter(depth_bin %in% c("2-5 ft", "6-10 ft", "11-15 ft", "16-20 ft", "21-25 ft", "26-30 ft"))

# --- SUMMARIZE & CALCULATE CONFIDENCE INTERVALS ---
# Define a helper function to calculate the binomial CI
get_binomial_ci <- function(successes, total) {
  # Use an "exact" method (e.g., Clopper-Pearson), which is robust for FOO
  res <- binom.confint(x = successes, n = total, method = "exact")
  return(data.frame(ymin = res$lower, ymax = res$upper))
}

plot_summary <- plot_data %>%
  group_by(depth_bin, year_numeric) %>%
  summarize(
    total_points = n(),
    veg_points = sum(is_vegetated),
    foo = veg_points / total_points, # mean frequency
    .groups = "drop"
  ) %>%
  # Map the helper function to calculate CIs for every row
  rowwise() %>%
  mutate(get_binomial_ci(veg_points, total_points)) %>%
  ungroup() %>%
  # Re-order the depth bins for the facet order
  mutate(depth_bin = factor(depth_bin, levels = c("2-5 ft", "6-10 ft", "11-15 ft", "16-20 ft", "21-25 ft", "26-30 ft")))

# 2. Updated Plot
ggplot(plot_summary, aes(x = factor(year_numeric), y = foo, fill = depth_bin, shape = depth_bin)) +
  # Subtle background tint for each panel
  geom_rect(aes(fill = depth_bin), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.04, inherit.aes = FALSE) +
  
  # Error bars
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0, color = "gray20", size = 0.5) +
  
  # Points with custom shapes (repeated/extended shapes for 6 categories)
  geom_point(color = "black", size = 3, stroke = 0.7) +
  
  facet_grid(. ~ depth_bin, scales = "free_x", space = "free_x") +
  
  # Color and Shape mapping
  scale_y_continuous(labels = label_percent(), limits = c(0, 1.05), breaks = seq(0, 1, 0.2), expand = c(0, 0)) +
  scale_shape_manual(values = c(22, 24, 21, 23, 25, 21)) + # Square, Up-Tri, Circle, Diamond, Down-Tri, Circle
  scale_fill_viridis_d(option = "mako", direction = -1) + # Professional blue-green gradient
  
  labs(
    y = "Frequency of Occurrence",
    x = NULL,
    title = "Vegetation Frequency of Occurrence by Depth",
    subtitle = "Points: Mean FOO | Bars: 95% Clopper-Pearson Binomial Confidence Intervals"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray92"),
    panel.border = element_rect(color = "gray85", fill = NA, size = 0.4),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 9),
    strip.background = element_rect(fill = "gray98", color = "gray85"),
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "none",
    panel.spacing = unit(0.3, "lines")
  )


#  ..rake density plot -------------------------------------------------------

# 1. Prepare Abundance Data
# We need to sum or average the species scores for every point first
abundance_summary <- carmans_native[] %>%
  mutate(
    depth_numeric = as.numeric(depth_ft),
    year_numeric = as.numeric(format(as.Date(survey_date), "%Y")),
    depth_bin = case_when(
      depth_numeric >= 2  & depth_numeric < 6  ~ "2-5 ft",
      depth_numeric >= 6  & depth_numeric < 11 ~ "6-10 ft",
      depth_numeric >= 11 & depth_numeric < 16 ~ "11-15 ft",
      depth_numeric >= 16 & depth_numeric < 21 ~ "16-20 ft",
      depth_numeric >= 21 & depth_numeric < 26 ~ "21-25 ft",
      depth_numeric >= 26 & depth_numeric < 31 ~ "26-30 ft",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(depth_bin)) %>%
  # Convert species columns to numeric ---
  mutate(across(aquatic_moss:zannichellia_palustris, as.numeric)) %>% 
rowwise() %>%
  mutate(total_abundance = sum(c_across(aquatic_moss:zannichellia_palustris), na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(depth_bin, year_numeric) %>%
  summarize(
    mean_abundance = mean(total_abundance, na.rm = TRUE),
    se = sd(total_abundance, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(depth_bin = factor(depth_bin, levels = c(
    "2-5 ft", "6-10 ft", "11-15 ft", "16-20 ft", "21-25 ft", "26-30 ft"
  )))

# 2. The Plot
ggplot(abundance_summary, aes(x = factor(year_numeric), y = mean_abundance, fill = depth_bin, shape = depth_bin)) +
  # Background tint
  geom_rect(aes(fill = depth_bin), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.04, inherit.aes = FALSE) +
  
  # Standard Error bars
  geom_errorbar(aes(ymin = mean_abundance - se, ymax = mean_abundance + se), 
                width = 0, color = "gray20", size = 0.5) +
  
  # Points
  geom_point(color = "black", size = 3.5, stroke = 0.8) +
  
  facet_grid(. ~ depth_bin, scales = "free_x", space = "free_x") +
  
  # Aesthetics
  scale_shape_manual(values = c(22, 24, 21, 23, 25, 21)) +
  scale_fill_viridis_d(option = "mako", direction = -1) +
  
  labs(
    y = "Mean Sample Abundance (Sum of Scores)",
    x = NULL,
    title = "Vegetation Abundance by Depth Bin",
    subtitle = "Points: Mean total abundance per station | Bars: ±1 Standard Error"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray92"),
    panel.border = element_rect(color = "gray85", fill = NA, size = 0.4),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 9),
    strip.background = element_rect(fill = "gray98", color = "gray85"),
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "none",
    panel.spacing = unit(0.3, "lines")
  )



# community metrics -------------------------------------------------------


# 1. Calculate FOO for every species per survey date
survey_foo_matrix <- carmans_native %>%
  mutate(survey_date = as.Date(survey_date)) %>%
  # Pivot all species to long format
  pivot_longer(cols = aquatic_moss:zannichellia_palustris, 
               names_to = "species", 
               values_to = "score") %>%
  # Determine presence (1) or absence (0)
  mutate(present = if_else(as.numeric(score) > 0, 1, 0)) %>%
  # Group by survey and species to get FOO
  group_by(survey_date, species) %>%
  summarize(foo = mean(present, na.rm = TRUE), .groups = "drop") %>%
  # Pivot back to a wide matrix format that 'vegan' likes
  pivot_wider(names_from = species, values_from = foo)

# 2. Extract the matrix (excluding the date column)
foo_data <- survey_foo_matrix %>% select(-survey_date)

# 3. Calculate Diversity Metrics at the Survey Level
survey_diversity <- survey_foo_matrix %>%
  mutate(
    # Total unique native species found in the bay
    richness = specnumber(foo_data),
    
    # Shannon index based on FOO distributions
    shannon = diversity(foo_data, index = "shannon"),
    
    # Evenness (Pielou's J)
    evenness = shannon / log(richness),
    
    # ENSpie (Inverse Simpson)
    `Diversity(enspie)` = diversity(foo_data, index = "invsimpson")
  ) %>%
  select(survey_date, richness, evenness, `Diversity(enspie)`)

# 4. Plot the results
survey_diversity_long <- survey_diversity %>%
  pivot_longer(cols = -survey_date, names_to = "metric", values_to = "value")

ggplot(survey_diversity_long, aes(x = survey_date, y = value, color = metric)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  facet_wrap(~metric, scales = "free_y", ncol = 1) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 year") +
  scale_color_viridis_d(option = "viridis", end = 0.8) +
  labs(
    title = "Bay-Wide Diversity Metrics (Based on FOO)",
    subtitle = "Metrics calculated using the frequency of occurrence for each survey date/n",
    y = "Value",
    x = "Survey Date"
  ) +
  theme_minimal() +
  theme(legend.position = "none", strip.text = element_text(face = "bold"))






# Species specific change -----------------------------------------------------


# 1. Identify Top 10 Species (by total abundance to keep consistency)
top_10_names <- carmans %>%
  summarize(across(aquatic_moss:zannichellia_palustris, ~sum(as.numeric(.x), na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "species", values_to = "total") %>%
  arrange(desc(total)) %>%
  slice(1:10) %>%
  pull(species)


# 2. Calculate Annual FOO and Binomial CIs
species_foo_ts <- carmans %>%
  mutate(year_numeric = format(as.Date(survey_date), "%Y")) %>%
  # Select only needed columns and pivot
  select(year_numeric, all_of(top_10_names)) %>%
  pivot_longer(cols = -year_numeric, names_to = "species", values_to = "score") %>%
  # Convert to Presence/Absence
  mutate(present = if_else(as.numeric(score) > 0, 1, 0)) %>%
  group_by(year_numeric, species) %>%
  summarize(
    successes = sum(present, na.rm = TRUE),
    trials = n(),
    foo = successes / trials,
    .groups = "drop"
  ) %>%
  # Calculate Binomial Confidence Intervals
  rowwise() %>%
  mutate(
    # Using 'exact' for Clopper-Pearson intervals
    ymin = binom.confint(successes, trials, method = "exact")$lower,
    ymax = binom.confint(successes, trials, method = "exact")$upper,
    species_label = gsub("_", " ", species) # Clean name for plot
  ) %>%
  ungroup()

# 3. The Plot
ggplot(species_foo_ts, aes(x = factor(year_numeric), y = foo, group = species)) +
  # Subtle background fill per species
  geom_rect(aes(fill = species), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.05) +
  
  # Binomial Error Bars
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.1, color = "gray30", size = 0.5) +
  
  # Trend lines and points
  geom_line(color = "gray50", size = 0.5, linetype = "dashed") +
  geom_point(aes(color = species), size = 2.5) +
  
  facet_wrap(~species_label, scales = "free_y", ncol = 2) +
  
  # Formatting
  scale_y_continuous(labels = label_percent(), expand = expansion(mult = c(0.05, 0.1))) +
  scale_color_viridis_d(option = "mako") +
  scale_fill_viridis_d(option = "mako") +
  
  labs(
    title = "Annual Native Species Frequency (FOO)",
    subtitle = "Points: % of sites present | Error bars: 95% Clopper-Pearson Binomial CI",
    y = "Frequency of Occurrence",
    x = "Year"
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    strip.background = element_rect(fill = "gray96", color = "gray80"),
    strip.text = element_text(face = "italic", size = 10),
    panel.border = element_rect(color = "gray85", fill = NA),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )






## Stubbs ------------------------------------------------------------------













