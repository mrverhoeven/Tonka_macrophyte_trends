
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

#' This script will work in a bay-by-bay framing
#' 1. pull in plant observation data from PI surveys
#' 2. calculate key metrics from those
#' 3. evaluate the change in thos metrics ove time
#' 
#' I'll start with a rough build of the concepts the rearrange as needed. 
#' 
#' 
#' 1 - updated file names to fit format (e.g., broken one =  Carson_St.Louis_PISurveyData_Combined .xlsx)
#' 2 - updated some tab names(e.g., broken one = 6-7and10-2017)
#' 3 - duplicated column names: 
        #' [1] "Found duplicate columns in the following locations:"
        #' # A tibble: 4 × 3
        #' bay                                  survey_date duplicate_column       
        #' <chr>                                <chr>       <chr>                  
        #'   1 Carson_St.Louis_PIData_Combined.xlsx 8-12-2013   lythrum_salicaria      
        #' 2 Grays_PIData_Combined.xlsx           8-9-2022    potamogeton_amplifolius (checked source-- 2nd one should have been P nodosus)
        #' 3 Northarm_PIData_Combined.xlsx        8-13-2025   lemna_trisulca         (checked source-- 1st one should have been L salicaria)
        #' 4 Northarm_PIData_Combined.xlsx        8-12-2024   spirodela_polyrrhiza (checked source-- 2nd one should have been P nodosus)
        #' #' 
#' 
#' 4 - Work through column names checking on the taxonomy and code switchin' to names. 
#' Some fixed in the files, others fixed in R using a renamer





#' # Document Preamble
knitr::opts_chunk$set(warning = FALSE, message = FALSE) 

strttime <- Sys.time()
getwd()


# load libraries ------------------------------------------------------------------

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


# load in functions -------------------------------------------------------
#' ## Functions

f_dowle3natozeros = function(DT, x) {
  # or by number (slightly faster than by name) :
  for (j in x)
    set(DT,which(is.na(DT[[j]])),j,"0")
}


# load in data -------------------------------------------------

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


# audit column names ------------------------------------------------------


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



# pull in data ------------------------------------------------------------


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


# renaming columns --------------------------------------------------------

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


#fillmatrix with zeros

f_dowle3natozeros(minnetonka_full_data, plant_cols)













