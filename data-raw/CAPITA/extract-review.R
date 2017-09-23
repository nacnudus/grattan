# A comparison of two scripts to extract data from a mucky spreadsheet

library(data.table)
library(dplyr)
library(purrr)
library(tidyr)
library(dtplyr)
library(readxl)
library(tidyxl)
library(here)

original_script_path <- here("data-raw", "CAPITA", "extract_clean_capita.R")
xlsx_path <- here("data-raw", "CAPITA", "CPS-v17-09-12.xlsx")

# Run the original script, and apply cosmetics for the sake of comparison with a
# review version.
source(original_script_path)
original <-
  capita %>%
  arrange(sheet_name, raw_name, start_date, end_date) %>%
  select(sheet_name, raw_name, start_date, end_date, value) %>%
  filter(!is.na(value))

# Now review version of the original script, using readxl instead of tidyxl

sheet_names <- excel_sheets(xlsx_path)
sheet_names <- sheet_names[sheet_names != "Contents"]
names(sheet_names) <- sheet_names

sheets <-
  sheet_names %>%
  map_df(read_excel,
         path = xlsx_path,
         skip = 6,
         .id = "sheet_name")

review <-
  sheets %>%
  rename(start_date = Date, end_date = Enddate) %>%
  select(-starts_with("X__")) %>% # Columns that are just notes
  gather(raw_name, value, -sheet_name, -start_date, -end_date) %>%
  arrange(sheet_name, raw_name, start_date, end_date) %>%
  group_by(sheet_name, raw_name) %>%
    mutate(end_date = if_else(is.na(end_date), lead(start_date), end_date)) %>%
  ungroup() %>%
  filter(!is.na(end_date), !is.na(value)) %>%
  select(sheet_name, raw_name, start_date, end_date, value) %>%
  as.data.table()

# Compare the two versions

# Rows in 'review' that aren't in 'original'.  These are due to a bug in readxl,
# which interprets the MedLevSurRate2 column on the MedicareAndLevies sheet as a
# date.
anti_join(review, original) %>% print(n = Inf)

# Rows in 'original' that aren't in 'review': As above, plus four more from the
# final (untitled) column in the 'SuperBenefits' sheet.
anti_join(original, review) %>% print(n = Inf)
