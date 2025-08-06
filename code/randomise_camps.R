#############################################
# Randomise all information regarding camps #
#############################################

library(dplyr)

# Read files
demog <- read.csv("./data/final/all_demographies_joined_red_incl_23.csv")

# Create a lookup table with random codes for each unique camp_name
set.seed(123)  # for reproducibility
unique_camps <- unique(demog$camp_name)
camp_codes <- data.frame(
  camp_name = unique_camps,
  code = sample(sprintf("CAMP%03d", 1:length(unique_camps)))  # creates codes like CAMP001, CAMP002, etc.
)

# Join the code back to the demog data frame
demog <- demog %>%
  left_join(camp_codes, by = "camp_name") %>%
  mutate(camp_name = code) %>%
  select(-code)  # drop code column if you want to keep the same column name

# write demog
write.csv(demog, "./data/final/all_demographies_joined_red_incl_23_randomised_camps.csv", row.names = FALSE)

# Save mapping to a file for reuse
write.csv(camp_codes, "./data/final/camp_name_codes.csv", row.names = FALSE)

# next df
mr <- read.csv("./data/final/all_mr_info_overlap_w_camp.csv")

# Function to apply anonymisation using camp_codes
anonymise_camp_column <- function(df, column_name, camp_codes) {
  df <- df %>%
    left_join(camp_codes, by = setNames("camp_name", column_name)) %>%
    mutate(!!sym(column_name) := code) %>%
    select(-code)
  return(df)
}


mr <- mr %>% anonymise_camp_column("nom", camp_codes)
mr <- mr %>% anonymise_camp_column("camp_name.x", camp_codes)
mr <- mr %>% anonymise_camp_column("camp_name.y", camp_codes)
mr <- mr %>% anonymise_camp_column("residence_camp", camp_codes)

# write mr
write.csv(mr, "./data/final/all_mr_info_overlap_w_camp_randomised_camps.csv", row.names = FALSE)

# next df
dis <- read.csv("./data/final/distance_to_kin_macmin.csv")

dis <- dis %>%
  left_join(camp_codes, by = c("residence_camp" = "camp_name")) %>%
  mutate(residence_camp = code) %>%
  select(-code)

# write dis
write.csv(dis, "./data/final/distance_to_kin_macmin_randomised_camps.csv", row.names = FALSE)
