# Script for cleaning programs/majors data from IPEDS

library(dplyr)
library(readr)
library(stringr)

# READ IN DATA

## Download the data and put it in data/raw/ folder

## https://nces.ed.gov/ipeds/data-generator?year=2024&tableName=C2024_A&HasRV=0&type=csv&t=639098924033492874 
C2024A <- read_csv("data/raw/c2024_a.csv")

## unitids_data: dataset containing the UNITIDs you want to keep
unitids_data <- read_csv("https://raw.githubusercontent.com/joycegill/Advanced-Statistical-Modeling/main/data/cleaned/unitids.csv")

# -----------------------------------------------
## constant list of cip groups
cip_labels <- c(
  "01" = "Agriculture",
  "03" = "Natural Resources",
  "04" = "Architecture",
  "05" = "Area/Ethnic/Cultural/Gender Studies",
  "09" = "Communication and Journalism",
  "10" = "Communications Technologies",
  "11" = "Computer and Information Sciences",
  "12" = "Personal and Culinary Services",
  "13" = "Education",
  "14" = "Engineering",
  "15" = "Engineering Technologies/Technicians",
  "16" = "Foreign Languages, Literature, and Linguistics",
  "19" = "Family & Consumer Sciences",
  "22" = "Legal Professions and Studies",
  "23" = "English Language and Literature",
  "24" = "Liberal Arts and Sciences, General Studies and Humanities",
  "25" = "Libary Science",
  "26" = "Biological and Biomedical Sciences",
  "27" = "Math & Statistics",
  "29" = "Military Technologies",
  "30" = "Multi/Interdisciplinary Studies",
  "31" = "Parks, Recreation, Leisure, and Fitness Studies",
  "38" = "Philosophy and Religious Studies",
  "39" = "Theology and Religious Vocations",
  "40" = "Physical Sciences",
  "41" = "Science Technologies/Technicians",
  "42" = "Psychology",
  "43" = "Security and Protective Services",
  "44" = "Public Administration and Social Services",
  "45" = "Social Sciences",
  "46" = "Construction Trades",
  "47" = "Mechanic and Repair Technologies/Technicians",
  "48" = "Precision Production",
  "49" = "Transportation and Materials Moving",
  "50" = "Visual and Performing Arts",
  "51" = "Health Professions",
  "52" = "Business, Management and Marketing",
  "54" = "History",
  "99" = "Grand Total"
)

cip_dictionary <- tibble(
  CIP = names(cip_labels),
  LABEL = unname(cip_labels)
)

write_csv(
  cip_dictionary,
  "data/cleaned/cip_dictionary.csv"
)

## Constant list of variable names
var_names <- c(
  "CTOTALT" = "Grand total",
  "CTOTALM" = "Grand total men",
  "CTOTALW" = "Grand total women",
  "CAIANT" = "American Indian or Alaska Native total ",
  "CASIAT" = "Asian total ",
  "CBKAAT" = "Black or African American total ",
  "CHISPT" = "Hispanic or Latino total ",
  "CNHPIT" = "Native Hawaiian or Other Pacific Islander total",
  "CWHITT" = "White total ",
  "C2MORT" = "Two or more races total",
  "CUNKNT" = "Race/ethnicity unknown total",
  "CNRALT" = "U.S. Nonresident total"
)

cip_var_names_dictionary <- tibble(
  NAME = names(cip_labels),
  LABEL = unname(cip_labels)
)

write_csv(
  cip_var_names_dictionary,
  "data/cleaned/c2024_a_var_names.csv"
)

# -----------------------------------------------
## Most popular: https://www.visualcapitalist.com/cp/charted-most-popular-u-s-undergraduate-degrees-2011-2021/
cip_keep <- c("09","11","13","14","23","24","26","31","42","43","44","45","50","51","52", "99")

# Keep all UNITIDs first
all_units <- unitids_data %>% select(UNITID)

# Prepare C2024A subset with the numeric columns we care about
C2024A_subset <- C2024A %>%
  filter(MAJORNUM == 1, AWLEVEL == 5) %>%  # keep only relevant rows
  mutate(CIPGROUP = str_sub(as.character(CIPCODE), 1, 2)) %>%
  filter(CIPGROUP %in% cip_keep) %>%
  select(-MAJORNUM, -AWLEVEL)

# Summarize numeric data by UNITID and CIPGROUP
CIP_summary <- C2024A_subset %>%
  group_by(UNITID, CIPGROUP) %>%
  summarise(
    across(
      where(is.numeric) & !any_of("UNITID"),
      ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) %>%
  # remove racial-gender data
  select(-CAIANM, -CAIANW, -CASIAM, -CASIAW, -CBKAAM, -CBKAAW,
         -CHISPM, -CHISPW, -CNHPIM, -CNHPIW, -CWHITM, -CWHITW, -C2MORM, -C2MORW,
         -CUNKNM, -CUNKNW, -CNRALM, -CNRALW) %>%
  # turn data into wide format
  pivot_wider(
    id_cols = UNITID,
    names_from = CIPGROUP,
    values_from = c(
      CTOTALT,
      CTOTALM,
      CTOTALW,
      CAIANT,
      CASIAT,
      CBKAAT,
      CHISPT,
      CNHPIT,
      CWHITT,
      C2MORT,
      CUNKNT,
      CNRALT
    ),
    names_glue = "{CIPGROUP}_{.value}",
    values_fill = NA  # fill missing rows with NA instead of 0
  )

# Ensure all UNITIDs from unitids_data are included
CIP_summary <- all_units %>%
  left_join(CIP_summary, by = "UNITID")

# Write to CSV
write_csv(CIP_summary, "data/cleaned/c2024_a_clean.csv")


