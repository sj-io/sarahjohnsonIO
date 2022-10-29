library(tidyverse)
library(readxl)

v_acs1 <- read_csv("census/data/v_acs1.csv")
v_acs10 <- read_csv("census/data/v_acs1_10.csv")
v_acs11 <- read_csv("census/data/v_acs1_11.csv")
v_acs12 <- read_csv("census/data/v_acs1_12.csv")

v_list <- read_excel("census/data/2021_DataProductList.xlsx") %>% 
  rename(name = 1,
         concept = 2,
         universe = 3,
         geo1 = 5,
         geo5 = 6) %>% 
  mutate(concept = str_to_title(concept),
         concept = str_replace_all(concept, "\\b20\\d{2}\\b", "ACSYEAR")) %>% 
  filter(str_detect(name, "^[BC]25"),
         !str_detect(name, "\\d[:alpha:]"))

# Get the top level variables
hsg_top <- v_acs1 %>% 
  filter(str_detect(name, "^[BC]25"),
         str_detect(name, "_001$"),           #' only top-level variables
         !str_detect(name, "[:alpha:]_"),     #' omit race variables
         !str_detect(label, "Estimate!!Aggregate") #' omit aggregates, medians, etc.
  ) %>% 
  # Standardize inflation variables across years
  mutate(name = str_remove_all(name, "_001"),
         concept = str_to_title(concept),
    across(c(label, concept), ~ str_replace_all(.x, "\\b20\\d{2}\\b", "ACSYEAR"))) %>% 
  select(-label) %>%  
  left_join(v_list)

# Find variables that have changed over the years
chg <- hsg_top %>% 
  filter(!is.na(concept)) %>% 
  # ACS variables from 2010 & 2011 are broken, show as NA
  select(-yr) %>% 
  distinct() %>% 
  count(name) %>% 
  filter(n > 1)

hmm <- str_c(chg$name, collapse = "|")

which_hmm <- hsg_top %>% 
  filter(!is.na(concept)) %>% 
  filter(str_detect(name, hmm))

# Clean up

hsg3 <- hsg %>% 
  mutate(across(c(label, concept), ~ str_replace_all(.x, "\\b20\\d{2}\\b", "ACSYEAR"))) %>% 
  filter(str_detect(name, "_001$"),           #' only top-level variables
         !str_detect(name, "[:alpha:]_"),     #' omit race variables
         str_detect(label, "Estimate!!Total") #' omit aggregates, medians, etc.
  ) %>% 
  select(-label)

hsg4 <- hsg3 %>% 
  filter(!is.na(concept)) %>% 
  select(-yr) %>% 
  distinct()

#' B25026 TOTAL POPULATION IN OCCUPIED HOUSING UNITS BY TENURE BY YEAR HOUSEHOLDER MOVED INTO UNIT
#' B25026 UNITS IN STRUCTURE BY RESIDENCE STATUS
#' 
#' B25027 MORTGAGE STATUS BY AGE OF HOUSEHOLDER
#' B25027 UNITS IN STRUCTURE BY YEAR STRUCTURE BUILT
#' 

ct_yr <- hsg3 %>% 
  group_by(name, concept) %>% 
  arrange(yr) %>% 
  mutate(years = str_flatten(yr, collapse = ", ")) %>% 
  select(-yr) %>% 
  distinct()

min_yr <- hsg3 %>% 
  filter(!is.na(concept)) %>% 
  group_by(name, concept) %>% 
  slice_min(yr) %>% 
  mutate(nm = "min")
max_yr <- hsg3 %>% 
  filter(!is.na(concept)) %>% 
  group_by(name, concept) %>% 
  slice_max(yr) %>% 
  mutate(nm = "max")

wh_yr <- rbind(min_yr, max_yr) %>% 
  pivot_wider(names_from = nm, values_from = yr) %>% 
  mutate(years = case_when(
    min != max & min != 2012 ~ paste0("(", min, "-", max, ")"),
    min != max & min == 2012 ~ paste0("(2010-", max, ")"),
    TRUE ~ paste0(min)
  )) %>% 
  arrange(name, min, max) %>% 
  select(-c(min, max)) %>% 
  distinct()

hsg5 <- hsg3 %>% 
  count(name)


v_acs_changes <- read_csv("census/data/2021-1yr-api-changes.csv") %>% 
  rename(tblID = 2)

v_hsg_chg <- v_acs_changes %>% 
  filter(str_starts(tblID, "B25"))

v_hsg_chg %>% count(Change)

updated21 <- v_hsg_chg %>% filter(Change == "Variable Updated")
no_match_21 <- v_hsg_chg %>% filter(Change == "No Match")
