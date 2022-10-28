library(tidyverse)

v_acs1 <- read_csv("census/data/v_acs1.csv")
v_acs10 <- read_csv("census/data/v_acs1_10.csv")

hsg <- v_acs1 %>% 
  filter(str_detect(name, "^B25"))

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

hsg2 <- hsg %>% 
  mutate(across(c(label, concept), ~ str_replace_all(.x, "\\b20\\d{2}\\b", "ACSYEAR")))

vyr <- hsg2 %>% 
  count(yr)

hsg3 <- hsg2 %>% 
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

hsg6 <- hsg3 %>% 
  select(-concept) %>% 
  

v05 <- hsg3 %>% 
  filter(yr == 2005) %>% 
  select(-yr)
v06 <- hsg3 %>% 
  filter(yr == 2006) %>% 
  select(-yr)
v07 <- hsg3 %>% 
  filter(yr == 2007) %>% 
  select(-yr)
v08 <- hsg3 %>% 
  filter(yr == 2008) %>% 
  select(-yr)
v09 <- hsg3 %>% 
  filter(yr == 2009) %>% 
  select(-yr)
v10 <- hsg3 %>% 
  filter(yr == 2010) %>% 
  select(-yr)
v11 <- hsg3 %>% 
  filter(yr == 2011) %>% 
  select(-yr)
v12 <- hsg3 %>% 
  filter(yr == 2012) %>% 
  select(-yr)
v13 <- hsg3 %>% 
  filter(yr == 2013) %>% 
  select(-yr)
v14 <- hsg3 %>% 
  filter(yr == 2014) %>% 
  select(-yr)
v15 <- hsg3 %>% 
  filter(yr == 2015) %>% 
  select(-yr)
v16 <- hsg3 %>% 
  filter(yr == 2016) %>% 
  select(-yr)
v17 <- hsg3 %>% 
  filter(yr == 2017) %>% 
  select(-yr)
v18 <- hsg3 %>% 
  filter(yr == 2018) %>% 
  select(-yr)
v19 <- hsg3 %>% 
  filter(yr == 2019) %>% 
  select(-yr)
v21 <- hsg3 %>% 
  filter(yr == 2021) %>% 
  select(-yr)


