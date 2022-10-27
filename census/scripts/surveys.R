library(tidyverse)
library(tidycensus)

y19 <- 2010:2019
y20 <- 2010:2020

#' https://www.census.gov/data/developers/data-sets/popest-popproj/popest/popest-vars/2019.html for DATE code definitions 
#' and to see why I filtered out the first two rows

PEP <- get_estimates(
  geography = "county",
  product = "housing",
  state = "TN",
  county = "Shelby",
  time_series = TRUE
  ) %>%
  filter(DATE >= 3) %>%
  mutate(year = y19,
         dataset = "PEP",
         estimate = value) %>% 
  select(-c(DATE, value))


ACS1 <- map_dfr(y19, ~ {
  get_acs(
    geography = "county",
    survey = "acs1",
    variables = "B25001_001",
    state = "TN",
    county = "Shelby",
    year = .x,
    cache_table = TRUE
  )
}, .id = "year") %>%
  mutate(year = y19,
         dataset = "ACS1")


ACS5 <- map_dfr(y20, ~ {
  get_acs(
    geography = "county",
    survey = "acs5",
    variables = "B25001_001",
    state = "TN",
    county = "Shelby",
    year = .x,
    cache_table = TRUE
  )
}, .id = "year") %>%
  mutate(year = y20,
         dataset = "ACS5")

write_csv(ACS1, "census/data/ACS1.csv")
write_csv(ACS5, "census/data/ACS5.csv")
write_csv(PEP, "census/data/PEP.csv")

hsg <- PEP %>% 
  full_join(ACS1) %>% 
  full_join(ACS5)

ggplot(hsg, aes(x = year, y = estimate, color = dataset)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = unique(hsg$year)) +
  labs(title = "Housing Units in Shelby County, 2010-2020",
       y = "Estimate",
       x = "Year",
       color = "Dataset") +
  theme_light()

ggsave("census/img/survey.png", height = 4.5, width = 7)
