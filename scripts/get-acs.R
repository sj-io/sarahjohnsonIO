new_data <- get_acs(
  geography = "place",
  state = "TN",
  # county = "Shelby",
  table = "B25106",
  summary_var = "B25106_001",
  cache_table = TRUE
) %>%
  # filter(str_detect(NAME, "Shelby")) %>%
  filter(str_detect(NAME, places)) %>%
  mutate(year = 2021, .before = 1)

existing <- read_csv("data/census/acs5/B25-hsg.csv") 
joined <- rbind(existing, new_data)
write_csv(joined, "data/census/acs5/B25-hsg.csv")

places <- c("Memphis", "Lakeland", "Bartlett", 
            "Germantown", "Collierville", "Millington", "Arlington") %>% 
  str_c(collapse = "|")

# Get unincorporated data
incorporated <- shelby_places %>% 
  group_by(variable) %>% 
  summarise(i_estimate = sum(estimate),
            i_moe = moe_sum(moe, estimate),
            i_summary_est = sum(summary_est),
            i_summary_moe = moe_sum(summary_moe, summary_est),
            ) %>% 
  distinct() %>% 
  bind_cols(new_data) %>% 
  ungroup()

unincorp <- incorporated %>% 
  mutate(year = year,
         GEOID = "4700000",
         NAME = "Unincorporated county",
         estimate = estimate - i_estimate,
         moe = NA,
         summary_est = summary_est - i_summary_est,
         summary_moe = NA,
         variable = variable...1) %>% 
  select(v_names)

joined <- rbind(existing, unincorp)
