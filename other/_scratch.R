off_work <- c(
  "2023-05-29",
  "2023-06-19",
  "2023-06-20",
  "2023-06-21",
  "2023-06-22",
  "2023-06-23",
  "2023-07-04",
  "2023-08-01",
  "2023-08-03",
  "2023-08-04",
  "2023-09-04",
  "2023-11-23",
  "2023-11-24"
) |> as_tibble_col("creationDate") |> mutate(creationDate = date(creationDate), holiday = TRUE)


watch_data <- read_parquet(paste0(path, "health/sleep.parquet")) |> 
  filter(str_detect(sourceName, "Watch")) |> 
  filter(str_detect(value, "Asleep")) |> 
  mutate(value = str_remove(value, "Asleep"),
         creationDate = date(creationDate)) |> 
  select(-starts_with("source")) |> 
  mutate(start_time = hms::as_hms(startDate),
         end_time = hms::as_hms(endDate)) |>
  mutate(across(ends_with("_time"), ~ ifelse(
    as.numeric(.x) <= 48000,
    .x + hms::hms(0, 0, 24),
    .x
  ) |> hms::as_hms()))

sleep_min <- watch_data |> 
  slice_min(start_time, by = creationDate)
sleep_max <- watch_data |> 
  slice_max(end_time, by = creationDate)
daily_total <- watch_data |> 
  mutate(cycle_time = endDate - startDate) |> 
  summarise(total_sleep = sum(cycle_time), .by = "creationDate")

weekly_again <- daily_total |> 
  mutate(week = week(creationDate),
         wday = wday(creationDate, label = TRUE)) |> 
  left_join(sleep_min |> select(creationDate, start_time), by = "creationDate") |> 
  left_join(sleep_max |> select(creationDate, end_time), by = "creationDate") |> 
  left_join(off_work, by = "creationDate") |>
  mutate(off_work = ifelse(wday %in% c("Sat", "Sun") | !is.na(holiday), 
                           "offwork", "work"),
         mid_sleep = hms::as_hms(mean(c(start_time, end_time))), .by = "creationDate")

weekly_again |> 
  ggplot(aes(x = mid_sleep, y = total_sleep)) +
  facet_wrap(~ off_work) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm") + theme_minimal()


weekly_again |> 
  ggplot(aes(x = start_time, y = total_sleep)) +
  facet_wrap(~ off_work) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE) +
  theme_minimal()

weekly_again |> 
  ggplot() +
  geom_histogram(aes(start_time)) +
  facet_wrap(~ off_work) + theme_minimal()


weekly_again |> 
  ggplot() +
  geom_histogram(aes(start_time, fill = off_work), position = "dodge") +
  # facet_wrap(~ off_work) + 
  theme_minimal()




work_rankings <- weekly_again |> 
  filter(is.na(off_work)) |> 
  arrange(desc(total_sleep)) |> 
  mutate(total_sleep_rank = row_number()) |> 
  arrange(start_time) |> 
  mutate(early_bed_rank = row_number()) |> 
  arrange(end_time) |> 
  mutate(early_rise_rank = row_number()) |> 
  arrange(desc(end_time)) |> 
  mutate(late_rise_rank = row_number()) |> 
  arrange(desc(start_time)) |> 
  mutate(late_bed_rank = row_number())

off_rankings <- weekly_again |> 
  filter(!is.na(off_work)) |> 
  arrange(desc(total_sleep)) |> 
  mutate(total_sleep_rank = row_number()) |> 
  arrange(start_time) |> 
  mutate(early_bed_rank = row_number()) |> 
  arrange(end_time) |> 
  mutate(early_rise_rank = row_number()) |> 
  arrange(desc(end_time)) |> 
  mutate(late_rise_rank = row_number()) |> 
  arrange(desc(start_time)) |> 
  mutate(late_bed_rank = row_number())

# Work Days
cor(work_rankings$total_sleep_rank, work_rankings$early_bed_rank)
# 0.772 very strong positive b/w going to sleep early & total sleep time
cor(work_rankings$early_bed_rank, work_rankings$early_rise_rank)
# 0.374 moderate positive b/w going to sleep early & waking up early
cor(work_rankings$total_sleep_rank, work_rankings$early_rise_rank)
# -0.056 no correlation b/w waking up early & total sleep time
cor(work_rankings$late_rise_rank, work_rankings$early_bed_rank)
# -0.374 weak negative b/w going to sleep early & waking up late
cor(work_rankings$late_rise_rank, work_rankings$total_sleep_rank)
# 0.057 no correlation b/w waking up late & total sleep time
cor(work_rankings$late_bed_rank, work_rankings$total_sleep_rank)
# -0.772 very strong negative b/w falling asleep late & total sleep time
cor(work_rankings$late_bed_rank, work_rankings$early_rise_rank)
# -0.374 moderately strong negative b/w falling asleep late & waking up early
cor(work_rankings$late_bed_rank, work_rankings$late_rise_rank)
# 0.374 moderately strong positive b/w falling asleep late & waking up late

# Off Days
cor(off_rankings$total_sleep_rank, off_rankings$early_bed_rank)
# 0.728 very strong positive b/w going to sleep early & total sleep time
cor(off_rankings$early_bed_rank, off_rankings$early_rise_rank)
# 0.445 moderate positive b/w going to sleep early & waking up early
cor(off_rankings$total_sleep_rank, off_rankings$early_rise_rank)
# -0.161 weak negative b/w waking up early & total sleep time
cor(off_rankings$late_rise_rank, off_rankings$early_bed_rank)
# -0.445 moderate negative b/w going to sleep early & waking up late
cor(off_rankings$late_rise_rank, off_rankings$total_sleep_rank)
# 0.161 weak negative b/w waking up late & total sleep time
cor(off_rankings$late_bed_rank, off_rankings$total_sleep_rank)
# -0.7.28 weak negative b/w waking up late & total sleep time

average_sleeps <- sleep_start |> 
  select(creationDate, start_time) |> 
  left_join(sleep_end |> select(creationDate, end_time), by = "creationDate") |> 
  mutate(
    week = week(creationDate)
  ) 

weekly_avg <- average_sleeps |> 
  summarise(avg_start_time = mean(start_time),
            avg_end_time = mean(end_time),
            earliest_time = min(start_time),
            latest_time = max(end_time),
            .by = "week") |> 
  mutate(
    across(ends_with("_time"), ~ ifelse(
      as.numeric(.x) >= 86400,
      .x - hms::hms(0, 0, 24),
      .x
    ) |> hms::as_hms())
  )


ggplot(data = weekly_again) +
  geom_histogram(aes(total_sleep))
