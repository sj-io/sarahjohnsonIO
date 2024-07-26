library(tidyverse)
library(arrow)
library(fastLink)

# filepaths
fp_adb <- file.path(Sys.getenv("MY_DATA_FOLDER"), "adb", "adb-2023.parquet")
fp_idis <- file.path(Sys.getenv("MY_DATA_FOLDER"), "idis", "rehab-231129.parquet")

# assessor's db
db_adb <- read_parquet(fp_adb, as_data_frame = FALSE) |>
  select(street_number = ADRNO, street_name = ADRSTR, parcel = PARID) |> 
  distinct() |> 
  mutate(
    street_number = as.numeric(street_number),
    street_name = str_to_upper(street_name)
    ) |> 
  collect()

# idis data: 1266 rows
db_idis <- read_parquet(fp_idis) |> 
  select(street_number, street_name) |> 
  # unite(street_name, c("dir_1", "street_name", "dir_2"), sep = " ", na.rm = TRUE) |>
  distinct() |> 
  mutate(
    street_number = as.numeric(street_number),
    street_name = str_to_upper(street_name),
    # direction = na_if(direction, ""),
    # across(c("street_name", "direction"), ~ str_to_upper(.))
  )

# all exact matches
exact_match_all <- inner_join(db_idis, db_adb, 
                               by = c("street_number", "street_name"), 
                               relationship = "one-to-many") 

# rows w/ multiple results in adb: 4
exact_match_dup <- exact_match_all |> 
  group_by(street_number, street_name) |> 
  count() |> 
  filter(n > 1)
# TODO: add directions to street name

# one-to-one exact matches: 1100
exact_match <- anti_join(exact_match_all, exact_match_dup)

# rows without an exact match: 162
db_idis_1 <- anti_join(db_idis, exact_match_all)

# run fastlink 
fl_1 <- fastLink(
  db_idis_1, db_adb, 
  varnames = c("street_number", "street_name"),
  stringdist.match = c("street_name"),
  partial.match = "street_name",
  threshold.match = 0.95
)

# match indices
match_1 <- fl_1$matches

# function that returns matched rows, concatenating results
get_fl_matches <- function(the_matches, db1, db2) {
  db1_id <- db1 |> rowid_to_column()
  db2_id <- db2 |> rowid_to_column()

  # inner join only have to unite, left join have to pivot and summarize
  match_out <- inner_join(db1_id, the_matches, by = c("rowid" = "inds.a"))
  match_out <- left_join(match_out, db2_id, by = c("inds.b" = "rowid"))
  
  joined_cols <- names(db1)[names(db1) %in% names(db2)]
  
  unite_data <- function(data, variable) {
    
    data |> 
      pivot_longer(
        cols = starts_with({{variable}}), 
        names_to = NULL, 
        values_to = variable
      ) |> 
      summarise({{variable}} := str_flatten(unique(.data[[variable]]), collapse = "; "), .by = "rowid")
    
  }
  
  match_out_2 <- map(joined_cols, ~ unite_data(match_out, .)) |> bind_cols() |> 
    select(rowid = 1, all_of(joined_cols))
    
}

# relative matches: 43
rel_match_1 <- get_fl_matches(match_1, db_idis_1, db_adb)
  
# function that returns unmatched rows
get_fl_no_matches <- function(the_matches, db1) {
  no_match_1 <- db1[!rownames(db1) %in% the_matches$inds.a,]
}

# unmatched remaining: 119
db_idis_2 <- get_fl_no_matches(match_1, db_idis_1)
