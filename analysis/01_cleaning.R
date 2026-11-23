# ============================================================
# 01_cleaning.R
# Reads analysis/raw_data.csv, cleans it, writes analysis/dataset.csv
# Run from the project root (open cafe-analysis.Rproj first).
# ============================================================

library(tidyverse)

# ---- 1. Load the raw data (never edited by hand) ----
raw <- read_csv("analysis/raw_data.csv", show_col_types = FALSE)

# ---- 2. Inspect before changing anything ----
dim(raw)                      # rows and columns
glimpse(raw)                  # type R gave each column
colSums(is.na(raw))           # missing values per column

# Empty rows: no café name
sum(is.na(raw$`Cafe name`))

# What do the empty rows contain in the formula columns?
raw %>%
  filter(is.na(`Cafe name`)) %>%
  distinct(`Distance from Pražský Orloj`, Latitude, Longitude)

# Exact duplicate rows
sum(duplicated(raw))

# ---- 3. Remove the empty rows ----
cafes <- raw %>% filter(!is.na(`Cafe name`))
nrow(raw); nrow(cafes)            # 299 -> 200
sum(duplicated(cafes))            # exact duplicates after removing empty rows

# ---- 4. Repeated cafe names: different branches or repeated entries? ----
cafes %>%
  mutate(name_key = str_to_lower(str_squish(`Cafe name`))) %>%
  group_by(name_key) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  arrange(name_key) %>%
  select(name_key, Name, `Cafe address (Google Maps)`, `Cafe Type`) %>%
  print(n = Inf, width = Inf)

# ---- 5. Categories ----
cafes %>% count(`Cafe Type`)      # look for typos, spaces, blanks

# ---- 6. Verify the number-of-ratings conversion ----
raw_chr <- read_csv("analysis/raw_data.csv",
                    col_types = cols(.default = col_character()))
raw_chr %>%
  filter(!is.na(`Cafe name`)) %>%
  filter(!str_detect(`Number of ratings (Google)`, "^[0-9]+$") |
           is.na(`Number of ratings (Google)`)) %>%
  select(`Cafe name`, `Number of ratings (Google)`)

summary(cafes$`Number of ratings (Google)`)   # min/max must make sense
summary(cafes$`Rating (Google)`)              # should lie between 1 and 5

# ---- 7. Duplicates by address, shown in full ----
cafes %>%
  mutate(sheet_row = row_number(),
         addr_key = str_to_lower(str_squish(
           str_remove(`Cafe address (Google Maps)`, ",\\s*Czechia\\s*$")))) %>%
  group_by(addr_key) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  arrange(addr_key, sheet_row) %>%
  select(sheet_row, `Cafe name`, Name, `Date and time of visit`,
         `Rating (Google)`, `Number of ratings (Google)`) %>%
  print(n = Inf, width = Inf)

# ---- 7b. Duplicates the address check misses ----
# The same cafe can be entered with a different spelling and a slightly different
# pin (for example "Cafe Milani" / "Caffe Milani"). Two entries with exactly the
# same number of Google ratings and pins within 50 m of each other are the same cafe.
near_pairs <- function(d) {
  a <- d %>%
    transmute(sheet_row, cafe_name = `Cafe name`, Name,
              n_ratings = `Number of ratings (Google)`,
              lat = as.numeric(Latitude), lon = as.numeric(Longitude)) %>%
    filter(!is.na(n_ratings))
  inner_join(a, a, by = "n_ratings", suffix = c("_1", "_2")) %>%
    filter(sheet_row_1 < sheet_row_2) %>%
    mutate(dist_m = sqrt(((lat_1 - lat_2) * 111000)^2 +
                           ((lon_1 - lon_2) * 111000 * cos(lat_1 * pi / 180))^2)) %>%
    filter(dist_m < 50) %>%
    select(n_ratings, sheet_row_1, cafe_name_1, Name_1,
           sheet_row_2, cafe_name_2, Name_2, dist_m) %>%
    arrange(sheet_row_1)
}
cafes <- cafes %>% mutate(sheet_row = row_number())
near_pairs(cafes) %>% print(n = Inf, width = Inf)

# ---- 8. The odd values ----
cafes %>%
  filter(`Number of ratings (Google)` == 0 | `Rating (Google)` == 5) %>%
  select(`Cafe name`, `Rating (Google)`, `Number of ratings (Google)`)

cafes %>% filter(is.na(`Cafe Type`)) %>% select(`Cafe name`, Name)

# ---- 9. Remove duplicate cafes (decision) ----
# The same cafe was recorded twice by different students (same address or same
# pin, identical or almost identical ratings). Rule: keep the first row in the sheet.
# Found by address (step 7) and by identical rating count + pin within 50 m (step 7b).
# Rows with the same address but different cafes (The Cup / Atomic matcha,
# Cafe Slavia / forbina, Rodinna farma / Kin & K Bakery) are NOT removed.
cafes <- cafes %>% mutate(sheet_row = row_number())

dup_rows <- c(97, 144, 200, 103, 142, 198, 197, 158, 159,   # same address (step 7)
              131, 196)                                     # same ratings and pin (step 7b)

cafes %>%
  filter(sheet_row %in% dup_rows) %>%
  select(sheet_row, `Cafe name`, Name)        # check: these are the 11 second entries

cafes_nodup <- cafes %>% filter(!sheet_row %in% dup_rows)
nrow(cafes); nrow(cafes_nodup)                # 200 -> 189

# ---- 10. Leftover check: no duplicates may remain ----
# Same pin (about 10 m) ...
cafes_nodup %>%
  mutate(pin = paste(round(as.numeric(Latitude), 4),
                     round(as.numeric(Longitude), 4))) %>%
  group_by(pin) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  arrange(pin, sheet_row) %>%
  select(sheet_row, `Cafe name`, Name, `Rating (Google)`,
         `Number of ratings (Google)`) %>%
  print(n = Inf, width = Inf)
# ... and identical rating count within 50 m (this must print 0 rows)
near_pairs(cafes_nodup) %>% print(n = Inf, width = Inf)

# ---- 11. Build the clean variables (cafes_nodup keeps the originals) ----
cafes_clean <- cafes_nodup %>%
  transmute(
    sheet_row,
    cafe_name = str_squish(`Cafe name`),
    # harmonise labels: case, spaces, typos such as "Prage 7" / "prauge 1"
    district  = str_replace(str_to_title(str_squish(`District (number)`)),
                            "^Pra[a-z]+ ", "Prague "),
    cafe_type = factor(`Cafe Type`,
                       levels = c("Independent", "Local Chain", "International Chain")),
    rating    = `Rating (Google)`,
    n_ratings = `Number of ratings (Google)`
  )

# Thresholds are computed in code, never typed in.
# C = mean Google rating of all cafes (needed for the Adjusted Rating).
C        <- mean(cafes_clean$rating, na.rm = TRUE)
median_n <- median(cafes_clean$n_ratings, na.rm = TRUE)
C; median_n

cafes_clean <- cafes_clean %>%
  mutate(
    # shrinkage: (n*R + 50*C) / (n + 50)
    adj_rating = (n_ratings * rating + 50 * C) / (n_ratings + 50),
    ratings_k  = n_ratings / 1000,                 # number of ratings per 1,000
    # A2, A5: a cafe exactly at the median belongs to neither group
    n_group_median = case_when(n_ratings > median_n ~ "Above median",
                               n_ratings < median_n ~ "Below median",
                               .default = NA_character_),
    # A4: few (<100), medium (100-1,000), many (>1,000)
    n_group_3 = factor(case_when(n_ratings < 100   ~ "Few (<100)",
                                 n_ratings <= 1000 ~ "Medium (100-1,000)",
                                 n_ratings > 1000  ~ "Many (>1,000)"),
                       levels = c("Few (<100)", "Medium (100-1,000)", "Many (>1,000)")),
    # A9: under 100 vs 100 or more
    n_group_2 = factor(case_when(n_ratings < 100  ~ "Under 100",
                                 n_ratings >= 100 ~ "100 or more"),
                       levels = c("Under 100", "100 or more")),
    # A5, A10: top-rated = rating of 4.7 or higher (1/0)
    top_rated = as.integer(rating >= 4.7)
  )

# ---- 12. Check the result ----
dim(cafes_clean)
colSums(is.na(cafes_clean))
count(cafes_clean, district)
count(cafes_clean, cafe_type)
count(cafes_clean, n_group_median)
count(cafes_clean, n_group_3)
count(cafes_clean, n_group_2)
count(cafes_clean, top_rated)
summary(cafes_clean %>% select(rating, n_ratings, adj_rating))

# Adjusted vs raw rating for the cafes with the fewest ratings
cafes_clean %>%
  filter(!is.na(adj_rating)) %>%
  arrange(n_ratings) %>%
  select(cafe_name, rating, n_ratings, adj_rating) %>%
  slice_head(n = 6)

# ---- 13. Write the cleaned data (no row numbers) ----
write_csv(cafes_clean, "analysis/dataset.csv")