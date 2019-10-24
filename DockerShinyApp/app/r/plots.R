
dat <- read_csv("N:/Transfer/KSkvoretz/AHRQ/data/sample/sample_data.csv", col_types = cols(FIPS = col_character()))
dd <- read_csv("N:/Transfer/KSkvoretz/AHRQ/data/sample/sample_dictionary.csv") %>%
  mutate(
    beg = case_when(
      nchar(description) < 10 ~ description,
      TRUE ~ substr(description, 1, 10)
    ),
    mid = case_when(
      nchar(description) < 10 ~ "",
      TRUE ~ substr(description, 11, 20)
    ),
    mid2 = case_when(
      nchar(description) < 20 ~ "",
      TRUE ~ substr(description, 21, 30)
    ),
    end = case_when(
      nchar(description) < 30 ~ "",
      TRUE ~ substr(description, 31, nchar(description))
    )) %>%
  mutate(mid = sub(" ", "/n", mid),
         mid2 = sub(" ", "/n", mid2),
         end = sub(" ", "/n", end),
         descrip_new = paste0(beg, mid, mid2, end)) 