# distance function to find my county matches

find_my_matches <- function(my_county, df, n_matches = 20) {
  df <- df %>% select(FIPS, starts_with("sdoh_score")) %>%
    column_to_rownames(var = "FIPS")
  distances <- data.frame(as.matrix(dist(df)))
  
  my_matches <- distances %>% 
    select(distance = paste0("X", my_county)) %>%
    rownames_to_column("FIPS") %>%
    filter(FIPS != my_county) %>%
    arrange(distance) %>%
    head(n_matches) %>%
    pull(FIPS)
  
}

