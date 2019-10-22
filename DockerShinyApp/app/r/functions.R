# distance function to find my county matches

find_my_matches <- function(my_county, df, n_matches = 20) {
  # my_county = FIPs of selected county
  df <- df %>% select(FIPS, starts_with("sdoh_score")) %>%
    column_to_rownames(var = "FIPS")
  distances <- data.frame(as.matrix(dist(df)))
  
  my_county_distances <- distances %>% 
    select(distance = paste0("X", my_county)) %>%
    rownames_to_column("FIPS")
  
  my_matches <-  my_county_distances %>%
    filter(FIPS != my_county) %>%
    arrange(distance) %>%
    head(n_matches) %>%
    pull(FIPS)
  
  list(my_county_distances, my_matches)
  
}


make_radar_data <- function(county_df, dd) {
  df <- rbind(rep(1, 6), rep(0, 6),
              # 50% circle color
              rep(.5, 6),
              # 100 % circle color
              rep(1, 6),
              county_df) %>%
    rename_at(vars(dd$column_name), ~ dd$descrip_new)
  df
}
