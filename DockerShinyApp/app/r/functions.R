# get specific column names from data dictionary -------------------------------
get_dd <- function(dd, column_type) {
  # column type can be: demographic, outcome, or sdoh_score
  dd %>% filter(eval(parse(text = column_type)) == 1) %>% 
    select(column_name, description, descrip_new)
}

# distance function to find my county matches

find_my_matches <- function(my_county, df, n_matches = 20) {
  # my_county = fips of selected county
  df_score <- df %>% select(fips, starts_with("sdoh_score")) %>%
    column_to_rownames(var = "fips")
  distances <- data.frame(as.matrix(dist(df_score)))
  
  my_county_distances <- distances %>% 
    select(distance = paste0("X", my_county)) %>%
    rownames_to_column("fips") %>%
    left_join(df %>% select(fips, county), by = "fips")
  
  my_matches <-  my_county_distances %>%
    filter(fips != my_county) %>%
    arrange(distance) %>%
    head(n_matches) %>%
    pull(fips)
  
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


make_density_graph <- function(data) {
  ggplot(data, aes(x=value)) + geom_density() + 
    geom_vline(data = filter(data, type != "other"),
               aes(xintercept = value, color = as.factor(type))) +
    ggtitle(first(str_wrap(data$description, 80)))
}
