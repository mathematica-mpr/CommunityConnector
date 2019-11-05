# get specific column names from data dictionary -------------------------------
get_dd <- function(dd, column_type) {
  # column type can be: demographic, outcome, or sdoh_score
  dd %>% filter(eval(parse(text = column_type)) == 1) %>% 
    select(column_name, description, descrip_new, higher_better)
}

# distance function to find my county matches ----------------------------------
find_my_matches <- function(my_county, df, n_matches = 20) {
  # my_county = fips of selected county
  # df = dataframe with all covariates

  my_county_distances <- dist_mat %>% 
    select(distance =  my_county) %>%
    rownames_to_column("fips") %>%
    left_join(df %>% select(fips, county, state), by = "fips")
  
  my_matches <-  my_county_distances %>%
    filter(fips != my_county) %>%
    arrange(distance) %>%
    head(n_matches) %>%
    pull(fips)
  
  list(my_county_distances, my_matches)
  
}

# function to create ranking of outcomes based on my county and my county matches
rank_outcome <- function(data, higher_better) {
  # data = data for specific outcome
  # higher_better flag (0 = lower is better, 1 = higher is better)
  
  my_county_value <- data %>% 
    filter(type == "selected") %>%
    pull(value)
  
  outcome_vals <- data %>% 
    filter(type != "other") %>%
    pull(value)
  
  median_val <- median(outcome_vals)
  
  std_val <- sd(outcome_vals)
  
  if (higher_better) {
    rank <- (median_val - my_county_value) / std_val
  } else {
    rank <- (my_county_value - median_val) / std_val
  }
  rank
}

arrange_rank <- function(data, outcome_sort) {
  if (outcome_sort == 'exceptional') {
    data %>% dplyr::arrange(desc(abs(rank)))
  } else if (outcome_sort == 'best') {
    data %>% dplyr::arrange(rank)
  } else if (outcome_sort == 'worst') {
    data %>% dplyr::arrange(desc(rank))
  }
  
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
