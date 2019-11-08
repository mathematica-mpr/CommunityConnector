# get specific column names from data dictionary -------------------------------
get_dd <- function(dd, column_type) {
  # column type can be: demographic, outcome, or sdoh_score
  dd %>% filter(eval(parse(text = column_type)) == 1) %>% 
    select(column_name, description, descrip_new, higher_better)
}

get_table_data <- function(data, dd, column_type) {
  sub_dd <- get_dd(dd, column_type)
  
  dd_cols <- sub_dd %>% pull(column_name)
  
  df <- data %>% select(dd_cols) %>%
    rename_at(vars(sub_dd$column_name), ~ sub_dd$description) %>%
    pivot_longer(cols = sub_dd$description)
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

# function for one county radar plot -------------------------------------------
radar_chart <- function(df, dictionary) {
  # df is my county; columns: county, state, sdoh_score 1:6
  # dictionary is data dictionary
  
  #vector of score names
  radar_names <- get_dd(dictionary, "sdoh_score") %>% 
    dplyr::pull(3)
  radar_names <- append(radar_names, radar_names[1])
  #vector of score values
  radar_points <- select(df, starts_with("sdoh"))
  radar_points <- append(radar_points, radar_points[1]) %>% 
    unlist()
  
  #plotting radar chart
  p <- plot_ly(
  ) %>% 
    add_trace(
      type = 'scatterpolar',
      mode = 'markers+lines',
      r = radar_points,
      theta = radar_names,
      #aesthetics
      fill = 'toself',
      fillcolor = paste0(config$colors$yellow50),
      line = list(dash = "solid", 
                  color = paste0(config$colors$red100), 
                  width = .8, 
                  shape = 'spline', 
                  smoothing = .9),
      marker = list(size = 7,
                    color = paste0(config$colors$red100),
                    opacity = 1),
      opacity = .9,
      #hover label
      name = paste(df$county, df$state),
      hovertemplate = ~paste('<b>Category</b>: %{theta}',
                             '<br><b>Score</b>: %{r:.2f}',
                             '<extra></extra>')
    ) %>% 
    layout(
      title = list(
        text = paste(df$county, df$state),
        font = list(
          size = 18
        ),
        xref = 'paper'
      ),
      polar = list(
        #tick labels
        radialaxis = list(
          range = c(0,1),
          angle = 90,
          tickangle = 90,
          dtick = .5, 
          tickwidth = 2,
          tickfont = list(size = 10)
        ),
        #category labels
        angularaxis = list(
          tickfont = list(size =  12),
          rotation = 0
        ),
        bgcolor = paste0(config$colors$tan25, '50')
      ),
      #hover label aesthetics
      hoverlabel = list(
        bordercolor = paste0(config$colors$black, '100'),
        bgcolor = paste0(config$colors$red50)
      ),
      margin = list(t=70)
    )
  return(p)
}

# function for two county radar plot -------------------------------------------
radar_chart_overlay <- function(df1, df2, dictionary) {
  # df1 is my county; columns: county, state, sdoh_score 1:6
  # df2 is comparison county, same columns as df1
  # dictionary is data dictionary
  
  #vector of score names
  radar_names <- get_dd(dictionary, "sdoh_score") %>% 
    dplyr::pull(3)
  radar_names <- append(radar_names, radar_names[1])
  #vector of score values
  radar_points1 <- select(df1, starts_with("sdoh"))
  radar_points1 <- append(radar_points1, radar_points1[1]) %>% 
    unlist()
  radar_points2 <- select(df2, starts_with("sdoh"))
  radar_points2 <- append(radar_points2, radar_points2[1]) %>% 
    unlist()
  
  #plotting radar chart
  p <- plot_ly(
  ) %>%     
    add_trace(
      type = 'scatterpolar',
      mode = 'markers+lines',
      r = radar_points2,
      theta = radar_names,
      fill = "toself",
      fillcolor = paste0(config$colors$teal100, "CC"),
      line = list(dash = "solid", 
                  color = paste0(config$colors$green100), 
                  width = .8, 
                  shape = 'spline', 
                  smoothing = .9),
      marker = list(size = 7,
                    color = paste0(config$colors$green100),
                    opacity = 1),
      opacity = .6,
      hoverinfo = 'none',
      name = paste(df2[1,1], "County")
    ) %>% 
    add_trace(
      type = 'scatterpolar',
      mode = 'markers+lines',
      r = radar_points1,
      theta = radar_names,
      #aesthetics
      fill = 'toself',
      fillcolor = paste0(config$colors$yellow50, "CC"),
      line = list(dash = "solid", 
                  color = paste0(config$colors$red100), 
                  width = .8, 
                  shape = 'spline', 
                  smoothing = .9),
      marker = list(size = 7,
                    color = paste0(config$colors$red100),
                    opacity = 1),
      opacity = .9,
      #hover label
      hoverinfo = 'none',
      name = paste(df1[1,1], "County")
    ) %>% 
    layout(
      title = list(
        text = paste(df2[1,1], "County,", df2[1,2]),
        font = list(
          size = 18
        ),
        xref = 'paper'
      ),
      polar = list(
        #tick labels
        radialaxis = list(
          range = c(0,1),
          angle = 90,
          tickangle = 90,
          dtick = .5, 
          tickwidth = 2,
          tickfont = list(size = 10)
        ),
        #category labels
        angularaxis = list(
          tickfont = list(size =  12),
          rotation = 0
        ),
        bgcolor = paste0(config$colors$tan25, '50')
      ),
      #hover label aesthetics
      hoverlabel = list(
        bordercolor = paste0(config$colors$black, '100'),
        bgcolor = paste0(config$colors$red50)
      ),
      margin = list(t=70),
      showlegend = T
    )
  return(p)
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
