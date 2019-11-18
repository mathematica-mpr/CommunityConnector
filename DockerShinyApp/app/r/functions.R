# get specific column names from data dictionary -------------------------------
get_dd <- function(dd, column_type) {
  # column type can be: demographic, outcome, or sdoh_score
  dd %>% filter(eval(parse(text = column_type)) == 1) %>% 
    select(column_name, description, descrip_new, higher_better)
}

get_table_data <- function(data, dd, column_type, county) {
  sub_dd <- get_dd(dd, column_type)
  
  dd_cols <- sub_dd %>% pull(column_name)
  
  county <- data$county
  
  df <- data %>% select(dd_cols) %>%
    rename_at(vars(sub_dd$column_name), ~ sub_dd$description) %>%
    pivot_longer(cols = sub_dd$description) %>%
    rename(!!(county) := value )
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
      name = paste0(df$county, ", ", df$state),
      hovertemplate = ~paste('<b>Category</b>: %{theta}',
                             '<br><b>Score</b>: %{r:.2f}',
                             '<extra></extra>')
    ) %>% 
    layout(
      title = list(
        text = paste0(df$county, ", ", df$state),
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
      name = paste0(df2$county, ", ", df2$state)
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
      name = paste0(df1$county, ", ", df1$state)
    ) %>% 
    layout(
      title = list(
        text = paste0(df1$county, ", ", df1$state),
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

# multiple radar chart grid ----------------------------------------------------
# !! a lot of this is hard coded and expects only 20 comparison counties !!
grid_radar <- function(df, dd, n_matches = 20, t = .007) {
  # get labels for sdohs
  radar_names <- get_dd(dd, "sdoh_score") %>% 
    dplyr::pull(descrip_new)
  radar_names <- append(radar_names, radar_names[1])
  
  # !! hard coding sdoh names as abbreviations
  radar_names <- c("ES", "NPE", "E", "F", "C", "HC", "ES")
  
  # parameters
  n_rows <- ceiling(n_matches / 4) # exec decision to make 4 columns
  
  # this should go outside of this function since it applies to all radar graphs
  radialaxis_list <- list(
    range = c(0,1),
    angle = 90,
    tickangle = 90,
    dtick = .5, 
    tickwidth = 2,
    tickfont = list(size = 8)
  )
  
  angularaxis_list <- list(
    tickfont = list(size =  10),
    rotation = 0
  )
  
  hoverlabel_list <- list(
    namelength = -1
  )
  
  margin_list <- list(
    t = 25
  ) 
  
  # get first county
  df1 <- df[1,]
  points1 <- select(df1, starts_with("sdoh"))
  points1 <- append(points1, points1[1]) %>% 
    unlist()
  
  # create first county radar chart
  p <- plot_ly(width = 800, height = 800) %>% 
    add_trace(
      type = 'scatterpolar',
      mode = 'markers+lines',
      r = points1,
      theta = radar_names,
      #aesthetics
      fill = 'toself',
      fillcolor = paste0(config$colors$teal100, "CC"),
      line = list(dash = "solid", 
                  color = paste0(config$colors$green100), 
                  width = .8, 
                  shape = 'spline', 
                  smoothing = .9),
      marker = list(size = 7,
                    color = paste0(config$colors$green100),
                    opacity = 1),
      opacity = .9,
      #hover label
      hoverinfo = 'text',
      text = paste0(df1$county, ", ", df1$state)
    ) %>% 
    layout(  
      polar = list(
        domain = list(
          x = c(0 + t, (1 / 4) - t),
          y = c(1 - (1 / n_rows) + t, 1 - t)
        ),
        radialaxis = radialaxis_list,
        angularaxis = angularaxis_list
      ),
      hoverlabel = hoverlabel_list,
      title = list(
        text = 'Scores for Matching Counties'
      ),
      margin = list(t = 30)
    ) 
  
  # create all subsequent radar charts
  for (i in 2:dim(df)[1]) {
    df_one <- df[i,]
    radar_points <- select(df_one, starts_with("sdoh"))
    radar_points <- append(radar_points, radar_points[1]) %>% 
      unlist()
    
    p <- p %>%  
      add_trace(
        type = 'scatterpolar',
        mode = 'markers+lines',
        r = radar_points,
        theta = radar_names,
        #aesthetics
        fill = 'toself',
        fillcolor = paste0(config$colors$teal100, "CC"),
        line = list(dash = "solid", 
                    color = paste0(config$colors$green100), 
                    width = .8, 
                    shape = 'spline', 
                    smoothing = .9),
        marker = list(size = 7,
                      color = paste0(config$colors$green100),
                      opacity = 1),
        opacity = .9,
        #hover label
        hoverinfo = 'text',
        text = paste0(df_one$county, ", ", df_one$state),
        subplot = paste0('polar', i)
      ) 
  }
  
  # hard coding positions of the 20 plots
  p <- p %>% 
    layout(
      polar2 = list(
        domain = list(
          x = c((1/4) + t, (2 / 4) - t),
          y = c(1 - (1 / n_rows) + t, 1 - t)
        ),
        radialaxis = radialaxis_list,
        angularaxis = angularaxis_list
      ),
      hoverlabel = hoverlabel_list
    ) %>% 
    layout(
      polar3 = list(
        domain = list(
          x = c((2/4) + t, (3 / 4) - t),
          y = c(1 - (1 / n_rows) + t, 1 - t)
        ),
        radialaxis = radialaxis_list,
        angularaxis = angularaxis_list
      ),
      hoverlabel = hoverlabel_list
    ) %>% 
    layout(
      polar4 = list(
        domain = list(
          x = c((3/4) + t, (4 / 4) - t),
          y = c(1 - (1 / n_rows) + t, 1 - t)
        ),
        radialaxis = radialaxis_list,
        angularaxis = angularaxis_list
      ),
      hoverlabel = hoverlabel_list
    ) %>%  
    layout(
      polar5 = list(
        domain = list(
          x = c(0 + t, (1 / 4) - t),
          y = c(1 - (2 / n_rows) + t, 1 - (1 / n_rows) -  t)
        ),
        radialaxis = radialaxis_list,
        angularaxis = angularaxis_list
      ),
      hoverlabel = hoverlabel_list
    ) %>%
    layout(
      polar6 = list(
        domain = list(
          x = c((1/4) + t, (2 / 4) - t),
          y = c(1 - (2 / n_rows) + t, 1 - (1 / n_rows) -  t)
        ),
        radialaxis = radialaxis_list,
        angularaxis = angularaxis_list
      ),
      hoverlabel = hoverlabel_list
    ) %>% 
    layout(
      polar7 = list(
        domain = list(
          x = c((2/4) + t, (3 / 4) - t),
          y = c(1 - (2 / n_rows) + t, 1 - (1 / n_rows) -  t)
        ),
        radialaxis = radialaxis_list,
        angularaxis = angularaxis_list
      ),
      hoverlabel = hoverlabel_list
    ) %>% 
    layout(
      polar8 = list(
        domain = list(
          x = c((3/4) + t, (4 / 4) - t),
          y = c(1 - (2 / n_rows) + t, 1 - (1 / n_rows) -  t)
        ),
        radialaxis = radialaxis_list,
        angularaxis = angularaxis_list
      ),
      hoverlabel = hoverlabel_list
    ) %>%  
    layout(
      polar9 = list(
        domain = list(
          x = c(0 + t, (1 / 4) - t),
          y = c(1 - (3 / n_rows) + t, 1 - (2 / n_rows) -  t)
        ),
        radialaxis = radialaxis_list,
        angularaxis = angularaxis_list
      ),
      hoverlabel = hoverlabel_list
    ) %>%
    layout(
      polar10 = list(
        domain = list(
          x = c((1/4) + t, (2 / 4) - t),
          y = c(1 - (3 / n_rows) + t, 1 - (2 / n_rows) -  t)
        ),
        radialaxis = radialaxis_list,
        angularaxis = angularaxis_list
      ),
      hoverlabel = hoverlabel_list
    ) %>% 
    layout(
      polar11 = list(
        domain = list(
          x = c((2/4) + t, (3 / 4) - t),
          y = c(1 - (3 / n_rows) + t, 1 - (2 / n_rows) -  t)
        ),
        radialaxis = radialaxis_list,
        angularaxis = angularaxis_list
      ),
      hoverlabel = hoverlabel_list
    ) %>% 
    layout(
      polar12 = list(
        domain = list(
          x = c((3/4) + t, (4 / 4) - t),
          y = c(1 - (3 / n_rows) + t, 1 - (2 / n_rows) -  t)
        ),
        radialaxis = radialaxis_list,
        angularaxis = angularaxis_list
      ),
      hoverlabel = hoverlabel_list
    ) %>%  
    layout(
      polar13 = list(
        domain = list(
          x = c(0 + t, (1 / 4) - t),
          y = c(1 - (4 / n_rows) + t, 1 - (3 / n_rows) -  t)
        ),
        radialaxis = radialaxis_list,
        angularaxis = angularaxis_list
      ),
      hoverlabel = hoverlabel_list
    ) %>%
    layout(
      polar14 = list(
        domain = list(
          x = c((1/4) + t, (2 / 4) - t),
          y = c(1 - (4 / n_rows) + t, 1 - (3 / n_rows) -  t)
        ),
        radialaxis = radialaxis_list,
        angularaxis = angularaxis_list
      ),
      hoverlabel = hoverlabel_list
    ) %>% 
    layout(
      polar15 = list(
        domain = list(
          x = c((2/4) + t, (3 / 4) - t),
          y = c(1 - (4 / n_rows) + t, 1 - (3 / n_rows) -  t)
        ),
        radialaxis = radialaxis_list,
        angularaxis = angularaxis_list
      ),
      hoverlabel = hoverlabel_list
    ) %>% 
    layout(
      polar16 = list(
        domain = list(
          x = c((3/4) + t, (4 / 4) - t),
          y = c(1 - (4 / n_rows) + t, 1 - (3 / n_rows) -  t)
        ),
        radialaxis = radialaxis_list,
        angularaxis = angularaxis_list
      ),
      hoverlabel = hoverlabel_list
    )  %>%  
    layout(
      polar17 = list(
        domain = list(
          x = c(0 + t, (1 / 4) - t),
          y = c(1 - (5 / n_rows) + t, 1 - (4 / n_rows) -  t)
        ),
        radialaxis = radialaxis_list,
        angularaxis = angularaxis_list
      ),
      hoverlabel = hoverlabel_list
    ) %>%
    layout(
      polar18 = list(
        domain = list(
          x = c((1/4) + t, (2 / 4) - t),
          y = c(1 - (5 / n_rows) + t, 1 - (4 / n_rows) -  t)
        ),
        radialaxis = radialaxis_list,
        angularaxis = angularaxis_list
      ),
      hoverlabel = hoverlabel_list
    ) %>% 
    layout(
      polar19 = list(
        domain = list(
          x = c((2/4) + t, (3 / 4) - t),
          y = c(1 - (5 / n_rows) + t, 1 - (4 / n_rows) -  t)
        ),
        radialaxis = radialaxis_list,
        angularaxis = angularaxis_list
      ),
      hoverlabel = hoverlabel_list
    ) %>% 
    layout(
      polar20 = list(
        domain = list(
          x = c((3/4) + t, (4 / 4) - t),
          y = c(1 - (5 / n_rows) + t, 1 - (4 / n_rows) -  t)
        ),
        radialaxis = radialaxis_list,
        angularaxis = angularaxis_list
      ),
      hoverlabel = hoverlabel_list
    ) %>% 
    layout(showlegend = F,
           grid = list(pattern = 'independent', 
                       domain = list(x = c(0,.1), y = c(0,.1)))
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

#density plot overlay function-------------------
density_plot <- function(data) {
  #function to output density plot for specific outcome
  
  #finding densities
  density_all <- density(data$value)
  density_matches <- filter(data, type == 'matches') %>% 
    pull(value) %>% 
    density()
  #Density Plot
  p <- plot_ly() %>%
    #Density plot for All Counties
    add_trace(
      type = 'scatter',
      mode = 'lines',
      x = ~density_all$x,
      y = ~density_all$y,
      line = list(
        color = paste0(config$colors$grey100),
        width = 2
      ),
      fill = 'tozeroy',
      fillcolor = paste0(config$colors$grey100, '70'),
      name = "Density Plot Of\nAll Counties",
      hoverinfo = 'name'
    ) %>% 
    #Density plot for Matching Counties
    add_trace(
      type = 'scatter',
      mode = 'lines',
      x = ~density_matches$x,
      y = ~density_matches$y,
      fill = 'tozeroy',
      fillcolor = paste0(config$colors$teal100, '65'),
      line = list(
        color = paste0(config$colors$teal100), 
        width = 2
      ),
      name = 'Density Plot of\nMatching Counties',
      hoverinfo = 'name'
    ) %>% 
    #Markers for Matching Counties
    add_trace(
      type = 'scatter',
      mode = 'markers+lines',
      x = filter(data, type == 'matches')$value,
      y = 0, 
      marker = list(
        symbol = 'diamond',
        color = paste0(config$colors$teal100),
        opacity = .8,
        size = 17,
        line = list(
          width = 1,
          color = paste0(config$colors$white100)
        )
      ),
      line = list(
        width = 0
      ),
      text = filter(data, type == 'matches')$county,
      hoverinfo = 'text',
      cliponaxis = F
    ) %>% 
    #Markers for my County
    add_trace(
      type = 'scatter',
      mode = 'markers+lines',
      x = filter(data, type == 'selected')$value,
      y = 0,
      marker = list(
        symbol = 'diamond',
        color = paste0(config$colors$yellow125),
        opacity = 1,
        size = 17,
        line = list(
          width = 1, 
          color = paste0(config$colors$yellow125)
        )
      ),
      text = filter(data, type == 'selected')$county,
      hoverinfo = 'text',
      cliponaxis = F
    ) %>% 
    layout(
      title = list(
        text = paste(data$description[1]),
        font = list(
          size = 18,
          color = paste0(config$colors$purple100)
        ),
        xref = 'paper',
        x = '0'
      ),
      hoverlabel = list(
        namelength = 40
      ),
      #Line for My County
      shapes = list(
        type = 'line',
        xref = 'x',
        yref = 'y',
        x0 = filter(data, type == 'selected')$value,
        x1 = filter(data, type == 'selected')$value,
        y0 = 0,
        y1 = max(density_all$y, density_matches$y)*.05 + max(density_all$y, density_matches$y),
        line = list(
          color = paste0(config$colors$yellow125),
          width = 3,
          dash = 'longdash'
        )
      ),
      xaxis = list(
        title = "",
        showgrid = F,
        zeroline = T
      ),
      yaxis = list(
        title = "Relative Frequency",
        showgrid = F,
        showline = T, 
        range = c(0, max(density_all$y, density_matches$y)*.05 + max(density_all$y, density_matches$y))
      ),
      showlegend = F
    )
  return(p)
}
