# format function for demographic tables ---------------------------------------
format_dat <- function(cols) {
  round(cols, digits = config$formatting$digits)
}

format_table_dat <- function(col, description){
  col <- format(col, nsmall = config$formatting$digit, big.mark = ",")
  col <- case_when(grepl("%", description) ~ paste0(col, "%"),
                   grepl("[$]", description) ~ paste0("$", col),
                   TRUE ~ col)
  return(col)
}

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
    mutate(value = format_table_dat(value, name)) %>% 
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

# function to filter outcomes based on selection
filter_category <- function(data, outcome_filter) {
  if (outcome_filter == "diabetes") {
    data %>% filter(grepl("diab", unique(column_name)))
  } else if (outcome_filter == "kidney") {
    data %>% filter(grepl("kidney", unique(column_name)))
  } else if (outcome_filter == "obesity") {
    data %>% filter(grepl("obes", unique(column_name)))
  } else {
    data
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
      hovertemplate = paste("<b>%{theta} Score:</b>", 
                            "<br>%{r:.2f}",
                            '<extra></extra>'),
      name = paste0(df$county, ", ", df$state)
    ) %>% 
    layout(
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
        bgcolor = paste0(config$colors$red50),
        namelength = -1
      ),
      margin = list(t=70),
      showlegend = T,
      dragmode = F
    )
  return(p %>% 
           config(displaylogo = FALSE,
                  modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "autoScale2d", 
                                             "hoverClosestCartesian", "hoverCompareCartesian", "toggleSpikelines")))
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
      fillcolor = paste0(config$colors$teal100, "70"),
      line = list(dash = "solid", 
                  color = paste0(config$colors$teal100), 
                  width = .8, 
                  shape = 'spline', 
                  smoothing = .9),
      marker = list(size = 7,
                    color = paste0(config$colors$teal100),
                    opacity = 1),
      #opacity = .6,
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
      showlegend = T,
      dragmode = F
    )
  return(p %>% 
           config(displaylogo = FALSE,
                  modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "autoScale2d", 
                                             "hoverClosestCartesian", "hoverCompareCartesian", "toggleSpikelines")))
}

# multiple radar chart grid ----------------------------------------------------
# !! a lot of this is hard coded and expects only 20 comparison counties !!
grid_radar <- function(df, dd, n_matches = 20, t = .003, ty = .025, txa = .125) {
  # get labels for sdohs
  radar_names <- get_dd(dd, "sdoh_score") %>% 
    dplyr::pull(descrip_new)
  radar_names <- append(radar_names, radar_names[1])
  
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
    showticklabels = F,
    rotation = 0
  )
  
  annotations_list <- list(
    xref = 'paper',
    yref = 'paper',
    xanchor = 'center',
    yanchor = 'bottom',
    yshift = 4,
    showarrow = F,
    font = list(size = 11)
  )
  
  # get first county
  df1 <- df[1,]
  points1 <- select(df1, starts_with("sdoh"))
  points1 <- append(points1, points1[1]) %>% 
    unlist()
  
  # create first county radar chart
  p <- plot_ly(width = 650, height = 750) %>% 
    add_trace(
      type = 'scatterpolar',
      mode = 'markers+lines',
      r = points1,
      theta = radar_names,
      #aesthetics
      fill = 'toself',
      fillcolor = paste0(config$colors$teal100, "CC"),
      line = list(dash = "solid", 
                  color = paste0(config$colors$teal100), 
                  width = .8, 
                  shape = 'spline', 
                  smoothing = .9),
      marker = list(size = 7,
                    color = paste0(config$colors$teal100),
                    opacity = 1),
      hovertemplate = paste("<b>%{theta} Score:</b>", 
                            "<br>%{r:.2f}",
                            '<extra></extra>'),
      name = paste0(df1$county, ", ", df1$state)
    ) %>% 
    layout(  
      polar = list(
        domain = list(
          x = c(0 + t, (1 / 4) - t),
          y = c(1 - (1 / n_rows) + ty, 1 - ty)
        ),
        radialaxis = radialaxis_list,
        angularaxis = angularaxis_list
      ),
      hoverlabel = list(
        namelength = -1,
        bgcolor = paste0(config$colors$teal100)
      ),
      margin = list(t = 7, l = 1, r = 7),
      annotations = c(list(
        x = 0 + t + txa,
        y = 1 - ty,
        text = paste0("<b>", df1$county, ", ", df1$state, "</b>")
      ), annotations_list),
      dragmode = F
    ) 
  
  # create all subsequent radar charts
  grid_titles <- vector()
  for (i in 2:dim(df)[1]) {
    df_one <- df[i,]
    radar_points <- select(df_one, starts_with("sdoh"))
    radar_points <- append(radar_points, radar_points[1]) %>% 
      unlist()
    
    grid_titles[i] <- paste0("<b>", df_one$county, ", ", df_one$state, "</b>")
    
    p <- p %>%  
      add_trace(
        type = 'scatterpolar',
        mode = 'markers+lines+text',
        r = radar_points,
        theta = radar_names,
        #aesthetics
        fill = 'toself',
        fillcolor = paste0(config$colors$teal100, "CC"),
        opacity = .85,
        line = list(dash = "solid", 
                    color = paste0(config$colors$teal100), 
                    width = .8, 
                    shape = 'spline', 
                    smoothing = .9),
        marker = list(size = 7,
                      color = paste0(config$colors$teal100),
                      opacity = 1),
        hovertemplate = paste("<b>%{theta} Score:</b>", 
                              "<br>%{r:.2f}",
                              '<extra></extra>'),
        name = paste0(df_one$county, ", ", df_one$state),
        subplot = paste0('polar', i)
      ) 
  }
  
  # hard coding positions of the 20 plots
  p <- p %>% 
    layout(
      polar2 = list(
        domain = list(
          x = c((1/4) + t, (2 / 4) - t),
          y = c(1 - (1 / n_rows) + ty, 1 - ty)
        ),
        radialaxis = radialaxis_list,
        angularaxis = angularaxis_list
      ),
      annotations = c(list(
        x = (1/4) + t + txa,
        y = 1 - ty,
        text = grid_titles[2]
      ), annotations_list)
    ) %>% 
    layout(
      polar3 = list(
        domain = list(
          x = c((2/4) + t, (3 / 4) - t),
          y = c(1 - (1 / n_rows) + ty, 1 - ty)
        ),
        radialaxis = radialaxis_list,
        angularaxis = angularaxis_list
      ),
      annotations = c(list(
        x = (2/4) + t + txa,
        y = 1 - ty,
        text = grid_titles[3]
      ), annotations_list)
    ) %>% 
    layout(
      polar4 = list(
        domain = list(
          x = c((3/4) + t, (4 / 4) - t),
          y = c(1 - (1 / n_rows) + ty, 1 - ty)
        ),
        radialaxis = radialaxis_list,
        angularaxis = angularaxis_list
      ),
      annotations = c(list(
        x = (3/4) + t + txa,
        y = 1 - ty,
        text = grid_titles[4]
      ), annotations_list)
    ) %>%  
    layout(
      polar5 = list(
        domain = list(
          x = c(0 + t, (1 / 4) - t),
          y = c(1 - (2 / n_rows) + ty, 1 - (1 / n_rows) -  ty)
        ),
        radialaxis = radialaxis_list,
        angularaxis = angularaxis_list
      ),
      annotations = c(annotations_list, list(
        x = 0 + t + txa,
        y = 1 - (1 / n_rows) -  ty,
        text = grid_titles[5]
      ))
    ) %>%
    layout(
      polar6 = list(
        domain = list(
          x = c((1/4) + t, (2 / 4) - t),
          y = c(1 - (2 / n_rows) + ty, 1 - (1 / n_rows) -  ty)
        ),
        radialaxis = radialaxis_list,
        angularaxis = angularaxis_list
      ),
      annotations = c(list(
        x = (1/4) + t + txa,
        y = 1 - (1 / n_rows) -  ty,
        text = grid_titles[6]
      ), annotations_list)
    ) %>% 
    layout(
      polar7 = list(
        domain = list(
          x = c((2/4) + t, (3 / 4) - t),
          y = c(1 - (2 / n_rows) + ty, 1 - (1 / n_rows) -  ty)
        ),
        radialaxis = radialaxis_list,
        angularaxis = angularaxis_list
      ),
      annotations = c(list(
        x = (2/4) + t + txa,
        y = 1 - (1 / n_rows) -  ty,
        text = grid_titles[7]
      ), annotations_list)
    ) %>% 
    layout(
      polar8 = list(
        domain = list(
          x = c((3/4) + t, (4 / 4) - t),
          y = c(1 - (2 / n_rows) + ty, 1 - (1 / n_rows) -  ty)
        ),
        radialaxis = radialaxis_list,
        angularaxis = angularaxis_list
      ),
      annotations = c(list(
        x = (3/4) + t + txa,
        y = 1 - (1 / n_rows) -  ty,
        text = grid_titles[8]
      ), annotations_list)
    ) %>%  
    layout(
      polar9 = list(
        domain = list(
          x = c(0 + t, (1 / 4) - t),
          y = c(1 - (3 / n_rows) + ty, 1 - (2 / n_rows) -  ty)
        ),
        radialaxis = radialaxis_list,
        angularaxis = angularaxis_list
      ),
      annotations = c(list(
        x = 0 + t + txa,
        y = 1 - (2 / n_rows) -  ty,
        text = grid_titles[9]
      ), annotations_list)
    ) %>%
    layout(
      polar10 = list(
        domain = list(
          x = c((1/4) + t, (2 / 4) - t),
          y = c(1 - (3 / n_rows) + ty, 1 - (2 / n_rows) -  ty)
        ),
        radialaxis = radialaxis_list,
        angularaxis = angularaxis_list
      ),
      annotations = c(list(
        x = (1/4) + t + txa,
        y = 1 - (2 / n_rows) -  ty,
        text = grid_titles[10]
      ), annotations_list)
    ) %>% 
    layout(
      polar11 = list(
        domain = list(
          x = c((2/4) + t, (3 / 4) - t),
          y = c(1 - (3 / n_rows) + ty, 1 - (2 / n_rows) -  ty)
        ),
        radialaxis = radialaxis_list,
        angularaxis = angularaxis_list
      ),
      annotations = c(list(
        x = (2/4) + t + txa,
        y = 1 - (2 / n_rows) -  ty,
        text = grid_titles[11]
      ), annotations_list)
    ) %>% 
    layout(
      polar12 = list(
        domain = list(
          x = c((3/4) + t, (4 / 4) - t),
          y = c(1 - (3 / n_rows) + ty, 1 - (2 / n_rows) -  ty)
        ),
        radialaxis = radialaxis_list,
        angularaxis = angularaxis_list
      ),
      annotations = c(list(
        x = (3/4) + t + txa,
        y = 1 - (2 / n_rows) -  ty,
        text = grid_titles[12]
      ), annotations_list)
    ) %>%  
    layout(
      polar13 = list(
        domain = list(
          x = c(0 + t, (1 / 4) - t),
          y = c(1 - (4 / n_rows) + ty, 1 - (3 / n_rows) -  ty)
        ),
        radialaxis = radialaxis_list,
        angularaxis = angularaxis_list
      ),
      annotations = c(list(
        x = 0 + t + txa,
        y = 1 - (3 / n_rows) -  ty,
        text = grid_titles[13]
      ), annotations_list)
    ) %>%
    layout(
      polar14 = list(
        domain = list(
          x = c((1/4) + t, (2 / 4) - t),
          y = c(1 - (4 / n_rows) + ty, 1 - (3 / n_rows) -  ty)
        ),
        radialaxis = radialaxis_list,
        angularaxis = angularaxis_list
      ),
      annotations = c(list(
        x = (1/4) + t + txa,
        y = 1 - (3 / n_rows) -  ty,
        text = grid_titles[14]
      ), annotations_list)
    ) %>% 
    layout(
      polar15 = list(
        domain = list(
          x = c((2/4) + t, (3 / 4) - t),
          y = c(1 - (4 / n_rows) + ty, 1 - (3 / n_rows) -  ty)
        ),
        radialaxis = radialaxis_list,
        angularaxis = angularaxis_list
      ),
      annotations = c(list(
        x = (2/4) + t + txa,
        y = 1 - (3 / n_rows) -  ty,
        text = grid_titles[15]
      ), annotations_list)
    ) %>% 
    layout(
      polar16 = list(
        domain = list(
          x = c((3/4) + t, (4 / 4) - t),
          y = c(1 - (4 / n_rows) + ty, 1 - (3 / n_rows) -  ty)
        ),
        radialaxis = radialaxis_list,
        angularaxis = angularaxis_list
      ),
      annotations = c(list(
        x = (3/4) + t + txa,
        y = 1 - (3 / n_rows) -  ty,
        text = grid_titles[16]
      ), annotations_list)
    )  %>%  
    layout(
      polar17 = list(
        domain = list(
          x = c(0 + t, (1 / 4) - t),
          y = c(1 - (5 / n_rows) + ty, 1 - (4 / n_rows) -  ty)
        ),
        radialaxis = radialaxis_list,
        angularaxis = angularaxis_list
      ),
      annotations = c(list(
        x = 0 + t + txa,
        y = 1 - (4 / n_rows) -  ty,
        text = grid_titles[17]
      ), annotations_list)
    ) %>%
    layout(
      polar18 = list(
        domain = list(
          x = c((1/4) + t, (2 / 4) - t),
          y = c(1 - (5 / n_rows) + ty, 1 - (4 / n_rows) -  ty)
        ),
        radialaxis = radialaxis_list,
        angularaxis = angularaxis_list
      ),
      annotations = c(list(
        x = (1/4) + t + txa,
        y = 1 - (4 / n_rows) -  ty,
        text = grid_titles[18]
      ), annotations_list)
    ) %>% 
    layout(
      polar19 = list(
        domain = list(
          x = c((2/4) + t, (3 / 4) - t),
          y = c(1 - (5 / n_rows) + ty, 1 - (4 / n_rows) -  ty)
        ),
        radialaxis = radialaxis_list,
        angularaxis = angularaxis_list
      ),
      annotations = c(list(
        x = (2/4) + t + txa,
        y = 1 - (4 / n_rows) -  ty,
        text = grid_titles[19]
      ), annotations_list)
    ) %>% 
    layout(
      polar20 = list(
        domain = list(
          x = c((3/4) + t, (4 / 4) - t),
          y = c(1 - (5 / n_rows) + ty, 1 - (4 / n_rows) -  ty)
        ),
        radialaxis = radialaxis_list,
        angularaxis = angularaxis_list
      ),
      annotations = c(list(
        x = (3/4) + t + txa,
        y = 1 - (4 / n_rows) -  ty,
        text = grid_titles[20]
      ), annotations_list)
    ) %>% 
    layout(
      showlegend = F
      )
  
  return(p %>% 
           config(displaylogo = FALSE,
                  modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "autoScale2d", 
                                             "hoverClosestCartesian", "hoverCompareCartesian", "toggleSpikelines")))

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
density_plot_overlay <- function(data, comparedata) {
  #function to output density plot for specific outcome
  
  #finding densities
  density_all <- density(data$value)
  density_matches <- filter(data, type == 'matches') %>% 
    pull(value) %>% 
    density()
  #Density Plot
  p <- plot_ly() %>%
    #density plot for all counties
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
    #density plot for matching counties 
    add_trace(
      type = 'scatter',
      mode = 'lines',
      x = ~density_matches$x,
      y = ~density_matches$y,
      fill = 'tozeroy',
      fillcolor = paste0(config$colors$teal100, '70'),
      line = list(
        color = paste0(config$colors$teal100), 
        width = 2
      ),
      name = 'Density Plot of\nMatching Counties',
      hoverinfo = 'name'
    ) %>% 
    #markers for matching counties
    add_trace(
      type = 'scatter',
      mode = 'markers+lines',
      x = filter(data, type == 'matches')$value,
      y = 0, 
      marker = list(
        symbol = 'diamond',
        color = paste0(config$colors$teal100),
        opacity = .65,
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
    #marker for my county
    add_trace(
      type = 'scatter',
      mode = 'markers+lines',
      x = filter(data, type == 'selected')$value,
      y = 0,
      marker = list(
        symbol = 'diamond',
        color = paste0(config$colors$yellow50),
        size = 17,
        line = list(
          width = 1, 
          color = paste0(config$colors$yellow50)
        )
      ),
      text = filter(data, type == 'selected')$county,
      hoverinfo = 'text',
      cliponaxis = F
    )
  
  #add line for selecting a county match
  if(!missing(comparedata)) {
    comparename <- comparedata %>% 
      pull(county)
    comparevalue <- comparedata %>% 
      pull(value)
    p <- p %>% 
      #marker for selected county
      add_trace(
        type = 'scatter',
        mode = 'markers+linses',
        x = comparevalue,
        y = 0,
        marker = list(
          symbol = 'diamond',
          color = paste0(config$colors$green100),
          opacity = 1,
          size = 17,
          line = list(
            width = 1.2, 
            color = paste0(config$colors$green100)
          )
        ),
        text = comparename,
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
        #line for my county and selected county
        shapes = list(
          list(
            type = 'line',
            xref = 'x',
            yref = 'y',
            x0 = filter(data, type == 'selected')$value,
            x1 = filter(data, type == 'selected')$value,
            y0 = 0,
            y1 = max(density_all$y, density_matches$y)*.05 + max(density_all$y, density_matches$y),
            line = list(
              color = paste0(config$colors$yellow50),
              width = 3,
              dash = 'longdash'
            )
          ), list(
            type = 'line',
            xref = 'x',
            yref = 'y',
            x0 = comparevalue,
            x1 = comparevalue,
            y0 = 0,
            y1 = max(density_all$y, density_matches$y)*.05 + max(density_all$y, density_matches$y),
            line = list(
              color = paste0(config$colors$green100),
              width = 3,
              dash = 'longdash'
            )
          )),
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
        showlegend = F,
        dragmode = F
      ) 
  } else {
    p <- p %>% 
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
        #line for my county
        shapes = list(
          type = 'line',
          xref = 'x',
          yref = 'y',
          x0 = filter(data, type == 'selected')$value,
          x1 = filter(data, type == 'selected')$value,
          y0 = 0,
          y1 = max(density_all$y, density_matches$y)*.05 + max(density_all$y, density_matches$y),
          line = list(
            color = paste0(config$colors$yellow50),
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
        showlegend = F,
        dragmode = F
      ) 
  }
  
  return(p)
  
}

density_plot <- function(data, comparedata) {
  #function to output density plot for specific outcome
  
  #finding densities
  density_all <- density(data$value)
  #Density Plot
  p <- plot_ly() %>%
    #density plot for all counties
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
    #marker for my county
    add_trace(
      type = 'scatter',
      mode = 'markers+lines',
      x = filter(data, type == 'selected')$value,
      y = 0,
      marker = list(
        symbol = 'diamond',
        color = paste0(config$colors$yellow50),
        opacity = 1,
        size = 17,
        line = list(
          width = 1, 
          color = paste0(config$colors$yellow50)
        )
      ),
      text = filter(data, type == 'selected')$county,
      hoverinfo = 'text',
      cliponaxis = F
    ) 
  
  #add line for selecting a matched county
  if (!missing(comparedata)) {
    comparename <- comparedata %>% 
      pull(county)
    comparevalue <- comparedata %>% 
      pull(value)
    p <- p %>% 
      #marker for selected county
      add_trace(
        type = 'scatter',
        mode = 'markers+linses',
        x = comparevalue,
        y = 0,
        marker = list(
          symbol = 'diamond',
          color = paste0(config$colors$green100),
          opacity = 1,
          size = 17,
          line = list(
            width = 1, 
            color = paste0(config$colors$green100)
          )
        ),
        text = comparename,
        hoverinfo = 'text',
        cliponaxis = F
      ) %>% layout(
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
        #line for my county and selected county
        shapes = list(
          list(
            type = 'line',
            xref = 'x',
            yref = 'y',
            x0 = filter(data, type == 'selected')$value,
            x1 = filter(data, type == 'selected')$value,
            y0 = 0,
            y1 = max(density_all$y)*.05 + max(density_all$y),
            line = list(
              color = paste0(config$colors$yellow50),
              width = 3,
              dash = 'longdash'
            )
          ), list(
            type = 'line',
            xref = 'x',
            yref = 'y',
            x0 = comparevalue,
            x1 = comparevalue,
            y0 = 0,
            y1 = max(density_all$y)*.05 + max(density_all$y),
            line = list(
              color = paste0(config$colors$green100),
              width = 3,
              dash = 'longdash'
            )
          )),
        xaxis = list(
          title = "",
          showgrid = F,
          zeroline = T
        ),
        yaxis = list(
          title = "Relative Frequency",
          showgrid = F,
          showline = T, 
          range = c(0, max(density_all$y)*.05 + max(density_all$y))
        ),
        showlegend = F,
        dragmode = F
      )
  } else {
    p <- p %>% 
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
        #line for my county
        shapes = list(
          type = 'line',
          xref = 'x',
          yref = 'y',
          x0 = filter(data, type == 'selected')$value,
          x1 = filter(data, type == 'selected')$value,
          y0 = 0,
          y1 = max(density_all$y)*.05 + max(density_all$y),
          line = list(
            color = paste0(config$colors$yellow50, "CC"),
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
          range = c(0, max(density_all$y)*.05 + max(density_all$y))
        ),
        showlegend = F,
        dragmode = F
      )
  }
  
  return(p)
}

get_compare_value <- function(data, comparison_name) {
  #function to pull selected county name and outcome value for outcomes df
  val <- filter(data, county==comparison_name) %>% 
    select(c(county, value))
  return(val)
}
