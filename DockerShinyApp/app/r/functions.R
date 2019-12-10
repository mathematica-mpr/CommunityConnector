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

## DT function -----------------------------------------------------------------
make_demo_dt <- function(county_dat, comp_county_dat,
                         comp_county_select, demo_select, dd) {
  DT::renderDT({
    df <- get_table_data(county_dat, dd, demo_select) 
    
    if (comp_county_select != "None") {
      comp_df <- get_table_data(comp_county_dat, dd, demo_select)
      df <- left_join(df, comp_df, by = "name")
    }
    
    if (demo_select == "demographic") {
      df <- df %>%
        rename(`Essential facts` = name)
    } else if (demo_select == "used_sdoh_1") {
      df <- df %>%
        rename(`Economic Stability` = name)
    } else if (demo_select == "used_sdoh_2") {
      df <- df %>%
        rename(`Neighborhood & Physical Environment` = name)
    } else if (demo_select == "used_sdoh_3") {
      df <- df %>%
        rename(`Education` = name)
    } else if (demo_select == "used_sdoh_4") {
      df <- df %>%
        rename(`Food` = name)
    } else if (demo_select == "used_sdoh_5") {
      df <- df %>%
        rename(`Community` = name)
    } else if (demo_select == "used_sdoh_6") {
      df <- df %>%
        rename(`Health Care System` = name)
    }
    
    DT::datatable(df, rownames = FALSE, class = "stripe", options = list(paging = F)) %>%
      DT::formatStyle(columns = colnames(df), fontSize = "9pt")
  })
}


# distance function to find my county matches ----------------------------------

find_my_matches <- function(my_county, df, n_matches = 20) {
  # my_county = fips of selected county
  # df = dataframe with all covariates

  my_county_distances <- dist_mat %>% 
    select(distance =  my_county) %>%
    rownames_to_column("fips") %>%
    left_join(df %>% select(fips, county, state), by = "fips") %>% 
    mutate(rounded_distance = round(distance*2)/2) %>% 
    left_join(color_mapping, by = "rounded_distance")
  
  my_matches <-  my_county_distances %>%
    filter(fips != my_county) %>%
    arrange(distance) %>%
    head(n_matches) %>%
    pull(fips)
  
  list(my_county_distances, my_matches)
  
}

# function to filter outcomes based on selection
filter_category <- function(data, outcome_filter) {
  data %>% filter(grepl(paste(outcome_filter, collapse = "|"), unique(column_name)))
}

# function to match sdoh score with sdoh category name
get_score_and_name <- function(df, dictionary) {
  #pull score number and score category name from dictionary
  radar_names <- get_dd(dictionary, "sdoh_score") %>% 
    dplyr::select(column_name, descrip_new)
  #pull score number and score value from selected county data frame
  radar_points <- select(df, starts_with("sdoh")) %>% 
    t() %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column() %>% 
    rename("value" = V1)
  #match score value with score category name by score number
  names_and_points <- merge(radar_names, radar_points, by.x = "column_name", by.y = "rowname")
  names_and_points[dim(names_and_points)[1]+1,] <- names_and_points[1,]
  return(names_and_points)
}

# function for one county radar plot -------------------------------------------
radar_chart <- function(df, dictionary) {
  # df is my county; columns: county, state, sdoh_score 1:6
  # dictionary is data dictionary
  
  #vector of scores and names
  axis_info <- get_score_and_name(df, dictionary)
  radar_names <- axis_info$descrip_new
  radar_points <- axis_info$value
  
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
                  color = paste0(config$colors$yellow100), 
                  width = .8, 
                  shape = 'spline', 
                  smoothing = .9),
      marker = list(size = 7,
                    color = paste0(config$colors$yellow100),
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
        bgcolor = paste0(config$colors$yellow50),
        namelength = -1
      ),
      showlegend = T,
      dragmode = F
    )
  return(p %>% 
           config(displaylogo = FALSE,
                  modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "autoScale2d", "hoverClosest3d",
                                             "hoverClosestCartesian", "hoverCompareCartesian", "toggleSpikelines", "toggleHover")))
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
  
  #vector of scores and names
  axis_info1 <- get_score_and_name(df1, dictionary)
  axis_info2 <- get_score_and_name(df2, dictionary)
  radar_names1 <- axis_info1$descrip_new
  radar_points1 <- axis_info1$value
  radar_names2 <- axis_info2$descrip_new
  radar_points2 <- axis_info2$value
  
  
  #plotting radar chart
  p <- plot_ly(
  ) %>%     
    add_trace(
      type = 'scatterpolar',
      mode = 'markers+lines',
      r = radar_points2,
      theta = radar_names2,
      fill = "toself",
      fillcolor = paste0(config$colors$green100, "70"),
      line = list(dash = "solid", 
                  color = paste0(config$colors$green100), 
                  width = .8, 
                  shape = 'spline', 
                  smoothing = .9),
      marker = list(size = 7,
                    color = paste0(config$colors$green100),
                    opacity = 1),
      #opacity = .6,
      hoverinfo = 'none',
      name = paste0(df2$county, ", ", df2$state)
    ) %>% 
    add_trace(
      type = 'scatterpolar',
      mode = 'markers+lines',
      r = radar_points1,
      theta = radar_names1,
      #aesthetics
      fill = 'toself',
      fillcolor = paste0(config$colors$yellow50, "CC"),
      line = list(dash = "solid", 
                  color = paste0(config$colors$yellow100), 
                  width = .8, 
                  shape = 'spline', 
                  smoothing = .9),
      marker = list(size = 7,
                    color = paste0(config$colors$yellow100),
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
        bgcolor = paste0(config$colors$yellow50)
      ),
      showlegend = T,
      dragmode = F
    )
  return(p %>% 
           config(displaylogo = FALSE,
                  modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "autoScale2d", "hoverClosest3d",
                                             "hoverClosestCartesian", "hoverCompareCartesian", "toggleSpikelines", "toggleHover")))
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
      name = lang_cfg$density_hoverlabel_all,
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
      name = lang_cfg$density_hoverlabel_matches,
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
          text = data$description[1],
          font = list(
            size = 18,
            color = paste0(config$colors$accent),
            family = "Arial"
          ),
          xref = 'paper',
          x = '0'
        ),
        hoverlabel = list(
          namelength = -1
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
          zeroline = F
        ),
        yaxis = list(
          title = list(
            text = lang_cfg$titles$density_y_axis,
            font = list(size = 12,
                        family = "Arial",
                        color = paste(config$colors$accent))
          ),
          showgrid = F,
          showline = T, 
          showticklabels = F,
          range = c(0, max(density_all$y, density_matches$y)*.05 + max(density_all$y, density_matches$y))
        ),
        showlegend = F,
        dragmode = F,
        margin = list(l = 11)
      ) 
  } else {
    p <- p %>% 
      layout(
        title = list(
          text = data$description[1],
          font = list(
            size = 18,
            color = paste0(config$colors$accent),
            family = "Arial"
          ),
          xref = 'paper',
          x = '0'
        ),
        hoverlabel = list(
          namelength = -1
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
          zeroline = F
        ),
        yaxis = list(
          title = list(
            text = lang_cfg$titles$density_y_axis,
            font = list(size = 12,
                        family = "Arial",
                        color = paste(config$colors$accent))
          ),
          showgrid = F,
          showline = T, 
          showticklabels = F, 
          range = c(0, max(density_all$y, density_matches$y)*.05 + max(density_all$y, density_matches$y))
        ),
        showlegend = F,
        dragmode = F,
        margin = list(l = 11)
      ) 
  }  
    
  return(p %>% config(displaylogo = FALSE,
                      modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "autoScale2d", 
                                                 "zoomIn2d", "zoomOut2d", "resetScale2d", 
                                                 "hoverClosestCartesian", "hoverCompareCartesian", "toggleSpikelines")) )
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
      name = lang_cfg$density_hoverlabel_all,
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
          text = data$description[1],
          font = list(
            size = 18,
            color = paste0(config$colors$accent),
            family = "Arial"
          ),
          xref = 'paper',
          x = '0'
        ),
        hoverlabel = list(
          namelength = -1
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
          zeroline = F
        ),
        yaxis = list(
          title = list(
            text = lang_cfg$titles$density_y_axis,
            font = list(size = 12,
                        family = "Arial",
                        color = paste(config$colors$accent))
          ),
          showgrid = F,
          showline = T, 
          showticklabels = F,
          range = c(0, max(density_all$y)*.05 + max(density_all$y))
        ),
        showlegend = F,
        dragmode = F,
        margin = list(l = 11)
      )
  } else {
    p <- p %>% 
      layout(
        title = list(
          text = data$description[1],
          font = list(
            size = 18,
            color = paste0(config$colors$accent),
            family = "Arial"
          ),
          xref = 'paper',
          x = '0'
        ),
        hoverlabel = list(
          namelength = -1
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
          zeroline = F
        ),
        yaxis = list(
          title = list(
            text = lang_cfg$titles$density_y_axis,
            font = list(size = 12,
                        family = "Arial",
                        color = paste(config$colors$accent))
          ),
          showgrid = F,
          showline = T, 
          showticklabels = F, 
          range = c(0, max(density_all$y)*.05 + max(density_all$y))
        ),
        showlegend = F,
        dragmode = F,
        margin = list(l = 11)
      )
  }
  
  return(p %>% config(displaylogo = FALSE,
                      modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "autoScale2d",  
                                                 "zoomIn2d", "zoomOut2d", "resetScale2d", 
                                                 "hoverClosestCartesian", "hoverCompareCartesian", "toggleSpikelines")))
}

get_compare_value <- function(data, comparison_name) {
  #function to pull selected county name and outcome value for outcomes df
  val <- filter(data, county==comparison_name) %>% 
    select(c(county, value))
  return(val)
}

# map function
county_map <- function(df, st_fips, cty_fips) {
  selected_state_shp <- st_shp %>%
    filter(str_sub(GEOID, 1, 2) == st_fips) %>%
    left_join(., df)
  
  # Compute approximate centroid and edges of study area
  selected_state_shp_box <- as.numeric(st_bbox(selected_state_shp))
  x_min <- selected_state_shp_box[1]
  x_mid <- (selected_state_shp_box[3] - selected_state_shp_box[1]) / 2 + selected_state_shp_box[1]
  x_max <- selected_state_shp_box[3]
  y_min <- selected_state_shp_box[2]
  y_mid <- (selected_state_shp_box[4] - selected_state_shp_box[2]) / 2 + selected_state_shp_box[2]
  y_max <- selected_state_shp_box[4]
  
  # Plot counties with proximity score
  # Remove selected county from consideration in popups, and plot separately
  selected_geo <- selected_state_shp %>% filter(GEOID == cty_fips)
  selected_state_shp <- selected_state_shp %>% filter(GEOID != cty_fips) %>%
    mutate(quantile_distance = ecdf(distance)(distance) * 100) %>%
    mutate_at(vars(quantile_distance), ~round(. * 2, digits = 0)) %>%
    mutate_at(vars(quantile_distance), ~(. / 2))
  
  county_label <- sprintf(
    "<strong>Comparison County:</strong> %s <br/>
      <strong>Selected County: </strong> %s <br/>",
    selected_state_shp$NAME, selected_geo$NAME[1]
  ) %>%
    lapply(htmltools::HTML)
  selected_county_label <- sprintf(
    "<strong>Selected County:</strong> %s <br/>",
    selected_geo$NAME[1]) %>%
    lapply(htmltools::HTML)
  
  color_pal <- colorFactor(palette = color_mapping$hex_color,
                           levels = color_mapping$rounded_distance,
                           na.color = config$colors$grey50)
  leaflet(options = leafletOptions(minZoom = 6, maxZoom = 13)) %>%
    setView(lng = x_mid, lat = y_mid, zoom = 6) %>%
    setMaxBounds(lng1 = x_min,
                 lat1 = y_min,
                 lng2 = x_max,
                 lat2 = y_max) %>%
    addProviderTiles(providers$Stamen.TonerLite) %>%
    addPolygons(data = selected_state_shp,
                color = ~color_pal(selected_state_shp$quantile_distance),
                weight = 1,
                smoothFactor = 1,
                fillOpacity = 0.6,
                label = county_label,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3 px 8 px"),
                  textsize = "15px",
                  direction = "auto"),
                highlightOptions = highlightOptions(color = "black",
                                                    weight =  2,
                                                    bringToFront = TRUE)) %>%
    addLegend(colors = c(config$colors$yellow50),
              labels = paste0("Selected County: ", selected_geo$NAME),
              opacity = 0.6) %>%
    addLegend(colors = c(color_mapping$hex_color[1],
                         color_mapping$hex_color[34],
                         color_mapping$hex_color[68],
                         color_mapping$hex_color[101],
                         color_mapping$hex_color[134],
                         color_mapping$hex_color[168],
                         color_mapping$hex_color[201]),
              labels = c("Most similar",
                         "",
                         "",
                         "",
                         "",
                         "",
                         "Least similar"),
              opacity = 0.6) %>%
    addPolygons(data = selected_geo,
                fillColor = config$colors$yellow50,
                stroke = TRUE,
                color = "black",
                fillOpacity = 0.9,
                weight = 2,
                smoothFactor = 1,
                label = selected_county_label,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3 px 8 px"),
                  textsize = "15px",
                  direction = "auto"),
                highlightOptions = highlightOptions(color = "black",
                                                    weight =  4,
                                                    bringToFront = TRUE))
}
