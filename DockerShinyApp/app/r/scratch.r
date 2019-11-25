sdoh_dd <- get_dd(dd, "sdoh_score")

sdohs <- sdoh_dd %>% pull(column_name)

df <- dat %>% select(fips, state, county, sdohs) %>%
  filter(fips %in% my_matches) 



n_matches <- 20
# function start grid_radar <- function(df, dd, n_matches)

# get labels for sdohs
radar_names <- get_dd(dd, "sdoh_score") %>% 
  dplyr::pull(descrip_new)
radar_names <- append(radar_names, radar_names[1])

df1 <- df[1,]
points1 <- select(df1, starts_with("sdoh"))
points1 <- append(points1, points1[1]) %>% 
  unlist()

# parameters


n_rows <- ceiling(n_matches / 4) # exec decision to make 4 columns

t = 0.05

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

# create first county radar chart
p <- plot_ly() %>% 
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
 #   hoverinfo = 'none',
    name = paste(df1$county, df1$state)
  ) %>% 
  layout(  
    polar = list(
      domain = list(
        x = c(0 + t, (1 / 4) - t),
        y = c(1 - (1 / n_rows) + t, 1 - t)
      ),
      radialaxis = radialaxis_list,
      angularaxis = angularaxis_list
    )) 


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
    #  hoverinfo = 'none',
      name = paste(df_one$county, df_one$state),
      subplot = paste0('polar', i)
    ) 
}

# hard coding positions of the 20 plots
p %>% 
  layout(
    polar2 = list(
      domain = list(
        x = c((1/4) + t, (2 / 4) - t),
        y = c(1 - (1 / n_rows) + t, 1 - t)
      ),
      radialaxis = radialaxis_list,
      angularaxis = angularaxis_list
    )) %>% 
  layout(
    polar3 = list(
      domain = list(
        x = c((2/4) + t, (3 / 4) - t),
        y = c(1 - (1 / n_rows) + t, 1 - t)
      ),
      radialaxis = radialaxis_list,
      angularaxis = angularaxis_list
    )) %>% 
  layout(
    polar4 = list(
      domain = list(
        x = c((3/4) + t, (4 / 4) - t),
        y = c(1 - (1 / n_rows) + t, 1 - t)
      ),
      radialaxis = radialaxis_list,
      angularaxis = angularaxis_list
    )) %>%  
  layout(
    polar5 = list(
      domain = list(
        x = c(0 + t, (1 / 4) - t),
        y = c(1 - (2 / n_rows) + t, 1 - (1 / n_rows) -  t)
      ),
      radialaxis = radialaxis_list,
      angularaxis = angularaxis_list
    )) %>%
  layout(
    polar6 = list(
      domain = list(
        x = c((1/4) + t, (2 / 4) - t),
        y = c(1 - (2 / n_rows) + t, 1 - (1 / n_rows) -  t)
      ),
      radialaxis = radialaxis_list,
      angularaxis = angularaxis_list
    )) %>% 
  layout(
    polar7 = list(
      domain = list(
        x = c((2/4) + t, (3 / 4) - t),
        y = c(1 - (2 / n_rows) + t, 1 - (1 / n_rows) -  t)
      ),
      radialaxis = radialaxis_list,
      angularaxis = angularaxis_list
    )) %>% 
  layout(
    polar8 = list(
      domain = list(
        x = c((3/4) + t, (4 / 4) - t),
        y = c(1 - (2 / n_rows) + t, 1 - (1 / n_rows) -  t)
      ),
      radialaxis = radialaxis_list,
      angularaxis = angularaxis_list
    )) %>%  
  layout(
    polar9 = list(
      domain = list(
        x = c(0 + t, (1 / 4) - t),
        y = c(1 - (3 / n_rows) + t, 1 - (2 / n_rows) -  t)
      ),
      radialaxis = radialaxis_list,
      angularaxis = angularaxis_list
    )) %>%
  layout(
    polar10 = list(
      domain = list(
        x = c((1/4) + t, (2 / 4) - t),
        y = c(1 - (3 / n_rows) + t, 1 - (2 / n_rows) -  t)
      ),
      radialaxis = radialaxis_list,
      angularaxis = angularaxis_list
    )) %>% 
  layout(
    polar11 = list(
      domain = list(
        x = c((2/4) + t, (3 / 4) - t),
        y = c(1 - (3 / n_rows) + t, 1 - (2 / n_rows) -  t)
      ),
      radialaxis = radialaxis_list,
      angularaxis = angularaxis_list
    )) %>% 
  layout(
    polar12 = list(
      domain = list(
        x = c((3/4) + t, (4 / 4) - t),
        y = c(1 - (3 / n_rows) + t, 1 - (2 / n_rows) -  t)
      ),
      radialaxis = radialaxis_list,
      angularaxis = angularaxis_list
    )) %>%  
  layout(
    polar13 = list(
      domain = list(
        x = c(0 + t, (1 / 4) - t),
        y = c(1 - (4 / n_rows) + t, 1 - (3 / n_rows) -  t)
      ),
      radialaxis = radialaxis_list,
      angularaxis = angularaxis_list
    )) %>%
  layout(
    polar14 = list(
      domain = list(
        x = c((1/4) + t, (2 / 4) - t),
        y = c(1 - (4 / n_rows) + t, 1 - (3 / n_rows) -  t)
      ),
      radialaxis = radialaxis_list,
      angularaxis = angularaxis_list
    )) %>% 
  layout(
    polar15 = list(
      domain = list(
        x = c((2/4) + t, (3 / 4) - t),
        y = c(1 - (4 / n_rows) + t, 1 - (3 / n_rows) -  t)
      ),
      radialaxis = radialaxis_list,
      angularaxis = angularaxis_list
    )) %>% 
  layout(
    polar16 = list(
      domain = list(
        x = c((3/4) + t, (4 / 4) - t),
        y = c(1 - (4 / n_rows) + t, 1 - (3 / n_rows) -  t)
      ),
      radialaxis = radialaxis_list,
      angularaxis = angularaxis_list
    ))  %>%  
  layout(
    polar17 = list(
      domain = list(
        x = c(0 + t, (1 / 4) - t),
        y = c(1 - (5 / n_rows) + t, 1 - (4 / n_rows) -  t)
      ),
      radialaxis = radialaxis_list,
      angularaxis = angularaxis_list
    )) %>%
  layout(
    polar18 = list(
      domain = list(
        x = c((1/4) + t, (2 / 4) - t),
        y = c(1 - (5 / n_rows) + t, 1 - (4 / n_rows) -  t)
      ),
      radialaxis = radialaxis_list,
      angularaxis = angularaxis_list
    )) %>% 
  layout(
    polar19 = list(
      domain = list(
        x = c((2/4) + t, (3 / 4) - t),
        y = c(1 - (5 / n_rows) + t, 1 - (4 / n_rows) -  t)
      ),
      radialaxis = radialaxis_list,
      angularaxis = angularaxis_list
    )) %>% 
  layout(
    polar20 = list(
      domain = list(
        x = c((3/4) + t, (4 / 4) - t),
        y = c(1 - (5 / n_rows) + t, 1 - (4 / n_rows) -  t)
      ),
      radialaxis = radialaxis_list,
      angularaxis = angularaxis_list
    )) %>% 
  layout(showlegend = F)

  





# hard code 4 as total number of columns
# n_rows (ex: 5) is ceiling(number of counties / 4) 
# tolerance 

