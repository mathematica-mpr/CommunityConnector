county_fips <- "8001"
county_fips <- "8071"
county_dat <- dat %>% filter(fips == county_fips)

st <- dat %>% pull(state) %>% unique()
state <- state.name[match(st, state.abb)]

my_matches <- find_my_matches(county_fips, dat)[[2]] 

#
# radar chart ----------------------------

radardd <- get_dd(dd,"sdoh_score")
df <- make_radar_data(county_dat %>% select(starts_with("sdoh_score")), radardd)

par(bg = config$colors$tan25)
radarchart(df, 
           pcol = c(NA, NA,
                    paste0(config$colors$red100, '80')), 
           plty = 0,
           pfcol = c(paste0(config$colors$grey50, '80'),
                     paste0(config$colors$grey25, '33'),
                     paste0(config$colors$yellow100, '33')),
           cglcol = config$colors$grey100,
           seg = 4, vlcex = 0.8)

# county map ------------------------------

df <- find_my_matches(county_fips, dat)[[1]] %>%
  rename(fips = fips) %>%
  mutate(county = gsub(" county", "", tolower(county)))

county_map_df <- map_data("county") %>%
  filter(region == tolower(state))

df <- full_join(df, county_map_df, by = c("county" = "subregion"))

df %>%
  group_by(group) %>%
  plot_ly(x = ~long, y = ~lat, color = ~fct_explicit_na(fct_rev(factor(distance))),
          colors = viridis_pal(option="D")(3),
          text = ~county, hoverinfo = 'text') %>%
  add_polygons(line = list(width = 0.4)) %>%
  add_polygons(
    fillcolor = 'transparent',
    line = list(color = 'black', width = 0.5),
    showlegend = FALSE, hoverinfo = 'none'
  ) %>%
  layout(
    xaxis = list(title = "", showgrid = FALSE,
                 zeroline = FALSE, showticklabels = FALSE),
    yaxis = list(title = "", showgrid = FALSE,
                 zeroline = FALSE, showticklabels = FALSE),
    showlegend = FALSE
  )

# single outcome density ----------------------------------
outcomes_dd <- get_dd(dd, "outcome")

outcomes <- outcomes_dd %>% pull(column_name)


# need to add coloring to the vlines
df_outcomes <- dat %>% select(fips, state, county, outcomes) %>%
  pivot_longer(cols = outcomes) %>%
  # selected county and matches to selected county
  mutate(type = case_when(
    fips %in% my_matches ~ "matches",
    fips == county_fips ~ "selected",
    TRUE ~ "other"
  )) %>%
  # left join data dictionary to get real outcome names
  rename(column_name = name) %>%
  left_join(outcomes_dd, by = "column_name") 

df_outcome1 <- df_outcomes %>% 
  filter(column_name == outcomes[2])

ggplot(df_outcome1, aes(x=value)) + geom_density() + 
  geom_vline(data = filter(df_outcome1, type != "other"),
             aes(xintercept = value, color = as.factor(type)))

#new changes

#Radar Chart with Plotly-----

#example dataframe
testdf <- c("Rio Grande", state, county_dat %>% select(starts_with("sdoh_score"))) %>% 
  as.data.frame()

#Radar Chart Function
radar_chart <- function(df, dictionary) {
  #function to output interactive polar plot of SDOH Scores for one county
  
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
      name = paste(df[1,1], "County"),
      hovertemplate = ~paste('<b>Category</b>: %{theta}',
                             '<br><b>Score</b>: %{r:.2f}',
                             '<extra></extra>')
    ) %>% 
    layout(
      title = list(
        text = paste(df[1,1], "County,", df[1,2]),
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

radar_chart(testdf, dd)


#Radar Chart Overlay Function-----

#test datframe
testdf
testdf2 <- c("Worcester", "MA", county_dat %>% select(starts_with("sdoh_score"))) %>% 
  as.data.frame()
testdf2[1, 3:8] <- c(.9, .8, .3, .9, .5, .6)

radar_chart_overlay <- function(df1, df2, dictionary) {
  #function to output interactive polar plot of SDOH Scores for one county
  
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
      r = radar_points1,
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
      hoverinfo = 'none',
      name = paste(df1[1,1], "County")
    ) %>% 
    add_trace(
      type = 'scatterpolar',
      mode = 'markers+lines',
      r = radar_points2,
      theta = radar_names,
      fill = "toself",
      fillcolor = paste0(config$colors$teal100),
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

radar_chart_overlay(testdf, testdf2, dd)

#Density plot: All + Matches, Plotly------

testdf <- df_outcome1

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
        size = 15,
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
        size = 15,
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
        title = "Proportion of Counties",
        showgrid = F,
        showline = T, 
        range = c(0, max(density_all$y, density_matches$y)*.05 + max(density_all$y, density_matches$y))
      ),
      showlegend = F
    )
  return(p)
}

density_plot(testdf)
