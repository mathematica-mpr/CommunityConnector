county_fips <- "8001"
county_fips <- "8071"
county_dat <- dat %>% filter(fips == county_fips)

st <- dat %>% pull(state) %>% unique()
state <- state.name[match(st, state.abb)]

my_matches <- find_my_matches(county_fips, dat)[[2]] 


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

#Radar Chart Function-----
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




#Density plot: Plotly, Dot Rug-----
dens <- density(df_outcome1$value)
plot_ly() %>% 
  add_trace(
    type = 'scatter',
    mode = 'lines',
    x = ~dens$x,
    y = ~dens$y,
    fill = 'tozeroy',
    fillcolor = paste0(config$colors$purple100, '55'),
    line = list(
      color = paste0(config$colors$purple100)
    ),
    hoverinfo = 'none'
  ) %>% 
  add_trace(
    type = 'scatter',
    mode = 'markers+lines',
    x = filter(df_outcome1, type == 'matches')$value,
    y = 0, 
    marker = list(
      symbol = 'diamond',
      color = paste0(config$colors$teal100),
      opacity = .8,
      size = 9
    ),
    line = list(
      width = 0
    ),
    text = filter(df_outcome1, type == 'matches')$county,
    hoverinfo = 'text',
    textposition = "top left",
    cliponaxis = F
  ) %>% 
  add_trace(
    type = 'scatter',
    mode = 'markers',
    x = filter(df_outcome1, type == 'selected')$value,
    y = 0,
    marker = list(
      symbol = 'diamond',
      color = paste0(config$colors$yellow125),
      opacity = 1,
      size = 9
    ),
    text = 'My County',
    hoverinfo = 'text',
    cliponaxis = F
  ) %>% 
  layout(
    title = list(
      text = paste(df_outcome1$description[1]),
      font = list(
        size = 18,
        color = paste0(config$colors$purple100)
      ),
      xref = 'paper',
      x = '0'
    ),
    shapes = list(
      type = 'line',
      xref = 'x',
      yref = 'y',
      x0 = filter(df_outcome1, type == 'selected')$value,
      x1 = filter(df_outcome1, type == 'selected')$value,
      y0 = 0,
      y1 = max(dens$y)*.05 + max(dens$y),
      line = list(
        color = paste0(config$colors$yellow125),
        width = 3,
        dash = 'dash'
      )
    ),
    xaxis = list(
      title = "",
      showgrid = F,
      zeroline = T
    ),
    yaxis = list(
      title = "Density",
      showgrid = F,
      showline = T
    ),
    showlegend = F
  )



#Density plot: All + Matches, Plotly, dot rug------
dens <- density(df_outcome1$value)
dens_matches <- filter(df_outcome1, type == 'matches') %>% 
  pull(value) %>% 
  density()

plot_ly() %>%
  add_trace(
    type = 'scatter',
    mode = 'lines',
    x = ~dens$x,
    y = ~dens$y,
    line = list(
      color = paste0(config$colors$grey100),
      width = 2
    ),
    fill = 'tozeroy',
    fillcolor = paste0(config$colors$grey100, '70'),
    name = "All Counties",
    hoverinfo = 'name'
  ) %>% 
  add_trace(
    type = 'scatter',
    mode = 'lines',
    x = ~dens_matches$x,
    y = ~dens_matches$y,
    fill = 'tozeroy',
    fillcolor = paste0(config$colors$teal100, '60'),
    line = list(
      color = paste0(config$colors$teal100), 
      width = 2
    ),
    name = 'Matching Counties',
    hoverinfo = 'name'
  ) %>% 
  add_trace(
    type = 'scatter',
    mode = 'markers+lines',
    x = filter(df_outcome1, type == 'matches')$value,
    y = 0, 
    marker = list(
      symbol = 'diamond',
      color = paste0(config$colors$teal100),
      opacity = .8,
      size = 9
    ),
    line = list(
      width = 0
    ),
    text = filter(df_outcome1, type == 'matches')$county,
    hoverinfo = 'text',
    textposition = "top left",
    cliponaxis = F
  ) %>% 
  add_trace(
    type = 'scatter',
    mode = 'markers+lines',
    x = filter(df_outcome1, type == 'selected')$value,
    y = 0,
    marker = list(
      symbol = 'diamond',
      color = paste0(config$colors$yellow125),
      opacity = 1,
      size = 9
    ),
    text = 'My County',
    hoverinfo = 'text',
    cliponaxis = F
  ) %>% 
  layout(
    title = list(
      text = paste(df_outcome1$description[1]),
      font = list(
        size = 18,
        color = paste0(config$colors$purple100)
      ),
      xref = 'paper',
      x = '0'
    ),
    hoverlabel = list(
      namelength = 18
    ),
    shapes = list(
      type = 'line',
      xref = 'x',
      yref = 'y',
      x0 = filter(df_outcome1, type == 'selected')$value,
      x1 = filter(df_outcome1, type == 'selected')$value,
      y0 = 0,
      y1 = max(dens$y, dens_matches$y)*.05 + max(dens$y, dens_matches$y),
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
      title = "Density",
      showgrid = F,
      showline = T
    ),
    showlegend = F
  )



#other Denstiy Plots
#Density plot: GGplot, Dot Rug-----
ggplot(df_outcome1, aes(x = value)) +
  geom_density(fill = paste0(config$colors$grey50), 
               color = paste0(config$colors$grey50), alpha = .7) +
  geom_point(data = filter(df_outcome1, type != 'other'), aes(x = value, y = 0, color = type),
             shape = 18,
             size = 3.5, 
             alpha = .7) +
  geom_vline(xintercept = filter(df_outcome1, type == 'selected')$value,
             color = paste0(config$colors$yellow125),
             size = 1,
             linetype = 5) +
  scale_color_manual(values = c(paste0(config$colors$teal100), 
                                paste0(config$colors$yellow125), 
                                paste0(config$colors$yellow125))) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  coord_cartesian(clip = 'off') +
  ggtitle(paste(df_outcome1$description[1])) +
  ylab("Density") +
  xlab("Value") 

#Density Plot Function-----
density_plot <- function(data, dictionary, c_fips, outcome_of_interest){
  #function to output density plot for one outcome
  
  #dataframe of all outcomes with matches for selected county
  my_matches <- find_my_matches(c_fips, data)[[2]]
  outcomes_dd <- get_dd(dictionary, "outcome")
  outcomes_list <- outcomes_dd %>% pull(column_name)
  df_outcomes <- dat %>% select(fips, state, county, outcomes_list) %>%
    pivot_longer(cols = outcomes_list) %>%
    # selected county and matches to selected county
    mutate(type = case_when(
      fips %in% my_matches ~ "matches",
      fips == c_fips ~ "selected",
      TRUE ~ "other"
    )) %>%
    # left join data dictionary to get real outcome names
    rename(column_name = name) %>%
    left_join(outcomes_dd, by = "column_name") 
  
  #outcome of interest datframe
  outcome_df <- df_outcomes %>% 
    filter(column_name == outcomes_list[outcome_of_interest])
  dens <- density(outcome_df$value)
  
  #filtering by match
  line_matches <- filter(outcome_df, type == 'matches') %>% 
    pull(value)
  line_other <- filter(outcome_df, type == 'other') %>% 
    pull(value)
  line_mycounty <- filter(outcome_df, type == 'selected') %>% 
    pull(value)
  #horizontal lines for each outcome value
  line <- list(
    type = "line",
    xref = "x",
    yref = "y",
    y0 = 0,
    y1 = max(dens$y)*.1 + max(dens$y)
  )
  vlines_matches <- list()
  for (i in line_matches) {
    line[["line"]] = list(color = paste0(config$colors$teal100))
    line[c("x0", "x1")] <- i
    line[['opacity']] <- .4
    vlines_matches <- c(vlines_matches, list(line))
  }
  vlines_other <- list()
  for (i in line_other) {
    line[["line"]] = list(color = "#360B7F")
    line[c("x0", "x1")] <- i
    line[['opacity']] <- .4
    vlines_other <- c(vlines_other, list(line))
  }
  #line[["line"]] = list(color = paste0(config$colors$yellow100), width = 5)
  line[["line"]] = list(color = "#FFFF00", width = 5)
  line[c("x0", "x1")] <- line_mycounty
  line[['opacity']] <- 1
  all <- c(vlines_matches, vlines_other, list(line))
  
  #plotly density plot
  p <- plot_ly(
    type = 'scatter',
    mode = 'lines',
    x = ~dens$x,
    y = ~dens$y,
    fill = 'tozeroy',
    fillcolor = paste0(config$colors$grey50),
    line = list(
      color = paste0(config$colors$grey50)
    ),
    hoverinfo = 'none'
  ) %>% 
    layout(
      title = list(
        text = paste(outcome_df$description[1]),
        font = list(
          size = 18,
          color = "#360B7F"
        ),
        xref = 'paper',
        x = '0'
      ),
      shapes = all,
      xaxis = list(
        title = ""
      ),
      yaxis = list(
        title = "Density"
      )
    )
  return(p)
}
density_plot(dat, dd, county_fips, 1)

#Density Plot Function 2----
density_plot <- function(outcome_df){
  #function to output density plot for one outcome
  dens <- density(outcome_df$value)
  #filtering by match
  line_matches <- filter(outcome_df, type == 'matches') %>% 
    pull(value)
  line_other <- filter(outcome_df, type == 'other') %>% 
    pull(value)
  line_mycounty <- filter(outcome_df, type == 'selected') %>% 
    pull(value)
  #horizontal lines for each outcome value
  line <- list(
    type = "line",
    xref = "x",
    yref = "y",
    y0 = 0,
    y1 = max(dens$y)*.1 + max(dens$y)
  )
  vlines_matches <- list()
  for (i in line_matches) {
    line[["line"]] = list(color = paste0(config$colors$teal100))
    line[c("x0", "x1")] <- i
    line[['opacity']] <- .4
    vlines_matches <- c(vlines_matches, list(line))
  }
  vlines_other <- list()
  for (i in line_other) {
    line[["line"]] = list(color = paste0(config$colors$purple100))
    line[c("x0", "x1")] <- i
    line[['opacity']] <- .4
    vlines_other <- c(vlines_other, list(line))
  }
  #line[["line"]] = list(color = paste0(config$colors$yellow100), width = 5)
  line[["line"]] = list(color = paste0(config$colors$yellow125), width = 5)
  line[c("x0", "x1")] <- line_mycounty
  line[['opacity']] <- 1
  all <- c(vlines_matches, vlines_other, list(line))
  
  #plotly density plot
  p <- plot_ly(
    type = 'scatter',
    mode = 'lines',
    x = ~dens$x,
    y = ~dens$y,
    fill = 'tozeroy',
    fillcolor = paste0(config$colors$grey50),
    line = list(
      color = paste0(config$colors$grey50)
    ),
    hoverinfo = 'none'
  ) %>% 
    layout(
      title = list(
        text = paste(outcome_df$description[1]),
        font = list(
          size = 18,
          color = paste0(config$colors$purple100)
        ),
        xref = 'paper',
        x = '0'
      ),
      shapes = all,
      xaxis = list(
        title = ""
      ),
      yaxis = list(
        title = "Density"
      )
    )
  return(p)
}
density_plot(df_outcome1)

#Other Styles
#Density Plots: With Rug----
ggplot(df_outcome1, aes(x = value)) +
  geom_density(fill = paste0(config$colors$grey50), 
               color = paste0(config$colors$grey50), 
               alpha = .7) +
  geom_rug(aes(color = type)) + 
  geom_vline(xintercept = filter(df_outcome1, type=='selected')$value, 
             color = 'yellow', 
             size = 1) +
  scale_color_manual(values = c(paste0(config$colors$teal100), 
                                'purple', 
                                'yellow')) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  ggtitle(paste(df_outcome1$description[1])) +
  ylab("Density") +
  xlab("Value")


#Violin Plot: matches and other----------
test <- filter(df_outcome1, type != 'selected')
ggplot(test, aes(x = type, y = value)) +
  geom_violin(aes(fill = type, 
                  color = type, alpha = .7), 
              trim = FALSE) + 
  geom_dotplot(aes(color = type, fill = type, alpha = .9), 
               binaxis = 'y', 
               binwidth = .15,
               stackdir = 'center', 
               position = position_dodge(1)) +
  geom_hline(yintercept = filter(df_outcome1, type=='selected')$value) +
  scale_fill_manual(values = c(paste0(config$colors$teal100), 
                               'purple', 
                               paste0(config$colors$yellow100))) +
  scale_color_manual(values = c(paste0(config$colors$teal100), 
                                'purple',
                                paste0(config$colors$yellow100))) +
  theme_bw() +
  theme(legend.position = "none") + 
  ggtitle(paste(df_outcome1$description[1])) +
  ylab("Density") +
  xlab("Value") +
  coord_flip()

#Density Plot: matches and non matches-----
ggplot(df_outcome1, aes(x = value)) +
  geom_density(aes(fill = type, 
                   color = type, 
                   alpha = .7)) +
  geom_point(aes(x = value, 
                 y = 0, 
                 color = type), 
             alpha = .7, 
             shape = 18, 
             size = 3) +
  geom_segment(aes(x = filter(df_outcome1, type=='selected')$value, 
                   xend = filter(df_outcome1, type=='selected')$value,
                   y = 0, 
                   yend = .4),
               color = 'yellow', 
               lineend = 18) +
  scale_color_manual(values = c('purple', 
                                paste0(config$colors$teal100), 
                                'yellow')) +
  scale_fill_manual(values = c('purple', 
                               paste0(config$colors$teal100), 
                               paste0(config$colors$yellow100))) +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle(paste(df_outcome1$description[1])) +
  ylab("Density") +
  xlab("Value")

#Density Plot: with Histogram----------
ggplot(df_outcome1, aes(x = value)) +
  geom_histogram(aes(y = ..density..), 
                 color = 'black', 
                 fill = 'grey', 
                 binwidth = .3, 
                 alpha = .3, 
                 size = .2) +
  geom_density(fill = paste0(config$colors$red100, '10'), 
               alpha = .5, 
               size = 0) +
  #geom_segment(aes(x = filter(df_outcome1, type=='selected')$value, xend = filter(df_outcome1, type=='selected')$value,
  #                y = 0, yend = .6), color = paste0(config$colors$red100)) +
  geom_vline(xintercept = 5, 
             color = 'yellow', 
             size = 1) +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle(paste(df_outcome1$description[1])) +
  ylab("Density") +
  xlab("Value")
