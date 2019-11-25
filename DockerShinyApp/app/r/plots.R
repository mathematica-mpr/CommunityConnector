county_fips <- "8067"
comparison_county_selection <- "8097"

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
  mutate(GEOID = str_pad(fips, width = 5, side = "left", pad = "0")) %>%
  mutate(NAME = str_replace(county, " County", ""))

co <- st_shp %>%
  filter(str_sub(GEOID, 1, 2) == "08")

co <- left_join(co, df)

# Compute approximate centroid and edges of study area
co_box <- as.numeric(st_bbox(co))
x_min <- co_box[1]
x_mid <- (co_box[3] - co_box[1]) / 2 + co_box[1]
x_max <- co_box[3]
y_min <- co_box[2]
y_mid <- (co_box[4] - co_box[2]) / 2 + co_box[2]
y_max <- co_box[4]

# Plot counties with proximity score
# Remove selected county from consideration in popups, and plot separately
selected_geo <- co %>% filter(GEOID == str_pad(county_fips, width = 5, side = "left", pad = "0"))
co <- co %>% filter(GEOID != str_pad(county_fips, width = 5, side = "left", pad = "0"))
county_label <- sprintf(
  "<strong>Comparison County:</strong> %s County (%s) <br/>
  <strong>Similarity to %s County:</strong> %s <br/>",
  co$NAME, co$GEOID, selected_geo$NAME[1], co$cat
) %>%
  lapply(htmltools::HTML)
selected_county_label <- sprintf(
  "<strong>Selected County:</strong> %s (%s) <br/>",
  selected_geo$NAME[1], selected_geo$GEOID[1]) %>%
  lapply(htmltools::HTML)
# Add labels for hospitals / overlap here

# Always 4 quantile breaks
co <- co %>%
  mutate(cat = cut(distance, breaks = c(quantile(distance, probs = seq(0, 1, by = 0.25))),
    labels = c("Very similar", "Somewhat similar", "Somewhat different", "Very different"),
    include.lowest = TRUE))

color_pal <- colorFactor("magma", co$cat, na.color = "gray")

map <- leaflet(options = leafletOptions(minZoom = 6, maxZoom = 13)) %>%
  setView(lng = x_mid, lat = y_mid, zoom = 6) %>%
  setMaxBounds(lng1 = x_min,
               lat1 = y_min,
               lng2 = x_max,
               lat2 = y_max) %>%
  addProviderTiles(providers$Stamen.TonerLite) %>%
  addPolygons(data = co,
              color = ~color_pal(co$cat),
              weight = 1,
              smoothFactor = 1,
              label = county_label,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3 px 8 px"),
                textsize = "15px",
                direction = "auto"),
              highlightOptions = highlightOptions(color = "black",
                                                  weight =  2,
                                                  bringToFront = TRUE)) %>%
  addLegend(pal = color_pal, values = co$cat, position = "topright",
            title = "Similarity to selected county") %>%
  addPolygons(data = selected_geo,
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              smoothFactor = 1,
              label = selected_county_label,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3 px 8 px"),
                textsize = "15px",
                direction = "auto"),
              highlightOptions = highlightOptions(color = "black",
                                                  weight =  2,
                                                  bringToFront = TRUE))

# single outcome density ----------------------------------
outcomes_dd <- get_dd(dd, "outcome")

outcomes <- outcomes_dd %>% pull(column_name)

comp_county_dat <- dat %>% filter(fips==comparison_county_selection)


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
  filter(column_name == outcomes[5])

ggplot(df_outcome1, aes(x=value)) + geom_density() + 
  geom_vline(data = filter(df_outcome1, type != "other"),
             aes(xintercept = value, color = as.factor(type)))

#new changes

#Radar Chart with Plotly-----

#example dataframe
testdf <- c("Cook", state, county_dat %>% select(starts_with("sdoh_score"))) %>% 
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


####Density Plot Compare------

density_plot <- function(data, comparedata) {
  #function to output density plot for specific outcome
  
  #finding densities
  density_all <- density(data$value)
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
    ) 
  
  if (!missing(comparedata)) {
    p <- p %>% 
      add_trace(
        type = 'scatter',
        mode = 'markers+linses',
        x = pull(comp_county_dat, df_outcome1$column_name[1]),
        y = 0,
        marker = list(
          symbol = 'diamond',
          color = paste0(config$colors$red100),
          opacity = 1,
          size = 17,
          line = list(
            width = 1, 
            color = paste0(config$colors$red100)
          )
        ),
        text = comp_county_dat$county,
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
        #Line for My County
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
              color = paste0(config$colors$yellow125),
              width = 3,
              dash = 'longdash'
            )
          ), list(
            type = 'line',
            xref = 'x',
            yref = 'y',
            x0 = pull(comp_county_dat, df_outcome1$column_name[1]),
            x1 = pull(comp_county_dat, df_outcome1$column_name[1]),
            y0 = 0,
            y1 = max(density_all$y)*.05 + max(density_all$y),
            line = list(
              color = paste0(config$colors$red100),
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
        showlegend = F
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
        #Line for My County
        shapes = list(
          type = 'line',
          xref = 'x',
          yref = 'y',
          x0 = filter(data, type == 'selected')$value,
          x1 = filter(data, type == 'selected')$value,
          y0 = 0,
          y1 = max(density_all$y)*.05 + max(density_all$y),
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
          range = c(0, max(density_all$y)*.05 + max(density_all$y))
        ),
        showlegend = F
      )
  }
  
  return(p)
}

density_plot_overlay <- function(data, comparedata) {
  #function to output density plot for specific outcome
  
  #finding densities and comparisons
  density_all <- density(data$value)
  density_matches <- filter(data, type == 'matches') %>% 
    pull(value) %>% 
    density()
  compare_county_name <- comparedata %>% 
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
      fillcolor = paste0(config$colors$teal100, '60'),
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
  
  if(!missing(comparevalue)) {
    p <- p %>% 
      add_trace(
        type = 'scatter',
        mode = 'markers+linses',
        x = comparevalue,
        y = 0,
        marker = list(
          symbol = 'diamond',
          color = paste0(config$colors$red100),
          opacity = 1,
          size = 17,
          line = list(
            width = 1, 
            color = paste0(config$colors$red100)
          )
        ),
        text = comp_county_dat$county,
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
              color = paste0(config$colors$red100),
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
        showlegend = F
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
        showlegend = F
      ) 
  }
  
  return(p)
  
}

density_plot_overlay(df_outcome1)

countyname <- comp_county_dat %>% 
  pull(county)
rep(countyname)
get_compare_value <- function(data, comparison_name) {
  val <- filter(data, county==comparison_name) %>% 
    select(c(county, value))
  return(val)
}
l <- get_compare_value(df_outcome1, countyname)
pull(l, value)
testing <- df_outcomes %>%
  group_by(column_name, higher_better) %>%
  nest() %>%
  mutate(comparename = rep(countyname)) %>% 
  mutate(comparison = purrr::map2(data, comparename, get_compare_value)) %>% 
  mutate(rank = unlist(purrr::map2(data, higher_better, rank_outcome))) %>%
  mutate(graphs = purrr::map2(data, comparison, density_plot)) %>% 
  pull(graphs)
testing
