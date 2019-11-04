county_fips <- "1"

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
  filter(column_name == outcomes[1])

ggplot(df_outcome1, aes(x=value)) + geom_density() + 
  geom_vline(data = filter(df_outcome1, type != "other"),
             aes(xintercept = value, color = as.factor(type)))

#new changes

#Radar Chart with Plotly

#package
library(plotly)
library(devtools)
packageVersion('plotly')

library(plotly)

#transposing dataframe

#example dataframe
df2 <- as.data.frame(t(df[5,]))
names(df2) <- "Cook"

#radar chart
plot_ly() %>% 
  add_trace(
    type = 'scatterpolar',
    mode = 'markers',
    r = df2[,],
    theta = c("Economic\nStability", 'Neighborhood\n& Physical\nEnvironment', 'Education', 
              'Food', 'Community', 'Health\nCoverage'),
    #aesthetics
    fill = 'toself',
    fillcolor = "#189394",
    opacity = .8,
    marker = list(size = 7,
                  color ="#eb9795",
                  opacity = 1),
    name = "SDOH Score",
    hovertemplate = ~paste('<b>Category</b>: %{theta}',
                          '<br><b>Score</b>: %{r:.2f}',
                          '<extra></extra>')
  ) %>% 
  layout(
    title = paste("SDOH Scores \nfor", names(df2), "County"),
    polar = list(
      #tick labels
      radialaxis = list(
        angle = 90,
        tickangle = 90,
        dtick = .2, 
        tickwidth = 2,
        tickfont = list(size = 10),
        hoverformat = ".2f"
      ),
      #category labels
      angularaxis = list(
        tickfont = list(size =  12)
      )
    ),
    hoverlabel = list(
      bordercolor = "black"
    )
  )

