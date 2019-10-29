county_FIPS <- "1"

county_dat <- dat %>% filter(fips == county_FIPS)

state <- dat %>% pull(state) %>% unique()

st <- state.abb[match(state, state.name)]

my_matches <- find_my_matches(county_FIPS, dat)[[2]] 


# radar chart ----------------------------

radardd <- dd %>% 
  filter(grepl("sdoh_score", column_name))
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

df <- find_my_matches(county_FIPS, dat)[[1]] %>%
  rename(fips = FIPS) %>%
  mutate(county = tolower(County))

county_map_df <- map_data("county") %>%
  filter(region == tolower(state))

df <- full_join(df, county_map_df, by = c("county" = "subregion"))

df %>%
  group_by(group) %>%
  plot_ly(x = ~long, y = ~lat, color = ~fct_explicit_na(fct_rev(factor(distance))),
          colors = viridis_pal(option="D")(3),
          text = ~County, hoverinfo = 'text') %>%
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
outcomes <- grep("outcome_", names(dat), value = T)

outcomesdd <- dd %>% 
  filter(grepl("outcome_", column_name)) %>% 
  select(column_name, description)

# need to add coloring to the vlines
df_outcomes <- dat %>% select(FIPS, State, County, outcomes) %>%
  pivot_longer(cols = outcomes) %>%
  # selected county and matches to selected county
  mutate(type = case_when(
    FIPS %in% my_matches ~ "matches",
    FIPS == county_FIPS ~ "selected",
    TRUE ~ "other"
  )) %>%
  # left join data dictionary to get real outcome names
  rename(column_name = name) %>%
  left_join(outcomesdd, by = "column_name") 

df_outcome1 <- df_outcomes %>% 
  filter(column_name == "outcome_1")

ggplot(df_outcome1, aes(x=value)) + geom_density() + 
  geom_vline(data = filter(df_outcome1, type != "other"),
             aes(xintercept = value, color = as.factor(type)))
