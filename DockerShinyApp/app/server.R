server <- function(input, output) {
  
#  county_selection_check <- reactive({
#    validate({
#      if (input$county_selection_type == "fips") {
#        need(nchar(input$county_selection) == 4, 'Please enter a four digit fips Code for your county.')
#      } else if (input$county_selection_type == "name") {
#        need(input$county_selection %in% dat$county, 'Please enter a valid county name.')
#      }
#    })
#  })
#  
#  output$county_selection_message <- renderText({
#    county_selection_check
#  })
  
  options(DT.options = list(dom = "t", ordering = F))

  # error handling checks ------------------------------------------------------
  county_check <- reactive({
    if (input$county_selection_type == "fips") {
      input$county_selection %in% dat$fips
    } else if (input$county_selection_type == "name") {
      input$county_selection %in% dat$county
    }
  })
  
  # reactive values and data frames --------------------------------------------
  county_fips <- reactive({
    req(county_check)
    if (input$county_selection_type == "fips") {
      input$county_selection
    } else if (input$county_selection_type == "name") {
      dat %>% filter(county == input$county_selection) %>% pull(fips)
    }
  })
  
  county_name <- reactive({
    req(county_check)
    if (input$county_selection_type == "name") {
      input$county_selection
    } else if (input$county_selection_type == "fips") {
      dat %>% filter(fips == input$county_selection) %>% pull(county)
    }
  })
  
  county_dat <- reactive({
    dat %>% filter(fips == county_fips())
  })
  
  # creates list of matched counties
  my_matches <- reactive({
    req(county_check())
    find_my_matches(county_fips(), dat, 20)[[2]] 
  })
  
  # outcomes data
  outcomes_dat <- reactive({
    req(county_check())
    
    outcomes_dd <- get_dd(dd, "outcome")
    
    outcomes <- outcomes_dd %>% pull(column_name)
    
    # need to add coloring to the vlines
    df <- dat %>% select(fips, state, county, outcomes) %>%
      pivot_longer(cols = outcomes) %>%
      # selected county and matches to selected county
      mutate(type = case_when(
        fips %in% my_matches() ~ "matches",
        fips == county_fips() ~ "selected",
        TRUE ~ "other"
      )) %>%
      # left join data dictionary to get real outcome names
      rename(column_name = name) %>%
      left_join(outcomes_dd, by = "column_name") 
    df
  })
  
  # output ---------------------------------------------------------------------

  output$my_county_name <- renderUI({
    req(county_check())
    HTML(paste0("<h3>My Selection<br/></h3>", "<h4>", county_name(), ", ", county_dat()$state, "</h4>"))
  })
  
  
  output$my_county_radar <- renderPlot({
    req(county_check())
    
    sdoh_dd <- get_dd(dd, "sdoh_score")
    
    sdohs <- sdoh_dd %>% pull(column_name)
    
    df <- make_radar_data(county_dat() %>% select(sdohs), sdoh_dd)
    
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
  })
  
  output$my_county_demo <- DT::renderDT({
    req(county_check())
    
    dd <- dd %>%
      filter(grepl("demographics_", column_name))
    df <- county_dat() %>% select(starts_with("demographics_")) %>%
      rename_at(vars(dd$column_name), ~ dd$descrip_new) %>%
      t() %>% data.frame() %>%
      rownames_to_column()
    DT::datatable(df, rownames = FALSE, colnames = c("Essential facts", ""), class = "stripe") %>%
      DT::formatStyle(columns = colnames(df), fontSize = "9pt",
                      background = config$colors$tan25)
  })
  
  output$compare_county_radars <- renderPlot({
    req(county_check())
    
    dd <- dd %>% 
      filter(grepl("sdoh_score", column_name)) %>% 
      select(column_name, description, descrip_new)
    
    # find number of rows for plot
    plot_nrows <- ceiling(length(my_matches()) / 5)

    par(mfrow = c(plot_nrows, 5), bg = config$colors$tan25)
    df <- dat %>% select(fips, state, county, starts_with("sdoh_score")) %>%
      filter(fips %in% my_matches()) %>%
      group_by(fips, county, state) %>%
      nest() %>%
      mutate(radar_data = purrr::map(data, make_radar_data, dd = dd)) %>%
      mutate(radar_char = purrr::map(radar_data, radarchart, pcol = c(NA, NA, paste0(config$colors$red100, '80')), 
                              plty = 0,
                              pfcol = c(paste0(config$colors$grey50, '80'),
                                        paste0(config$colors$grey25, '33'),
                                        paste0(config$colors$teal100, '33')),
                              cglcol = config$colors$grey100,
                              seg = 4, vlcex = 0.8,
                              title = paste0(county, ", ", state)))
  })

  output$map <- renderPlotly({
    req(county_check())
    
    state <- dat %>% pull(state) %>% unique()
    st <- state.abb[match(state, state.name)]
    
    df <- find_my_matches(county_fips(), dat, 20)[[1]] %>%
      rename(fips = fips) %>%
      mutate(county = tolower(county))
    
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
                     
  })
  
  # dynamic number of density graphs -------------------------------------------
  density_graphs <- eventReactive(input$county_selection, {
    req(outcomes_dat())
    
    outcomes_dat() %>%
      group_by(column_name) %>%
      nest() %>%
      mutate(graphs = purrr::map(data, make_density_graph)) %>%
      arrange(column_name) %>%
      pull(graphs)
  })
  
  observeEvent(input$county_selection, {
    req(density_graphs())
    
    purrr::iwalk(density_graphs(), ~{
      output_name <- paste0("density_graph", .y)
      output[[output_name]] <- renderPlot(.x)
    })
  })
  
  output$density_graphs_ui <- renderUI({
    req(density_graphs())
    
    density_plots_list <- purrr::imap(density_graphs(), ~{
      tagList(
        plotOutput(
          outputId = paste0("density_graph", .y)
        ),
        br()
      )
    })
    tagList(density_plots_list)
  })
  # r2d3
  
 # output$test <- renderD3({
 #   r2d3(
 #     c(0.3, 0.6, 0.8, 0.95, 0.40, 0.20),
 #     script = "./d3/radar_chart.js"
 #   )
 # })

  
}
