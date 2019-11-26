server <- function(input, output) {
  
  # mathematica logo
  output$logo <- renderUI({
    img(src='logo.png', aligh = 'right', height = '50px')
  })
  
  
  options(DT.options = list(dom = "t", ordering = F))

  # error handling checks ------------------------------------------------------
  county_check <- reactive({
    if (input$county_selection_type == "fips") {
      gsub("^0", "", input$county_selection) %in% dat$fips
    } else if (input$county_selection_type == "name") {
      input$county_selection %in% dat$county
    }
  })
  
  # reactive values and data frames --------------------------------------------
  county_fips <- reactive({
    req(county_check())
    if (input$county_selection_type == "fips") {
      gsub("^0", "", input$county_selection)
    } else if (input$county_selection_type == "name") {
      dat %>% filter(county == input$county_selection) %>% pull(fips)
    }
  })
  
  county_name <- reactive({
    req(county_check())
    if (input$county_selection_type == "name") {
      input$county_selection
    } else if (input$county_selection_type == "fips") {
      dat %>% filter(fips == gsub("^0", "", input$county_selection)) %>% pull(county)
    }
  })
  
  county_state <- reactive({
    req(county_check())
    dat %>% filter(fips == county_fips()) %>% pull(state)
  })
  
  county_dat <- reactive({
    dat %>% filter(fips == county_fips())
  })
  
  comp_county_dat <- reactive({
    req(county_check())
    req(input$comparison_county_selection)
    dat %>% filter(county == input$comparison_county_selection)
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
  output$select_my_county <- renderUI({
    req(input$county_selection_type)
    
    if (input$county_selection_type == "fips") {
      choice_list <- sort(str_pad(dat$fips, width = 5, pad = "0"))
    } else if (input$county_selection_type == "name") {
      choice_list <- sort(dat$county)
    }
    selectizeInput('county_selection', label = lang_cfg$titles$county_selection,
                   choices = choice_list)
  })
  
  ## methodology modal dialogue ------------------------------------------------
  observeEvent(input$method_read_more, {
    showModal(modalDialog(
      title = lang_cfg$titles$method_modal,
      HTML(lang_cfg$method),
      size = "l",
      footer = modalButton("Close"),
      easyClose = T
    ))
  })
  
  ## data sources modal dialogue -----------------------------------------------
  observeEvent(input$data_read_more, {
    showModal(modalDialog(
      title = lang_cfg$titles$data_source_modal,
      HTML(lang_cfg$data),
      size = "l",
      footer = modalButton("Close"),
      easyClose = T
    ))
  })
  
  ## radar chart description modal dialogue ------------------------------------
  observeEvent(input$radar_read_more, {
    showModal(modalDialog(
      title = lang_cfg$titles$radar_modal_title,
      HTML(lang_cfg$howto),
      size = "l",
      footer = modalButton("Close"),
      easyClose = T
    ))
  })
  
  ## selected county information -----------------------------------------------
  output$my_county_header <- renderUI({
    req(county_check())
    HTML(paste0("<h3>Social Determinants of Health in <br>", county_name(), ", ", county_state(), "</h3>"))
  })
  
  
  output$my_county_radar <- renderPlotly({
    req(county_check())
    req(input$comparison_county_selection)
    
    sdoh_dd <- get_dd(dd, "sdoh_score")
    
    sdohs <- sdoh_dd %>% pull(column_name)
    
    my_county_df <- county_dat() %>% select(county, state, sdohs)
    
    if (input$comparison_county_selection == "None") {
      radar_chart(my_county_df, dd)
    } else {
      comp_county_df <- comp_county_dat() %>% select(county, state, sdohs)
      
      radar_chart_overlay(my_county_df, comp_county_df, dd)
    }
  })
  
  output$my_county_demo <- DT::renderDT({
    req(county_check())
    req(input$comparison_county_selection)
    df <- get_table_data(county_dat(), dd, "demographic") 
    
    if (input$comparison_county_selection != "None") {
      comp_df <- get_table_data(comp_county_dat(), dd, "demographic")
      df <- left_join(df, comp_df, by = "name")
    }
    
    df <- df %>%
      rename(`Essential facts` = name)
    
    DT::datatable(df, rownames = FALSE, class = "stripe") %>%
      DT::formatStyle(columns = colnames(df), fontSize = "9pt")
  })
  
  output$my_county_econ_stab <- DT::renderDT({
    req(county_check())
    req(input$comparison_county_selection)
    df <- get_table_data(county_dat(), dd, "used_sdoh_1") 
    
    if (input$comparison_county_selection != "None") {
      comp_df <- get_table_data(comp_county_dat(), dd, "used_sdoh_1")
      df <- left_join(df, comp_df, by = "name")
    }
    
    df <- df %>%
      rename(`Economic Stability` = name)
    
    DT::datatable(df, rownames = FALSE, class = "stripe") %>%
      DT::formatStyle(columns = colnames(df), fontSize = "9pt")
  })
  
  output$my_county_neigh <- DT::renderDT({
    req(county_check())
    req(input$comparison_county_selection)
    df <- get_table_data(county_dat(), dd, "used_sdoh_2") 
    
    if (input$comparison_county_selection != "None") {
      comp_df <- get_table_data(comp_county_dat(), dd, "used_sdoh_2")
      df <- left_join(df, comp_df, by = "name")
    }
    
    df <- df %>%
      rename(`Neighborhood & Physical Environment` = name)
    
    DT::datatable(df, rownames = FALSE, class = "stripe") %>%
      DT::formatStyle(columns = colnames(df), fontSize = "9pt")
  })
  
  output$my_county_edu <- DT::renderDT({
    req(county_check())
    req(input$comparison_county_selection)
    df <- get_table_data(county_dat(), dd, "used_sdoh_3") 
    
    if (input$comparison_county_selection != "None") {
      comp_df <- get_table_data(comp_county_dat(), dd, "used_sdoh_3")
      df <- left_join(df, comp_df, by = "name")
    }
    
    df <- df %>%
      rename(`Education` = name)
    
    DT::datatable(df, rownames = FALSE, class = "stripe") %>%
      DT::formatStyle(columns = colnames(df), fontSize = "9pt")
  })
  
  output$my_county_food <- DT::renderDT({
    req(county_check())
    req(input$comparison_county_selection)
    df <- get_table_data(county_dat(), dd, "used_sdoh_4") 
    
    if (input$comparison_county_selection != "None") {
      comp_df <- get_table_data(comp_county_dat(), dd, "used_sdoh_4")
      df <- left_join(df, comp_df, by = "name")
    }
    
    df <- df %>%
      rename(`Food` = name)
    
    DT::datatable(df, rownames = FALSE, class = "stripe") %>%
      DT::formatStyle(columns = colnames(df), fontSize = "9pt")
  })
  
  output$my_county_community <- DT::renderDT({
    req(county_check())
    req(input$comparison_county_selection)
    df <- get_table_data(county_dat(), dd, "used_sdoh_5") 
    
    if (input$comparison_county_selection != "None") {
      comp_df <- get_table_data(comp_county_dat(), dd, "used_sdoh_5")
      df <- left_join(df, comp_df, by = "name")
    }
    
    df <- df %>%
      rename(`Community` = name)
    
    DT::datatable(df, rownames = FALSE, class = "stripe") %>%
      DT::formatStyle(columns = colnames(df), fontSize = "9pt")
  })
  
  output$my_county_health <- DT::renderDT({
    req(county_check())
    req(input$comparison_county_selection)
    df <- get_table_data(county_dat(), dd, "used_sdoh_6") 
    
    if (input$comparison_county_selection != "None") {
      comp_df <- get_table_data(comp_county_dat(), dd, "used_sdoh_6")
      df <- left_join(df, comp_df, by = "name")
    }
    
    df <- df %>%
      rename(`Health Care System` = name)
    
    DT::datatable(df, rownames = FALSE, class = "stripe") %>%
      DT::formatStyle(columns = colnames(df), fontSize = "9pt")
  })
  
  
  ## selected comparison county info -------------------------------------------
  output$select_comparison_county <- renderUI({
    req(my_matches())
    comp_counties <- dat %>% filter(fips %in% my_matches()) %>% pull(county)
    selectInput('comparison_county_selection', label = lang_cfg$titles$comparison_county_selection,
                choices = c("None", comp_counties), selected = "None")
  })
  
  output$comp_county_demo <- DT::renderDT({
    req(comp_county_dat())

    # duplicated descriptions for different variables. the .copy column will be dropped once those duplicates are removed
    df <- get_table_data(comp_county_dat(), dd, "demographic") 
    
    DT::datatable(df, rownames = FALSE, colnames = c("Essential facts", ""), class = "stripe") %>%
      DT::formatStyle(columns = colnames(df), fontSize = "9pt")
  })
  
  output$comp_county_econ_stab <- DT::renderDT({
    req(comp_county_dat())
    # duplicated descriptions for different variables. the .copy column will be dropped once those duplicates are removed
    df <- get_table_data(comp_county_dat(), dd, "used_sdoh_1") 
    
    DT::datatable(df, rownames = FALSE, colnames = c("Economic Stability", ""), class = "stripe") %>%
      DT::formatStyle(columns = colnames(df), fontSize = "9pt")
  })
  
  output$comp_county_neigh <- DT::renderDT({
    req(comp_county_dat())
    # duplicated descriptions for different variables. the .copy column will be dropped once those duplicates are removed
    df <- get_table_data(comp_county_dat(), dd, "used_sdoh_2") 
    
    DT::datatable(df, rownames = FALSE, colnames = c("Neighborhood & Physical Environment", ""), class = "stripe") %>%
      DT::formatStyle(columns = colnames(df), fontSize = "9pt")
  })
  
  output$comp_county_edu <- DT::renderDT({
    req(comp_county_dat())
    # duplicated descriptions for different variables. the .copy column will be dropped once those duplicates are removed
    df <- get_table_data(comp_county_dat(), dd, "used_sdoh_3") 
    
    DT::datatable(df, rownames = FALSE, colnames = c("Education", ""), class = "stripe") %>%
      DT::formatStyle(columns = colnames(df), fontSize = "9pt")
  })
  
  output$comp_county_food <- DT::renderDT({
    req(comp_county_dat())
    # duplicated descriptions for different variables. the .copy column will be dropped once those duplicates are removed
    df <- get_table_data(comp_county_dat(), dd, "used_sdoh_4") 
    
    DT::datatable(df, rownames = FALSE, colnames = c("Food", ""), class = "stripe") %>%
      DT::formatStyle(columns = colnames(df), fontSize = "9pt")
  })
  
  output$comp_county_community <- DT::renderDT({
    req(comp_county_dat())
    # duplicated descriptions for different variables. the .copy column will be dropped once those duplicates are removed
    df <- get_table_data(comp_county_dat(), dd, "used_sdoh_5") 
    
    DT::datatable(df, rownames = FALSE, colnames = c("Community", ""), class = "stripe") %>%
      DT::formatStyle(columns = colnames(df), fontSize = "9pt")
  })
  
  output$comp_county_health <- DT::renderDT({
    req(comp_county_dat())
    # duplicated descriptions for different variables. the .copy column will be dropped once those duplicates are removed
    df <- get_table_data(comp_county_dat(), dd, "used_sdoh_6") 
    
    DT::datatable(df, rownames = FALSE, colnames = c("Health Coverage", ""), class = "stripe") %>%
      DT::formatStyle(columns = colnames(df), fontSize = "9pt")
  })
  ## comparison counties info --------------------------------------------------
  output$comp_radar_header <- renderUI({
    req(my_matches())
    tagList(
      HTML(paste0("<h4>What Counties are Most Similar to ", county_name(), ", ", county_state(), "</h4>"))
    )
  })
  
  output$compare_county_radars <- renderPlotly({
    req(county_check())
    
    sdoh_dd <- get_dd(dd, "sdoh_score")
    
    sdohs <- sdoh_dd %>% pull(column_name)
    
    df <- dat %>% select(fips, state, county, sdohs) %>%
      filter(fips %in% my_matches())
    
    grid_radar(df, dd)
  })
  
  output$map_header <- renderUI({
    req(my_matches())
    tagList(
      HTML(paste0("<h3>County Map<br/></h3>"))
    )
  })
  
  output$map <- renderLeaflet({
    req(county_check())
    
    df <- find_my_matches(county_fips(), dat)[[1]] %>%
      mutate(GEOID = str_pad(fips, width = 5, side = "left", pad = "0")) %>%
      mutate(NAME = str_replace(county, " County", ""))
    
    st_fips <- str_sub(df$GEOID, 1, 2)[1]
    cty_fips <- str_pad(county_fips(), width = 5, side = "left", pad = "0")
    
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
      mutate(cat = cut(distance, breaks = c(quantile(distance, probs = seq(0, 1, by = 0.25))),
                       labels = c("Very similar", "Somewhat similar", "Somewhat different", "Very different"),
                       include.lowest = TRUE))
    
    county_label <- sprintf(
      "<strong>Comparison County:</strong> %s County (%s) <br/>
      <strong>Similarity to %s County: </strong> %s <br/>",
      selected_state_shp$NAME, selected_state_shp$GEOID,
      selected_geo$NAME[1], selected_state_shp$cat
    ) %>%
      lapply(htmltools::HTML)
    selected_county_label <- sprintf(
      "<strong>Selected County:</strong> %s (%s) <br/>",
      selected_geo$NAME[1], selected_geo$GEOID[1]) %>%
      lapply(htmltools::HTML)
    
    # Always 4 quantile breaks
    color_pal <- colorFactor("magma", selected_state_shp$cat, na.color = "gray")
    
    leaflet(options = leafletOptions(minZoom = 6, maxZoom = 13)) %>%
      setView(lng = x_mid, lat = y_mid, zoom = 6) %>%
      setMaxBounds(lng1 = x_min,
                   lat1 = y_min,
                   lng2 = x_max,
                   lat2 = y_max) %>%
      addProviderTiles(providers$Stamen.TonerLite) %>%
      addPolygons(data = selected_state_shp,
                  color = ~color_pal(selected_state_shp$cat),
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
      addLegend(pal = color_pal, values = selected_state_shp$cat, position = "topright",
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
  })
  
  # dynamic number of density graphs -------------------------------------------

  output$health_outcomes_header <- renderUI({
    req(county_check())
    tagList(
      fluidRow(
        column(width = 6, selectInput('outcome_filter', label = 'Filter by health conditions:', 
                           choices = c('All' = 'all', 
                                       'Diabetes' = 'diabetes',  
                                       'Kidney Disease' = 'kidney',
                                       'Obesity' = 'obesity'),
                           selected = 'all')
        ),
        column(width = 6, checkboxInput(inputId = 'show_matches', 
                                        label = 'Include Density Plot from Matching Counties'),
               value = F)))
  })
  
  
  density_graphs <- eventReactive(
    {outcomes_dat()
     comp_county_dat()
     input$outcome_filter
     input$show_matches
     }, {
    
    if(input$comparison_county_selection == "None") {
      if (!input$show_matches) {
        outcomes_dat() %>%
          group_by(column_name) %>%
          nest() %>%
          # filter by dropdown selection
          filter_category(input$outcome_filter) %>%
          mutate(graphs = purrr::map(data, density_plot)) %>%
          pull(graphs)
      } else {
        outcomes_dat() %>%
          group_by(column_name, higher_better) %>%
          nest() %>%
          # filter by dropdown selection
          filter_category(input$outcome_filter) %>%
          mutate(graphs = purrr::map(data, density_plot_overlay)) %>%
          pull(graphs)
      }
    } else {
      if (!input$show_matches) {
        compare_countyname <- comp_county_dat() %>% 
          pull(county)
        outcomes_dat() %>%
          group_by(column_name) %>%
          nest() %>%
          # filter by dropdown selection
          filter_category(input$outcome_filter) %>%
          mutate(compare_name = compare_countyname) %>% 
          mutate(compare_value = purrr::map2(data, compare_name, get_compare_value)) %>% 
          mutate(graphs = purrr::map2(data, compare_value, density_plot)) %>%
          pull(graphs)
      } else {
        compare_countyname <- comp_county_dat() %>% 
          pull(county)
        outcomes_dat() %>%
          group_by(column_name, higher_better) %>%
          nest() %>%
          # filter by dropdown selection
          filter_category(input$outcome_filter) %>%
          mutate(compare_name = compare_countyname) %>% 
          mutate(compare_value = purrr::map2(data, compare_name, get_compare_value)) %>% 
          mutate(graphs = purrr::map2(data, compare_value, density_plot_overlay)) %>%
          pull(graphs)
      }
    }
    
    
  })
  
  observeEvent(
    {outcomes_dat()
    comp_county_dat()
    input$outcome_filter
    input$show_matches}, {
    req(density_graphs())
    
    purrr::iwalk(density_graphs(), ~{
      output_name <- paste0("density_graph", .y)
      output[[output_name]] <- renderPlotly(.x)
    })
  })
  
  
  
  output$density_graphs_ui <- renderUI({
    req(county_check())
    req(density_graphs())
    
    density_plots_list <- purrr::imap(density_graphs(), ~{
      tagList(
        plotlyOutput(
          outputId = paste0("density_graph", .y),
          height = '200px'
        ) #%>% withSpinner(type = getOption("spinner.type", default = 1))
        ,
        br()
      )
    })
    tagList(density_plots_list)
  })
  
  
}
