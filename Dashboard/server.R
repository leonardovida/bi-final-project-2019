# source("/Users/leonardovida/Development/bi-project-2019/Dashboard/global.R")
# 
server <- function(input, output, session) {
  
  # =========================================================================
  # Server outputs: UI widgets
  # =========================================================================

  
  output$timelineControl <- renderUI({
    sliderInput(inputId = "timeline", "Timeline:", 
                min = 2010, max = 2019, value = c(2011, 2017), round = TRUE)
  })
  
  output$countryControl <- renderUI({
    checkboxGroupInput('country', 'Select one or more countries:', 
                       c("Germany" = "DE",
                         "Sweden" = "SE",
                         "Poland" = "PL",
                         "Israel" = "IL"), selected = countries$country_id)
  })
  
  output$csfControl <- renderUI({
    checkboxGroupInput('csf', 'Select one or more csfs:', 
                       c("Environment" = "envsus",
                         "Innovation" = "innenv",
                         "Business" = "easbus"), selected = csfs$csf_id)
  })
  
  # =========================================================================
  # Top dashboard outputs
  # =========================================================================
  
  output$csf1box <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = selectTop("innenv"),
      subtitle = "Top performer in Strong Innovative Environment for Startups",
      icon = icon("lightbulb", class = NULL, lib = "font-awesome"),
      color = "blue"
    )
  })
  
  output$csf2box <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = selectTop("easbus"),
      subtitle = "Top performer in Ease of Doing Business",
      icon = icon("briefcase", class = NULL, lib = "font-awesome"),
      color = "yellow"
    )
  })
  
  output$csf3box <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = selectTop("envsus"),
      subtitle = "Top performer in Green Economic Growth",
      icon = icon("leaf", class = NULL, lib = "font-awesome"),
      color = "green"
    )
  })
  
  output$csf1rank1 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = "1st",
      subtitle = selectRankCsf("innenv",1),
      icon = icon("lightbulb", class = NULL, lib = "font-awesome"),
      color = "blue"
    )
  })
  
  output$csf1rank2 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = "2nd",
      subtitle = selectRankCsf("innenv",2),
      icon = icon("lightbulb", class = NULL, lib = "font-awesome"),
      color = "blue"
    )
  })
  
  output$csf1rank3 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = "3rd",
      subtitle = selectRankCsf("innenv",3),
      icon = icon("lightbulb", class = NULL, lib = "font-awesome"),
      color = "blue"
    )
  })
  
  output$csf1rank4 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = "4th",
      subtitle = selectRankCsf("innenv",4),
      icon = icon("lightbulb", class = NULL, lib = "font-awesome"),
      color = "blue"
    )
  })
  
  output$csf2rank1 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = "1st",
      subtitle = selectRankCsf("easbus",1),
      icon = icon("briefcase", class = NULL, lib = "font-awesome"),
      color = "yellow"
    )
  })
  
  output$csf2rank2 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = "2nd",
      subtitle = selectRankCsf("easbus",2),
      icon = icon("briefcase", class = NULL, lib = "font-awesome"),
      color = "yellow"
    )
  })
  
  output$csf2rank3 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = "3rd",
      subtitle = selectRankCsf("easbus",3),
      icon = icon("briefcase", class = NULL, lib = "font-awesome"),
      color = "yellow"
    )
  })
  
  output$csf2rank4 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = "4th",
      subtitle = selectRankCsf("easbus",4),
      icon = icon("briefcase", class = NULL, lib = "font-awesome"),
      color = "yellow"
    )
  })
  
  output$csf3rank1 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = "1st",
      subtitle = selectRankCsf("envsus",1),
      icon = icon("leaf", class = NULL, lib = "font-awesome"),
      color = "green"
    )
  })
  
  output$csf3rank2 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = "2nd",
      subtitle = selectRankCsf("envsus",2),
      icon = icon("leaf", class = NULL, lib = "font-awesome"),
      color = "green"
    )
  })
  
  output$csf3rank3 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = "3rd",
      subtitle = selectRankCsf("envsus",3),
      icon = icon("leaf", class = NULL, lib = "font-awesome"),
      color = "green"
    )
  })
  
  output$csf3rank4 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = "4th",
      subtitle = selectRankCsf("envsus",4),
      icon = icon("leaf", class = NULL, lib = "font-awesome"),
      color = "green"
    )
  })
  
  # =========================================================================
  # Prepare datasets for outputs functions
  # =========================================================================
  
  # Function to select top countries in csf
  selectTop <- function(x) {
    result <- data %>% 
      filter(csf_id == x) %>% 
      group_by(country_id) %>%
      summarize(count = sum(rank_relative)) %>% 
      arrange(desc(count))
    value <- result[[1,1]]
    # Gather the name of the country corresponding to the code
    value <- filter(countries, country_id  == value)[[1]]
    
    return (value)
  }
  
  # Function to select top countries in csf
  selectRankCsf <- function(x, pos) {
    result <- data %>% 
      filter(csf_id == x) %>% 
      group_by(country_id) %>%
      summarize(count = sum(rank_relative)) %>% 
      arrange(desc(count))
    value <- result[[pos,1]]
    # Gather the name of the country corresponding to the code
    value <- filter(countries, country_id  == value)[[1]]
    
    return (value)
  }
  
  # Function for main retrieval of data filtered by input
  getGenInputData <- reactive({
    minYear <- input$timeline[1]
    maxYear <-input$timeline[2]
    countriesCode <- input$country
    csfsCode <- input$csf
    
    result <- data %>% filter(year_id >= minYear, year_id <= maxYear,
                            country_id %in% countriesCode,
                            csf_id %in% csfsCode) %>% 
      select(year_id, country_id, csf_id, indicator_id, rank_relative, value)
    
    return(result)
  })
  
  getCsfInputData <- reactive({
    minYear <- input$timeline[1]
    maxYear <-input$timeline[2]
    countriesCode <- input$country
    
    result <- data %>% filter(year_id >= minYear, year_id <= maxYear,
                              country_id %in% countriesCode) %>% 
      select(year_id, country_id, csf_id, indicator_id, rank_relative, value)
    
    return(result)
  })
  
  # Function for main Vis
  groupRank <- function(input) {
    result <- input %>%
      group_by(country_id, csf_id, year_id) %>% # Grouping variables
      summarise(ranks = sum(rank_relative)) %>% 
      ungroup()
    
    return(result)
  }
  
  # Function to retrieve data not filtered
  getGenData <- reactive({
    countriesCode <- input$country
    result <- data %>% filter(country_id %in% countriesCode)
    
    return(result)
  })
  
  # Get last data for gauges
  getLastData <- reactive({
    maxYear <- input$timeline[2]
    result <- data %>% filter(year_id == maxYear)
    
    return(result)
  })
  
  retrieveGaugeData <- function(input, indicatorIdGauge, countryIdGauge) {
    result <- input %>% 
      filter(indicator_id == indicatorIdGauge, country_id == countryIdGauge)
    
    return(result)
  }
  
  # Function to filter by csf_id and display dumbbells
  groupRankByCsf <- function(input, csfId) {
    result <- input %>%
      filter(csf_id == csfId) %>% 
      group_by(country_id, csf_id, year_id) %>% # Grouping variables
      summarise(ranks = sum(rank_relative)) %>% 
      ungroup()
    
    return(result)
  }
  
  # Function for KPI pages - ranks' plot
  retrieveRankedCsf <- function(input, csfName) {
    result <- input %>%
      filter(csf_id == csfName) %>% 
      group_by(country_id, csf_id, year_id) %>% # Grouping variables
      summarise(points = sum(rank_relative)) %>% 
      ungroup()
    
    return(result)
  }
  
  # Function for KPI pages - KPIs plot
  retrieveKpi <- function(input, kpiName) {
    result <- input %>%
      filter(indicator_id == kpiName)
    
    return(result)
  }
  
  # =========================================================================
  # Dahsboard page outputs
  # =========================================================================
  
  # Small Dumbells
  output$genSmallTable1 <- renderPlot({
    plotDotTable(groupRankByCsf(getGenData(), "innenv"), 2010, 2017)
  })
  
  output$genSmallTable2 <- renderPlot({
    plotDotTable(groupRankByCsf(getGenData(), "easbus"), 2010, 2017)
  })
  
  output$genSmallTable3 <- renderPlot({
    plotDotTable(groupRankByCsf(getGenData(), "envsus"), 2010, 2017)
  })
  
  # Chart
  output$genPlot <- renderPlotly({
    plotGenVis(groupRank(getGenInputData()))
  })
  
  output$genTable <- renderDataTable({
    data
  })
  
  # Hover events for bar chart
  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover on a point!" else d
  })
  
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) "Click to keep data (double-click to clear)" else d
  })
  
  # Pivot table
  output$pivotTable <- renderRpivotTable({
    dataFrameEurostat <- data.frame(data.eur)
    
    rpivotTable(data = dataFrameEurostat, rows = "country_id",
                cols=c("year_id"),
                vals = "value",
                rendererName = "Pivot Table")
  })
  
  # =========================================================================
  # CSF1 page outputs
  # =========================================================================
  
  output$genCsf1 <- renderPlotly({
    plotCsfRankData(retrieveRankedCsf(getCsfInputData(), "innenv"))
  })
  
  output$csf1kpi1 <- renderPlotly({
    plotKpiData(retrieveKpi(getGenInputData(), "smeinn"))
  })

  output$csf1kpi2 <- renderPlotly({
    plotKpiData(retrieveKpi(getGenInputData(), "ppcmln"))
  })
  
  output$csf1kpi3 <- renderPlotly({
    plotKpiData(retrieveKpi(getGenInputData(), "pcored"))
  })
  
  output$csf1kpi4 <- renderPlotly({
    plotKpiData(retrieveKpi(getGenInputData(), "trdbln"))
  })
  
  #Gauges
  # output$gaugecsf1kpi1 <- renderGauge({
  #   gauge(42, min = 0, max = 100, symbol = '%', gaugeSectors(success = c(80, 100), warning = c(40, 79), danger = c(0, 39)
  #   ))
  # })
  
  #kpi1
  
  output$gaugecsf1kpi1de <- renderGauge({
    plotGauge(retrieveGaugeData(getLastData(),"smeinn", "DE"), 48.26, 25, 50, 0, "%")
  })
  
  output$gaugecsf1kpi1se <- renderGauge({
    plotGauge(retrieveGaugeData(getLastData(),"smeinn", "SE"), 48.26, 25, 50, 0, "%")
  })
  
  output$gaugecsf1kpi1pl <- renderGauge({
    plotGauge(retrieveGaugeData(getLastData(),"smeinn", "PL"), 48.26, 25, 50, 0, "%")
  })
  
  output$gaugecsf1kpi1il <- renderGauge({
    plotGauge(retrieveGaugeData(getLastData(),"smeinn", "IL"), 48.26, 25, 50, 0, "%")
  })
  
  #kpi2
  
  output$gaugecsf1kpi2de <- renderGauge({
    plotGauge(retrieveGaugeData(getLastData(),"ppcmln", "DE"), 260.58, 220, 280, 20, "")
  })
  
  output$gaugecsf1kpi2se <- renderGauge({
    plotGauge(retrieveGaugeData(getLastData(),"ppcmln", "SE"), 260.58, 220, 280, 20, "")
  })
  
  output$gaugecsf1kpi2pl <- renderGauge({
    plotGauge(retrieveGaugeData(getLastData(),"ppcmln", "PL"), 260.58, 220, 280, 20, "")
  })
  
  output$gaugecsf1kpi2il <- renderGauge({
    plotGauge(retrieveGaugeData(getLastData(),"ppcmln", "IL"), 260.58, 220, 280, 20, "")
  })
  
  #kpi3
  
  output$gaugecsf1kpi3de <- renderGauge({
    plotGauge(retrieveGaugeData(getLastData(),"pcored", "DE"), 0.5, 0.25, 0.6, 0, "%")
  })
  
  output$gaugecsf1kpi3se <- renderGauge({
    plotGauge(retrieveGaugeData(getLastData(),"pcored", "SE"), 0.5, 0.25, 0.6, 0, "%")
  })
  
  output$gaugecsf1kpi3pl <- renderGauge({
    plotGauge(retrieveGaugeData(getLastData(),"pcored", "PL"), 0.5, 0.25, 0.6, 0, "%")
  })
  
  output$gaugecsf1kpi3il <- renderGauge({
    plotGauge(retrieveGaugeData(getLastData(),"pcored", "IL"), 0.5, 0.25, 0.6, 0, "%")
  })
  
  
  #kpi4
  
  output$gaugecsf1kpi4de <- renderGauge({
    plotGauge(retrieveGaugeData(getLastData(),"trdbln", "DE"), 43.14, 25, 45, 0, "")
  })
  
  output$gaugecsf1kpi4se <- renderGauge({
    plotGauge(retrieveGaugeData(getLastData(),"trdbln", "SE"), 43.14, 25, 45, 0, "")
  })
  
  output$gaugecsf1kpi4pl <- renderGauge({
    plotGauge(retrieveGaugeData(getLastData(),"trdbln", "PL"), 43.14, 25, 45, 0, "")
  })
  
  output$gaugecsf1kpi4il <- renderGauge({
    plotGauge(retrieveGaugeData(getLastData(),"trdbln", "IL"), 43.14, 25, 45, 0, "")
  })
  
  # =========================================================================
  # CSF2 page outputs
  # =========================================================================
  
  output$genCsf2 <- renderPlotly({
    plotCsfRankData(retrieveRankedCsf(getCsfInputData(), "easbus"))
  })
  
  output$csf2kpi1 <- renderPlotly({
    plotKpiData(retrieveKpi(getGenInputData(), "getcre"))
  })
  
  output$csf2kpi2 <- renderPlotly({
    plotKpiData(retrieveKpi(getGenInputData(), "strday"))
  })
  
  output$csf2kpi3 <- renderPlotly({
    plotKpiData(retrieveKpi(getGenInputData(), "payhou"))
  })
  
  output$csf2kpi4 <- renderPlotly({
    plotKpiData(retrieveKpi(getGenInputData(), "enfday"))
  })
  
  #Gauges
  
  #kpi1
  
  output$gaugecsf2kpi1de <- renderGauge({
    plotGauge(retrieveGaugeData(getLastData(),"getcre", "DE"), 100, 50, 100, 0, "")
  })
  
  output$gaugecsf2kpi1se <- renderGauge({
    plotGauge(retrieveGaugeData(getLastData(),"getcre", "SE"), 100, 50, 100, 0, "")
  })
  
  output$gaugecsf2kpi1pl <- renderGauge({
    plotGauge(retrieveGaugeData(getLastData(),"getcre", "PL"), 100, 50, 100, 0, "")
  })
  
  output$gaugecsf2kpi1il <- renderGauge({
    plotGauge(retrieveGaugeData(getLastData(),"getcre", "IL"), 100, 50, 100, 0, "")
  })
  
  #kpi2
  
  output$gaugecsf2kpi2de <- renderGauge({
    plotGauge2(retrieveGaugeData(getLastData(),"strday", "DE"), 0.5, 10, 50, 0, "")
  })
  
  output$gaugecsf2kpi2se <- renderGauge({
    plotGauge2(retrieveGaugeData(getLastData(),"strday", "SE"), 0.5, 10, 50, 0, "")
  })
  
  output$gaugecsf2kpi2pl <- renderGauge({
    plotGauge2(retrieveGaugeData(getLastData(),"strday", "PL"), 0.5, 10, 50, 0, "")
  })
  
  output$gaugecsf2kpi2il <- renderGauge({
    plotGauge2(retrieveGaugeData(getLastData(),"strday", "IL"), 0.5, 10, 50, 0, "")
  })
  
  #kpi3
  
  output$gaugecsf2kpi3de <- renderGauge({
    plotGauge2(retrieveGaugeData(getLastData(),"payhou", "DE"), 12, 50, 200, 0, "")
  })
  
  output$gaugecsf2kpi3se <- renderGauge({
    plotGauge2(retrieveGaugeData(getLastData(),"payhou", "SE"), 12, 50, 200, 0, "")
  })
  
  output$gaugecsf2kpi3pl <- renderGauge({
    plotGauge2(retrieveGaugeData(getLastData(),"payhou", "PL"), 12, 50, 200, 0, "")
  })
  
  output$gaugecsf2kpi3il <- renderGauge({
    plotGauge2(retrieveGaugeData(getLastData(),"payhou", "IL"), 12, 50, 200, 0, "")
  })
  
  
  #kpi4
  
  output$gaugecsf2kpi4de <- renderGauge({
    plotGauge2(retrieveGaugeData(getLastData(),"enfday", "DE"), 164, 220, 365, 1, "")
  })
  
  output$gaugecsf2kpi4se <- renderGauge({
    plotGauge2(retrieveGaugeData(getLastData(),"enfday", "SE"), 164, 220, 365, 1, "")
  })
  
  output$gaugecsf2kpi4pl <- renderGauge({
    plotGauge2(retrieveGaugeData(getLastData(),"enfday", "PL"), 164, 220, 365, 1, "")
  })
  
  output$gaugecsf2kpi4il <- renderGauge({
    plotGauge2(retrieveGaugeData(getLastData(),"enfday", "IL"), 164, 220, 365, 1, "")
  })
  
  
  
  # =========================================================================
  # CSF3 page outputs
  # =========================================================================
  
  output$genCsf3 <- renderPlotly({
    plotCsfRankData(retrieveRankedCsf(getCsfInputData(), "innenv"))
  })
  
  output$csf3kpi1 <- renderPlotly({
    plotKpiData(retrieveKpi(getGenInputData(), "exppmt"))
  })
  
  output$csf3kpi2 <- renderPlotly({
    plotKpiData(retrieveKpi(getGenInputData(), "taxrev"))
  })
  
  output$csf3kpi3 <- renderPlotly({
    plotKpiData(retrieveKpi(getGenInputData(), "shrrnw"))
  })
  
  output$csf3kpi4 <- renderPlotly({
    plotKpiData(retrieveKpi(getGenInputData(), "prdtps"))
  })
  
  #Gauges
  
  #kpi1
  
  output$gaugecsf3kpi1de <- renderGauge({
    plotGauge2(retrieveGaugeData(getLastData(),"exppmt", "DE"), 12, 20, 80, 0, "")
  })
  
  output$gaugecsf3kpi1se <- renderGauge({
    plotGauge2(retrieveGaugeData(getLastData(),"exppmt", "SE"), 12, 20, 80, 0, "")
  })
  
  output$gaugecsf3kpi1pl <- renderGauge({
    plotGauge2(retrieveGaugeData(getLastData(),"exppmt", "PL"), 12, 20, 80, 0, "")
  })
  
  output$gaugecsf3kpi1il <- renderGauge({
    plotGauge2(retrieveGaugeData(getLastData(),"exppmt", "IL"), 12, 20, 80, 0, "")
  })
  
  
  #kpi2
  
  output$gaugecsf3kpi2de <- renderGauge({
    plotGauge(retrieveGaugeData(getLastData(),"taxrev", "DE"), 4, 1.5, 5, 0, "")
  })
  
  output$gaugecsf3kpi2se <- renderGauge({
    plotGauge(retrieveGaugeData(getLastData(),"taxrev", "SE"), 4, 1.5, 5, 0, "")
  })
  
  output$gaugecsf3kpi2pl <- renderGauge({
    plotGauge(retrieveGaugeData(getLastData(),"taxrev", "PL"), 4, 1.5, 5, 0, "")
  })
  
  output$gaugecsf3kpi2il <- renderGauge({
    plotGauge(retrieveGaugeData(getLastData(),"taxrev", "IL"), 4, 1.5, 5, 0, "")
  })
  
  #kpi 3
  
  output$gaugecsf3kpi3de <- renderGauge({
    plotGauge(retrieveGaugeData(getLastData(),"shrrnw", "DE"), 15, 10, 25, 0, "")
  })
  
  output$gaugecsf3kpi3se <- renderGauge({
    plotGauge(retrieveGaugeData(getLastData(),"shrrnw", "SE"), 15, 10, 25, 0, "")
  })
  
  output$gaugecsf3kpi3pl <- renderGauge({
    plotGauge(retrieveGaugeData(getLastData(),"shrrnw", "PL"), 15, 10, 25, 0, "")
  })
  
  output$gaugecsf3kpi3il <- renderGauge({
    plotGauge(retrieveGaugeData(getLastData(),"shrrnw", "IL"), 15, 10, 25, 0, "")
  })
  
  
  #kpi4
  
  output$gaugecsf3kpi4de <- renderGauge({
    plotGauge(retrieveGaugeData(getLastData(),"prdtps", "DE"), 15143, 10000, 17000, 5000, "")
  })
  
  output$gaugecsf3kpi4se <- renderGauge({
    plotGauge(retrieveGaugeData(getLastData(),"prdtps", "SE"), 15143, 10000, 17000, 5000, "")
  })
  
  output$gaugecsf3kpi4pl <- renderGauge({
    plotGauge(retrieveGaugeData(getLastData(),"prdtps", "PL"), 15143, 10000, 17000, 5000, "")
  })
  
  output$gaugecsf3kpi4il <- renderGauge({
    plotGauge(retrieveGaugeData(getLastData(),"prdtps", "IL"), 15143, 10000, 17000, 5000, "")
  })
  
  # Correlation and Prediction

  getCorrData <- reactive({
    var1 <- input$csfCorr1
    result <- data %>% filter(csf_id == var1)

    return(result)
  })

  output$corrPlot <- renderPlot({
    inputData <- getCorrData()
    inputData <- dcast(inputData, country_id + year_id ~ indicator_id)
    # Select variable columns
    inputData <- inputData[, c(3,4,5,6)]
    # Create corr matrix
    cormat <- round(cor(inputData),2)
    # Get upper triangle matrix
    # From http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
    cormat[lower.tri(cormat)] <- NA 
    upper_tri <- cormat
    # Reshape data
    melted_cormat <- melt(upper_tri, na.rm = TRUE)
    
    g <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="Pearson\nCorrelation") +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                       size = 12, hjust = 1)) +
      coord_fixed()
    
    return(g)
  })

  getPredData <- reactive({
    var <- input$predInd
    countryP <- input$countryPred
    result <- data %>% filter(indicator_id == var, country_id == countryP)
    
    return(result)
  })
  
  
  output$predPlot <- renderPlotly({
  
    inputData <- getPredData()
    inputData <- dcast(inputData, country_id + year_id ~ indicator_id)
    
    # Convert into timeseries
    myts <- ts(inputData[,3], start=min(inputData$year_id), end=max(inputData$year_id), frequency=1)
    
    # Predict with auto arima
    dif <- diff(myts)
    fit = auto.arima(dif, seasonal = FALSE, allowmean = TRUE, allowdrift = TRUE)
    pred = predict(fit, n.ahead = 5)
    input_pred <- myts[length(myts)]
    for(i in 1:length(pred$pred)){
      input_pred[i+1]<-input_pred[i]+pred$pred[i]
    }
    input_pred <- ts(input_pred, start=max(inputData$year_id), frequency=1)
    
    # From ts to df
    end_ts <- ts(c(myts,input_pred), start=start(myts), frequency=frequency(myts))
    df <- data.frame(years=index(end_ts), coredata(end_ts)) %>% 
      rename(Values = coredata.end_ts.)
    
    # Convert to years to plot
    df$years <- lubridate::ymd(df$years, truncated = 2L)

    # Plot prediction + historic
    p <- ggplot(df, aes(x = years, y = Values)) + 
      geom_line(size = 1) +
      # scale_x_continuous(breaks=seq(2010, 2022, 1)) +
      xlab("Date") +
      ylab("Value") +
      theme_minimal()
    
    gg <- plotly_build(p)
    return(gg)
    
  })

  
}
