# =========================================================================
# Useful to setup
# =========================================================================

# The following need to be installed:
# devtools::install_github(c("ramnathv/htmlwidgets", "smartinsightsfromdata/rpivotTable"))
# devtools::install_github('rCharts', 'ramnathv')
# devtools::install_github("FrissAnalytics/shinyJsTutorials/widgets/C3")
# To set the path to folder to use 'runApp()'
# setwd("/Users/leonardovida/Development/bi-project-2019/Dashboard/")
# or
# setwd("~/Development/bi-project-2019/Dashboard/")
# if want to drop all db connections: lapply(dbListConnections(PostgreSQL()), dbDisconnect)

# =========================================================================
# Load libraries and scripts
#
# Please load them before using the 'runApp()' command
# =========================================================================

require(devtools)
require(data.table)

library(DT)
library(BH)
library(xts)
library(pool)
library(ggalt)
library(shiny)
library(astsa)
library(dplyr)
library(rjson)
library(plotly)
library(dbplyr)
library(ggplot2)
library(tseries)
# library(rCharts)
library(ggimage)
library(forecast)
library(reshape2)
library(markdown)
library(ggfortify)
library(rsconnect)
library(rpivotTable)
library(RPostgreSQL)
library(highcharter)
library(flexdashboard)
library(shinydashboardPlus)
library(shinydashboard)

# =========================================================================
# Connection to postgres
# =========================================================================

pool <- dbPool(
  drv = RPostgreSQL::PostgreSQL(),
  host = "datawarehouse.cgsnvepxm7s7.eu-central-1.rds.amazonaws.com",
  user = "biteam2019",
  password = "teamteam",
  dbname = "datawarehouse",
  idleTimeout = 360000
)
onStop(function() {
  print("DB Closed")
  poolClose(pool)
})

# =========================================================================
# DB functions
# =========================================================================

eurFact <- function(){
  facts <- pool %>% tbl("fact_gen") %>% collect() %>% filter(csf_id == "innenv")
  return(facts)
}

wbFact <- function(){
  facts <- pool %>% tbl("fact_wb") %>%collect() %>% filter(csf_id == "easbus")
  return(facts)
}

susFact <- function(){
  facts <- pool %>% tbl("fact_sus") %>% collect() %>% filter(csf_id == "envsus") 
  return(facts)
}

yearDim <- function(){
  dimension <- pool %>% tbl("dim_year") %>% collect()
  return(dimension)
}

csfDim <- function(){
  dimension <- pool %>% tbl("dim_csf") %>% collect()
  return(dimension)
}

countryDim <- function(){
  dimension <- pool %>% tbl("dim_country") %>% collect()
  return(dimension)
}

indicatorDim <- function(){
  dimension <- pool %>% tbl("dim_indicator") %>% collect()
  return(dimension)
}

# =========================================================================
# ui.R variables
# =========================================================================

data.eur <- eurFact()
data.wb <- wbFact()
data.sus <- susFact()

data <- rbind(data.eur, data.wb, data.sus)
year.ids <- sort(unique(data$year_id))

countries <- countryDim()
indicators <- indicatorDim()
csfs <- csfDim()

# countries 
country.names <- sort(unique(countries$country_name))
country.ids <- sort(unique(countries$country_id))

# indicators
indicator.names <- sort(unique(indicators$indicator_name))
indicator.ids <- sort(unique(indicators$indicator_id))

#csfs
csf.names <- sort(unique(csfs$csf_name))
csf.ids <- sort(unique(csfs$csf_id))

# =========================================================================
# Global functions
# =========================================================================

# Plot individual KPI 
plotKpiData <- function(kpiInputData){
  kpi <- kpiInputData %>% distinct(indicator_id)
  indicatorAttr <- indicators[indicators$indicator_id==kpi$indicator_id,]
  
  p <- ggplot(kpiInputData, aes(x = year_id, y = value, color = country_id)) +
    geom_line() + 
    # geom_smooth(method="glm", method.args=list(family="binomial"),  # this must be fixed to overlay predictions
    #             fullrange=TRUE, se=FALSE) +
    theme(text = element_text(size = 12),
          axis.text.x = element_text(angle = 60, hjust = 1)) +
    labs(x = "Years",
         y = paste0(indicatorAttr$indicator_id),
         colour = "Country") +
    scale_x_continuous(breaks=seq(2010, 2022, 1))
    
  gg <- plotly_build(p)
  return(gg)
}

# Plot individual CSF rank data
plotCsfRankData <- function(csfInputData) {

  p <- ggplot(csfInputData, aes(x = year_id, y = points, fill = country_id)) +
    geom_bar(stat = "identity") +
    theme(text = element_text(size = 12),
          axis.text.x = element_text(angle = 60, hjust = 1),
          legend.position="bottom") +
    labs(x = "Years",
         y = "Relative points",
         colour = "Country") +
    scale_y_continuous(breaks=seq(1, 40, 2)) +
    scale_x_continuous(breaks=seq(2010, 2019, 1))
  
  gg <- plotly_build(p)
  return(gg)

}

# Plot indiviudal dumbbells
plotDotTable <- function(inputData, beginning, ending) {
  data1 <- inputData %>% filter(year_id == beginning) %>%
    select(c(country_id, ranks))  %>% rename(ranks_beg = ranks)
  
  data2 <- inputData %>% filter(year_id == ending) %>%
    select(c(country_id, ranks)) %>% rename(ranks_end = ranks)

  data3 <- data.frame(inner_join(data1, data2, by = "country_id"))
  df = tidyr::gather(data3, group, value, -country_id)
    
  p <- ggplot(data3, aes(y = country_id)) + 
    geom_point(data = df, aes(x = value, color = group), size = 3) +
    geom_dumbbell(aes(x=ranks_beg, xend=ranks_end), size=3, color="#e3e2e1", 
                      colour_x = "blue", colour_xend = "green",
                      dot_guide=FALSE, dot_guide_size=0.5) +
    theme_bw() +
    scale_color_manual(name = "Ranks",
                       values = c("blue", "green"),
                       labels = c("Initial ranking", "Ending ranking")) +
    theme(legend.position="bottom",
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15)) +
    scale_x_continuous(breaks=seq(4, 16, 2)) +
    xlab("Ranking points") +
    ylab("Country")
  
  p + geom_flag(y = -2, aes(image = country_id)) +
  
  return(p)
}

# Plot Graph in dashboard page
plotGenVis <- function(inputData) {
  p <- ggplot(inputData, aes(x = year_id, y = ranks, fill = country_id, group = country_id)) +
    geom_bar(stat = "identity") +
    theme(text = element_text(size = 12),
          axis.text.x = element_text(angle = 60, hjust = 1)) +
    labs(title = "Evolution of relative ranking across selected countries",
         x = "Years",
         y = "Relative points per year",
         colour = "Country") +
    scale_y_continuous(breaks=seq(4, 16, 1)) +
    scale_x_continuous(breaks=seq(2010, 2019, 1))
  
  gg <- plotly_build(p)
  return(gg)
}

# Gauge 
plotGauge <- function(inputGauge, targetValue, midTarget, maxValue, minValue, symbolValue) {
p <- gauge(inputGauge$value, min = minValue, max = maxValue, symbol = symbolValue,
        gaugeSectors(success = c(targetValue, maxValue), warning = c(midTarget, (targetValue-1)), danger = c(minValue, (midTarget-1))))
return(p)

}

plotGauge2 <- function(inputGauge, targetValue, midTarget, maxValue, minValue, symbolValue) {
  p <- gauge(inputGauge$value, min = minValue, max = maxValue, symbol = symbolValue,
             gaugeSectors(danger = c(midTarget+1, maxValue), warning = c(midTarget, (targetValue+1)), success = c(minValue, (targetValue))))
  return(p)
  
}


# =========================================================================
# Useful dataframes
# =========================================================================

indicators <- data.frame(
  indicator_name = c("SMEs introducing product or process innovations as percentage of SMEs",
                  "Public-private co-publications per million population",
                  "Private co-funding of public R&D expenditures",
                  "Trademark applications per billion GDP",
                  "Starting a business score",
                  "Getting credit score",
                  "Paying taxes score",
                  "Enforcing contracts score",
                  "Mean population exposure to PM 2.5",
                  "Shares of renewable consumption on total consumption of energy",
                  "Energy productivity",
                  "Environemntal Tax revenue",
                  "Paying taxes hours",
                  "Enforcing contracts days",
                  "Starting a business days"),
  indicator_id = c(
                     "smeinn",				
                     "ppcmln",		
                     "pcored",	
                     "trdbln",		
                     "strbus",		
                     "getcre",		
                     "paytax",
                     "enfcon",
                     "exppmt",
                     "shrrnw",
                     "prdtps",		
                     "taxrev",
                     "payhou",
                     "enfday",
                     "strday")
)
