header <- dashboardHeaderPlus(
  title = "R&I in SMEs", 
  # titleWidth = 230,
  enable_rightsidebar = TRUE,
  rightSidebarIcon = "filter"
  )

rightsidebar <- rightSidebar(
  background = "dark",
  rightSidebarTabContent(
    id = 1,
    title = "Change time",
    icon = "calendar",
    active = TRUE,
    uiOutput("timelineControl")
  ),
  rightSidebarTabContent(
    id = 2,
    icon = "globe",
    title = "Change country",
    uiOutput("countryControl")
  ),
  rightSidebarTabContent(
    id = 3,
    icon = "bullseye",
    title = "Change CSFs",
    uiOutput("csfControl")
  )
)

sidebar <- dashboardSidebar(
    width = 230,
    sidebarMenu(
      menuItem("Highlights", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("CSF1", icon = icon("lightbulb", class = NULL, lib = "font-awesome"),
               menuSubItem("CSF1", tabName = "csf1", icon = icon("bar-chart-o")),
               menuSubItem("KPI1", tabName = "csf1kpi1menu", icon = icon("bar-chart-o")),
               menuSubItem("KPI2", tabName = "csf1kpi2menu", icon = icon("bar-chart-o")),
               menuSubItem("KPI3", tabName = "csf1kpi3menu", icon = icon("bar-chart-o")),
               menuSubItem("KPI4", tabName = "csf1kpi4menu", icon = icon("bar-chart-o"))
               ),
      menuItem("CSF2", icon = icon("briefcase", class = NULL, lib = "font-awesome"),
               menuSubItem("CSF2", tabName = "csf2", icon = icon("bar-chart-o")),
               menuSubItem("KPI1", tabName = "csf2kpi1menu", icon = icon("bar-chart-o")),
               menuSubItem("KPI2", tabName = "csf2kpi2menu", icon = icon("bar-chart-o")),
               menuSubItem("KPI3", tabName = "csf2kpi3menu", icon = icon("bar-chart-o")),
               menuSubItem("KPI4", tabName = "csf2kpi4menu", icon = icon("bar-chart-o"))
      ),
      menuItem("CSF3", icon = icon("leaf", class = NULL, lib = "font-awesome"),
               menuSubItem("CSF3", tabName = "csf3", icon = icon("bar-chart-o")),
               menuSubItem("KPI1", tabName = "csf3kpi1menu", icon = icon("bar-chart-o")),
               menuSubItem("KPI2", tabName = "csf3kpi2menu", icon = icon("bar-chart-o")),
               menuSubItem("KPI3", tabName = "csf3kpi3menu", icon = icon("bar-chart-o")),
               menuSubItem("KPI4", tabName = "csf3kpi4menu", icon = icon("bar-chart-o"))
      ),
      menuItem("Correlations and Predictions",
               tabName = "corr",
               icon = icon("chart-line", class = NULL, lib = "font-awesome")
               ),
      menuItem("About",
               tabName = "about",
               icon = icon("info", class = NULL, lib = "font-awesome")
               )
    )
)
  
body <- dashboardBody(
  tabItems(
    # 1st tab content: Highlights
    tabItem(tabName = "dashboard",
            fluidRow(
              box(
                  width = 12,
                  background = "light-blue",
                  h2("Main Highlights", align = "center"),
                  p("In order to display the graph the database must be pooled. 
                    Please click on the right sidebar and click on each of the filters' tabs", align = "center")
              )
            ),
            fluidRow(
              shinydashboard::valueBoxOutput("csf1box", width = 4),
              shinydashboard::valueBoxOutput("csf2box", width = 4),
              shinydashboard::valueBoxOutput("csf3box", width = 4)
            ),
            fluidRow(
              box(
                solidHeader = FALSE,
                title = "KPI that showed the best improvement for best performer (2010-2017)",
                background = NULL,
                width = 4,
                status = "danger",
                footer = fluidRow(
                    descriptionBlock(
                      # What do we put here? Points of best KPI?
                      number = "20.05%",
                      number_color = "green",
                      number_icon = "fa fa-caret-up",
                      header = "12%",
                      text = "Private co-funding of private R&D expenditure",
                      right_border = TRUE,
                      margin_bottom = FALSE
                    )
                )
              ),
              box(
                solidHeader = FALSE,
                title = "KPI that showed the best improvement for best performer (2010-2017)",
                background = NULL,
                width = 4,
                status = "danger",
                footer = fluidRow(
                  descriptionBlock(
                    number = "-4.29%",
                    number_color = "green",
                    number_icon = "fa fa-caret-up",
                    header = "201",
                    text = "Days spent enforcing contracts",
                    right_border = TRUE,
                    margin_bottom = FALSE
                  )
                )
              ),
              box(
                solidHeader = FALSE,
                title = "KPI that showed the best improvement for best performer (2010-2017)",
                background = NULL,
                width = 4,
                status = "danger",
                footer = fluidRow(
                  descriptionBlock(
                    number = "25.52%",
                    number_color = "green",
                    number_icon = "fa fa-caret-up",
                    header = "9626.82",
                    text = "Energy productivity ($/TPS)",
                    right_border = TRUE,
                    margin_bottom = FALSE
                  )
                )
              )
            ),
            fluidRow(
              boxPlus(width = 4,
                  title = "Change of CSF 1 rankings across selected years",
                  plotOutput("genSmallTable1",height = 250),
                  status = "primary",
                  collapsible = TRUE,
                  closable = FALSE),
              boxPlus(width = 4,
                  title = "Change of CSF 2 rankings across selected years",
                  plotOutput("genSmallTable2", height = 250),
                  status = "primary",
                  collapsible = TRUE,
                  closable = FALSE),
              boxPlus(width = 4,
                  title = "Change of CSF 3 rankings across selected years",
                  plotOutput("genSmallTable3", height = 250),
                  status = "primary",
                  collapsible = TRUE,
                  closable = FALSE)
            ),
            fluidRow(
              boxPlus(width = 4,
                  title = "Strong Innovative Environment for Startups",
                  p("A Strong Innovative Environment for Startups should consist of factors that
                    put a premium on the development of entrepreneurship and be built to bring innovative
                    ideas from research to life."),
                  status = "info",
                  collapsible = TRUE,
                  closable = FALSE
              ),
              boxPlus(width = 4,
                  title = "Ease of Doing Business",
                  p("Ease of Doing Business provides an understanding of whether the regulations
                    in a country are promoting entrepreneurship."),
                  status = "info",
                  collapsible = TRUE,
                  closable = FALSE
              ),
              boxPlus(width = 4,
                  title = "Green Economic Growth",
                  p("Green Economic Growth measures the importance each country
                    places on fostering green growth and sustainable prosperity."),
                  status = "info",
                  collapsible = TRUE,
                  closable = FALSE
              )
            ),
            fluidRow(
              tabBox(width = 12,
                     title = "Pivot table and filter table",
                     id = "genRanking",
                     selected = "Pivot",
                     tabPanel(p(icon("line-chart"), "Pivot"), 
                              rpivotTableOutput("pivotTable", width = "100%")
                              ),
                     tabPanel(p(icon("table"), "Table"), 
                              dataTableOutput("genTable"))
              )
            )
    ),
    
    # 2nd page: CSF 1 highlights
    tabItem(tabName = "csf1",
        fluidRow(
          box(
            width = 12,
            background = "light-blue",
            h2("Strong Innovative Environment for Startups: Highlights",align="center")
          )
        ),
        fluidRow(
          shinydashboard::valueBoxOutput("csf1rank1", width = 3),
          shinydashboard::valueBoxOutput("csf1rank2", width = 3),
          shinydashboard::valueBoxOutput("csf1rank3", width = 3),
          shinydashboard::valueBoxOutput("csf1rank4", width = 3)
        ),
      fluidRow(
        boxPlus(width = 9,
           title = "Ranking for Strong Innovative Environment for Startups",
           plotlyOutput("genCsf1"),
           status = "primary",
           collapsible = TRUE,
           closable = FALSE
        ),
        boxPlus(width = 3,
            title = "Strong Innovative Environment for Startups",
            p("A Strong Innovative Environment for Startups should consist of factors that put a 
              premium on the development of entrepreneurship and be built to bring innovative ideas 
              from research to life."),
            status = "primary",
            collapsible = TRUE,
            closable = FALSE
            )
      )
    ),
    
    # 3rd page: CSF 1 - KPI1
    tabItem(tabName = "csf1kpi1menu",
        fluidRow(
          box(
            width = 12,
            background = "light-blue",
            h2("Startups introducing product or process innovations as percentage of SMEs",align="center")
          )
        ),
        fluidRow(
          boxPlus(width=3,
                    title="Germany",
                    flexdashboard::gaugeOutput("gaugecsf1kpi1de"),
                    height = 200
              ),
          boxPlus(width=3,
                  title="Sweden",
                  flexdashboard::gaugeOutput("gaugecsf1kpi1se"),
                  height = 200
          ),
          boxPlus(width=3,
                  title="Poland",
                  flexdashboard::gaugeOutput("gaugecsf1kpi1pl"),
                  height = 200
          ),
          boxPlus(width=3,
                  title="Israel",
                  flexdashboard::gaugeOutput("gaugecsf1kpi1il"),
                  height = 200
          )
        ),
        fluidRow(
          boxPlus(width = 9,
              title = "Startups introducing product or process innovations as percentage of SMEs",
              plotlyOutput("csf1kpi1"),
              status = "primary",
              collapsible = TRUE,
              closable = FALSE
          ),
          boxPlus(width = 3,
                  title = "Description of this KPI", 
                  p(h3("Target: Belgium 48.26%"),
                  "Technological innovation is an important measure of 
                  innovation in the production industry. The capacity to 
                  innovate is highly reflected in the levels of innovation activities."),
                  status = "primary",
                  collapsible = TRUE,
                  closable = FALSE
          )
        )
    ),

    
    # 4th page: CSF 1 - KPI2
    tabItem(tabName = "csf1kpi2menu",
        fluidRow(
          box(
            width = 12,
            background = "light-blue",
            h2("Public-private co-publications per million population",align="center")
          )
        ),
        fluidRow(
          boxPlus(width=3,
                  title="Germany",
                  flexdashboard::gaugeOutput("gaugecsf1kpi2de"),
                  height = 200
          ),
          boxPlus(width=3,
                  title="Sweden",
                  flexdashboard::gaugeOutput("gaugecsf1kpi2se"),
                  height = 200
          ),
          boxPlus(width=3,
                  title="Poland",
                  flexdashboard::gaugeOutput("gaugecsf1kpi2pl"),
                  height = 200
          ),
          boxPlus(width=3,
                  title="Israel",
                  flexdashboard::gaugeOutput("gaugecsf1kpi2il"),
                  height = 200
          )
        ),
        fluidRow(
          boxPlus(width = 9,
              title = "Public-private co-publications per million population",
              plotlyOutput("csf1kpi2"),
              status = "primary",
              collapsible = TRUE,
              closable = FALSE
          ),
          boxPlus(width = 3,
              title = "Description of this KPI", 
              p(h3("Target: Switzerland 260.58"),
              "The collaboration between researchers in the public
              and private sectors is crucial to maintain a flourishing academic research ecosystem."),
              status = "primary",
              collapsible = TRUE,
              closable = FALSE
          )
        )
    ),
    
    # 5th page: CSF 1 - KPI3
    tabItem(tabName = "csf1kpi3menu",
        fluidRow(
          box(
            width = 12,
            background = "light-blue",
            h2("Private co-funding of public R&D expenditures as a percentage of GDP",align="center")
          )
        ),
        fluidRow(
          boxPlus(width=3,
                  title="Germany",
                  flexdashboard::gaugeOutput("gaugecsf1kpi3de"),
                  height = 200
          ),
          boxPlus(width=3,
                  title="Sweden",
                  flexdashboard::gaugeOutput("gaugecsf1kpi3se"),
                  height = 200
          ),
          boxPlus(width=3,
                  title="Poland",
                  flexdashboard::gaugeOutput("gaugecsf1kpi3pl"),
                  height = 200
          ),
          boxPlus(width=3,
                  title="Israel",
                  flexdashboard::gaugeOutput("gaugecsf1kpi3il"),
                  height = 200
          )
        ),
        fluidRow(
          boxPlus(width = 9,
              title = "Private co-funding of public R&D expenditures as a percentage of GDP",
              plotlyOutput("csf1kpi3"),
              status = "primary",
              collapsible = TRUE,
              closable = FALSE
          ),
          boxPlus(width = 3,
              title = "Description of this KPI",
              p(h3("Target: Israel 0.572% (GDP)"),
              "The cooperation between public and private sectors is key to prosperity.
              Research activity is usually expected to serve the needs of the business industry."),
              status = "primary",
              collapsible = TRUE,
              closable = FALSE
          )
        )
    ),
    
    # 6th page: CSF 1 - KPI4
    tabItem(tabName = "csf1kpi4menu",
        fluidRow(
          box(
            width = 12,
            background = "light-blue",
            h2(" Trademark applications per billion GDP (in PPS)",align="center")
          )
        ),
        fluidRow(
          boxPlus(width=3,
                  title="Germany",
                  flexdashboard::gaugeOutput("gaugecsf1kpi4de"),
                  height = 200
          ),
          boxPlus(width=3,
                  title="Sweden",
                  flexdashboard::gaugeOutput("gaugecsf1kpi4se"),
                  height = 200
          ),
          boxPlus(width=3,
                  title="Poland",
                  flexdashboard::gaugeOutput("gaugecsf1kpi4pl"),
                  height = 200
          ),
          boxPlus(width=3,
                  title="Israel",
                  flexdashboard::gaugeOutput("gaugecsf1kpi4il"),
                  height = 200
          )
        ),
        fluidRow(
          boxPlus(width = 9,
              title = " Trademark applications per billion GDP (in PPS)",
              plotlyOutput("csf1kpi4"),
              status = "primary",
              collapsible = TRUE,
              closable = FALSE
          ),
          boxPlus(width = 3,
              title = "Description of this KPI",
              p(h3("Target: Cyrpus 43.14 (per Billion PPS)"),
              "Trademarks identify the origin of goods and services, 
              guarantees consistent quality through evidence of the organization’s commitment to the consumer and innovation."),
              status = "primary",
              collapsible = TRUE,
              closable = FALSE
          )
        )
    ),
      
    # 7th page: CSF 2 - Highlights
    tabItem(tabName = "csf2",
        fluidRow(
          box(
            width = 12,
            background = "light-blue",
            h2("Ease of Doing Business: Highlights",align="center")
          )
        ),
        fluidRow(
          shinydashboard::valueBoxOutput("csf2rank1", width = 3),
          shinydashboard::valueBoxOutput("csf2rank2", width = 3),
          shinydashboard::valueBoxOutput("csf2rank3", width = 3),
          shinydashboard::valueBoxOutput("csf2rank4", width = 3)
        ),
        fluidRow(
          boxPlus(width = 9,
              title = "Ranking for Ease of Doing Business",
              plotlyOutput("genCsf2"),
              status = "warning",
              collapsible = TRUE,
              closable = FALSE
          ),
          boxPlus(width = 3,
              title = "Description of this CSF",
              "Ease of Doing Business provides an understanding
              of whether the regulations in a country are promoting entrepreneurship.",
              status = "warning",
              collapsible = TRUE,
              closable = FALSE
          )
        )
    ),
    
    
    # 9th page: CSF 2 - KPI 1
    tabItem(tabName = "csf2kpi1menu",
            fluidRow(
              box(
                width = 12,
                background = "light-blue",
                h2("Ease of getting credit (score)",align="center")
              )
            ),
            fluidRow(
              boxPlus(width=3,
                      title="Germany",
                      flexdashboard::gaugeOutput("gaugecsf2kpi1de"),
                      height = 200
              ),
              boxPlus(width=3,
                      title="Sweden",
                      flexdashboard::gaugeOutput("gaugecsf2kpi1se"),
                      height = 200
              ),
              boxPlus(width=3,
                      title="Poland",
                      flexdashboard::gaugeOutput("gaugecsf2kpi1pl"),
                      height = 200
              ),
              boxPlus(width=3,
                      title="Israel",
                      flexdashboard::gaugeOutput("gaugecsf2kpi1il"),
                      height = 200
              )
            ),
            fluidRow(
              boxPlus(width = 9,
                      title = "Ease of getting credit (score)",
                      plotlyOutput("csf2kpi1"),
                      status = "warning",
                      collapsible = TRUE,
                      closable = FALSE
              ),
              boxPlus(width = 3,
                      title = "Description of this KPI",
                      status = "warning",
                      collapsible = TRUE,
                      closable = FALSE,
                      p(h3("Target: New Zealand 100 (score)"),
                        "This score covers access to finance twofold: 1) the robustness of credit 
              reporting systems and 2) the effectiveness of collateral/bankruptcy laws in enabling lending."),
                      br(),
                      timelineBlock(
                        timelineEnd(color = "danger"),
                        timelineLabel(2019, color = "teal"),
                        timelineItem(
                          title = "Last available year of data",
                          color = "olive",
                          time = "2019"),
                        timelineLabel(2014, color = "orange"),
                        timelineItem(
                          title = "Methodology changes",
                          icon = "gear",
                          color = "maroon",
                          time = "2014"),
                        timelineStart(color = "gray")
                      )
              )
            )
    ),
    
    
    # 8th page: CSF 2 - KPI 2
    tabItem(tabName = "csf2kpi2menu",
      fluidRow(
        box(
          width = 12,
          background = "light-blue",
          h2("Days needed to start a business", align="center")
        )
      ),
      fluidRow(
        boxPlus(width=3,
                title="Germany",
                flexdashboard::gaugeOutput("gaugecsf2kpi2de"),
                height = 200
        ),
        boxPlus(width=3,
                title="Sweden",
                flexdashboard::gaugeOutput("gaugecsf2kpi2se"),
                height = 200
        ),
        boxPlus(width=3,
                title="Poland",
                flexdashboard::gaugeOutput("gaugecsf2kpi2pl"),
                height = 200
        ),
        boxPlus(width=3,
                title="Israel",
                flexdashboard::gaugeOutput("gaugecsf2kpi2il"),
                height = 200
        )
      ),
      fluidRow(
        boxPlus(width = 9,
            title = "Days to start a business",
            plotlyOutput("csf2kpi2"),
            status = "warning",
            collapsible = TRUE,
            closable = FALSE
        ),
        boxPlus(width = 3,
            title = "Description of this KPI",
            collapsible = TRUE,
            closable = FALSE,
            status = "warning",
            p(h3("Target: New Zealand 0.5 (days/year)"),
            "This score measures the number of procedures,
            time, cost and paid-in minimum capital requirement for an SME to begin operations."),
            br(),
            timelineBlock(
              timelineEnd(color = "danger"),
              timelineLabel(2018, color = "teal"),
              timelineItem(
                title = "Last available year of data",
                color = "olive",
                time = "2019"),
              timelineLabel(2015, color = "orange"),
              timelineItem(
                title = "Methodology changes",
                icon = "gear",
                color = "maroon",
                time = "2016"),
              timelineStart(color = "gray")
            )
        )
      )
    ),
  
    # 10th page: CSF 2 - KPI 3
    tabItem(tabName = "csf2kpi3menu",
        fluidRow(
          box(
            width = 12,
            background = "light-blue",
            h2("Paying taxes (hours per year)",align="center")
          )
        ),
        fluidRow(
          boxPlus(width=3,
                  title="Germany",
                  flexdashboard::gaugeOutput("gaugecsf2kpi3de"),
                  height = 200
          ),
          boxPlus(width=3,
                  title="Sweden",
                  flexdashboard::gaugeOutput("gaugecsf2kpi3se"),
                  height = 200
          ),
          boxPlus(width=3,
                  title="Poland",
                  flexdashboard::gaugeOutput("gaugecsf2kpi3pl"),
                  height = 200
          ),
          boxPlus(width=3,
                  title="Israel",
                  flexdashboard::gaugeOutput("gaugecsf2kpi3il"),
                  height = 200
          )
        ),
        fluidRow(
          boxPlus(width = 9,
              title = "Paying taxes (hours per year)",
              plotlyOutput("csf2kpi3"),
              status = "warning",
              collapsible = TRUE,
              closable = FALSE
          ),
          boxPlus(width = 3,
              title = "Description of this KPI",
              status = "warning",
              collapsible = TRUE,
              closable = FALSE,
              p(h3("Target: United Arab Emirates 12 (hours/year)"),
              "This score measures the amount of taxes and mandatory contributions that an
              SME must pay, as well as the administrative load of paying taxes."),
              br(),
              timelineBlock(
                timelineEnd(color = "danger"),
                timelineLabel(2019, color = "teal"),
                timelineItem(
                  title = "Last available year of data",
                  color = "olive",
                  time = "2019"),
                timelineLabel(2016, color = "orange"),
                timelineItem(
                  title = "Methodology changes",
                  icon = "gear",
                  color = "maroon",
                  time = "2016"),
                timelineStart(color = "gray")
              )
          )
        )
    ),
    
    # 11th page: CSF 2 - KPI 4
    tabItem(tabName = "csf2kpi4menu",
      fluidRow(
        box(
          width = 12,
          background = "light-blue",
          h2("Days spent in enforcing contracts",align="center")
        )
      ),
      fluidRow(
        boxPlus(width=3,
                title="Germany",
                flexdashboard::gaugeOutput("gaugecsf2kpi4de"),
                height = 200
        ),
        boxPlus(width=3,
                title="Sweden",
                flexdashboard::gaugeOutput("gaugecsf2kpi4se"),
                height = 200
        ),
        boxPlus(width=3,
                title="Poland",
                flexdashboard::gaugeOutput("gaugecsf2kpi4pl"),
                height = 200
        ),
        boxPlus(width=3,
                title="Israel",
                flexdashboard::gaugeOutput("gaugecsf2kpi4il"),
                height = 200
        )
      ),
      fluidRow(
        boxPlus(width = 9,
            title = "Days spent in enforcing contracts",
            plotlyOutput("csf2kpi4"),
            status = "warning",
            collapsible = TRUE,
            closable = FALSE
        ),
        boxPlus(width = 3,
            title = "Description of this KPI",
            status = "warning",
            collapsible = TRUE,
            closable = FALSE,
            p(h3("Target: Singapore 164 (days/year)"),
            "This score measures the time and cost for settling a commercial 
        dispute and the quality of judicial processes index, assessing whether an 
        economy has incorporated good practices that stimulate quality in the court system."),
            br(),
            timelineBlock(
              timelineEnd(color = "danger"),
              timelineLabel(2019, color = "teal"),
              timelineItem(
                title = "Last available year of data",
                color = "olive",
                time = "2019"),
              timelineLabel(2016, color = "orange"),
              timelineItem(
                title = "Additional methodology changes",
                icon = "gear",
                color = "maroon",
                time = "2016"),
              timelineLabel(2015, color = "orange"),
              timelineItem(
                title = "Methodology changes",
                icon = "gear",
                color = "maroon",
                time = "2015"),
              timelineStart(color = "gray")
            )
        )
      )
    ),
    
    
    # 12th page: CSF 3 - Highlights
    tabItem(tabName = "csf3",
      fluidRow(
        box(
          width = 12,
          background = "light-blue",
          h2("Green Economic Growth: Highlights",align="center")
        )
      ),
        fluidRow(
          shinydashboard::valueBoxOutput("csf3rank1", width = 3),
          shinydashboard::valueBoxOutput("csf3rank2", width = 3),
          shinydashboard::valueBoxOutput("csf3rank3", width = 3),
          shinydashboard::valueBoxOutput("csf3rank4", width = 3)
        ),
        fluidRow(
          box(width = 9,
              title = "Ranking for Green Economic Growth",
              plotlyOutput("genCsf3")
          ),
          box(width = 3,
              title = "Description of this CSF",
              "Green Economic Growth measures the importance each 
              country places on fostering green growth and sustainable prosperity."
          )
        )
    ),
    
    # 13th page: CSF 3 - KPI 1
    tabItem(tabName = "csf3kpi1menu",
      fluidRow(
        box(
          width = 12,
          background = "light-blue",
          h2("Mean population exposure to PM 2.5",align="center")
        )
      ),
      fluidRow(
        boxPlus(width=3,
                title="Germany",
                flexdashboard::gaugeOutput("gaugecsf3kpi1de"),
                height = 200
        ),
        boxPlus(width=3,
                title="Sweden",
                flexdashboard::gaugeOutput("gaugecsf3kpi1se"),
                height = 200
        ),
        boxPlus(width=3,
                title="Poland",
                flexdashboard::gaugeOutput("gaugecsf3kpi1pl"),
                height = 200
        ),
        boxPlus(width=3,
                title="Israel",
                flexdashboard::gaugeOutput("gaugecsf3kpi1il"),
                height = 200
        )
      ),
      fluidRow(
          boxPlus(width = 9,
              title = "Mean population exposure to PM 2.5 ",
              plotlyOutput("csf3kpi1"),
              status = "success",
              collapsible = TRUE,
              closable = FALSE
          ),
          boxPlus(width = 3,
              title = "Description of this KPI",
              p(h3("Target: 12 (micrograms per meter)"),
              "Exposure to airborne particulate matter (PM) poses a great risk health 
              risk and a threat to not only humans but all life as we know on earth. 
              The KPI is calculated by weighting concentrations with populations."),
              status = "success",
              collapsible = TRUE,
              closable = FALSE
          )
      )
    ),
    
    # 14th page: CSF 3 - KPI 2
    tabItem(tabName = "csf3kpi2menu",
      fluidRow(
        box(
          width = 12,
          background = "light-blue",
          h2("Environmental tax revenue",align="center")
        )
      ),
      fluidRow(
        boxPlus(width=3,
                title="Germany",
                flexdashboard::gaugeOutput("gaugecsf3kpi2de"),
                height = 200
        ),
        boxPlus(width=3,
                title="Sweden",
                flexdashboard::gaugeOutput("gaugecsf3kpi2se"),
                height = 200
        ),
        boxPlus(width=3,
                title="Poland",
                flexdashboard::gaugeOutput("gaugecsf3kpi2pl"),
                height = 200
        ),
        boxPlus(width=3,
                title="Israel",
                flexdashboard::gaugeOutput("gaugecsf3kpi2il"),
                height = 200
        )
      ),
        fluidRow(
          boxPlus(width = 9,
              title = " Environmental tax revenue ",
              plotlyOutput("csf3kpi2"),
              status = "success",
              collapsible = TRUE,
              closable = FALSE
          ),
          boxPlus(width = 3,
              title = "Description of this KPI",
              p(h3("Target: Denmark 4% (GDP)"),
              "Environmental tax revenue is around 5% of all tax revenue in the OECD area. 
              Environmental tax can be in the form of taxes on nuclear and fossil fuel, carbon taxes, 
              and local air pollution taxes."),
              status = "success",
              collapsible = TRUE,
              closable = FALSE
          )
        )
    ),
    
    # 15th page: CSF 3 - KPI 3
    tabItem(tabName = "csf3kpi3menu",
      fluidRow(
        box(
          width = 12,
          background = "light-blue",
          h2("Share of renewable consumption on vs. total consumption of energy (IES)",align="center")
        )
      ),
      fluidRow(
        boxPlus(width=3,
                title="Germany",
                flexdashboard::gaugeOutput("gaugecsf3kpi3de"),
                height = 200
        ),
        boxPlus(width=3,
                title="Sweden",
                flexdashboard::gaugeOutput("gaugecsf3kpi3se"),
                height = 200
        ),
        boxPlus(width=3,
                title="Poland",
                flexdashboard::gaugeOutput("gaugecsf3kpi3pl"),
                height = 200
        ),
        boxPlus(width=3,
                title="Israel",
                flexdashboard::gaugeOutput("gaugecsf3kpi3il"),
                height = 200
        )
      ),
        fluidRow(
          boxPlus(width = 9,
              title = "Share of renewable consumption on vs. total consumption of energy (IES)",
              plotlyOutput("csf3kpi3"),
              status = "success",
              collapsible = TRUE,
              closable = FALSE
          ),
          boxPlus(width = 3,
              title = "Description of this KPI",
              p(h3("Target: European 2030 target at least 32% (Ktons/total)"),
              "Measure the shares of consumption of renewable energy - hydropower, 
              geothermal and wind on the overall consumption of energy."),
              status = "success",
              collapsible = TRUE,
              closable = FALSE
          )
        )
    ),
    
    # 16th page: CSF 3 - KPI 4
    tabItem(tabName = "csf3kpi4menu",
      fluidRow(
        box(
          width = 12,
          background = "light-blue",
          h2("Energy productivity GDP",align="center")
        )
      ),
      fluidRow(
        boxPlus(width=3,
                title="Germany",
                flexdashboard::gaugeOutput("gaugecsf3kpi4de"),
                height = 200
        ),
        boxPlus(width=3,
                title="Sweden",
                flexdashboard::gaugeOutput("gaugecsf3kpi4se"),
                height = 200
        ),
        boxPlus(width=3,
                title="Poland",
                flexdashboard::gaugeOutput("gaugecsf3kpi4pl"),
                height = 200
        ),
        boxPlus(width=3,
                title="Israel",
                flexdashboard::gaugeOutput("gaugecsf3kpi4il"),
                height = 200
        )
      ),
        fluidRow(
          boxPlus(width = 9,
              title = "Energy productivity GDP",
              plotlyOutput("csf3kpi4"),
              status = "success",
              collapsible = TRUE,
              closable = FALSE
          ),
          boxPlus(width = 3,
              title = "Description of this KPI",
              p(h3("Target: 15.143 (GDP per unit of TPES)"),
              "Energy productivity of the economy provides a measure for progress towards 
              green growth. It is defined as the “economic output, in terms of GDP, generated 
              per unit of primary energy uses."),
              status = "success",
              collapsible = TRUE,
              closable = FALSE
          )
        )
    ),

    tabItem(tabName = "corr",
        fluidRow(
          box(
            width = 12,
            background = "light-blue",
            h2("Correlations and Predictions",align="center")
          )
        ),
        fluidRow(
          boxPlus(
            width = 6,
            title = "Correlation Plot", 
            closable = TRUE, 
            status = "primary", 
            solidHeader = FALSE, 
            collapsible = TRUE,
            enable_sidebar = TRUE,
            sidebar_width = 30,
            sidebar_start_open = TRUE,
            sidebar_content = tagList(
              selectInput(
                "csfCorr1", "Choose a CSF:", c("Environment" = "envsus",
                                               "Innovation" = "innenv",
                                               "Business" = "easbus"), selected = "easbus"
              )
            ),
            plotOutput("corrPlot")
          ),
          boxPlus(
            width = 6,
            title = "Prediction Plot", 
            closable = TRUE, 
            status = "primary", 
            solidHeader = FALSE, 
            collapsible = TRUE,
            enable_sidebar = TRUE,
            sidebar_width = 30,
            sidebar_start_open = TRUE,
            sidebar_content = tagList(
              selectInput(
                "predInd", "Choose an indicator:",
                c("SMEs introducing product or process innovations as percentage of SMEs" = "smeinn",
                   "Public-private co-publications per million population" = "ppcmln",
                   "Private co-funding of public R&D expenditures" = "pcored",
                   "Trademark applications per billion GDP" = "trdbln",
                   "Starting a business score" = "strbus",
                   "Getting credit score" = "getcre",
                   "Paying taxes score" = "paytax",
                   "Enforcing contracts score"=  "enfcon",
                   "Mean population exposure to PM 2.5"  = "exppmt",
                   "Shares of renewable consumption on total consumption of energy" = "shrrnw",
                   "Energy productivity" = "prdtps",
                   "Environemntal Tax revenue" = "taxrev"),
                selected = "smeinn"
              ),
              selectInput(
                "countryPred", 
                'Select one country:', 
                c("Germany" = "DE",
                  "Sweden" = "SE",
                  "Poland" = "PL",
                  "Israel" = "IL"), selected = "IL"
                )
            ),
            plotlyOutput("predPlot")
          )
        )
    ),

    tabItem(tabName = "about",
        fluidRow(
          box(
            width = 12,
            background = "light-blue",
            h2("About the team",align="center")
          )
        ),
        fluidRow(
          widgetUserBox(
            title = "Leonardo Vida",
            subtitle = "Master student of Business Informatics",
            width = 6,
            type = 2,
            src = "leo.png",
            color = "blue",
            p("I am currently studying the MSc in Business Informatics at Utrecht University and am on 
            track to graduating with honors from the Applied Data Science profile. I am working as a 
            Data Scientist at the Research-IT department of Utrecht University, where I am running big 
            data analyses on assessment-related data. I have a bachelor’s degree in International Economics 
            from Maastricht University.I worked as a business analyst for one year in a boutique management 
            consulting firm between Europe, Hong Kong, and Brazil. I also organized the first FinTech 
            incubation program in Italy where 10 Italian banks invested more than €1M. I have a keen 
            interest in data analysis, machine learning and venture capital. I spend my free time 
            volunteering for Volt, the first pan-European political movement, and researching about 
            social innovation.")
          ),
          widgetUserBox(
            title = "Sara Altamirano",
            subtitle = "Master student of Business Informatics",
            width = 6,
            type = 2,
            src = "sara.png",
            color = "blue",
            p("I am currently studying the MSc in Business Informatics at Utrecht University in the Netherlands
            with the Applied Data Science profile. I am also a researcher at the UMC Utrecht in the Department of
            Rheumatology and Clinical Immunology. I have a bachelor’s degree in Information Systems Engineering and 
            hold an MBA with a marketing concentration from INCAE Business School. I was recently a business and 
            marketing professor at Keiser University while also managing digital marketing for the 20 Keiser campuses 
            in Florida, USA. I am a writer and editor for Dracma Magazine. In 2015, I cofounded ACG which provides 
            consulting services for the oil & energy industry. I have over 12 years of experience in the ICT and 
            marketing industries. I have worked in the technical area as a systems administrator as well as in the 
            commercial area as marketing manager of companies of national and international prestige. I have technical,
             marketing, and management experience in a variety of industries ranging from telecommunications and 
            enterprise architecture to automotive, consumer goods, and education. I'm passionate about data science, 
            health, innovation, and marketing. During my tenure at Keiser, I volunteered mentoring students from the 
            KU Business Club and I'm also a member of the Latinas in STEM Foundation.")
          )
        ),
        fluidRow(
          widgetUserBox(
          title = "Gudrun Thorsteinsdottir",
          subtitle = "Master student of Business Informatics",
          width = 6,
          type = 2,
          src = "gudi.png",
          color = "blue",
          p("I am currently pursuing a master’s degree in Business Informatics at Utrecht University. 
            Before that, I studied Information Sciences in Sweden and generated along the way a curiosity 
            and drive towards learning more about the management perspective of information technology. 
            During my education I have aimed to go the extra mile and engage in university affairs. I
            published a paper in the Medical Informatics field called Health Information Seeking among 
            Young Adults in Sweden, which can be found in the IEEE digital library. Additionally, I was 
            a member of the student committee for the IEEE CBMS International Symposium on Computer-Based 
            Medical Systems conference and took part in the planning of the conference. Finally, one of the 
            co-authors of this report (Sara Altamirano) and I submitted the paper, Mobile Application for 
            Celiac Disease Patients’ Wellness and Support, to the conference MobiHealth 2019, which is 
            currently under review.")
        ),
        widgetUserBox(
          title = "Noel Bainathsah",
          subtitle = "Master student of Business Informatics",
          width = 6,
          type = 2,
          src = "noel.png",
          color = "blue",
          p("I amm currently studying a master’s in Business Informatics at Utrecht University. 
            I am currently working as a freelance mobile app developer, developing an application 
            for iOS. With a primary focus on healthcare applications and giving power to the patient. 
            I have a bachelor’s degree in medical informatics from the University of Amsterdam and was 
            a board member of the medical informatics study society with a focus on acquisition, 
            fundraising and organizing business events ranging from the arrangement of a congress 
            to networking events. I have worked in the ICT advisory domain as a technical consultant 
            to solve PC related issues around Amsterdam and worked as a program tester in the AUMC 
            hospital regarding the implementation of a new EHR called EPIC. In my spare time, I like 
            to express my creative mind through photo and video editing and fulfill my competitive 
            drive through video games. Additionally, I like to participate in voluntary work, which 
            range from going to Uganda to build an elementary school for children in need to helping 
            at local events like the pedal journey in the Netherlands.")
          )
        )
    )
  )
)

ui <- dashboardPagePlus(
  header, 
  sidebar, 
  body,
  rightsidebar
)
