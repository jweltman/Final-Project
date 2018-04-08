library(leaflet)

# Decide what variables we want to make available to color and size the
# markers by.
vars <- c(
  # "# of Fatalities" = "nkill",
  # "Attack Type" = "attacktype1_txt"
  "Total Successful Attacks" = "LocationTotal",
  # "Total Attacks By Decade" = "TotalSuccessfulAttacks_Decade",
  # "Attacks Against Telecom and Utilities" = "TotalTelecomAndUtilityAttacks",
  "Total Property Damage" = "PropertyDamage"
)

navbarPage("Global Terrorism DB", id="nav",
  tabPanel("Interactive map",
    div(class="outer",
        tags$head(
          includeCSS("styles.css"),
          includeScript("gomap.js")
        ),
        
        leafletOutput("map", width="100%", height="100%"),
        
        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                      width = 330, height = "auto",
                      
                      h2("GTD Explorer"),
                      
                      selectInput("select_map", "Please select the data set:", vars, selected="Total Successful Attacks")
                      #,
                      #selectInput("size", "Size", vars, selected = "nkill"),
                      #conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                      #                 # Only prompt for threshold when coloring or sizing by superzip
                      #                 numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                      )
                      #,
                      
                     # plotOutput("histCentile", height = 200),
                      #plotOutput("scatterCollegeIncome", height = 250)
       # ),
        #tags$div(id="cite",
         #        'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960–2010'), ' by Charles Murray (Crown Forum, 2012).'
        #)
    )
  )
  #,
  
#  tabPanel("Data explorer",
#           fluidRow(
#             column(3,
#                    selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
#             ),
#             column(3,
#                    conditionalPanel("input.states",
#                                     selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
#                    )
#             ),
#             column(3,
#                    conditionalPanel("input.states",
#                                     selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
#                    )
#             )
#           ),
#           fluidRow(
#             column(1,
#                    numericInput("minScore", "Min score", min=0, max=100, value=0)
#             ),
#             column(1,
#                    numericInput("maxScore", "Max score", min=0, max=100, value=100)
#             )
#           ),
#           hr(),
#           DT::dataTableOutput("gtdtable")
#  ),
#  
#  conditionalPanel("false", icon("crosshair"))
)