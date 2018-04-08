library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

# couldn't make connection, so commenting some snippits below out in case it doesn't work on tbl, ha!

# gtd_tbl <- get_gtddb() %>%
#  tbl("events") 

confirmed_db <- tbl(get_gtddb,"events")
confirmed_attacks <- confirmed_db %>%
  select(iyear,imonth,iday,country_txt,provstate,city,latitude,longitude,attacktype1_txt,targtype1,
         targtype1_txt,claimed,property,propextent,propvalue,doubtterr,success,gname) %>%
  rename(Year=iyear,Country=country_txt) %>%
  filter(doubtterr==0 & success==1) %>%
  select(-doubtterr,-success)

total_attacks_by_country <- confirmed_attacks %>%
  group_by(Country) %>%
  tally() %>%
  rename(TotalSuccessfulAttacks = n)

total_attacks_by_decade <- confirmed_attacks %>%
  select(Year,Country) %>%
  mutate(Decade = case_when(
    Year >= 1970 & Year < 1980 ~ "1970s",
    Year >= 1980 & Year < 1990 ~ "1980s",
    Year >= 1990 & Year < 2000 ~ "1990s",
    Year >= 2000 & Year < 2010 ~ "2000s",
    Year >= 2010 ~ "2010s"
  )) %>%
  group_by(Decade,Country) %>%
  tally() %>%
  rename(TotalSuccessfulAttacks = n)

total_telecom_and_utility_attacks <- confirmed_attacks %>%
  select(Year,imonth,iday,targtype1,targtype1_txt,gname,latitude,longitude,attacktype1_txt) %>%
  rename(Month = imonth, Day = iday, TargetType=targtype1_txt, PerpetratorGroup = gname, AttackType = attacktype1_txt) %>%
  filter(!is.na(latitude) & targtype1 %in% c("16","21")) %>%
  mutate(AttackDate = case_when(
    Day == "0" ~ paste(Year,"-", Month, sep=""),
    Day != "0" ~ paste(Year, "-", Month, "-", Day, sep="")
  ))

total_property_damage <- confirmed_attacks %>%
  select(Year,imonth,iday,attacktype1_txt,property,propextent,propvalue) %>%
  rename(Month = imonth, Day = iday, PropertyDamage = propvalue, AttackType = attacktype1_txt) %>%
  filter(property=="1" & propextent %in% c("1","2")) %>%
  mutate(
    PropertyDamage = if_else(PropertyDamage == "", NA_character_, PropertyDamage),
    AttackDate = case_when(
      Day == "0" ~ paste(Year,"-", Month, sep=""),
      Day != "0" ~ paste(Year, "-", Month, "-", Day, sep="")),
    PropertyDamage = case_when(
      is.na(PropertyDamage) && propextent == "1" ~ "> $1000000000",
      is.na(PropertyDamage) && propextent == "2" ~ "Between $1000000 and $100000000",
      TRUE ~ paste("$",PropertyDamage,sep="")
    )
  )
  
get_totals <- collect(total_attacks)
get_totals_by_country <- collect(total_attacks_by_country)
get_totals_by_decade <- collect(total_attacks_by_decade)
get_totals_telecome_and_utility <- collect(total_telecom_and_utility_attacks)
get_totals_property_damage <- collect(total_property_damage)

function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  # Define color palette for attacks
  
  pal <- colorFactor(
    palette = 'Dark2',
    domain = map$attacktype1_txt
  )
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      setView(lat = 29.119758, lng = -171.594481, zoom = 2)
  })
  
  #   hist(zipsInBounds()$centile,
  # A reactive expression that returns the set of zips that are
  # in bounds right now
  zipsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(gtd_tbl[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(gtd_tbl,
           latitude >= latRng[1] & latitude <= latRng[2] &
             longitude >= lngRng[1] & longitude <= lngRng[2])
  })
  
  # Precalculate the breaks we'll need for the two histograms
  # centileBreaks <- hist(plot = FALSE, allzips$centile, breaks = 20)$breaks
  
  # output$histCentile <- renderPlot({
  #   # If no zipcodes are in view, don't plot
  #   if (nrow(zipsInBounds()) == 0)
  #     return(NULL)
  #   
  #        breaks = centileBreaks,
  #        main = "SuperZIP score (visible zips)",
  #        xlab = "Percentile",
  #        xlim = range(allzips$centile),
  #        col = '#00DD00',
  #        border = 'white')
  # })
  # 
  # output$scatterCollegeIncome <- renderPlot({
  #   # If no zipcodes are in view, don't plot
  #   if (nrow(zipsInBounds()) == 0)
  #     return(NULL)
  #   
  #   print(xyplot(income ~ college, data = zipsInBounds(), xlim = range(allzips$college), ylim = range(allzips$income)))
  # })
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    colorBy <- input$color
    sizeBy <- input$size

 
    colorData <- gtd_tbl %>%
      select(colorBy) %>%
      collect()
    
    pal <- brewer_pal("qual")(8)

    sizeData <- gtd_tbl %>%
      select(sizeBy) %>%
      collect()
    
    radius <- sizeData / max(sizeData) * 30000

    leafletProxy("map", data = gtd_tbl %>% collect()) %>%
      clearShapes() %>%
      addCircles(~longitude, ~latitude, radius=radius, layerId=~eventid,
                 stroke=FALSE, fillOpacity=0.4) %>%
      addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                layerId="colorLegend")
  })

  
  # Show a popup at the given location. Need to define a custom popup for
  # when someone clicks a particular event. Some high level info
  showZipcodePopup <- function(zipcode, lat, lng) {
    selectedEvent <- gtd_tbl %>%
      filter(eventid = eventid)
    content <- as.character(tagList(
      tags$h4("Score:", as.integer(selectedZip$centile)),
      tags$strong(HTML(sprintf("%s, %s %s",
                               selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
      ))), tags$br(),
      sprintf("Median household income: %s", dollar(selectedZip$income * 1000)), tags$br(),
      sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
      sprintf("Adult population: %s", selectedZip$adultpop)
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showZipcodePopup(event$id, event$lat, event$lng)
    })
  })
  
  
  ## Data Explorer ###########################################
  
  # observe({
  #   cities <- if (is.null(input$states)) character(0) else {
  #     filter(cleantable, State %in% input$states) %>%
  #       `$`('City') %>%
  #       unique() %>%
  #       sort()
  #   }
  #   stillSelected <- isolate(input$cities[input$cities %in% cities])
  #   updateSelectInput(session, "cities", choices = cities,
  #                     selected = stillSelected)
  # })
  # 
  # observe({
  #   zipcodes <- if (is.null(input$states)) character(0) else {
  #     cleantable %>%
  #       filter(State %in% input$states,
  #              is.null(input$cities) | City %in% input$cities) %>%
  #       `$`('Zipcode') %>%
  #       unique() %>%
  #       sort()
  #   }
  #   stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
  #   updateSelectInput(session, "zipcodes", choices = zipcodes,
  #                     selected = stillSelected)
  # })
  # 
  # observe({
  #   if (is.null(input$goto))
  #     return()
  #   isolate({
  #     map <- leafletProxy("map")
  #     map %>% clearPopups()
  #     dist <- 0.5
  #     zip <- input$goto$zip
  #     lat <- input$goto$lat
  #     lng <- input$goto$lng
  #     showZipcodePopup(zip, lat, lng)
  #     map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
  #   })
  # })
  # 
  # output$gtdtable <- DT::renderDataTable({
  #   df <- cleantable %>%
  #     filter(
  #       Score >= input$minScore,
  #       Score <= input$maxScore,
  #       is.null(input$states) | State %in% input$states,
  #       is.null(input$cities) | City %in% input$cities,
  #       is.null(input$zipcodes) | Zipcode %in% input$zipcodes
  #     ) %>%
  #     mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
  #   action <- DT::dataTableAjax(session, df)
  #   
  #   DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  # })
}