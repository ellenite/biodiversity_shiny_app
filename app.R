#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Loading the necessary packages

library(shiny)
library(shinydashboard)
library(DT)
library(shinyWidgets)
library(dplyr)
library(echarts4r)
library(leaflet)
library(googlesheets4)

# Define UI for the application
ui <- dashboardPage(

    dashboardHeader(
        title = "Biodiversity in National Parks", titleWidth = 400
    ),
    dashboardSidebar(
        sidebarMenu(
            menuItem(
                tabName = "species", text = "Species"
            )
            , menuItem(
                tabName = "map", text = "Map"
            )
            , menuItem(
                tabName = "journal", text = "Journal"
            )
        )
    ),
    dashboardBody(
      includeCSS("www/style.css")
        , tabItems(
            tabItem(
                tabName = "species",
                fluidPage(
                    fluidRow(
                        box(width = 4, status = "primary", title = "Filters"
                            , uiOutput("park_picker")
                            , div(class = "filter_box"
                                  , uiOutput("category_check_box"))
                            , height = "40vh"
                            )
                        , box(width = 4, status = "primary", title = "Overview"
                              , uiOutput("vb_present")
                              , uiOutput("vb_concern")
                              , uiOutput("vb_endangered")
                              , height = "40vh")
                        , box(width = 4, status = "primary", title = "Category Count"
                              , echarts4rOutput("category_count_bar", height = "33vh")
                              , height = "40vh")
                    )
                    , fluidRow(
                        box(width = 12, status = "primary", title = "Table"
                            , DTOutput("species_table"))
                    )
                )
            )
            , tabItem(
                tabName = "map"
                , fillPage(
                    fluidRow(
                        box(width = 12, status = "primary"
                            , chooseSliderSkin(skin = "Shiny", color = "green")
                            , uiOutput("acreage_slider"))
                    )
                    , fluidRow(
                        box(width = 12, status = "primary"
                            , leafletOutput("map", height = "75vh"))
                    )
                )
            )
            , tabItem(
                tabName = "journal"
                , fluidPage(
                    fluidRow(
                        box(width = 12, status = "primary"
                            , column(width = 1
                                     , actionBttn("add_entry", icon = icon("plus"))
                                     ) 
                            , column(width = 1
                                     , actionBttn("edit_entry", icon = icon("edit"))
                                      ) 
                            , column(width = 1
                                     , actionBttn("delete_entry", icon = icon("trash-alt"), color = "danger")
                            ) 
                            , br()
                            , br()
                            , DTOutput("journal"))
                    )
                )
            )
        )
    ),
    title = "Biodiversity in National Parks"
    
)

# Define server logic
server <- function(input, output) {
    park_data <- read.csv("data/parks.csv")
    species_data <- read.csv("data/species.csv")
    species_data <- species_data[, -c(14)]
    
    # Species Tab -----
    # Filters -----
    output$park_picker <- renderUI({
      choices <- park_data$Park.Name
      pickerInput("park_picker", "Select Park"
                  , choices = choices)
    })
    
    output$category_check_box <- renderUI({
      req(input$park_picker)
      categories <- species_data %>%
        filter(Park.Name == input$park_picker) %>%
        select(Category) %>% pull() %>% unique()
      categories <- categories[categories!=""]
      checkboxGroupButtons(
        "category_check_box"
        , label = "Species Category"
        , choices = categories
        , selected = categories
        , status = "default"
        , size = "normal"
        , direction = "vertical"
        , width = "100%"
      )
      
    })
    
    
    # Reactive Data -----
    
    reactive_species_data <- reactive({
      req(input$park_picker)
      req(input$category_check_box)
      species_data %>%
        filter(Park.Name == input$park_picker
               , Category %in% input$category_check_box)
    })
    
    # Species Table -----
    
    output$species_table <- renderDT({
      reactive_species_data() %>%
        select("Common Name"=Common.Names
               , "Scientific Name"=Scientific.Name
               , Category
               , Occurrence
               , Nativeness
               , Abundance
               , "Conservation Status"=Conservation.Status) %>%
        datatable(rownames=F
        , options = list(paging=F
                         , scrollY = "30vh"
                         , scrollX = "100%"))
    })
    
    # Value Boxes -----
    output$vb_present <- renderValueBox({
      present <- reactive_species_data() %>%
        filter(Occurrence== "Present") %>%
        nrow()
      
      valueBox(value = format(present, big.mark=","), subtitle = "Present Species")
    })
    
    output$vb_concern <- renderValueBox({
      concern <- reactive_species_data() %>%
        filter(Conservation.Status== "Species of Concern") %>%
        nrow()
      
      valueBox(value = format(concern, big.mark=","), subtitle = "Species of Concern")
    })
    
    output$vb_endangered <- renderValueBox({
      endangered <- reactive_species_data() %>%
        filter(Conservation.Status== "Endangered") %>%
        nrow()
      
      valueBox(value = format(endangered, big.mark=","), subtitle = "Endangered Species")
    })
    
    # Category Count Bar -----
    output$category_count_bar <- renderEcharts4r({
      reactive_species_data() %>%
        select(Species.ID, Category) %>%
        group_by(Category) %>%
        table() %>%
        as.data.frame() %>%
        group_by(Category) %>%
        summarise(count = sum(Freq)) %>%
        e_chart(Category) %>%
        e_bar(count, name = "Count Category", color = "green") %>%
        e_legend(show = F) %>%
        e_tooltip() %>%
        e_axis(axisLabel = list(interval = 0, rotate = 45)) %>%
        e_grid(right = 100, top = 10)
    })
    
    
    # Map Tab -----
    
    output$acreage_slider <- renderUI({
      max_acreage <- 8323148
      min_acreage <- 5550
      
      sliderInput("AcreageSlider", "Acreage Slider"
                  , min_acreage, max_acreage, value = c(min_acreage, max_acreage)
                  , post = " acres")
    })
    
    reactive_map_data <- reactive({
      req(input$AcreageSlider)
      park_data %>%
        filter(Acres >= input$AcreageSlider[1]
               , Acres <= input$AcreageSlider[2])
      
    })
    
    output$map <- renderLeaflet({
      reactive_map_data() %>%
        mutate(
          acres_f = format(Acres, big.mark=",")
          , popup = paste(
          "<b>"
          , Park.Name
          , "</b>"
          , "<br>"
          , State
          , "<br>"
          , acres_f
          , "acres"
        )) %>%
        leaflet() %>%
        addProviderTiles(provider = "OpenTopoMap") %>%
        addCircleMarkers(
          lng = ~Longitude
          , lat = ~Latitude
          , radius = ~Acres/150000
          , label = ~Park.Name
          , popup = ~popup
        ) %>%
        setView(lat = "44.00000", lng = "-120.50000", zoom = 3)
    })
    
    # Journal Tab -----
    # Using the google auth from cache directory
    
    cache_directory <- ".cache/"
    
    # Connect to google drive and reading the google sheet
    gs4_auth(email = "kpavode.ellenite@gmail.com", cache = cache_directory)
    journal_url <- "https://docs.google.com/spreadsheets/d/129JJLJOChipFBJ0DtjX8LjMm8cQidsJFkwt24jPNlvw/edit#gid=0"
    journal_data <- range_read(ss = journal_url)
    
    r <- reactiveValues()
    r$journal_data <- journal_data
    
    output$journal <- renderDT({
      datatable(r$journal_data, rownames = F, selection = c("single")
                , options = list(scrollY = "75vh", paging = F))
    })
    
    # Adding a new entry
    observeEvent(input$add_entry, {
      showModal(
        modalDialog(title = "New Entry"
                    , footer = fluidPage(
                      column(width = 6
                             , actionBttn("save", icon = icon("save"))
                             )
                      , column(width = 6
                               , actionBttn("dismiss", icon = icon("times"))
                               )
                    )
                    , fluidRow(
                      column(width = 6
                             , textInput("name", "Trip Name", value = "Trip Name")
                             , dateInput("date", "Trip Date")
                             )
                      , column(width = 6
                               , textInput("parkName", "Park Name", value = "Park Name")
                               , textAreaInput("notes", "Notes", value = "Notes ...")
                               )
                    )
          
        )
      )
    })
    
    observeEvent(input$dismiss, {
      removeModal()
    })
    
    observeEvent(input$save, {
      
      tripName <- input$name
      tripDate <- input$date
      parkName <- input$parkName
      notes <- input$notes
      
      new_entry <- tibble("Trip Name" = c(tripName)
                          , "Trip Date" = c(format(tripDate, "%d/%m/%Y"))
                          , "Park Name" = c(parkName)
                          , "Notes" = c(notes)
                          )
      sheet_append(ss = journal_url, new_entry)
      showNotification("Entry added", type = "message")
      r$journal_data <- r$journal_data %>% rbind(new_entry)
      removeModal()
    })
    
    # Edit an existing entry
    observeEvent(input$edit_entry, {
     
      row_selected = input$journal_rows_selected
      
      if (is.null(row_selected)) {
        showNotification("Please select a row first", type = "warning")
      }
      else 
      {
        showModal(
          modalDialog(title = "Edit Entry"
                      , footer = fluidPage(
                        column(width = 6
                               , actionBttn("save_e", icon = icon("save"))
                        )
                        , column(width = 6
                                 , actionBttn("dismiss_e", icon = icon("times"))
                        )
                      )
                      
                      , fluidRow(
                        column(width = 6
                               , textInput("name_e", "Trip Name", value = r$journal_data[row_selected, 1])
                               , textInput("date_e", "Trip Date", value = r$journal_data[row_selected, 2])
                        )
                        , column(width = 6
                                 , textInput("parkName_e", "Park Name", value = r$journal_data[row_selected, 3])
                                 , textAreaInput("notes_e", "Notes", value = r$journal_data[row_selected, 4])
                        )
                      )
                      
          )
        )
      }
      
     
    })
    
    observeEvent(input$dismiss_e, {
      removeModal()
    })
    
    observeEvent(input$save_e, {
      
      tripName <- input$name_e
      tripDate <- input$date_e
      parkName <- input$parkName_e
      notes <- input$notes_e
      
      edit_entry <- tibble("Trip Name" = c(tripName)
                          , "Trip Date" = c(tripDate)
                          , "Park Name" = c(parkName)
                          , "Notes" = c(notes)
      )
      
  
      row_selected = input$journal_rows_selected
      
      range_target <- paste("A", row_selected + 1, ":D", row_selected + 1, sep = "")
      
      range_write(ss = journal_url, edit_entry, range = range_target, col_names = F)
      showNotification("Entry edited", type = "message")
      
      r$journal_data[row_selected,] <- edit_entry
      
      removeModal()
    })
    
    # Delete an existing entry
    observeEvent(input$delete_entry, {
      
      row_selected = input$journal_rows_selected
      
      if (is.null(row_selected)) {
        showNotification("Please select a row first", type = "warning")
      }
      else 
      {
        showModal(
          modalDialog(title = "Delete Entry"
                      , footer = fluidPage(
                        column(width = 6
                               , actionBttn("bt_yes", icon = icon("thumbs-up"))
                        )
                        , column(width = 6
                                 , actionBttn("bt_no", icon = icon("undo"), color = "danger")
                        )
                      )
                      
                      , fluidRow(
                        column(width = 12
                               , "Do you want to delete the selected row?"
                        )
                      )
                      
          )
        )
      }
      
      
      
    })
    
    observeEvent(input$bt_no, {
      removeModal()
    })
    
    observeEvent(input$bt_yes, {
      
      row_selected = input$journal_rows_selected
      
      range_target <- paste("A", row_selected + 1, ":D", row_selected + 1, sep = "")
      
      range_delete(ss = journal_url, range = range_target, shift = "up")
      
      showNotification("Entry deleted", type = "message")
      
      r$journal_data <- r$journal_data[-row_selected,]
      
      removeModal()
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
