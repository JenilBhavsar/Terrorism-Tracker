#Importing required packages install all the packages
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(ggplot2)
library(maps)
library(plotly)
library(leaflet)
library(RColorBrewer)
library(dplyr)
library(geojsonsf)
library(sf)
library(forcats)
library(colourpicker)

#UI of R shiny with bootstrapPage instead of fluidPage
ui <-bootstrapPage(
    #creating navbar using shiny theme package
        navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
               HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Terrorism Tracker</a>'), 
               id="nav",
               windowTitle = "Terrorism tracker",
               # Tab for terrorism mapper
               tabPanel("Terrorism mapper",
                        div(class="outer",
                            #including basic styles for the tab
                            tags$head(includeCSS("styles.css")),
                            #creating leaflet output
                            leafletOutput("mymap", width="100%", height="100%"),
                            #controlling the side tab panel
                            absolutePanel(id = "controls", class = "panel panel-default",
                                          top = 75, left = 55, width = 250, fixed=TRUE,
                                          draggable = TRUE, height = "auto",
                                          #Display output on year selected
                                          h3(textOutput("reactive_year"), align = "right"),
                                          #Display output on attack count of that year
                                          h4(textOutput("reactive_attack_count"), align = "right"),
                                          #Display output of kill count for that year
                                          h6(textOutput("reactive_death_count"), align = "right"),
                                #Creating slider for the years 1970-2017
                                sliderTextInput("plot_year",
                                                label = h5("Select mapping year"),
                                                choices = unique(data1$Year),
                                                selected = max(unique(data1$Year)),
                                                grid = FALSE,
                                                animate=animationOptions(interval = 3000, loop = FALSE))
                            )
                        )
               ),
               #Tab 2 affected areas
               tabPanel("Affected Areas",
                        
                        sidebarLayout(
                            sidebarPanel(
                                #input selection for user
                                selectInput("var1", h3("Select levels:"),
                                             c("Country" = "NAME_LONG",
                                               "State" = "State",
                                               "City" = "City")),
                                numericInput("maxbars", "Bars to Show", 10),
                                #unique color selection using colourpicker package
                                colourInput("color", h3("Select Color for barplot:"),"red"),
                                #display the text based on input selected
                                textOutput("epi_notes_1"),
                                textOutput("epi_notes_2"),
                                textOutput("epi_notes_3")
                                        ),
                            #Output of the plot based on user selection
                            mainPanel(plotOutput("tp_plot"))
                                    )
                        ),
               #Tab 3 death types
               tabPanel("Death Types",
                        
                        sidebarLayout(
                            sidebarPanel(
                                #User input selection
                                selectInput("var2", h3("Kills based on different type:"),
                                            c("Attack Type" = "AttackType",
                                              "Weapon" = "Weapon_type",
                                              "Target" = "Target_type")),
                                #radio buttons to change the color of the bars
                                radioButtons("color1", h3("Select Color for barplot:"),
                                             c("Red" = "#C34A36",
                                               "Blue" = "#0081CF",
                                               "Yellow" = "#F9F871"))),
                                #output of the plot
                                mainPanel(plotOutput("kill_plot"))
                    )
        
                ),
               #Tab 4 Data
               tabPanel("Data",
                        #max rows to display input
                        numericInput("maxrows", "Rows to show", 25),
                        #table display
                        verbatimTextOutput("rawtable"),
                        #download button to download csv
                        downloadButton("downloadCsv", "Download as CSV"),tags$br(),tags$br(),
                        #link to find full dataset
                        "Full dataset is available on Kaggle: ", tags$a(href="https://www.kaggle.com/START-UMD/gtd", "Global 
                                                                        Terrorism Database")
               ),
               #Tab 5 About
               tabPanel("About",
                        tags$div(
                         tags$h2(tags$b("No Terrorism. Save Humanity.")),tags$br(),
                         "My aim is to complement the available resources with several interactive features, including the 
                         timeline and the ablility to overlay past attacks",
                         tags$br(), tags$br(), tags$h4(tags$b("Background")),
                         "The main objective of this application is to provide insights of terrorism activities across the 
                         world with different types of attacks conducted by the terrorist. It will also show case the types 
                         of weapons used by them, and the number of deaths occurred during the time.",
                         "The data is obtained through the biggest registry available on terrorism", 
                         tags$a(href="https://start.umd.edu/gtd/", "GTD. "), tags$br(), tags$br(),
                         "The GTD is an open-source database, which provides information on domestic and 
                         international terrorist attacks around the world since 1970, and now includes more 
                         than 200,000 events. For each event, a wide range of information is available, including 
                         the date and location of the incident, the weapons used, nature of the target, the number of 
                         casualties, and – when identifiable – the group or individual responsible.",
                         "The GTD defines terrorism as:",
                         tags$br(), tags$br(), tags$b("The threatened or actual use of illegal force and 
                                                      violence by a non-state actor to attain a political, economic, religious, 
                                                      or social goal through fear, coercion, or intimidation"),
                         tags$br(), tags$br(), tags$h4(tags$b("Code")),
                         "Code and input data used to generate this Shiny mapping tool are available on ",
                         tags$a(href="", "Github."),
                         tags$br(),tags$br(),tags$h4(tags$b("Sources")),
                         tags$b("Dataset: "), tags$a(href="https://www.kaggle.com/START-UMD/gtd","Global Terrorism Dataset. "),
                         tags$br(),
                         tags$b("Country Coordinates: "), 
                         tags$a(href="https://github.com/eparker12/nCoV_tracker/blob/master/input_data/50m.geojson", "Geojson"),
                         tags$br(),
                         tags$b("Designing Inspiration: "), 
                         tags$a(href="https://shiny.rstudio.com/gallery/covid19-tracker.html","R shiny Gallery"),tags$br(),
                         tags$b("Mapping: "), tags$a(href="https://rstudio.github.io/leaflet/shiny.html","Leaflet Package "),tags$br(),
                         tags$b("Logo: "), tags$a(href="https://www.trentu.ca/","Trent University "),
                         tags$br(),tags$br(),tags$h4(tags$b("Author")),
                         "Jenil Nimishkumar Bhavsar, Trent University(0696927)",
                         tags$br(),tags$br(),tags$h4(tags$b("Contact")),
                         "jbhavsar@trentu.ca",tags$br(),tags$br(),
                         tags$img(src = "logo.png", width = "230px", height = "80px"),tags$br(),tags$br()
                        )
                    )
               
               )
        )
#server file of shiny app
server <- function(input, output, session) {
    #setting up the path change path for proper use
    setwd("E:/Trent/Data ANalytics with R/Term Project/Term Project")
    #reading the csv when downloading the data name might be different
    terror <- read.csv("E:/Trent/Data ANalytics with R/Term Project/Term Project/globalterrorismdb.csv")
    #renaming the columns
    terror1 <- terror %>% rename(Year=iyear,
                                 Month=imonth,
                                 Extended=extended,
                                 Day=iday,
                                 NAME_LONG=country_txt,
                                 State=provstate,
                                 Region=region_txt,
                                 AttackType=attacktype1_txt,
                                 Target=target1,
                                 Killed=nkill,
                                 Wounded=nwound,
                                 Target_type=targtype1_txt,
                                 Weapon_type=weaptype1_txt,
                                 Weapon_detail=weapdetail,
                                 City=city,
                                 Latitude=latitude,
                                 Longitude=longitude)
    #selecting the columns used for the project
    data <- terror1 %>% select(Year,Month,Extended,Day,NAME_LONG,success,suicide,State,Region,City,Latitude,Longitude,
                               AttackType,Killed,Wounded,Target,Target_type,Weapon_type)
    #Filtering distinct values
    data1 <- data %>% distinct()
    #changing NA values to zero
    data1[is.na(data1)] <- 0
    
    #importing jeojson file for mapping
    worldcountry <- geojson_sf("E:/Trent/Data ANalytics with R/Term Project/Term Project/50m.geojson.json")
    
    #TAB 1
    #group by country name, year, attacks and kills
    map1 <- data1 %>% group_by(NAME_LONG,Year) %>% summarise(total_attacks = sum(length(Year)), total_kill = sum(Killed))
    #filtering for year 2017
    map2 <- filter(map1, Year == 2017)
    #joining to geo json file
    map_final <- inner_join(worldcountry, map2, by="NAME_LONG")
    
    #making color palette function
    pal <- colorNumeric(
        palette = "plasma", NULL)
    #Map displayed when startup
    basemap <- leaflet(map_final) %>% 
        addTiles() %>%
        #Theme of the map
        addProviderTiles("CartoDB.Positron") %>% 
        fitBounds(~-100,-60,~60,70) %>% 
        addLegend("bottomright", pal = pal, values = map_final$total_attacks,
                  title = "<small>Total Attacks</small>") 
    #Reactive for selecting the year based on input slider
    formatted_year <- reactive({
        year = input$plot_year
    })
    
    #Reactive to filter the data of that year
    reactive_db <- reactive({
        map1 %>% filter(Year == formatted_year())
    })
    #Reactively joining it to Geo json file
    reactive_db_large <- reactive({
        inner_join(worldcountry, reactive_db(), by='NAME_LONG')
    })
    #Adding to a seperate dataframe
    reactive_polygons = reactive({
        data <- reactive_db_large()
    })
    #Reactive year displayed on the side panel of terrorism tracker tab
    output$reactive_year <- renderText({
        paste0(prettyNum(input$plot_year), " year summary")
    })
    #Represents the total attacks 
    output$reactive_attack_count <- renderText({
        paste0(prettyNum(sum(reactive_db()$total_attacks), big.mark = ","), " total attacks")
    })
    #represents the total death
    output$reactive_death_count <- renderText({
        paste0(prettyNum(sum(reactive_db()$total_kill), big.mark = ","), " total deaths")
    })
    #Output of map when loaded
    output$mymap <- renderLeaflet({ 
         basemap
     })
    #Changing the map based on user input of the year
    observeEvent(input$plot_year, {
        leafletProxy("mymap") %>% 
            clearMarkers() %>%
            clearControls() %>%
            clearShapes() %>%
            addPolygons(data = reactive_polygons(), stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.4, 
                        fillColor = ~pal(reactive_polygons()$total_attacks),
                        label = paste0(reactive_polygons()$NAME_LONG, ": ", 
                                       formatC(reactive_polygons()$total_attacks, big.mark = ",")),
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "15px",
                            direction = "auto"))%>%
            addLegend("bottomright", pal = pal, values = reactive_polygons()$total_attacks,
                      title = "<small>Total Attacks</small>")
            
    })
    #TAB 2
    #filtering the data based on user input
    datasetInput <- reactive({
        temp <- data1 %>% group_by(!!!rlang::syms(input$var1)) %>% 
            summarise(total_attacks = sum(length(Year))) %>% arrange(desc(total_attacks)) %>%
            slice(1:input$maxbars)
    })
    #plotting the data 
    output$tp_plot <- renderPlot({
        mydata <- datasetInput()
        ggplot(mydata, aes(x=reorder(!!as.name(input$var1), +total_attacks), y=total_attacks)) +
            geom_bar(stat="identity", fill=input$color, alpha=.6, width=.4) +
            geom_text(aes(label = sprintf("%.1f", total_attacks), y= total_attacks),
                      position = position_dodge(width= 1), vjust= 0.3, hjust = -0.01, size = 3)+
            coord_flip() +
            ylab("Total Attacks") +
            xlab("")+
            theme_bw()
    })
    
    #add footnote for country
    output$epi_notes_1 <- renderText({
        if(input$var1=="NAME_LONG") {
            paste0("Iran, Pakistan, Afghanistan are the most affected countries due to terrorism.")
        }
    })
    
    # add footnote for state
    output$epi_notes_2 <- renderText({
        if(input$var1=="State") {
            paste0("Baghdad, Northern Ireland are the most affected states due to terrorism.")
        }
    })

    # add note for city
    output$epi_notes_3 <- renderText({
        if(input$var1=="City") {
            paste0("According to dataset Unknown place, Baghdad, Karachi are the most affected cities due to terrorism.")
        }
    })
    
    #TAB 3
    #filtering the data based on user input
    datasetInput1 <- reactive({
        df <- data1 %>% group_by(!!!rlang::syms(input$var2)) %>% summarise(total_kill = sum(Killed)) %>% arrange(desc(total_kill))
        print(df)
    })
    #plotting the data
    output$kill_plot <- renderPlot({
        data <- datasetInput1()
        ggplot(data, aes(x=reorder(!!as.name(input$var2), +total_kill), y=total_kill)) +
            geom_bar(stat="identity", fill=input$color1, alpha=.6, width=.4) +
            geom_text(aes(label = sprintf("%.1f", total_kill), y= total_kill),position = position_dodge(width= 1), 
                      vjust= 0.3, hjust = -0.01, size = 3)+
            coord_flip() +
            ylab("Total Kills") +
            xlab("")+
            theme_bw()
    })
    
    #TAB 4
    #output of download csv 
    output$downloadCsv <- downloadHandler(
        #function for the file to save the data
        filename = function() {
            paste("globalterrorismdb", ".csv", sep="")
        },
        #content of the file
        content = function(file) {
            write.csv(data1, file)
        }
    )
    #printing the table 
    output$rawtable <- renderPrint({
        orig <- options(width = 1000)
        print(tail(data1, input$maxrows), row.names = FALSE)
        options(orig)
    })
    
}
# Run the application
shinyApp(ui, server)
 

