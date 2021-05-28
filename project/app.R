library(plotly)
library(shinyjs)
library(tmap)
library(sf)
library(tidyverse)
library(mapdeck)
library(data.table)

MAPBOX_KEY <- ""#"pk.eyJ1IjoiY29tbXRyYWNrZXIiLCJhIjoiY2ptajV6ZnNsMDZxMTN3cWx1azVnYnZpdyJ9.yu2bfOHMRKWjzGOIC-6Jgw"

#read data in
accidentsData <- fread("C:/Users/miket/Downloads/GEO3PROJECT/US_Accidents_Dec20.csv")
placeNames <- unlist(accidentsData$`City, County, State`)
projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
dataFocuses <- c("General", "Severity", "Temperature.F.", "Civil_Twilight")
to_hide = c("aspectText", "dataFocusBox", "tmapText",
            "map", "heatmapText", "gridMap", "chartText",
            "plot")

ui <- fluidPage(
    useShinyjs(),
    tags$head(tags$style(
        "#intro * {
            display:inline;
        }")),
    titlePanel("Car Accident Visualization Generator"),
    tags$div(id = "intro",
        "This application generates mapping and chart visualizations for car accident data available for a given place. The data comes from ",
        tags$a(href="https://www.kaggle.com/sobhanmoosavi/us-accidents", "this"),
        "dataset, which was created by Ohio State University researchers, detailing approximately 3 million accident records in the United States from 2016-2020. The visualization generator looks at these records from the scope of places, and as such to generate the visualizations you first need to specify an input place. Note that there are thousands of places to choose from, so be sure to explore! Also note that these generated visualizations are meant for academic use."
    ),
    titlePanel(""),
    selectizeInput(inputId = "searchBox", label = "Place Search:",
        choices = NULL, selected=FALSE),
    actionButton("getData", "Retrieve Data"),
    titlePanel(""),
    textOutput("aspectText"),
    titlePanel(""),
    hidden(selectizeInput(inputId = "dataFocusBox", label = "Choose Data Aspect",
        choices = dataFocuses, selected="General")),
    titlePanel(""),
    textOutput("loadText"),
    titlePanel(""),
    textOutput("tmapText"),
    titlePanel(""),
    tmapOutput("map"),
    titlePanel(""),
    textOutput("heatmapText"),
    titlePanel(""),
    mapdeckOutput(outputId = "gridMap"),
    titlePanel(""),
    textOutput("chartText"),
    titlePanel(""),
    plotlyOutput("plot"),
    titlePanel(""),
    textOutput("endText")
)

server <- function(input, output, session) {
    updateSelectizeInput(session = session, "searchBox",
        label = "Place Search (Name, County, State):", choices = placeNames, server = TRUE,
        options = list(placeholder = "Select an American city or town",
            onInitialize = I("function(){this.setValue('');}")),
            selected = ""
    )
    observeEvent(input$dataFocusBox, {
        output$gridMap <- renderMapdeck({
            #possible style args: “dark”, “light”, “outdoors”, “streets”, “satellite”, “satellite-streets”
            mapdeck(token=MAPBOX_KEY, style = mapdeck_style("satellite-streets"))
        })
        observeEvent(input$getData, {
            output$loadText <- renderText({paste("Retrieving Data...")})
            chosen_Col = "red"
            pal_colors = NULL
            remove_colors = c() #for when there's no data for a specific color correspondance
            if(input$dataFocusBox != "General"){
                chosen_Col = input$dataFocusBox
                if(input$dataFocusBox == "Civil_Twilight"){
                    #twilight: turquoise rgba(141, 211, 199, 1) #8dd3c7
                    #pale yellow: rgba(255, 255, 179, 1) #ffffb3
                    pal_colors = c("#8dd3c7", "#ffffb3")
                }
                if(input$dataFocusBox == "Severity"){
                    #blue #0000FF, green #00FF00, yellow #FFFF00, orange #FFA500, red #FF0000
                    pal_colors = c("#0000FF", "#00FF00", "#FFFF00", "#FFA500", "#FF0000")
                }
            }
            placeName <- input$searchBox
            #split placeChosen by ", "
            placeChosen <- unlist(str_split(input$searchBox, ", "))
            if(length(placeChosen) == 3){
                name = placeChosen[1]
                county = placeChosen[2]
                state = placeChosen[3]
                #filter accident data to the place
                data <- accidentsData[accidentsData$City==name &
                            accidentsData$County==county &
                            accidentsData$State==state,]
                if(input$dataFocusBox == "Civil_Twilight"){
                    if(nrow(data[data$Civil_Twilight == "Day",]) == 0){
                        remove_colors <- c(remove_colors, pal_colors[1])
                    }
                    if(nrow(data[data$Civil_Twilight == "Night",]) == 0){
                        remove_colors <- c(remove_colors, pal_colors[2])
                    }
                    pal_colors <- pal_colors[!pal_colors %in% remove_colors]
                }
                if(input$dataFocusBox == "Severity"){
                    for(x in 1:5){
                        if(nrow(data[data$Severity == x,]) == 0){
                            remove_colors <- c(remove_colors, c(pal_colors[x]))
                        }
                    }
                    pal_colors <- pal_colors[!pal_colors %in% remove_colors]
                }
                if(nrow(data) > 0){
                    output$loadText <- renderText({paste("Data retrieved for ", placeName, ":")})
                    data_df <- data.frame(data)
                    aspectText <- "Here you get to focus on a specific attribute of these accident records to gain additional insight. Keep in mind that when you change the place input, be sure to click the 'Retrieve Data' button. Note that severity levels are in respect to the accident's impact on traffic."
                    output$aspectText <- renderText({paste(aspectText)})
                    tmapText <- "Below is the map visualization depicting the spatial distribution of accidents as points. You can adjust the map style and also click on the points to gain more information. Note that there could be accidents that occurred at the same latitude longitude coordinates, but happened at different points in time."
                    output$tmapText <- renderText({paste(tmapText)})
                    output$map <- renderTmap({
                        points <- st_as_sf(x = data_df, coords = c("Start_Lng", "Start_Lat"), crs = projcrs)
                        tm_basemap(providers$OpenStreetMap) + tm_basemap(providers$Esri.WorldGrayCanvas) +
                        tm_shape(points) + tm_dots(col = chosen_Col, legend.hist = TRUE, size = 0.05, palette=pal_colors)
                    })
                    output$heatmapText <- renderText({paste("Below is the heatmap visualization of these accident points depicted in the previous map:")})
                    #https://symbolixau.github.io/mapdeck/articles/tips_tricks.html
                    mapdeck_update(map_id = "gridMap") %>%
                        clear_heatmap(layer_id = "heatmap_layer")
                    mapdeck_update(map_id="gridMap") %>% #pitch = 60
                        add_heatmap(
                            data = data_df, layer_id="heatmap_layer", lat = "Start_Lat", lon = "Start_Lng",
                            colour_range = colourvalues::colour_values(1:6, palette = "inferno"))
                    show("dataFocusBox")
                    if(input$dataFocusBox != "General"){
                        to_plot = NULL
                        x_data = NULL
                        y_data = NULL
                        title_text=""
                        ylab_text=""
                        xlab_text=""
                        type = "bar"
                        if(input$dataFocusBox == "Severity"){
                            plot_data = data.frame(data_df$Severity) #table
                            title_text = "Accident Severity Distribution"
                            xlab_text = "Severity Rating"
                            ylab_text = "Number of Accidents"
                            to_plot <- as.data.frame(table(plot_data))
                            x_data <- to_plot$plot_data
                            y_data <- to_plot$Freq
                        }
                        if(input$dataFocusBox == "Civil_Twilight"){
                            plot_data = data.frame(data_df$Civil_Twilight)
                            title_text = "Civil Twilight Distribution"
                            xlab_text = "Outside Conditions"
                            ylab_text = "Number of Accidents"
                            to_plot <- as.data.frame(table(plot_data))
                            x_data <- to_plot$plot_data
                            y_data <- to_plot$Freq
                        }
                        if(input$dataFocusBox == "Temperature.F."){
                            title_text = "Severity and Mean Temperature of Accidents"
                            xlab_text = "Severity Level"
                            ylab_text = "Mean Temperature (Farenheit)"
                            temp_data <- data.frame(data_df$Temperature.F.)
                            sev_data <- data.frame(data_df$Severity)
                            sev_temp_table <- cbind(temp_data, sev_data)
                            to_plot <- as.data.frame(sev_temp_table)
                            x_data = c(1, 2, 3, 4, 5)
                            y_data = c(0, 0, 0, 0, 0)
                            for(x in 1:5){
                                sev_df <- sev_temp_table[sev_temp_table$data_df.Severity == x,]
                                if(nrow(sev_df) > 0 && ncol(sev_df) > 0){
                                    y_data[x] = mean(sev_df$data_df.Temperature.F., na.rm = TRUE)
                                }else{
                                    y_data[x] = 0
                                }
                            }
                        }
                        chartText <- ""
                        output$chartText <- renderText({paste(chartText)})
                        #create bar chart
                        output$plot <- renderPlotly({
                            plot_ly(to_plot,
                                    x = ~x_data,  y = ~y_data,
                                    type = type,  marker = list(color = pal_colors)
                                    ) %>%
                                layout(title=title_text,
                                       xaxis = list(title = xlab_text),
                                       yaxis = list(title = ylab_text),
                                       bargap = 0.1)
                        })
                        show("plot")
                        for(h in to_hide){
                            show(h)
                        }
                    }else{
                        output$chartText <- renderText({paste("")})
                        hide("plot")
                    }
                }else{
                    #no data available for place
                    output$loadText <- renderText({paste("Apologies, no data for place was able to be retrieved.")})
                    for(h in to_hide){
                        hide(h)
                    }
                }
            }else{
                #couldn't find place
                output$loadText <- renderText({paste("Sorry, we weren't able to find the place you entered. Select an available option instead.")})
                for(h in to_hide){
                    hide(h)
                }
            }
        })
    })
    endText <- "Disclaimer:  It is worth noting that the quantity of data varies widely from place to place, as well as quality of data in terms of missing values. Additionally, the data is not fully comprehensive in encapsulating all of the accidents that occurred in the city, and may even overemphasize accident records that occured in certain parts of the city. Regardless, the dataset provides a very comprehensive and publicly accessible sample of accidents for a plethora of places around the United States, and as such serves as a valuable tool for conducting traffic-related analysis on these places."
    output$endText <- renderText({paste(endText)})
}
shinyApp(ui, server)
