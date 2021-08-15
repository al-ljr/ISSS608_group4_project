library(shiny)
library(tidyverse)
library(visNetwork)
library(DT)
library(dplyr)
library(shinythemes)
library(igraph)
library(rsconnect)
library(bslib)
library(shinyWidgets)
library(crosstalk)
library(RColorBrewer)
library(plotly)
library(tidygraph)
library(readr)
library(raster)
library(sf)
library(tmap)
library(tmaptools)
library(clock)
library(rgdal)
library(tidytext)
library(hms)
library(data.table)
library(stringi)
library(mapview)
library(htmlwidgets)
library(stringr)


#================================= Mayur's data ==================================================================

dt = read_rds( "rds/dt.rds")
bgmap = raster("data/MC2-tourist.tif")
Abila_st = read_rds("rds/Abila_st.rds")
gps_sf = read_rds("rds/gps_sf.rds")
cc = read_rds("rds/cc.rds")
car_assign = read_rds("rds/car_assign.rds")

#================================= Darius (Yeo Ding Run)'s data ==================================================

cc_dar <- read_csv("data/cc.csv")
cc_DT <- read_csv("data/cc.csv")

# Convert the date to date-time format
cc_dar$date = as.Date(cc_dar$date, 
                      format = "%d/%m/%Y")

# Convert the last4ccnum and location to factor data format
cc_dar$last4ccnum = as.factor(cc_dar$last4ccnum)

cc_dar$location = as.factor(cc_dar$location)

#================================= Jin Ru's data ==================================================================

nodes <- read_csv("data/GAStech_nodes_updated.csv")
nodes_2 <- read_csv("data/GAStech_nodes_updated.csv")
processed_email_headers_1 <- read_csv("data/processed_email_headers_v1.csv")
processed_email_headers_2 <- read_csv("data/processed_email_headers_v2.csv")
processed_email_headers_3 <- read_csv("data/processed_email_headers_v3.csv")

#================================= UI ==================================================================

# Define UI
ui <- 
  
  fluidPage(
    list(tags$style(HTML("
      .navbar .navbar-nav {float: right; 
                           #color: #ff3368; 
                           font-size: 22px; 
                           #background-color: #FFFF00 ; 
                           } 
      .navbar.navbar-default.navbar-static-top{ 
                                      #color: #ff3368; 
                                      font-size: 22px; 
                                      #background-color: #FFFF00 ;
                                      }
      .navbar .navbar-header {float: left; } 
      .navbar-default .navbar-brand { 
                                      #color: #ff3368; 
                                      font-size: 22px; 
                                      #background-color: #FFFF00 ;
                                      } 

  "))),
    
    navbarPage(
      
      #shinythemes::themeSelector(),
      theme = bslib::bs_theme(bootswatch = "flatly"), 
      
      #(div(style="font-size:19pt;", style="font-weight: bold;", "VAST Challenge 2021 - The Kronos Incident")), 
      title = "VAST Challenge 2021 - The Kronos Incident", 
      
      tabPanel("Introduction", tags$style(
        ".navbar-nav li a {
        font-size: 20px;
        #font-weight: bold;
      }
    "
      ),
      
      
      fluidRow(
        column(6, div(style="font-size:14pt;",
                      style='padding-left:50px; padding-right:50px; padding-top:5px; padding-bottom:5px',
                      includeMarkdown("shinyapp-introduction.Rmd")
        )),
        column(3, style='padding-left:30px; padding-right:5px; padding-top:80px; padding-bottom:50px',
               img(class="img-polaroid",
                   src=paste0("https://vast-challenge.github.io/2021/img/site-image_v1.jpg"
                   )),
               tags$small(
                 "Image Source:  ",
                 a(href="https://vast-challenge.github.io/2021/index.html",
                   "VAST Challenge 2021"))
        ))),
      tags$head(
        tags$style(HTML("
      thead {
        font-size: 16px;
      }
    "))
      ),
      
      # ---------------------------------  Darius (Yeo Ding Run)'s tabPanel ---------------------------------
      
      tabPanel(" Credit Card Transactions", 
               # Application title
               titlePanel(div(style="font-size:16pt;font-weight:bold;", "Bipartite Network Analysis")),
               
               # Sidebar with a select input for weight and checkbox input for data table 
               sidebarLayout(
                 sidebarPanel(div(style="font-size:12pt;",
                                  
                                  dateRangeInput(inputId = "dateVariable",
                                                 label = "Date Range:",
                                                 start = as.Date("2014-01-06"),
                                                 end = as.Date("2014-01-19"),
                                                 min = as.Date("2014-01-06"),
                                                 max = as.Date("2014-01-19"),
                                                 separator = " to ", width = '1000%'), tags$head(
                                                   tags$style("
                                  .form-control {font-size: 0.85em !important;}
              #.input-daterange input {
              #  min-height: 40px; font-size: 30px;
              #} 
            ")
                                                 ),
                                  
                                  selectInput(inputId = "timePeriodVariable",
                                              label = "Time Period",
                                              choice = c("12am to 2:59am" = "12am to 2:59am",
                                                         "3am to 5:59am" = "3am to 5:59am",
                                                         "6am to 8:59am" = "6am to 8:59am",
                                                         "9am to 11:59am" = "9am to 11:59am",
                                                         "12pm to 2:59pm" = "12pm to 2:59pm",
                                                         "3pm to 5:59pm" = "3pm to 5:59pm",
                                                         "6pm to 8:59pm" = "6pm to 8:59pm",
                                                         "9pm to 11:59pm" = "9pm to 11:59pm"),
                                              selected = "6am to 8:59am"),
                                  tagList(
                                    tags$style(type = 'text/css', '#big_slider .irs-grid-text {font-size: 16px}'), 
                                    div(id = 'big_slider',
                                        sliderInput(inputId = "weightVariable",
                                                    label = "Minimum Weight:",
                                                    min = 0,
                                                    max = 2,
                                                    step = 1,
                                                    value = c(0)))),
                                  checkboxInput(inputId = "show_data_dar",
                                                label = "Show data table",
                                                value = TRUE),
                                  submitButton(div(style="font-size:12pt;","Apply Changes"))
                 )),
                 
                 # Show a plot of the generated Bipartite Network graph and data table
                 mainPanel(
                   tabsetPanel(
                     tabPanel("Plot", visNetworkOutput("visNetwork_object", 
                                                       height = "600px"),
                              #plotOutput("visIgraph", height = "600px"),
                              DT::dataTableOutput(outputId = "bipartiteTable") 
                     ),
                     tabPanel("Usage Help", includeMarkdown("usagehelp_creditcard.Rmd") )
                   )
                 )
               )
      ),
      
      
      # ---------------------------------  Mayur's tabPanel ---------------------------------
      
      tabPanel(" Vehicle Stoppings", 
               titlePanel(div(style="font-size:16pt;font-weight:bold;", "Vehicle Stoppings")),
               sidebarLayout(
                 sidebarPanel(div(style="font-size:12pt;",
                                  selectInput(inputId="Name",
                                              label="Employee Name: ",
                                              choices = c('Ada Campo-Corrente','Adra Nubarron','Axel Calzas','Bertrand Ovan','Birgitta Frente','Brand Tempestad','Edvard Vann','Elsa Orilla','Felix Balas','Felix Resumir',
                                                          'Gustav Cazar','Hennie Osvaldo','Hideki Cocinaro','Inga Ferro','Ingrid Barranco','Isak Baza','Isande Borrasca','Isia Vann','Kanon Herrero','Kare Orilla',
                                                          'Lars Azada','Lidelse Dedos','Linnea Bergen','Loreto Bodrogi','Lucas Alcazar','Marin Onda','Minke Mies','Nils Calixto','Orhan Strum','Sten Sanjorge Jr.',
                                                          'Stenig Fusil','Sven Flecha','Varja Lagos','Vira Frente','Willem Vasco-Pais','101','104','105','106','107'),
                                              selected = 'Nils Calixto', multiple = TRUE),
                                  selectInput(inputId="day",
                                              label="Day Of Month: ",
                                              choices = c(6,7,8,9,10,11,12,13,14,15,16,17,18,19),
                                              selected = 6, multiple = TRUE),
                                  sliderInput(inputId="hour",
                                              label="Hour Range: ",
                                              min=1,
                                              max=24,
                                              step = 1,
                                              value=c(1,24)),
                                  checkboxInput(inputId = "show_data",
                                                label = "Show Datatable",
                                                value = TRUE),
                                  submitButton(div(style="font-size:12pt;","Apply Changes")),
                                  verbatimTextOutput("text")
                 )),
                 mainPanel(
                   tabsetPanel(
                     tabPanel( 
                       "Plot", tmapOutput(outputId="map"),
                       br(),
                       DT::dataTableOutput(outputId = "dt")
                     ),
                     tabPanel("Usage Help",  includeMarkdown("usagehelp_vehicle.Rmd")  )
                   )
                 )
               ) ),
      
      # ---------------------------------  Jin Ru's tabPanels ---------------------------------
      
      navbarMenu(" Employee Emails", 
                 tabPanel("Investigating email anomalies", 
                          titlePanel(div(style="font-size:16pt;font-weight:bold;", "Investigating Email Anomalies")),
                          sidebarLayout(
                            sidebarPanel(div(style="font-size:12pt;",
                                             selectInput("Subject_Type_1", "Email Subject:",
                                                         c("Non-work related", "Work related")),
                                             submitButton(div(style="font-size:12pt;","Apply Changes"))
                            )),
                            
                            mainPanel(
                              tabsetPanel(
                                tabPanel(
                                  "Plot", plotlyOutput("plot1_a1", height = "850px" )
                                ),
                                tabPanel(
                                  "Usage Help", includeMarkdown("usagehelp_emailsplotly.Rmd")
                                )
                              )
                            )
                            
                          )),
                 tabPanel("Understanding relationships", 
                          titlePanel(div(style="font-size:16pt;font-weight:bold;", "Understanding Relationships")),
                          sidebarLayout(
                            sidebarPanel(div(style="font-size:12pt;",
                                             selectInput("Focus_2", "Focus on employee id :",
                                                         c(1:54)),
                                             selectInput("Subject_Type_2", "Subject:",
                                                         c("Non-work related", "Work related")),
                                             selectInput("SentHour_2", "Email Sent Hour:",
                                                         c("During_work_hours", "Outside_work_hours")),
                                             dateRangeInput("date_a2", "Date range:",
                                                            start = "2014-01-06",
                                                            end   = "2014-01-17",
                                                            min =  "2014-01-06",
                                                            max = "2014-01-17"),
                                             tagList(
                                               tags$style(type = 'text/css', '#big_slider .irs-grid-text {font-size: 16px}'), 
                                               div(id = 'big_slider',
                                                   sliderInput("weight_a2", "Min Emails Exchanged:",
                                                               1, 10, 3, dragRange = FALSE))),
                                             submitButton(div(style="font-size:12pt;","Apply Changes"))
                            )),
                            
                            mainPanel(
                              tabsetPanel(
                                tabPanel(
                                  "Plot",  
                                  DTOutput('tbl_plot1_a2'),
                                  br(), 
                                  visNetworkOutput("network_viz2", height = "700px"),
                                  br(),
                                  br(),
                                  DTOutput('tbl_plot2_a2')
                                ),
                                tabPanel("Usage Help", includeMarkdown("usagehelp_emailsvisnetwork.Rmd"))
                              )
                            )
                            
                          ))
                 
      )
    )
  )

#================================= SERVER ==================================================================

server <- function(input, output) {
  
  # ---------------------------------  Mayur's server output codes ---------------------------------
  
  
  observe(output$map<-renderTmap({
    gps_sf_selected <- gps_sf %>%
      dplyr::filter(Name %in% input$Name & day %in% input$day & hour>=input$hour[1] & hour<input$hour[2])
    
    tmap_mode("view")
    tm_shape(bgmap) +
      tm_rgb(bgmap, r = 1, g = 2, b = 3,
             alpha = NA,
             saturation = 1,
             interpolate = TRUE,
             max.value = 255) +
      tm_shape(gps_sf_selected)+
      tm_bubbles("Details" ,col = "Name", alpha = 0.5, palette = 'Dark2', border.col = 'black', border.lwd = 2,
                 size = 0.25, labels = NULL)+
      tmap_options(max.categories = 44)
  }))
  
  
  
  
  setDT(cc)[location %like% "^Katerina", location := "Katerina's Cafe"]
  
  output$dt <- DT::renderDataTable({
    if(input$show_data){
      DT::datatable(cc,
                    filter = 'top') %>%
        formatStyle(columns = 0,
                    target = 'row',
                    lineHeight = '100%', fontSize = '100%') %>%
        formatDate(1, "toLocaleString")
    }
  })
  
  
  # ---------------------------------  Darius (Yeo Ding Run)'s server output codes ---------------------------------
  
  # Render plot
  output$visNetwork_object <- renderVisNetwork({
    
    # Creating variables for date input, time period and weight inputs
    dateInput <- input$dateVariable
    
    timePeriodInput <- input$timePeriodVariable
    
    weightInput <- input$weightVariable
    
    #centralityInput <- input$centralityVariable
    
    # Filter the data based on the selected date and time period inputs
    cc_by_date <- cc_dar %>%
      dplyr::select(date, 
                    time_period, 
                    last4ccnum, 
                    location) %>%
      dplyr::filter(date == dateInput & 
                      time_period == timePeriodInput)
    
    # Remove the date and time period columns from the data
    cc_by_date_subset <- subset(cc_by_date,
                                select = -c(date, 
                                            time_period))
    
    # Calculate weights for the filtered data
    cc_network_edges_selectInput <- cc_by_date_subset %>% 
      group_by(last4ccnum, 
               location) %>%
      summarise(weight = n()) %>%
      dplyr::filter(weight >= weightInput) %>%
      ungroup()
    
    # Remove the weight column from the data to generate the Bipartite Network graph
    cc_network_edges_selectInput <- subset(cc_network_edges_selectInput, 
                                           select = -c(weight))
    
    # Generating the igraph object for the Bipartite Network graph
    g <- graph_from_data_frame(cc_network_edges_selectInput)
    
    bipartite.mapping(g)
    
    V(g)$type <- bipartite_mapping(g)$type
    
    V(g)$color <- ifelse(V(g)$type, 
                         "lightblue", 
                         "salmon")
    
    
    V(g)$shape <- ifelse(V(g)$type, 
                         "square", 
                         "circle")
    
    E(g)$color <- "lightgray"
    
    V(g)$label.color <- "black"
    
    
    V(g)$label.cex <- 0.5
    
    
    bipartite_layout <- layout_as_bipartite(g)
    
    # Render a visNetwork object from an igraph object
    visIgraph(g,
              idToLabel = TRUE,
              layout = "layout_as_bipartite",
              type = "full") %>% visInteraction(navigationButtons = TRUE) 
  })
  
  # Tidy up the data table's header names
  names(cc_DT)[names(cc_DT) == "last4ccnum"] <- "last 4 credit card number"
  names(cc_DT)[names(cc_DT) == "loyaltynum"] <- "loyalty card number"
  
  
  # Render data table
  output$bipartiteTable <- DT::renderDataTable({
    if(input$show_data_dar){
      DT::datatable(data = cc_DT %>% dplyr::select(1:8), 
                    filter = "top",
                    options= list(pageLength = 10),
                    rownames = FALSE) %>%
        formatStyle(0, target = 'row', lineHeight='80%', fontSize = '100%')
    }
  }) 
  
  
  
  
  # --------------------------------- Jin Ru's server output codes ---------------------------------
  
  ## First sub-plot
  
  output$plot1_a1 <- renderPlotly({
    
    data_a1 <-  processed_email_headers_1 %>%
      dplyr::filter(Subject_Type_1 == input$Subject_Type_1) %>%
      group_by(SentTime_1, SentDate_1, Weekday_1, Sender_fullname_1) %>%
      summarise(weight_a1 = n()) %>%
      ungroup()
    
    data_a1$SentDate_1 <- lubridate::dmy(data_a1$SentDate_1)
    
    
    data_a1$SentTime_1 <-  format(as.POSIXct(data_a1$SentTime_1), format = "%H:%M")
    
    
    names(data_a1)[names(data_a1) == "weight_a1"] <- "Emails"
    
    
    d <- highlight_key(data_a1)
    m <- list(
      l = 100,
      r = 100,
      b = 100,
      t = 100,
      pad = 4
    )
    base <- plot_ly(d, showlegend = FALSE) %>%  hide_colorbar() %>% plotly::layout(
      xaxis = list(tickfont = list(size = 16)), 
      yaxis = list(tickfont = list(size = 16)), margin = m)
    
    
    subplot(
      add_markers(base, x = ~SentTime_1, y = ~Emails),
      add_boxplot(base, x = ~Weekday_1, y = ~Emails) %>%
        add_markers(x = ~Weekday_1, y = ~Emails, alpha = 0.1),
      add_boxplot(base, x = ~SentDate_1, y = ~Emails) %>%
        add_markers(x = ~SentDate_1, y = ~Emails, alpha = 0.1),
      add_trace(base, x = ~Sender_fullname_1, y = ~Emails, type = "scatter") %>%
        add_markers(x = ~Sender_fullname_1, y = ~Emails, alpha = 0.1),
      shareY = TRUE, nrows = 4, margin = 0.07
    ) %>% 
      plotly::layout(title = 'Emails sent by GAStech employees', titlefont=list(size=20), barmode = "overlay") %>%
      highlight("plotly_selected", dynamic = FALSE, color = "red", opacityDim = 0.15) 
  })
  
  ## Second sub-plot
  
  
  processed_email_headers_2$SentDate_2 <- lubridate::dmy(processed_email_headers_2$SentDate_2)
  
  
  reactive_data_2 <- reactive({
    nodes_2[nodes_2$id == input$Focus_2, ]
  })
  
  sd_2 <- SharedData$new(reactive_data_2)
  
  
  observeEvent(input$Focus_2, {
    visNetworkProxy("network_viz2") %>%
      visFocus(id = input$Focus_2, scale = 0.8)
    
  })
  
  output$network_viz2 <- renderVisNetwork({
    
    
    nodes$title <- paste0("Id:", nodes$id, "<br> Name:", nodes$label,  "<br> Department: ",
                          nodes$Department,
                          "<br> Title: ", nodes$Title)
    
    
    per_route <- processed_email_headers_2 %>%  
      dplyr::filter(Subject_Type_2 == input$Subject_Type_2) %>%
      dplyr::filter(SentHour_2 == input$SentHour_2) %>%
      dplyr::filter(SentDate_2 >= input$date_a2[1] & SentDate_2 <= input$date_a2[2]) %>%
      group_by(Sender_fullname_2, Recipient_fullname_2) %>%
      summarise(weight_a2 = n()) %>%
      dplyr::filter(weight_a2 >= input$weight_a2) %>%
      ungroup()
    
    edges <- per_route %>% 
      left_join(nodes, by = c("Sender_fullname_2" = "label")) %>% 
      rename(from = id)
    
    edges <- edges %>% 
      left_join(nodes, by = c("Recipient_fullname_2" = "label")) %>% 
      rename(to = id)
    
    edges$width = edges$weight_a2
    
    set.seed(123)
    
    
    GAStech_graph <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE)
    
    
    clp <- cluster_label_prop(GAStech_graph)
    class(clp)
    nodes$group <- clp$membership
    
    
    eigencentrality_ <- eigen_centrality(GAStech_graph)$vector
    nodes$size <- eigencentrality_*100
    
    visNetwork(nodes, edges, main= list(text = "Eigenvector Centrality & Community Detection Network", style = "font-size:20px;text-align:center;font-weight:bold;"),
               submain = list(text = 'Nodes that are more influential are bigger in size, thicker arrows represent more emails sent btween nodes', style = "font-size:16px;text-align:center;"),
               width = "100%") %>%
      visLayout(randomSeed = 123) %>%
      visInteraction(navigationButtons = TRUE) %>%
      
      
      # Optimization
      visIgraphLayout(layout = "layout_nicely") %>%
      visEdges(
        arrows =list(from = list(enabled = TRUE, scaleFactor = 0.8), to = list(enabled = TRUE, scaleFactor = 0.8)),
        color = list(color = "lightgray", highlight = "red")) %>%
      visPhysics(stabilization = FALSE) %>%
      
      # Customization
      visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE, tooltipDelay = 0) %>%
      visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE, degree = 1), 
                 nodesIdSelection = list(enabled = TRUE, selected = "1", main = "Employee", style = 'width: 300px; height: 28px; font-size: 18px;'),
                 selectedBy = list(variable="group", main="Community", multiple = TRUE, sort = TRUE, style = 'width: 300px; height: 28px; font-size: 18px;')) %>%
      visLegend(width=0.2, position="right")
  })
  
  
  output$tbl_plot1_a2 <- DT::renderDT({
    DT::datatable(sd_2, options = list(dom = 't'), caption = htmltools::tags$caption(
      style = 'caption-side: top; text-align: center;font-size:100%;',
      'Table 1: ', htmltools::em('Employee details from focus on employee id'))) %>%
      formatStyle(0, target = 'row', lineHeight='80%', fontSize = '120%')
  }, server = FALSE)
  
  
  output$tbl_plot2_a2 <- DT::renderDataTable({
    DT::datatable(processed_email_headers_3, filter = 'top', caption = htmltools::tags$caption(
      style = 'caption-side: top; text-align: center;font-size:100%;',
      'Table 2: ', htmltools::em('All email records of GAStech employees'))) %>%
      formatStyle(0, target = 'row', lineHeight='80%', fontSize = '100%')
  })
  
}

#================================= Run application ==================================================================

shinyApp(ui = ui, server = server)

