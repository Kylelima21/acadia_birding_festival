### Acadia Birding Festival Shiny App
### Created by Kyle Lima, Becky Marvil

## Read source functions, packages, and input data
source("abf_app_funcs.R")

fullnosp <- read.csv("www/datasets/abf_clean_data.csv") %>% 
  filter(!grepl("sp.", common.name)) %>% 
  arrange(common.name)

summaryst <- read.csv("www/datasets/number_summary.csv")




## Build UI
ui <- function() {
  
  ## Set up and menu creation
  pagePiling(
    sections.color = c('white', 'white', 'white', 'white', '#607B8B', '#4A708B'),
    opts = list(direction = "vertical",
                css3 = "true"),
    menu = c(
      "Home" = "home",
      "Summary Map" = "map",
      "Species Explorer" = "species",
      "Trip Explorer" = "locs",
      "Trends" = "trends",
      "About" = "about"
    ),
      
      
      ## Home Page
      pageSectionImage(
        center = TRUE,
        img = "img/atpu.jpg",
        menu = "home",
        
        h1(textOutput("title"), class = "headerw shadow-dark", 
           tags$head(includeCSS("www/css/styles.css"))),
        
        h3("Observations Explorer", class = "subheader shadow-dark"),
        
        absolutePanel(id = "logo", bottom = "8%", right = 20, width = "auto", fixed = TRUE, draggable = FALSE, height = "12%",
                      img(src = 'img/schoodic_stacked.jpeg', height = '100%', width = 'auto')),
        
        absolutePanel(id = "logo", bottom = "8%", left = 20, width = "auto", fixed = TRUE, draggable = FALSE, height = "12%",
                      img(src = 'img/abf_logo.png', height = '100%', width = 'auto')),
        
        absolutePanel(id = "logo1", bottom = 0, left = 0, width = "100%",  height = "6%", fixed = TRUE, draggable = FALSE),
        
        absolutePanel(id = "loglink", bottom = 9, right = 180, width = 30, fixed = TRUE, draggable = FALSE, height = "auto",
                      actionButton("twitter_share", label = "", icon = icon("twitter"), style = 'padding:6px',
                                   onclick = sprintf("window.open('%s')", 
                                                     "https://twitter.com/SchoodicInst"))),
        
        absolutePanel(id = "loglink", bottom = 9, right = 140, width = 30, fixed = TRUE, draggable = FALSE, height = "auto",
                      actionButton("facebook_share", label = "", icon = icon("facebook"), style = 'padding:6px',
                                   onclick = sprintf("window.open('%s')", 
                                                     "https://www.facebook.com/SchoodicInstitute"))),
        
        absolutePanel(id = "loglink", bottom = 9, right = 100, width = 30, fixed = TRUE, draggable = FALSE, height = "auto",
                      actionButton("instagram_share", label = "", icon = icon("instagram"), style = 'padding:6px',
                                   onclick = sprintf("window.open('%s')", 
                                                     "https://www.instagram.com/schoodicinst/"))),
        
        absolutePanel(id = "loglink", bottom = 9, right = 60, width = 30, fixed = TRUE, draggable = FALSE, height = "auto",
                      actionButton("youtube_share", label = "", icon = icon("youtube"), style = 'padding:6px',
                                   onclick = sprintf("window.open('%s')", 
                                                     "https://www.youtube.com/user/SchoodicInstitute"))),
        
        absolutePanel(id = "loglink", bottom = 9, right = 20, width = 30, fixed = TRUE, draggable = FALSE, height = "auto",
                      actionButton("linkedin_share", label = "", icon = icon("linkedin"), style = 'padding:6px',
                                   onclick = sprintf("window.open('%s')", 
                                                     "https://www.linkedin.com/company/schoodicinstitute/"))),
        
        absolutePanel(id = "loglink", bottom = 9, left = 20, width = 30, fixed = TRUE, draggable = FALSE, height = "auto",
                      actionButton("facebook_share", label = "", icon = icon("facebook"), style = 'padding:6px',
                                   onclick = sprintf("window.open('%s')", 
                                                     "https://www.facebook.com/AcadiaBirdingFestival/")))
      ),
      
      
      ## Summary Map
      pageSection(
        center = TRUE,
        menu = "map",
        leafletOutput("mapsum", height = "100%"),
        
        absolutePanel(id = "instruct", class = "panel",
                      top = 15, right = 40, width = 360, fixed = TRUE,
                      draggable = FALSE, height = "auto",
  
                      h2("Acadia Birding Festival Observations", class = "black"),
                      h4("(2010 to present)", class = "black"),
                      tags$br(),
                      h3("Festival participants have been busy!", align = "left", class = "black"),
                      h4(textOutput("num_sp"), align = "left", class = "black"),
                      h4(textOutput("num_loc"), align = "left", class = "black"),
                      h4(textOutput("num_check"), align = "left", class = "black"),
                      h4(textOutput("num_birds"), align = "left", class = "black")
                      ),
        
        absolutePanel(id = "controls", class = "panel",
                      bottom = 20, left = 10, width = 360, fixed = TRUE,
                      draggable = FALSE, height = "auto",
                      
                      h4("How to use this page:"),
                      h5("Click on a circle marker to expand the sightings.", align = "left"),
                      h5("Hover your cursor over any blue point to see the species.",
                         align = "left"),
                      h5("Click on any blue point to open a window with a link to the eBird checklist.",
                         align = "left")
                      )
      ),
    
      
      ## Species Explorer
      pageSection(
        center = TRUE,
        menu = "species",
        leafletOutput("reactspmap", height = "100%"),
        
        absolutePanel(id = "instruct", class = "panel panel-default",
                      top = 15, right = 40, width = 325, fixed = TRUE,
                      draggable = FALSE, height = "auto",
                      
                      h1("Species Explorer"),
                      tags$br(),

                      pickerInput("species_select",
                                  label = "Select a species:",
                                  choices = as.character(unique(fullnosp$common.name)),
                                  choicesOpt = list(noneSelectedText = "Search for a species"),
                                  options = list(`none-selected-text` = "Select a species...",
                                                 `live-search` = TRUE,
                                                 header = "Search Menu",
                                                 size = 12),
                                  selected = as.character(unique(fullnosp$common.name)[1]),
                                  multiple = FALSE)),
      
        absolutePanel(id = "darkplots", class = "panel",
                      bottom = 5, right = 0, width = 587, fixed = TRUE,
                      draggable = FALSE, height = 177,

                      plotOutput("spplot", width = 585, height = 175))
        
      ),
      
     
      ## Trip Explorer
      pageSectionImage(
        center = TRUE,
        menu = "locs",
        img = "img/bith.jpg",
        
        h2(textOutput("triptitle"), class = "triphead black"),
        
        absolutePanel(id = "trips", class = "panel panel-default",
                      top = "32%", left = "2%", width = "47%", fixed = TRUE,
                      draggable = FALSE, height = "auto",

                      h2("Trip Description", align = "left", class = "white"),
                      
                      h6(textOutput("diff"), align = "left", class = "white"),
                      h5(textOutput("descrip"), align = "left", class = "white"),
                      
                      h4("Download the species list here:", class = "white"),
                         downloadButton("downloadCsv", "Download as CSV"),br(),br()),
        
        absolutePanel(id = "trips", class = "panel panel-default",
                      top = "16.5%", right = "4%", width = "45%", fixed = TRUE,
                      draggable = FALSE, height = "auto",
        
                      h2("Trip Summary", align = "left", class = "white"),

                      h4(htmlOutput("trip_text", align = "left", class = "white")),br(),
                      
                      leafletOutput("tripmap", width = "100%", height = 370),

                      h6("Use the scroll wheel, or double click to zoom in. Click and drag
                         to move around.", align = "left", class = "white")),
        
        absolutePanel(id = "trips", class = "panel panel-default",
                      top = "16.5%", left = "9.5%", width = "30%", fixed = TRUE,
                      draggable = FALSE, height = "auto",
                      
                      h3("Select a trip:", align = "left", class = "white"),
                      
                      pickerInput("trip_select",
                                  choices = as.character(unique(fullnosp %>% drop_na() %>% pull(trip))),
                                  choicesOpt = list(noneSelectedText = "Select a trip..."),
                                  options = list(`none-selected-text` = "Select a trip...",
                                                 `live-search` = TRUE,
                                                 header = "Search Menu",
                                                 size = 14),
                                  width = "100%",
                                  selected = as.character(unique(fullnosp %>% drop_na() %>% pull(trip))[1]),
                                  multiple = FALSE))
      ),
    
    
      ## Trends
      pageSectionImage(
        center = TRUE,
        menu = "trends",
        img = "img/monhegan.jpg",
        
        h1("Festival Trends Through Time", class = "triphead black"),
        
        absolutePanel(id = "plots", class = "panel",
                      top = "23%", left = "10%", 
                      width = "80%", height = "70%",
                      fixed = TRUE, draggable = FALSE,

                      tabsetPanel(type = "pills",
                                  tabPanel("Species", plotOutput("spy_plot",
                                                                        height = 450,
                                                                        width = "100%")),
                                  tabPanel("Total Birds", plotOutput("tpy_plot",
                                                                     height = 450,
                                                                     width = "100%")),
                                  tabPanel("Participants", plotOutput("partypy_plot",
                                                                        height = 450,
                                                                        width = "100%")),
                                  tabPanel("Trips", plotOutput("trippy_plot",
                                                                        height = 450,
                                                                        width = "100%"))
                                  
          )
        )

      ),

      
      ## About
      pageSectionImage(
        menu = "about",
        center = TRUE,
        img = "img/yewa.jpg",
        
        absolutePanel(id = "about", class = "panel panel-default",
                      top = 70, left = 20, width = 817, fixed = TRUE,
                      draggable = FALSE, height = "auto",
                      
                      h4("Last updated"),
                      "Fall 2022.",
                      br(),
                      
                      h4("Background"),
                      "This application was built to display eBird data that the 
                      Acadia Birding Festival has collected since 2010, as well as provide 
                      an interactive exploration tool for festival participants.",
                      br(),
                      
                      h4("Code"),
                      "Code and required elements to generate this Shiny app are available 
                      on ", a("Github.", href="https://github.com/Kylelima21/acadia_birding_festival", 
                              target = "_blank"),
                      br(),
                      
                      h4("Sources"),
                      "Data supplied by ", a("eBird.", href = "https://www.ebird.org/", target = "_blank"),
                      br(),
                      
                      h4("Authors"),
                      "Kyle Lima, ", a("Schoodic Institute at Acadia National Park", 
                                       href = "https://schoodicinstitute.org/", target = "_blank"), br(),
                      "Becky Marvil, ", a("Acadia Birding Festival",
                                          href = "https://www.acadiabirdingfestival.com/",
                                                 target = "_blank"), br(),
                      
                      h4("Get in touch!"),
                      "• If you have any questions about the Acadia Birding Festival, please 
                      contact Becky Marvil: beckym@acadiabirdingfestival.com",
                      br(),
                      "• You can direct questions about this application or about Schoodic Institute
                      to Kyle Lima: klima@schoodicinstitute.org",
                      br(),
                      "• Please checkout our websites for more information."),
        
        absolutePanel(id = "logo", class = "card", bottom = 25, right = 20, width = "auto", fixed = TRUE, draggable = FALSE, height = "12%",
                      tags$a(tags$img(src = 'img/schoodic_stacked.jpeg', height = '100%', width = 'auto'), 
                             href = 'https://schoodicinstitute.org/', target = "_blank")),
        
        absolutePanel(id = "logo", class = "card", bottom = 25, left = 20, width = "auto", fixed = TRUE, draggable = FALSE, height = "12%",
                      tags$a(tags$img(src = 'img/abf_logo.png', height = '100%', width = 'auto'),
                             href = 'https://www.acadiabirdingfestival.com/', target = "_blank"))
      )
  )
}




## Build server
server <- function(input, output, session) {
  
  ## Title page header
  output$title <- renderText("Acadia Birding Festival")
  
  ## Summary leaflet map of all obs
  output$mapsum <- renderLeaflet({ 
    leaflet_summary(fullnosp)
  })
  
  ## Number of species recorded
  output$num_sp <- renderText({
    paste("•", comma(summaryst$total[1]), "species recorded", sep = " ")
  })
  
  ## Number of locations birded
  output$num_loc <- renderText({
    paste("•", comma(summaryst$total[2]), "locations birded", sep = " ")
  })
  
  ## Number of checklists sumbitted
  output$num_check <- renderText({
    paste("•", comma(summaryst$total[3]), "checklists submitted", sep = " ")
  })
  
  ## Number of individuals seen
  output$num_birds <- renderText({
    paste("•", comma(summaryst$total[4]), "individual birds tallied", sep = " ")
  })
  
  ## Reactive data frame for Species Explorer tab
  species_reactive_db <- reactive({
    fullnosp %>% 
      filter(common.name == paste(input$species_select))
  })
  
  ## Reactive map to display species obs for Species Explorer tab - uses species_reactive_db
  output$reactspmap <- renderLeaflet({ 
    species_leaflet(species_reactive_db())
  })
  
  ## Reactive graphic to display species trends for Species Explorer tab - uses species_reactive_db
  output$spplot <- renderPlot({
    sp_trends(species_reactive_db())
  })
  
  ## Reactive data frame for Trip Explorer tab
  trip_reactive_db <- reactive({
    fullnosp %>% 
      filter(trip == paste(input$trip_select))
  })
  
  ## Trip title
  output$triptitle <- renderText({
    trip_reactive_db()$trip[1]
  })
  
  ## Reactive output to display summary for trips for Trip Explorer tab - uses trip_reactive_db
  output$trip_text <- renderUI({ 
    str1 = paste("• ", trip_summary(trip_reactive_db())[1,2], " species recorded.")
    str2 = paste("• ", trip_summary(trip_reactive_db())[2,2], " checklists submitted.")
    str3 = paste("• ", trip_summary(trip_reactive_db())[3,2], " years run.")
    str4 = paste("• ", trip_summary(trip_reactive_db())[4,2], " participants on the average trip.")

    HTML(paste("<p style='line-height:1.7'>", paste(str1, str2, str3, str4, sep = '<br/>'),
               "</p>"))
  })
  
  ## Trip description
  output$descrip <- renderText({
    trip_reactive_db()$descrip[1]
  })
  
  ## Trip difficulty
  output$diff <- renderText({
    trip_reactive_db()$diff[1]
  })
  
  ## Reactive leaflet map for trip locations
  output$tripmap <- renderLeaflet({ 
    trip_leaflet(trip_reactive_db())
  })
  
  ## Output to download data as a CSV
  output$downloadCsv <- downloadHandler(
    filename = function() {
      paste0(str_replace_all(input$trip_select, "[:space:]", "_"), "_trip_list.csv")
    },
    content = function(file) {
      triplist = fullnosp %>% 
        filter(trip == paste(input$trip_select)) %>% 
        select(common.name, scientific.name, date, count) %>% 
        mutate(date = year(date)) %>% 
        group_by(common.name, scientific.name, date) %>% 
        summarize(count = sum(count), .groups = "drop") %>% 
        pivot_wider(names_from = date, values_from = count) %>% 
        mutate_all(~replace(., is.na(.), 0)) %>% 
        select(common.name, scientific.name, sort(names(.[-c(1:2)])))
        
      write.csv(triplist, file, row.names = F)
    })
  
  ## Species/year ggplot for Trends tab
  output$spy_plot <- renderPlot({
    spy_plot(read.csv("www/datasets/species_per_year.csv"))
  })
  
  ## Total birds/year ggplot for Trends tab
  output$tpy_plot <- renderPlot({
    tpy_plot(read.csv("www/datasets/abf_clean_data.csv"))
  })
  
  ## Number of participants/year ggplot for Trends tab
  output$partypy_plot <- renderPlot({
    partypy_plot(read.csv("www/datasets/abf_participants_trips.csv"))
  })
  
  ## Number of trips/year ggplot for Trends tab
  output$trippy_plot <- renderPlot({
    trippy_plot(read.csv("www/datasets/abf_participants_trips.csv"))
  })
  
}




## Run app
shinyApp(ui, server)

