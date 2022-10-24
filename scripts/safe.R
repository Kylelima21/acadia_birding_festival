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
    sections.color = c('white', 'white', 'white', '#607B8B', '#607B8B', '#4A708B'),
    opts = list(easing = "swing"),
    menu = c(
      "Home" = "home",
      "Summary Map" = "map",
      "Species Explorer" = "species",
      "Location Explorer" = "locs",
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
      
      absolutePanel(id = "logo", class = "card", bottom = 25, right = 20, width = "auto", fixed = TRUE, draggable = FALSE, height = "12%",
                    tags$a(href = 'https://schoodicinstitute.org/', tags$img(src = 'img/schoodic_stacked.jpeg', height = '100%', width = 'auto'))),
      
      absolutePanel(id = "logo", class = "card", bottom = 25, left = 20, width = "auto", fixed = TRUE, draggable = FALSE, height = "12%",
                    tags$a(href = 'https://www.acadiabirdingfestival.com/', tags$img(src = 'img/abf_logo.png', height = '100%', width = 'auto')))
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
      
      absolutePanel(class = "panel panel-default",
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
                                               size = 14),
                                selected = as.character(unique(fullnosp$common.name)[1]),
                                multiple = FALSE)),
      
      absolutePanel(id = "darkplots", class = "panel",
                    bottom = 5, right = 0, width = 587, fixed = TRUE,
                    draggable = FALSE, height = 177,
                    
                    plotOutput("spplot", width = 585, height = 175))
      
    ),
    
    
    ## Location Explorer
    pageSection(
      center = TRUE,
      menu = "locs",
      
      fullSlideImage(
        img = "img/yewa.jpg"),
    ),
    
    
    ## Trends
    pageSectionImage(
      center = TRUE,
      menu = "trends",
      img = "img/btnw.jpeg",
      
      h1("Festival Trends Through Time", class = "header shadow-dark"),
      # absolutePanel(class = "panel panel-default",
      #               top = "10%", left = "1%", width = "35%", fixed = TRUE,
      #               draggable = FALSE, height = "auto",
      #               
      #               h1("Festival Trends Through Time")),
      
      absolutePanel(class = "panel panel-default",
                    bottom = "5%", left = "1%", width = "43%", fixed = TRUE,
                    draggable = FALSE, height = "auto",
                    
                    plotOutput("spy_plot")),
      
      absolutePanel(class = "panel panel-default",
                    bottom = "0%", left = "45%", width = "51%", fixed = TRUE,
                    draggable = FALSE, height = "auto",
                    
                    plotOutput("tpy_plot"))
    ),
    
    
    ## About
    pageSectionImage(
      menu = "about",
      center = TRUE,
      img = "img/spgr.jpg",
      
      absolutePanel(id = "about", class = "panel panel-default",
                    top = 75, right = 150, width = 540, fixed = TRUE,
                    draggable = FALSE, height = "auto",
                    
                    h1("About this site:", class = "white"),
                    tags$br(),
                    tags$br(),
                    h4("• This Shiny application was created by Schoodic Institute at 
                              Acadia National Park and the Acadia Birding Festival.",
                       class = 'white', align = "left"),
                    tags$br(),
                    h4("• If you have any questions about this website, please contact 
                         Kyle Lima (klima@schoodicinstitute.org).", class = 'white', align = "left"),
                    tags$br(),
                    h4("• For questions or information about the Acadia Birding Festival, 
                         please contact Becky Marvil (beckym@acadiabirdingfestival.com).",
                       class = 'white', align = "left"),
                    tags$br(),
                    h4("• Please checkout our websites for more information by clicking on the 
                      logos in the bottom corners of this page.",
                       class = 'white', align = "left"),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br(),
      ),
      
      
      absolutePanel(id = "logo", class = "card", bottom = 25, right = 20, width = "auto", fixed = TRUE, draggable = FALSE, height = "12%",
                    tags$a(href = 'https://schoodicinstitute.org/', tags$img(src = 'img/schoodic_stacked.jpeg', height = '100%', width = 'auto'))),
      
      absolutePanel(id = "logo", class = "card", bottom = 25, left = 20, width = "auto", fixed = TRUE, draggable = FALSE, height = "12%",
                    tags$a(href = 'https://www.acadiabirdingfestival.com/', tags$img(src = 'img/abf_logo.png', height = '100%', width = 'auto'))),
      
      absolutePanel(class = "panel",
                    bottom = "27%", right = 325, width = 200, fixed = TRUE,
                    draggable = FALSE, height = "auto",
                    
                    pageButtonTo(h4("Return home", class = "black"), section = 1))
    )
  )
}




## Build server
server <- function(input, output, session) {
  
  ## Title page header
  output$title <- renderText("Acadia Birding Festival")
  
  ## Summary leaflet map of all obs
  output$mapsum <- renderLeaflet({ 
    leaflet_summary(read.csv("www/datasets/abf_clean_data.csv"))
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
    read.csv("www/datasets/abf_clean_data.csv") %>% 
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
  
  ## Species/year ggplot for Changes tab
  output$spy_plot <- renderPlot({
    spy_plot(read.csv("www/datasets/species_per_year.csv"))
  })
  
  ## Total birds/year ggplot for Changes tab
  output$tpy_plot <- renderPlot({
    tpy_plot(read.csv("www/datasets/abf_clean_data.csv"))
  })
  
}




## Run app
shinyApp(ui, server)

