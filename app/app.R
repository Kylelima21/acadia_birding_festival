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
  #pagePiling(
  fullPage(
    #sections.color = c('white', 'white', 'white', '#607B8B', '#607B8B', '#3E4A57'),
    opts = list(sections.color = c('white', 'white', 'white', '#607B8B', '#607B8B', '#3E4A57')),
    menu = c(
      "Home" = "home",
      "Summary Map" = "map",
      "Species Explorer" = "species",
      "Changes" = "changes",
      "Photo Gallery" = "photos",
      "About" = "about"
    ),
      
      
      ## Home Page
      pageSectionImage(
        center = TRUE,
        img = "img/background.jpg",
        menu = "home",
        h1(textOutput("title"), class = "header shadow-dark", 
           tags$head(includeCSS("www/css/styles.css"))),
        h3("Observations Explorer", class = "subheader shadow-dark"),
        absolutePanel(id = "logo", class = "card", bottom = 20, right = 80, width = 40, fixed = TRUE, draggable = FALSE, height = "12%",
                      tags$a(href = 'https://schoodicinstitute.org/', tags$img(src = 'img/schoodic_stacked.jpeg', height = '100%', width = 'auto'))),
        absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 40, fixed = TRUE, draggable = FALSE, height = "12%",
                      tags$a(href = 'https://www.acadiabirdingfestival.com/', tags$img(src = 'img/abf_logo.png', height = '100%', width = 'auto')))
      ),
      
      
      ## Summary Map
      pageSection(
        center = TRUE,
        menu = "map",
        leafletOutput("mapsum", height = "100%"),
        absolutePanel(id = "controls", class = "panel",
                      top = 15, right = 20, width = 400, fixed = TRUE,
                      draggable = FALSE, height = "auto",
  
                      h2("Acadia Birding Festival Observations"),
                      h4("(2010 to present)"),
                      tags$br(),
                      h3("The festival has contributed a lot of eBird data:", align = "left"),
                      h4(textOutput("num_sp"), align = "left"),
                      h4(textOutput("num_loc"), align = "left"),
                      h4(textOutput("num_check"), align = "left"),
                      h4(textOutput("num_birds"), align = "left")
                      )
      ),
    
      
      ## Species Explorer
      pageSection(
        center = TRUE,
        menu = "species",
        leafletOutput("reactspmap", height = "100%"),
        absolutePanel(class = "panel panel-default",
                      top = 10, right = 40, width = 300, fixed = TRUE,
                      draggable = FALSE, height = "auto",

                      pickerInput("species_select",
                                  label = h3("Select a species to explore:"),
                                  choices = as.character(unique(fullnosp$common.name)),
                                  choicesOpt = list(noneSelectedText = "Search for a species"),
                                  options = list(`none-selected-text` = "Select a species...",
                                                 `live-search` = TRUE,
                                                 size = 16),
                                  selected = as.character(unique(fullnosp$common.name)[1]),
                                  multiple = FALSE)),
      
        absolutePanel(class = "panel panel-default",
                      bottom = 15, right = 4, width = 677, fixed = TRUE,
                      draggable = FALSE, height = 167,

                      plotOutput("spplot", width = 675, height = 165))
        
      ),
      
     
      ## Changes
      pageSection(
        center = TRUE,
        menu = "changes",
        absolutePanel(id = "controls", class = "panel panel-default",
                      top = 75, left = 20, width = 700, fixed = TRUE,
                      draggable = FALSE, height = "auto",

                      plotOutput("spy_plot"))
      ),
      
      
      ## Photo Gallery
      pageSection(
        center = TRUE,
        menu = "photos",
        
        fullSlide(
          img = "img/schoodic_stacked.jpeg"),

        fullSlideImage(
          img = paste0(
            "https://raw.githubusercontent.com/alvarotrigo/",
            "fullPage.js/master/examples/imgs/bg5.jpg"),
          h2("Image background"))
      ),

      
      ## About
      pageSection(
        menu = "about",
        center = TRUE,
        h1("About this site:", class = "headerl white"),
        h3(HTML("• This Shiny application was created by Schoodic Institute at Acadia National Park<br>with the Acadia Birding Festival"),
           class = 'white'),
        tags$br(),
        h3("• If you have any questions about this website, please contact Kyle Lima at klima@schoodicinstitute.org", class = 'white'),
        tags$br(),
        h3("• For questions or information about the Acadia Birding Festival, please contact Becky Marvil at beckym@acadiabirdingfestival.com",
           class = 'white'),
        h3(tags$a("Acadia Birding Festival", href = "https://www.acadiabirdingfestival.com/", 
                  class = "footerl link"),
           tags$a("Schoodic Institute at Acadia National Park", 
                  href = "https://schoodicinstitute.org/",
                  class = "footerr link"))
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
    paste("•", summaryst$total[1], "species recorded", sep = " ")
  })
  
  ## Number of locations birded
  output$num_loc <- renderText({
    paste("•", summaryst$total[2], "locations birded", sep = " ")
  })
  
  ## Number of checklists sumbitted
  output$num_check <- renderText({
    paste("•", summaryst$total[3], "checklists submitted", sep = " ")
  })
  
  ## Number of individuals seen
  output$num_birds <- renderText({
    paste("•", summaryst$total[4], "individual birds tallied", sep = " ")
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
  
}




## Run app
shinyApp(ui, server)

