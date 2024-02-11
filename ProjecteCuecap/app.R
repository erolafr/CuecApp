library(shiny)
library(readxl)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Encuentra tu cueca"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput('Titulo', 'Selecciona el titulo', choices = NULL),
      
      actionButton("printButton", "Buscar"),
      actionButton("clearButton", "Clear"),
      actionButton("randomSong", "Cueca al azar"),
      
      selectInput('Tono', 'Selecciona el tono', choices = NULL),
      selectInput("titlesByTone", "Selecciona título por tono", choices = NULL)
    ),
    
    mainPanel(
      h3(textOutput("selectedTitle")),
      h4(textOutput("selectedTone")),
      uiOutput("lyricsOutput")
    )
  )
)

server <- function(input, output, session) {
  # Define cuecasdf as a reactive expression
  cuecasdf <- reactive({
    read_excel("C:/Users/zool2620/Dropbox/Varis/ProjecteCuecap/DataSetCuecas.xlsx")
  })
  
  # Reactive value to track if "Buscar" button is clicked
  buscarButtonClicked <- reactiveVal(FALSE)
  
  # Update selectInput choices based on the data
  observe({
    # Check if cuecasdf() is not NULL before attempting to subset it
    if (!is.null(cuecasdf())) {
      titulos <- setdiff(unique(cuecasdf()$Titulo), "Titulo")
      tonos <- setdiff(unique(cuecasdf()$Tono), "Tono")
      updateSelectInput(session, "Titulo", choices = titulos)
      updateSelectInput(session, "Tono", choices = tonos)
    }
  })
  
  # Render the lyrics based on the selected title
  output$lyricsOutput <- renderUI({
    if (buscarButtonClicked()) { # Check if the "Buscar" button has been clicked
      req(input$Titulo) # Require that a title is selected
      if (!is.null(cuecasdf())) {
        selected_title <- input$Titulo
        lyrics_path <- cuecasdf()$RutaArxiuLletra[cuecasdf()$Titulo == selected_title]
        # Read the lyrics from the file
        if (length(lyrics_path) > 0 && file.exists(lyrics_path)) {
          lyrics <- readLines(lyrics_path)
          lyrics_text <- paste(lyrics, collapse = "<br>") # Use <br> for line breaks
          return(HTML(lyrics_text))
        } else {
          return("Lyrics not available")
        }
      }
    }
  })
  
  # Render the selected title
  output$selectedTitle <- renderText({
    req(buscarButtonClicked()) # Wait for the "Buscar" button to be clicked
    req(input$Titulo) # Require that a title is selected
    return(input$Titulo)
  })
  
  output$selectedTone <- renderText({
    req(buscarButtonClicked()) # Wait for the "Buscar" button to be clicked
    req(input$Titulo) # Require that a title is selected
    if (!is.null(cuecasdf())) {
      selected_title <- input$Titulo
      selected_tone <- cuecasdf()$Tono[cuecasdf()$Titulo == selected_title]
      return(selected_tone)
    }
  })
  
  # Show a random song's title and lyrics
  observeEvent(input$randomSong, {
    if (!is.null(cuecasdf())) {
      random_index <- sample.int(nrow(cuecasdf()), 1)
      random_title <- cuecasdf()$Titulo[random_index]
      random_lyrics_path <- cuecasdf()$RutaArxiuLletra[random_index]
      if (length(random_lyrics_path) > 0 && file.exists(random_lyrics_path)) {
        random_lyrics <- readLines(random_lyrics_path)
        updateSelectInput(session, "Titulo", selected = random_title)
        buscarButtonClicked(TRUE)  # Set the "Buscar" button clicked to TRUE
      }
    }
  })
  
  # Set "Buscar" button clicked to TRUE when the button is clicked
  observeEvent(input$printButton, {
    buscarButtonClicked(TRUE)
  })
  
  # Clear the displayed text when the "Clear" button is clicked
  observeEvent(input$clearButton, {
    buscarButtonClicked(FALSE)
  })
  
  # Update titlesByTone selectInput based on selected tone
  observe({
    req(input$Tono)
    if (!is.null(cuecasdf())) {
      titles_with_tone <- cuecasdf()$Titulo[cuecasdf()$Tono == input$Tono]
      updateSelectInput(session, "titlesByTone", choices = titles_with_tone)
    }
  })
  
  # Handle the selection of titles by tone
  observeEvent(input$titlesByTone, {
    buscarButtonClicked(TRUE)  # Set the "Buscar" button clicked to TRUE
    updateSelectInput(session, "Titulo", selected = input$titlesByTone)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
