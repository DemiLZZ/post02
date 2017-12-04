#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(magick)
library(shiny)
app <- shinyApp(
  ui = fluidPage(
    titlePanel("Magick Shiny Demo"),
    
    sidebarLayout(
      sidebarPanel(
        fileInput(
          "upload",
          "Upload new image",
          accept = c('image/png', 'image/jpeg')
        ),
        textInput("size", "Size", value = "500x500!"),
        sliderInput("rotation", "Rotation", 0, 360, 0),
        sliderInput("blur", "Blur", 0, 20, 0),
        sliderInput("implode", "Implode",-1, 1, 0, step = 0.01),
        
        checkboxGroupInput(
          "effects",
          "Effects",
          choices = list("edge", "charcoal", "negate", "flip", "flop")
        )
      ),
      mainPanel(imageOutput("img"))
    )
  ),
  
  server = function(input, output, session) {
    # Start with placeholder img
    pikachu <-
      image_read('https://raw.githubusercontent.com/DemiLZZ/post02/master/pikachu.png')
    
    # When uploading new image
    observeEvent(input$upload, {
      if (length(input$upload$datapath))
        pikachu <<- image_convert(image_read(input$upload$datapath), "jpeg")
      info <- image_info(pikachu)
      updateCheckboxGroupInput(session, "effects", selected = "")
      updateTextInput(session, "size", value = paste0(info$width, "x", info$height, "!"))
    })
    
    # A plot of fixed size
    output$img <- renderImage({
      # Boolean operators
      if ("edge" %in% input$effects)
        pikachu <- image_edge(pikachu)
      
      if ("charcoal" %in% input$effects)
        pikachu <- image_charcoal(pikachu)
      
      if ("negate" %in% input$effects)
        pikachu <- image_negate(pikachu)
      
      if ("flip" %in% input$effects)
        pikachu <- image_flip(pikachu)
      
      if ("flop" %in% input$effects)
        pikachu <- image_flop(pikachu)
      
      # Numeric operators
      tmpfile <- pikachu %>%
        image_resize(input$size) %>%
        image_implode(input$implode) %>%
        image_blur(input$blur, input$blur) %>%
        image_rotate(input$rotation) %>%
        image_write(tempfile(fileext = 'jpg'), format = 'jpg')
      
      # Return a list
      list(src = tmpfile, contentType = "image/jpeg")
    })
  }
)
