library(pacman)
pacman::p_load(shiny,
               tidyverse,
               ggplot2,
               readxl,
               writexl,
               plotly,
               DT,
               tools,
               rmarkdown)

min_agg <- function(x) {
  if (class(x) != "factor") {
    min(x, na.rm = TRUE)
  } else {
    return(NULL)
  }
}

function(input, output, session) {
  data <- reactiveValues(curr = NULL, orig = NULL)
  
  observeEvent(
    input$speed_file,
    {
      file <- input$speed_File
      req(input$speed_file)
      data$orig <- read_excel(path = as.character(input$speed_file$datapath)) %>%
        mutate(Date = as.Date(Date)) %>%
        as.data.frame()

      data$curr <- data$orig
    }
  )
  
  observeEvent(
    input$speed_file,
    {
      req(input$speed_file)
      
      tmp <- data$curr
      choices <- sort(unique(data$curr$Athlete))
      
      updateSelectInput(
        inputId = 'athlete_select',
        choices = choices
      )
    }
  )

  output$showcase_1 <- renderUI({
    value_box(
      title       = 'Best Split',
      value       = min(data$curr$Split[data$curr$Athlete == input$athlete_select]),
      showcase    = bs_icon('graph-up'),
      full_screen = TRUE,
      theme       = 'success'
    )
  })
  
  output$showcase_2 <- renderUI({
    value_box(
      title       = 'Best Sprint',
      value       = min(data$curr$Sprint[data$curr$Athlete == input$athlete_select]),
      showcase    = bs_icon('graph-up'),
      full_screen = TRUE,
      theme       = 'success'
    )
  })
  
  output$showcase_3 <- renderUI({
    value_box(
      title       = 'Improvement',
      value       = sprintf('%.2f', 
                            max(data$curr$Sprint[data$curr$Athlete == input$athlete_select]) - min(data$curr$Sprint[data$curr$Athlete == input$athlete_select])),
      showcase    = bs_icon('graph-up'),
      full_screen = TRUE,
      theme       = 'success'
    )
  })
  
  output$table <- renderDataTable({
    req(input$speed_file)
    
    data$curr %>%
      # select(Date, Athlete, Split, Sprint) %>%
      datatable(
        rownames = FALSE,
        width      = "90%",
        filter     = "top",
        editable   = "cell",
        extensions = c('RowGroup', 'Scroller'),
        options    = list(
          columnDefs = list(list(className = 'dt-center', targets = 0:(ncol(data$curr) - 1))),
          pageLength  = 20,
          lengthMenu  = c(5, 10, 20, 50),
          scrollX     = TRUE,
          dom         = 'Bfrtip',
          rowGroup    = list(dataSrc = which(colnames(data$curr) == "Date") - 1),
          deferRender = TRUE
        )
      )
  })
}