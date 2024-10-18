library(pacman)
pacman::p_load(plyr,
	       shiny,
               tidyverse,
               ggplot2,
               readxl,
               writexl,
               plotly,
               DT,
               tools)

min_agg <- function(x) {
  if (class(x) != "factor") {
    min(x, na.rm = TRUE)
  } else {
    return(NULL)
  }
}

function(input, output, session) {
  data <- reactiveValues(curr = NULL, orig = NULL)

  session$onSessionEnded(function() {
     stopApp()
  })
  #-----------------------------------------------------------------------------
  # Read data from the selected files
  #-----------------------------------------------------------------------------
  observeEvent(
    input$speed_file,
    {
      file <- input$speed_File
      req(input$speed_file)
      data$orig <- read_excel(path = as.character(input$speed_file$datapath)) %>%
        mutate(Date   = as.Date(Date)) %>%
        as.data.frame()

      print(data$orig)
      data$curr <- data$orig
    }
  )
  
  observeEvent(
    input$test_file,
    {
      file <- input$test_file
      req(input$test_file)
      tmp <- read_excel(path = as.character(input$test_file$datapath)) %>%
        mutate(Date   = as.Date(Date),
               Split  = as.numeric(SPLIT_10),
               Sprint = as.numeric(SPRINT_20)) %>%
        select(Date, Athlete, Split, Sprint) %>%
        as.data.frame()
      
      data$curr <- plyr::rbind.fill(data$curr, tmp) %>%
        arrange(Date)
    }
  )
  
  #-----------------------------------------------------------------------------
  # Update Athlete selection widget
  #-----------------------------------------------------------------------------
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
  
  observeEvent(
    input$test_file,
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
  
  #-----------------------------------------------------------------------------
  # Add Data
  #-----------------------------------------------------------------------------
  observeEvent(
    input$add_data,
    {
      req(input$speed_file)

      tmp <- data.frame(
        Date    = as.Date(input$date),
        Athlete = input$athlete,
        Run     = as.numeric(input$run),
        Split   = as.numeric(input$split),
        Sprint  = as.numeric(input$sprint)  
      )
      print(names(data$curr))
      print(names(tmp))
      
      data$curr <- data$curr %>%
        rbind(tmp)
    }
  )
  
  #-----------------------------------------------------------------------------
  # Download Handler for data
  #-----------------------------------------------------------------------------
  output$download <- downloadHandler(
    filename = function() {
      ext  <- paste(".", file_ext(input$speed_file$name), sep = "")
      name <- paste('Speed', gsub('-', '_', Sys.Date()), sep = '_')
      paste(name, '.xlsx', sep = '')
    },
    
    content  = function(file) {
      writexl::write_xlsx(data$curr, path = file)
    }
  )
  
  #-----------------------------------------------------------------------------
  # Create showcase boxes
  #-----------------------------------------------------------------------------

  output$showcase_1 <- renderUI({
    req(input$speed_file)
    
    plot <- data$curr[data$curr$Athlete == input$athlete_select, ] %>%
      group_by(Date) %>%
      mutate(Split = min_agg(Split)) %>%
      ggplot(mapping = aes(x = Date, y = Split)) +
      geom_point() +
      geom_line()
    
    plot <- ggplotly(plot) %>%
      layout( 
        xaxis = list(visible = F, showgrid = F, title = ""),
        yaxis = list(visible = F, showgrid = F, title = ""),
        hovermode = "x",
        margin = list(t = 0, r = 0, l = 0, b = 0),
        font = list(color = "white"),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      ) %>%
      config(displayModeBar = F) %>%
      htmlwidgets::onRender(
        "function(el) {
              el.closest('.bslib-value-box')
                .addEventListener('bslib.card', function(ev) {
                  Plotly.relayout(el, {'xaxis.visible': ev.detail.fullScreen});
                })
            }"
      )
    
    value_box(
      title       = 'Best Split',
      value       = min(data$curr$Split[data$curr$Athlete == input$athlete_select],
                        na.rm = TRUE),
      showcase    = plot,
      full_screen = TRUE,
      theme       = 'success'
    )
  })
  
  output$showcase_2 <- renderUI({
    req(input$speed_file)
    
    plot <- data$curr[data$curr$Athlete == input$athlete_select, ] %>%
      group_by(Date) %>%
      mutate(Sprint = min_agg(Sprint)) %>%
      ggplot(mapping = aes(x = Date, y = Sprint)) +
      geom_point() +
      geom_line()
    
    plot <- ggplotly(plot) %>%
      layout(
        xaxis = list(visible = F, showgrid = F, title = ""),
        yaxis = list(visible = F, showgrid = F, title = ""),
        hovermode = "x",
        margin = list(t = 0, r = 0, l = 0, b = 0),
        font = list(color = "white"),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      ) %>%
      config(displayModeBar = F) %>%
      htmlwidgets::onRender(
        "function(el) {
              el.closest('.bslib-value-box')
                .addEventListener('bslib.card', function(ev) {
                  Plotly.relayout(el, {'xaxis.visible': ev.detail.fullScreen});
                })
            }"
      )
    value_box(
      title       = 'Best Sprint',
      value       = min(data$curr$Sprint[data$curr$Athlete == input$athlete_select],
                        na.rm = TRUE),
      showcase    = plot,
      full_screen = TRUE,
      theme       = 'success'
    )
  })
  
  output$showcase_3 <- renderUI({
    req(input$speed_file)
    
    value_box(
      title       = 'Improvement',
      value       = sprintf('%.2f', 
                            head(data$curr$Sprint[data$curr$Athlete == input$athlete_select], 1) - min(data$curr$Sprint[data$curr$Athlete == input$athlete_select])),
      showcase    = bs_icon('graph-up'),
      full_screen = TRUE,
      theme       = 'success'
    )
  })
  
  #-----------------------------------------------------------------------------
  # Create data table
  #-----------------------------------------------------------------------------
  
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