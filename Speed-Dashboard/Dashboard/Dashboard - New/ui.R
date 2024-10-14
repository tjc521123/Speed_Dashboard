library(pacman)
pacman::p_load(shiny,
               bslib,
               bsicons,
               tidyverse,
               ggplot2,
               readxl,
               writexl,
               plotly,
               DT,
               tools,
               rmarkdown)

page_sidebar(
  title = 'Speed Dashboard',
  window_title = 'Speed Dashboard',
  
  sidebar = card(
    fileInput(
      inputId = 'speed_file',
      label   = 'Select Speed File',
      accept  = c('.xlsx'),
      width   = '300px'
    ),
    
    card(
      selectInput(
        inputId = 'athlete_select',
        label   = 'Select Athlete',
        choices = ''
      )
    ),
    
    card(
      dateInput(
        inputId = 'date',
        label   = 'Enter Date',
        value   = Sys.Date(),
        max     = Sys.Date()
      ),
      textInput(
        inputId = 'athlete_new',
        label   = 'Athlete Name'
      ),
      numericInput(
        inputId = 'run',
        label   = 'Run',
        value   = 1,
        min     = 1
      ),
      numericInput(
        inputId = 'split',
        label   = '10-Yd Split',
        value   = 0,
        min     = 0
      ),
      numericInput(
        inputId = 'sprint',
        label   = '20-Yd Sprint',
        value   = 0,
        min     = 0
      )
    )
  ),
  
  layout_columns(
    row_heights = '75px',
    uiOutput('showcase_1'),
    uiOutput('showcase_2'),
    uiOutput('showcase_3')
  ),
  
  card(
    dataTableOutput(
      outputId = 'table'
    ),
    full_screen = TRUE
  )
)