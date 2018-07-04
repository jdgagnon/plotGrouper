# Copyright 2017-2018 John Gagnon
# This program is distributed under the terms of the GNU General Public License


library(shiny)
library(tidyverse)
library(Hmisc)
library(scales)
library(readxl)
library(gridExtra)
library(egg)
library(ggpubr)
library(shinyjs)
library(shinythemes)
library(colourpicker)

# outputDir <- "responses"
# saveData <- function(inData) {
#   # Create a unique file name
#   fileName <- sprintf("%s_%s.txt", as.integer(Sys.time()), digest::digest(inData))
#   # Write the file to the local system
#   dput(inData, file = paste0(outputDir, '/', fileName))
# }


plist <- list() # Initiallize a list of plots to arrange
# respectList <- c()
# wlist <- c()
# hlist <- c()

gplot <- dget('gplot.R') # Load plotting function


# UI ----------------------------------------------------------------------

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(
      HTML(".shiny-notification {
           position:fixed;
           top: calc(50%);;
           left: calc(50%);;
           }
           "
      )
      )
      ),
  theme = shinythemes::shinytheme("cosmo"),
  titlePanel("Grouped Plot", windowTitle = 'Grouped Plot'),
  sidebarLayout(
    sidebarPanel(
      fileInput('file', 'Choose info-file to upload', multiple = T,
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv',
                  '.xlsx',
                  '.xls'
                )
      ),
      fluidRow(
        column(8, selectInput('sheet', 'Select sheet', multiple = T, choices = NULL)),
        column(2, style = "margin-top: 25px;", checkboxInput('header', 'Header', T))),
      
      selectInput("columns", "Select columns to exclude From gather", multiple = T, choices = NULL), # no choices before uploading 
      
      selectInput("variables", "Variables to plot", multiple = T, choices = NULL),
      
      fluidRow(
        column(6, selectInput('comp', "Compare by", choices = NULL)),
        column(6, selectInput('group', "Group by", choices = NULL))
        ),
      
      hr(),
      
      fluidRow(
        column(8, textInput('filename', "Filename", 'Plot1')),
        column(4, style = "margin-top: 30px;", downloadButton('downloadPlot', "Save", 
                                                              class="btn btn-primary btn-sm", 
                                                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
        ),
      
      fluidRow(
        column(6, sliderInput('save.height', "Save height (mm)", min = 20, max = 150, value = 50)),
        column(6, sliderInput('save.width', "Save width (mm)", min = 20, max = 150, value = 50))
        ),
      
      # actionButton('submit', "Save inputs"),
      # actionButton('load', "Load inputs"),
      
      hr(),
      
      fluidRow(
        column(8, textInput('y.lab', 'Replace y axis label', value = NULL)),
        column(1.5, style = "margin-top: 25px;", checkboxInput('scientific', "10^x", value = F))
      ),
      
      fluidRow(
        column(6, textInput('split.on', "Split on text", value = NULL)),
        column(3, style = "margin-top: 25px;", checkboxInput('split', 'Split', T)),
        column(3, style = "margin-top: 25px;", checkboxInput('angle.x', "Angle", F))
        ),
      textInput('trim', 'Trim text from right side of group labels', value = 'none'),
      
      hr(),
      
      selectInput('trans.y', "Transform y", choices = c('identity', 'log2', 'log10'), selected = 'identity'),
      

      
      selectInput('errortype', "Select errorbar type", choices = c('mean_se', 'mean_sdl'), selected = 'mean_se'),
      
      fluidRow(
        column(8, selectInput('method', "Stat test", choices = c('t.test', 'wilcox.test', 'anova', 'kruskal.test'))),
        column(4, style = "margin-top: 25px;", checkboxInput('paired', "Paired", value = F))
      ),

      fluidRow(
        column(6, sliderInput('width', "Width", min = 0, max = 2, step = 0.05, value = 0.90)),
        column(6, sliderInput('dodge', "Dodge", min = 0, max = 2, step = 0.05, value = 0.90))
        ),
      
      hr(),
      
      selectInput('id', "ID column", choices = NULL),
      
      fluidRow(
        column(6, numericInput('bead', "# Beads/sample", value = NULL)),
        column(6, numericInput('dilution', "Dilution factor", value = NULL))
        ),

      hr(style="margin-top: 500px"),
      
      actionButton('clear', 'Clear last',
                   class="btn btn-primary btn-sm",
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      
      hr(),
      
      radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'), inline = TRUE),
      fluidRow(
        column(8, textInput('report', "Filename", 'Report1')),
        column(4, style = "margin-top: 30px;", downloadButton('downloadReport', "Save", 
                                                              class="btn btn-primary btn-sm", 
                                                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
        )
      
    ),
    mainPanel(
      tabsetPanel(id = 'tab',
                  type = "tabs",
                  tabPanel('Plot',
                           h1('Plot'),
                           fluidRow(
                             column(3, selectInput('geom', "Select geoms to plot", 
                                       choices = c('bar', 'crossbar', 'errorbar', 
                                                   'point', 'dot', 'stat','seg',
                                                   'box', 'violin','line',
                                                   'line_point', 'line_error', 'line_point_stat'),
                                       selected = c('bar','errorbar','point','stat','seg'), multiple = T)),
                             column(3, selectInput('legend', "Select legend position", choices = c('top','right','bottom','left','none'))),
                             column(3, sliderInput('aspect.ratio', "Aspect ratio", min = 0.25, max = 4, value = 1, step = 0.25)),
                             column(3, style = "margin-top: 30px;", actionButton("plt2rprt", label = "Include in report", 
                                                                                 class="btn btn-primary btn-sm", 
                                                                                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                             ),
                           
                           hr(),
                           
                           fluidRow(
                             column(12, align="center", plotOutput('plot_display', width = '600px', height = '600px'))
                             ),
                           
                           hr(),
                           
                           fluidRow(
                             column(4, h4('Shapes') ,uiOutput("shapes")),
                             column(4, h4('Color'), uiOutput("colors")),
                             column(4, h4('Fill'), uiOutput("fills"))
                             ),
                           
                           fluidRow(
                             column(4, checkboxInput('lock.shapes', 'Lock', F)),
                             column(4, checkboxInput('lock.cols', 'Lock', F)),
                             column(4, checkboxInput('lock.fills', 'Lock', F))
                             ),

                          selectInput('comps', "Order of comparisons", multiple = T, choices = NULL),
                           
                           fluidRow(
                             column(3, sliderInput('font', "Font size", min = 8, max = 15, value = 9, step = 0.5)),
                             column(3, sliderInput('size', "Point size", min = 0.5, max = 10, value = 1, step = 0.5)),
                             column(3, sliderInput('stroke', "Stroke size", min = 0.25, max = 5, value = 0.5, step = 0.25))
                             ),
                          textAreaInput("console", "Pass code to manipulate data frame", value = 'dataframe <<- dataframe %>%', width = 800, height = 200),
                          actionButton('run', "Run", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                          
                          hr(),
                          h1('Report'),
                          plotOutput('regPlot', width = 612, height = 792),
                          hr()
                  ),
                  
                  tabPanel('Stats',
                           dataTableOutput('stat_display'),
                           downloadButton("save.stat", "Download", 
                                          style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                  
                  tabPanel('Data',dataTableOutput('data_table_display')),
                  tabPanel('Raw Data', dataTableOutput('raw_data_table_display'))
                  )
      )
    )
)



# Server ------------------------------------------------------------------


server <- function(input, output, session) { # added session for updateSelectInput
 
  # Input file and read in the sheets
  info <- eventReactive(input$file, {
    assign('inFile', input$file, envir = globalenv())
    # Instead # if (is.null(inFile)) ... use "req"
    req(inFile)
    
    # Identify sheets and use them as choices to load file
    sheets <- readxl::excel_sheets(inFile$datapath)
    updateSelectInput(session, 'sheet', 'Select Sheet', choices = sheets, selected = sheets[1])
    sheets
    })
  
  # If a file is read in create a tibble from all the sheets
  fi <- eventReactive({
    input$sheet
    input$file}, {
      
      req(input$sheet, input$file)
      # Read excel file in
      for (i in 1:length(input$sheet)) {
        a <- readxl::read_excel(inFile$datapath, sheet = input$sheet[i], col_names = input$header) %>%
          mutate(Sheet = input$sheet[i]) %>%
          select(Sheet, everything())
        
        if(i == 1) {
          f <- a
          } else f <- bind_rows(f, a)
          rm(a)
      }
      
      column_names <- names(f)
      column_names <- str_replace_all(column_names, c(",Freq. of Parent" = " %",
                                                      ",Count" = " #",
                                                      "—" = "-",
                                                      ",," = "",
                                                      ",Median,<.*>," = " MFI "))
        
      colnames(f) <- column_names
      
      vars <- names(f)
      columns_select <- c('Experiment', 'Sheet', 'Genotype', 'Sample', 'Condition', 'Mouse', 'Target', 'Species')
      variables <- vars[which(!vars %in% c(columns_select, 'Bead %'))]
      updateSelectInput(session, "columns", choices = vars, 
                      selected = vars[which(vars %in% columns_select)])
      updateSelectInput(session, 'variables', choices = variables, selected = variables[1])
      updateSelectInput(session, 'comp', choices = vars, 
                        selected = vars[which(vars %in% c('Genotype', 'Condition', 'Species'))])
      updateSelectInput(session, 'id', choices = vars, selected = 'Sample')
      updateSelectInput(session, 'group', choices = c('variable', vars),
                        selected = 'variable')
      if (length(input$sheet) > 1) {
        updateSelectInput(session, 'group', choices = c('variable', vars),
                          selected = 'Sheet')
      }
      assign('rawData', f, envir = globalenv())
      f
  })
  
  palette_cols <- reactiveVal(
    c('#000000', '#000000')
  )
  
  palette_fills <- reactiveVal(
    c('#00000080','#00000000')
  )
  
  observe({
    req(input$comp)
    file <- input$file
    sheet <- input$sheet

    vars <- unique(rawData[[input$comp]])

    if (is.null(vars)) {
      vars <- character(0)
    }

    updateSelectInput(session, 'comps', choices = vars, selected = vars)
  })
  
  # Based on the selected variables, subset the tibble
  df <- eventReactive({
    input$file
    input$sheet
    input$variables
    input$bead
    input$dilution
    input$comp
    input$group
    input$comps}, {
      
      req(input$file, input$sheet, input$columns, input$comp, input$comps, input$variables)
      
      d <- gather(rawData, variable, value, -c(input$columns)) %>%
        filter(get(input$comp) %in% c(input$comps))
      
      if (!is.na(input$bead) & !is.na(input$dilution) & str_detect(c(input$variables)[1], '#')) {
        d <- d %>%
          group_by_(input$id) %>%
          mutate(value = ifelse(str_detect(variable, '#') & !is.na(input$bead), 
                                value/value[variable == 'Bead #']*input$bead*input$dilution, value)) %>%
          ungroup() %>%
          filter(variable %in% c(input$variables)) %>%
          filter(!grepl('Bead|Ungated',variable))
      } else {
        d <- d %>%
          filter(variable %in% c(input$variables)) %>%
          filter(!grepl('Bead|Ungated',variable))
      }

      assign('dataframe', d, envir = globalenv())
      d
  })
  
  plotInput <- function() {
    
    variables <- c(input$variables)
    groups <- unique(dataframe[[input$group]])
    comparisons <- unique(dataframe[[input$comp]])
    comps <- c(input$comps)
    
    levs.comps <- order(factor(unique(dataframe[[input$comp]]), levels = comps))

    if (input$group == 'variable') {
      levs <- order(factor(unique(dataframe[[input$group]]), levels = variables))
    } else {
      levs <- order(factor(groups), levels = groups)
    }
    
    cols <- c()
    fills <- c()
    shapes <- c()
    
    lapply(1:length(comparisons), function(i) {
      cols[i] <<- input[[paste0('col',i)]]
      fills[i] <<- input[[paste0('fill',i)]]
      shapes[i] <<- as.numeric(input[[paste0('shape',i)]])
      
    })
    
    if (input$trim == '') {
      updateTextInput(session, 'trim', value = 'none')
    }
    
    if (input$run) {
      isolate({
        assign('a', input$console)
        a <- gsub("[“”]", "\"", gsub("[‘’]", "'", a))
        eval(parse(text = a))
      })
    }
    
    gplot(dataset = dataframe, 
          comparison = input$comp,
          group.by = input$group,
          geom = input$geom,
          errortype = input$errortype,
          method = input$method,
          paired = input$paired,
          size = input$size,
          stroke = input$stroke,
          width = input$width,
          dodge = input$dodge,
          font_size = input$font,
          aspect.ratio = input$aspect.ratio,
          trans.y = input$trans.y,
          split = input$split,
          trim = input$trim,
          angle = input$angle.x,
          y.lab = input$y.lab,
          leg.pos = input$legend,
          levs = levs,
          levs.comps = levs.comps,
          color.groups = cols,
          fill.groups = fills,
          shape.groups = shapes,
          sci = input$scientific
    )
    
  }
  
  stats <- function() {
    
    variables <- c(input$variables)
    groups <- unique(dataframe[[input$group]])
    
    if (input$group == 'variable') {
      levs <- order(factor(unique(dataframe[[input$group]]), levels = variables))
    } else {
      levs <- order(factor(groups), levels = groups)
    }
    
    gplot(dataset = dataframe,
          comparison = input$comp,
          group.by = input$group,
          errortype = input$errortype,
          method = input$method,
          paired = input$paired,
          levs = levs,
          stats = T)
  }
  
  # AllInputs <- reactive({
  #   reactiveValuesToList(input)
  # })
  # 
  # observeEvent(input$submit, {
  #   saveData(AllInputs())
  # })
  # 
  # observeEvent(input$load,{
  #   files <- list.files(outputDir, full.names = TRUE)
  #   inData <- dget(file = files)
  # for (i in 1:length(inData)) {
  #   session$sendInputMessage(names(inData)[i], list(inData[[names(inData)[i]]]))
  # }
  # })
  
  # Create a shape picker ui for each level of the comparison
  output$shapes <- renderUI({
    req(input$variables, input$comp, input$comps, input$group, df())
    
    sheets <- info()
    c <- fi()
    d <- df()
    comparison <- c(input$comp)
    comparisons <- c(input$comps)
    options <- c(19,21,17,24,15,22)
    choices <- c(19,21,17,24,15,22)
    comps <- input$comps
    
    if (input$lock.shapes) {
      choices <- c()
      lapply(1:length(comparisons), function(i) {
        choices[i] <<- as.numeric(input[[paste0('shape',i)]])
      })
    }
    
    selection <- rep(choices[1:length(comparisons)], length(comparisons))
    
    lapply(1:length(comparisons), function(i) {
      tags$div(style="margin-bottom:25px;", selectInput(inputId = paste0('shape',i), label = comparisons[i], choices = options, selected = selection[i]))
    })
  })
  
  # Create a color picker ui for each level of the comparison
  output$colors <- renderUI({
    req(input$variables, input$comp, input$comps, input$group, df())
    
    sheets <- info()
    c <- fi()
    d <- df()
    comparison <- c(input$comp)
    comparisons <- c(input$comps)
    choices <- palette_cols()
    comps <- input$comps
    
    if (input$lock.cols) {
      choices <- c()
      lapply(1:length(comparisons), function(i) {
        choices[i] <<- input[[paste0('col',i)]]
      })
    }
    
    selection <- rep(choices, length(comparisons))
    
    lapply(1:length(comparisons), function(i) {
      colourpicker::colourInput(inputId = paste0('col',i), label = comparisons[i], value = selection[i])
    })
  })
  
  # Create a color picker ui for each level of the comparison
  output$fills <- renderUI({
    req(input$variables, input$comp, input$comps, input$group, df())
    
    sheets <- info()
    c <- fi()
    d <- df()
    comparison <- c(input$comp)
    comparisons <- c(input$comps)
    comps <- input$comps
    
    choices <- palette_fills()
    
    if (input$lock.fills) {
      choices <- c()
      lapply(1:length(comparisons), function(i) {
        choices[i] <<- input[[paste0('fill',i)]]
      })
    }
    
    selection <- rep(choices, length(comparisons))
    
    lapply(1:length(comparisons), function(i) {
      colourpicker::colourInput(inputId = paste0('fill',i), label = comparisons[i], value = selection[i], allowTransparent = T)
    })
  })
  
  # Plot the data
  output$plot_display <- renderPlot({

    sheets <- info()
    fileIn <- fi()
    d <- df()

    req(input$file, input$geom, input$comps, input$save.height, input$save.width)
    
    lapply(1:length(unique(dataframe[[input$comp]])), function(i) {
      req(input[[paste0('shape',i)]], input[[paste0('col',i)]], input[[paste0('fill',i)]])
    })
    
    plotInput()
    
  }, height = function() input$save.height*3.7795275591, width = function() input$save.width*3.7795275591)
  
# Save plot
  output$downloadPlot <- downloadHandler(
    filename = function() { paste0(input$filename, '.pdf') },
    content = function(file) {
      ggsave(file, plot = plotInput(), useDingbats = F, 
             height = input$save.height, width = input$save.width, 
             units = 'mm', device = "pdf")
    }
  )
  
# Return a statistics table
  output$stat_display <- renderDataTable({
    sheets <- info()
    c <- fi()
    d <- df()
    
    req(input$variables)
    
    stats()

  })
  
# Save statistics table
  output$save.stat <- downloadHandler(
    filename = function() {
      paste0(input$filename, "_stats", ".csv")
    },
    content = function(file) {
      write_csv(stats(), file, col_names = T)
    }
  )
  
  # Return a table with all plotted data
  output$data_table_display <- renderDataTable({
    req(input$variables)
    sheets <- info()
    c <- fi()
    d <- df()
    dataframe
  })
  
  # Return a table with all data
  output$raw_data_table_display <- renderDataTable({
    req(input$variables)
    sheets <- info()
    c <- fi()
    d <- df()
    rawData
  })
  
  
  # eventReactive to add current plot to the report
  observeEvent(input$plt2rprt, {
    l <- length(plist)
    p <- plotInput()
    eggp <- egg::set_panel_size(p, 
                                width = unit(input$save.width, 'mm'),
                                height = unit(input$save.height, 'mm'))
    # respectList[l + 1] <<- as.numeric(input$aspect.ratio)
    # wlist[[l + 1]] <<- as.numeric(input$save.width)
    # hlist[[l + 1]] <<- as.numeric(input$save.height)
    plist[[l + 1]] <<- eggp
  })
  
  observeEvent({
    input$clear}, {
      l <- length(plist)
      if (l > 0) {
        plist[[length(plist)]] <<- NULL
      }
  })
  
  # Create a report
  output$regPlot <- renderPlot({
    g <- input$plt2rprt
    r <- input$clear
    if (length(plist) > 0) {
      numcol <- floor(sqrt(length(plist)+1))
      p <- do.call("grid.arrange", c(plist,
                                     ncol = numcol,
                                     top = str_remove(inFile$name, '.xlsx')))
    }
  })
  
  # eventReactive to create the plots to be saved
  plots <- eventReactive(input$plt2rprt, {
      g <- input$plt2rprt
      r <- input$clear
      if (length(plist) > 0) {
        numcol <- floor(sqrt(length(plist)+1))
        do.call("arrangeGrob", c(grobs = plist,
                                 ncol = numcol,
                                 top = str_remove(inFile$name, '.xlsx')))
      }
  })
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste(input$report, sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      ggsave(file, plot = plots(), useDingbats = F, 
             height = 11, width = 8.5, 
             units = 'in', device = "pdf")
    }
  )

  session$onSessionEnded(function(x, env = globalenv()) 
  {
    x <- c('inFile', 'rawData', 'dataframe')
    for (i in x) {
      if(exists(i, envir = env)) {
        rm(list = i, envir = env)
      }
    }
  })
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  
}

shinyApp(ui, server)
