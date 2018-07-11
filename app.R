
# Copyright 2017-2018 John Gagnon
# This program is distributed under the terms of the GNU General Public License

library(tidyverse)

# outputDir <- "responses"
# saveData <- function(inData) {
#   # Create a unique file name
# fileName <- sprintf("%s_%s.txt",
#                     as.integer(Sys.time()),
#                     digest::digest(inData))
#   # Write the file to the local system
#   dput(inData, file = paste0(outputDir, '/', fileName))
# }

plist <- list() # Initiallize a list of plots to arrange
wlist <- c() # Initialize a vector of plot width values
hlist <- c() # Initialize a vector of plot height values
legend <- NULL
gplot <- dget("gplot.R") # Load plotting function

# UI ----------------------------------------------------------------------

ui <- fluidPage(title = 'plotGrouper',
  # shinyjs::useShinyjs(),
  theme = shinythemes::shinytheme("cosmo"),
  navbarPage(
    (tags$img(src="logo_white.png", width="100px", height = "100px")),
    fluid = T,
    position = 'fixed-top',
    tabPanel(
      h4("Plot", style = "margin-top: 40px; margin-bottom: 40px"),
      fluidPage(
        sidebarPanel(
          tags$style(type="text/css", "body {padding-top: 140px;}"),
          fileInput("file",
            "Choose info-file to upload",
            accept = c(
              "text/csv",
              "text/comma-separated-values",
              "text/tab-separated-values",
              "text/plain",
              ".csv",
              ".tsv",
              ".xlsx",
              ".xls"
            )
          ),

          fluidRow(
            column(8, selectInput("sheet",
              "Select sheet",
              multiple = T,
              choices = NULL
            )),
            column(2,
              style = "margin-top: 25px;",
              checkboxInput(
                "header",
                "Header",
                value = T
              )
            )
          ),

          selectInput("columns",
            "Select columns to exclude from gather",
            multiple = T,
            choices = NULL
          ),

          selectInput("variables",
            "Variables to plot",
            multiple = T,
            choices = NULL
          ),

          fluidRow(
            column(6, selectInput("comp",
              "Compare by",
              choices = NULL
            )),
            column(6, selectInput("group",
              "Group by",
              choices = NULL
            ))
          ),

          hr(),

          fluidRow(
            column(8, textInput(
              "filename",
              "Filename",
              "Plot1"
            )),
            column(4,
              style = "margin-top: 30px;",
              downloadButton("downloadPlot",
                "Save",
                class = "btn btn-primary btn-sm",
                style = "color: #fff; 
                                   background-color: #337ab7; 
                                   border-color: #2e6da4"
              )
            )
          ),

          fluidRow(
            column(6, sliderInput("save.height",
              "Save height (mm)",
              min = 20,
              max = 150,
              value = 50
            )),
            column(6, sliderInput("save.width",
              "Save width (mm)",
              min = 20,
              max = 150,
              value = 50
            ))
          ),

          # actionButton('submit', "Save inputs"),
          # actionButton('load', "Load inputs"),

          hr(),

          fluidRow(
            column(8, textInput("y.lab",
              "Replace y axis label",
              value = NULL
            )),
            column(1.5,
              style = "margin-top: 25px;",
              checkboxInput("scientific",
                "10^x",
                value = F
              )
            )
          ),

          fluidRow(
            column(
              6,
              textInput("split.on",
                "Split on text",
                value = NULL
              )
            ),
            column(3,
              style = "margin-top: 25px;",
              checkboxInput(
                "split",
                "Split",
                value = T
              )
            ),
            column(3,
              style = "margin-top: 25px;",
              checkboxInput(
                "angle.x",
                "Angle",
                value = F
              )
            )
          ),

          textInput("trim",
            "Trim text from right side of group labels",
            value = "none"
          ),

          hr(),

          selectInput("trans.y",
            "Transform y",
            choices = c("identity", "log2", "log10"),
            selected = "identity"
          ),

          selectInput("errortype",
            "Select errorbar type",
            choices = c("mean_se", "mean_sdl"),
            selected = "mean_se"
          ),

          fluidRow(
            column(4, selectInput("method",
              "Stat test",
              choices = c(
                "t.test",
                "wilcox.test",
                "anova",
                "kruskal.test"
              )
            )),

            column(4,
              style = "margin-top: 25px;",
              checkboxInput("refGroup",
                "Reference group",
                value = F
              )
            ),
            column(4,
              style = "margin-top: 25px;",
              checkboxInput("paired",
                "Paired",
                value = F
              )
            )
          ),

          fluidRow(
            column(6, sliderInput("width",
              "Width",
              min = 0,
              max = 1,
              step = 0.05,
              value = 0.80
            )),
            column(6, sliderInput("dodge",
              "Dodge",
              min = 0,
              max = 2,
              step = 0.05,
              value = 0.85
            ))
          ),

          hr(),

          selectInput("id",
            "ID column",
            choices = NULL
          ),

          fluidRow(
            column(6, numericInput("bead",
              "# Beads/sample",
              value = NULL
            )),
            column(6, numericInput("dilution",
              "Dilution factor",
              value = NULL
            ))
          )
        ),

        mainPanel(
          fluidRow(
            column(4, selectInput("geom",
              "Select geoms to plot",
              choices = c(
                "bar",
                "crossbar",
                "errorbar",
                "point",
                "dot",
                "stat",
                "seg",
                "box",
                "violin",
                "line",
                "line_point",
                "line_error",
                "line_point_stat",
                "density"
              ),
              selected = c(
                "bar",
                "errorbar",
                "point",
                "stat",
                "seg"
              ),
              multiple = T
            )),
            column(4, selectInput("legend",
              "Select legend position",
              choices = c(
                "top",
                "right",
                "bottom",
                "left",
                "none"
              ),
              selected = "right"
            )),
            # column(3, sliderInput('aspect.ratio',
            #                       "Aspect ratio",
            #                       min = 0.25,
            #                       max = 4,
            #                       value = 1,
            #                       step = 0.25)
            #        ),
            column(4,
              style = "margin-top: 30px;",
              actionButton("plt2rprt",
                label = "Include in report",
                class = "btn btn-primary btn-sm",
                style = "color: #fff; 
                                               background-color: #337ab7; 
                                               border-color: #2e6da4"
              )
            )
          ),

          hr(),

          fluidRow(
            column(12,
              align = "center",
              plotOutput("plot_display",
                width = "600px",
                height = "600px"
              )
            )
          ),

          hr(),

          fluidRow(
            column(4, h4("Shapes"), uiOutput("shapes")),
            column(4, h4("Color"), uiOutput("colors")),
            column(4, h4("Fill"), uiOutput("fills"))
          ),

          fluidRow(
            column(4, checkboxInput("lock.shapes", "Lock", F)),
            column(4, checkboxInput("lock.cols", "Lock", F)),
            column(4, checkboxInput("lock.fills", "Lock", F))
          ),

          selectInput("comps",
            "Order of comparisons",
            multiple = T,
            choices = NULL
          ),

          fluidRow(
            column(3, sliderInput("font",
              "Font size",
              min = 8,
              max = 25,
              value = 9,
              step = 0.5
            )),
            column(3, sliderInput("size",
              "Point size",
              min = 0.5,
              max = 10,
              value = 1,
              step = 0.5
            )),
            column(3, sliderInput("stroke",
              "Stroke size",
              min = 0.25,
              max = 2,
              value = 0.5,
              step = 0.25
            ))
          ),

          textAreaInput("console",
            "Pass code to manipulate data frame",
            value = "dataFrame <<- dataFrame %>%",
            width = 800,
            height = 200
          ),

          actionButton("run",
            "Run",
            style = "color: #fff; 
                                      background-color: #337ab7; 
                                      border-color: #2e6da4"
          )
        )
      )
    ),

    tabPanel(
      h4("Report", style = "margin-top: 40px; margin-bottom: 40px"),
      fluidPage(
        mainPanel(
          fluidRow(
            column(
              4,
              actionButton("clear",
                "Clear last",
                class = "btn btn-primary btn-md",
                style = "color: #fff; 
                                               background-color: #337ab7; 
                                               border-color: #2e6da4"
              )
            ),

            column(4, actionButton("clearAll",
              "Clear All",
              class = "btn btn-primary btn-md",
              style = "color: #fff;
                                        background-color: #337ab7; 
                                        border-color: #2e6da4"
            ))
          ),

          fluidRow(
            column(3, textInput(
              "report",
              "Filename",
              "Report1"
            )),
            column(3,
              style = "margin-top: 25px;",
              downloadButton("downloadReport",
                "Save",
                class = "btn btn-primary btn-md",
                style = "color: #fff; 
                                                 background-color: #337ab7; 
                                                 border-color: #2e6da4"
              )
            ),
            column(4, radioButtons("format",
              "Document format",
              choices = c(
                "PDF",
                "HTML",
                "Word"
              ),
              inline = TRUE
            ))
          ),

          fluidRow(
            column(12,
              align = "center",
              uiOutput("regPlot")
            )
          )
        )
      )
    ),

    tabPanel(
      h4("Statistics", style = "margin-top: 40px; margin-bottom: 40px"),
      fluidPage(
        mainPanel(
          dataTableOutput("stat_display"),

          downloadButton("save.stat",
            "Download",
            style = "color: #fff; 
                                        background-color: #337ab7; 
                                        border-color: #2e6da4"
          )
        )
      )
    ),

    tabPanel(
      h4("Plot Data", style = "margin-top: 40px; margin-bottom: 40px"),
      fluidPage(
        mainPanel(
          dataTableOutput("data_table_display")
        )
      )
    ),

    tabPanel(
      h4("Raw Data", style = "margin-top: 40px; margin-bottom: 40px"),
      fluidPage(
        mainPanel(
          dataTableOutput("raw_data_table_display")
        )
      )
    )
  )
)

# Server ------------------------------------------------------------------


server <- function(input, output, session) {

  # Get file/read sheets ####
  observeEvent(input$file, {
    inFile <<- input$file
    req(inFile)

    # Identify sheets and use them as choices to load file
    sheets <- readxl::excel_sheets(inFile$datapath)
    updateSelectInput(session,
      "sheet",
      "Select Sheet",
      choices = sheets,
      selected = sheets[1]
    )
  })

  # Make tibble from file ####
  observeEvent({
    input$sheet
    input$file
  }, {
    req(input$sheet, inFile)
    # Read excel file in
    for (i in 1:length(input$sheet)) {
      a <- readxl::read_excel(inFile$datapath,
        sheet = input$sheet[i],
        col_names = input$header
      ) %>%
        mutate(Sheet = input$sheet[i]) %>%
        select(Sheet, everything())

      if (i == 1) {
        f <- a
      } else {
        f <- bind_rows(f, a)
      }
      rm(a)
    }

    column_names <- names(f)
    column_names <- str_replace_all(column_names, c(
      ",Freq. of Parent" = " %",
      ",Count" = " #",
      "—" = "-",
      ",," = "",
      ",Median,<.*>," = " MFI "
    ))

    colnames(f) <- column_names

    vars <- names(f)
    columns_select <- c(
      "Experiment",
      "Sheet",
      "Genotype",
      "Sample",
      "Condition",
      "Mouse",
      "Target",
      "Species"
    )
    variables <- vars[which(!vars %in% c(columns_select, "Bead %"))]
    updateSelectInput(session, "columns",
      choices = vars,
      selected = vars[which(vars %in% columns_select)]
    )
    updateSelectInput(session,
      "variables",
      choices = variables,
      selected = variables[1]
    )
    updateSelectInput(session, "comp",
      choices = vars,
      selected = vars[which(vars %in% c("Genotype", "Condition", "Species"))]
    )
    updateSelectInput(session, "id", choices = vars, selected = "Sample")
    updateSelectInput(session, "group",
      choices = c("variable", vars),
      selected = "variable"
    )
    if (length(input$sheet) > 1) {
      updateSelectInput(session, "group",
        choices = c("variable", vars),
        selected = "Sheet"
      )
    }

    rawData <<- f

    f
  })

  palette_cols <- reactiveVal(
    c("#000000", "#000000")
  )

  palette_fills <- reactiveVal(
    c("#444444", "#00000000")
  )

  # Incorporate aspect ratio
  # observe({
  #   input$aspect.ratio
  #   current_height <- input$save.height
  #   updateSliderInput(session,
  #                     'save.height',
  #                     value = current_height*input$aspect.ratio)
  # })

  observe({
    req(input$comp)
    file <- input$file
    sheet <- input$sheet

    vars <- unique(rawData[[input$comp]])

    if (is.null(vars)) {
      vars <- character(0)
    }

    updateSelectInput(session,
      "comps",
      choices = vars,
      selected = vars
    )
  })


  # Filter tibble ####
  observeEvent({
    input$file
    input$sheet
    input$variables
    input$bead
    input$dilution
    input$comp
    input$group
    input$comps
  }, {
    req(
      inFile,
      input$sheet,
      input$columns,
      input$comp,
      input$comps,
      input$variables
    )

    d <- gather(
      rawData,
      variable,
      value,
      -c(input$columns)
    ) %>%
      filter(get(input$comp) %in% c(input$comps))

    if (!is.na(input$bead) &
      !is.na(input$dilution) &
      str_detect(c(input$variables)[1], "#")) {
      d <- d %>%
        group_by_(input$id) %>%
        mutate(value = ifelse(str_detect(variable, "#") &
          !is.na(input$bead),
        value / value[variable == "Bead #"] * input$bead * input$dilution,
        value
        )) %>%
        ungroup() %>%
        filter(variable %in% c(input$variables)) %>%
        filter(!grepl("Bead|Ungated", variable))
    } else {
      d <- d %>%
        filter(variable %in% c(input$variables)) %>%
        filter(!grepl("Bead|Ungated", variable))
    }

    dataFrame <<- d
  })

  # Create plot object ####
  plotInput <- function() {
    variables <- c(input$variables)
    groups <- unique(dataFrame[[input$group]])
    comparisons <- unique(dataFrame[[input$comp]])
    comps <- c(input$comps)

    ref.group <- NULL

    if (input$refGroup == T) {
      ref.group <- comps[1]
    }

    levs.comps <- order(factor(unique(dataFrame[[input$comp]]),
      levels = comps
    ))

    if (input$group == "variable") {
      levs <- order(factor(unique(dataFrame[[input$group]]),
        levels = variables
      ))
    } else {
      levs <- order(factor(groups), levels = groups)
    }

    cols <- c()
    fills <- c()
    shapes <- c()

    lapply(1:length(comparisons), function(i) {
      cols[i] <<- input[[paste0("col", i)]]
      fills[i] <<- input[[paste0("fill", i)]]
      shapes[i] <<- as.numeric(input[[paste0("shape", i)]])
    })

    if (input$trim == "") {
      updateTextInput(session, "trim", value = "none")
    }

    if (input$run) {
      isolate({
        assign("a", input$console)
        a <- gsub("[“”]", "\"", gsub("[‘’]", "'", a))
        eval(parse(text = a))
      })
    }

    gplot(
      dataset = dataFrame,
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
      ref.group = ref.group,
      # aspect.ratio = input$aspect.ratio,
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

  # Create current plot ####
  currentPlot <- reactive({
    egg::set_panel_size(plotInput(),
      width = unit(input$save.width, "mm"),
      height = unit(input$save.height, "mm")
    )
  })

  # Store current plot height ####
  cpHeight <- reactive({
    pheight <- sum(as.numeric(grid::convertUnit(currentPlot()$heights, "mm")))
    leg <- ggpubr::get_legend(plotInput())
    lheight <- sum(as.numeric(grid::convertUnit(leg$height, "mm")))
    if (input$legend %in% c('top', 'bottom')) {
      total.height <- sum(pheight, lheight)
    } else {
      total.height <- pheight
    }
    # return total height in pixels
    # print(paste('lheight:',lheight))
    # print(paste('pheight:',pheight))
    # print(paste('theight:',total.height))
    # print(paste('pxheight:',total.height * 3.7795275591))
    return(total.height * 3.7795275591)
  })

  # Store current plot width ####
  cpWidth <- reactive({
    pwidth <- sum(as.numeric(grid::convertUnit(currentPlot()$widths, "mm")))
    leg <- ggpubr::get_legend(plotInput())
    lwidth <- sum(as.numeric(grid::convertUnit(leg$width, "mm")))
    if (input$legend %in% c('top', 'bottom')) {
      total.width <- sum(pwidth, c(lwidth - pwidth))
    } else {
      total.width <- sum(pwidth, lwidth)
    }
    # return total width in pixels
    # print(lwidth)
    # print(pwidth)
    return(total.width * 3.7795275591)
  })


  # Calculate stats ####
  stats <- function() {
    variables <- c(input$variables)
    groups <- unique(dataFrame[[input$group]])

    if (input$group == "variable") {
      levs <- order(factor(unique(dataFrame[[input$group]]),
        levels = variables
      ))
    } else {
      levs <- order(factor(groups), levels = groups)
    }

    gplot(
      dataset = dataFrame,
      comparison = input$comp,
      group.by = input$group,
      errortype = input$errortype,
      method = input$method,
      paired = input$paired,
      levs = levs,
      stats = T
    )
  }


  # Store user settings (not working) ####
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
  #   updateSelectInput(session, 'sheet', selected = inData$sheet)
  #   updateSelectInput(session, 'columns', selected = inData$columns)
  #   updateSelectInput(session, 'variables', selected = inData$variables)
  #   updateSelectInput(session, 'comp', selected = inData$comp)
  #   updateSelectInput(session, 'group', selected = inData$group)
  #   updateSelectInput(session, 'group', selected = inData$group)
  #
  # })
  #


  
  # Create shape picker ####
  output$shapes <- renderUI({
    req(input$variables, input$comp, input$comps, input$group)
    comparison <- c(input$comp)
    comparisons <- c(input$comps)
    options <- c(19, 21, 17, 24, 15, 22)
    choices <- c(19, 21, 17, 24, 15, 22)
    comps <- input$comps

    if (input$lock.shapes) {
      choices <- c()
      lapply(1:length(comparisons), function(i) {
        choices[i] <<- as.numeric(input[[paste0("shape", i)]])
      })
    }

    selection <- rep(choices[1:length(comparisons)], length(comparisons))

    lapply(1:length(comparisons), function(i) {
      tags$div(
        style = "margin-bottom:25px;",
        selectInput(
          inputId = paste0("shape", i),
          label = comparisons[i],
          choices = options,
          selected = selection[i]
        )
      )
    })
  })

  # Create color picker ####
  output$colors <- renderUI({
    req(input$variables, input$comp, input$comps, input$group)

    comparison <- c(input$comp)
    comparisons <- c(input$comps)
    choices <- palette_cols()
    comps <- input$comps

    if (input$lock.cols) {
      choices <- c()
      lapply(1:length(comparisons), function(i) {
        choices[i] <<- input[[paste0("col", i)]]
      })
    }

    selection <- rep(choices, length(comparisons))

    lapply(1:length(comparisons), function(i) {
      colourpicker::colourInput(
        inputId = paste0("col", i),
        label = comparisons[i],
        value = selection[i]
      )
    })
  })

  # Create fill picker ####
  output$fills <- renderUI({
    req(input$variables, input$comp, input$comps, input$group)

    comparison <- c(input$comp)
    comparisons <- c(input$comps)
    comps <- input$comps

    choices <- palette_fills()

    if (input$lock.fills) {
      choices <- c()
      lapply(1:length(comparisons), function(i) {
        choices[i] <<- input[[paste0("fill", i)]]
      })
    }

    selection <- rep(choices, length(comparisons))

    lapply(1:length(comparisons), function(i) {
      colourpicker::colourInput(
        inputId = paste0("fill", i),
        label = comparisons[i],
        value = selection[i],
        allowTransparent = T
      )
    })
  })

  # Plot the data ####
  output$plot_display <- renderPlot({
    req(inFile, input$geom, input$comps, input$save.height, input$save.width)

    lapply(1:length(unique(dataFrame[[input$comp]])), function(i) {
      req(
        input[[paste0("shape", i)]],
        input[[paste0("col", i)]],
        input[[paste0("fill", i)]]
      )
    })

    gridExtra::grid.arrange(currentPlot())
  })
  #
  #   # Create UI for plot ####
  #   output$plot_display <- renderUI({
  #     plotOutput("plot_contents",
  #       height = cpHeight(),
  #       width = cpWidth()
  #     )
  #   })

  # Save plot ####
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste0(input$filename, ".pdf")
    },
    content = function(file) {
      ggsave(file,
        plot = currentPlot(),
        useDingbats = F,
        height = sum((cpHeight() / 3.7795275591), 15),
        width = cpWidth() / 3.7795275591,
        units = "mm", 
        device = "pdf"
      )
    }
  )

  # Create stats table ####
  output$stat_display <- renderDataTable({
    req(input$variables)

    stats()
  })

  # Save stats table ####
  output$save.stat <- downloadHandler(
    filename = function() {
      paste0(input$filename, "_stats", ".csv")
    },
    content = function(file) {
      write_csv(stats(), file, col_names = T)
    }
  )

  # Create table of plotted data ####
  output$data_table_display <- renderDataTable({
    req(input$variables)
    dataFrame
  })

  # Create table with raw data ####
  output$raw_data_table_display <- renderDataTable({
    req(input$variables)
    rawData
  })

  # Add current plot to report ####
  observeEvent(input$plt2rprt, {
    h <<- NULL
    w <<- NULL
    l <- length(plist)
    prev_numcol <- floor(sqrt(l + 1))
    current_numcol <- floor(sqrt(l + 2))
    if (l + 1 == 1) {
      wlist[l + 1] <<- cpWidth()
      hlist[l + 1] <<- cpHeight()
    } else if (current_numcol > prev_numcol) {
      wlist[length(wlist) + 1] <<- cpWidth()
    } else {
      hlist[length(hlist) + 1] <<- cpHeight()
    }
    
    h <<- sum(hlist)
    w <<- sum(wlist)
    plist[[l + 1]] <<- currentPlot()
  })

  # Clear last report from report ####
  observeEvent(input$clear, {
    l <- length(plist)
    ncols <- length(wlist)
    nrows <- length(hlist)
    
    if (l > 0) {
      plist[[l]] <<- NULL
      new_ncols <- floor(sqrt(l))
      new_nrows <- floor((l)/new_ncols)
      
      if (l == 2) {
        hlist <<- head(hlist, -1)
      }
      
      if (new_ncols < ncols) {
        wlist <<- head(wlist, -1)
      }
      
      if (new_nrows < nrows) {
        hlist <<- head(hlist, -1)
      }

      h <<- sum(hlist)
      w <<- sum(wlist)
    }
  })

  # Clear all plots from report ####
  observeEvent({
    input$clearAll
  }, {
    plist <<- list()
    wlist <<- c()
    hlist <<- c()
    h <<- 800
    w <<- 600
  })

  reportHeight <- reactive({
    pltrprt <- input$plt2rprt
    clearlast <- input$clear
    clearall <- input$clearAll
    return(h)
  })

  reportWidth <- reactive({
    pltrprt <- input$plt2rprt
    clearlast <- input$clear
    clearall <- input$clearAll
    return(w)
  })

  # Create report ####
  output$contents <- renderPlot({
    g <- input$plt2rprt
    r <- input$clear
    ra <- input$clearAll

    if (length(plist) > 0) {
      numcol <- floor(sqrt(length(plist) + 1))
      gridExtra::grid.arrange(
        grobs = plist,
        ncol = numcol
      )
    }
  })

  # Create UI for report ####
  output$regPlot <- renderUI({
    plotOutput("contents",
      height = reportHeight(),
      width = reportWidth()
    )
  })

  # Store plots to be saved in report ####
  plots <- eventReactive(input$plt2rprt, {
    req(h, w)
    g <- input$plt2rprt
    r <- input$clear
    ra <- input$clearAll
    if (length(plist) > 0) {
      numcol <- floor(sqrt(length(plist) + 1))
      gridExtra::arrangeGrob(
        grobs = plist,
        ncol = numcol
      )
    }
  })

  # Download report ####
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste(input$report,
        switch(input$format, PDF = "pdf", HTML = "html", Word = "docx"),
        sep = "."
      )
    },

    content = function(file) {
      ggsave(file,
        plot = plots(),
        useDingbats = F,
        height = (h / 3.7795275591),
        width = (w / 3.7795275591),
        units = "mm",
        device = "pdf"
      )
    }
  )

  # Stop app on close ####
  session$onSessionEnded(stopApp)

}

shiny::shinyApp(ui, server)
