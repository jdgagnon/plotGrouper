
# Copyright 2017-2018 John Gagnon
# This program is distributed under the terms of the GNU General Public License

library(tidyverse)


plist <- list() # Initiallize a list of plots to arrange
wlist <- c() # Initialize a vector of plot width values
hlist <- c() # Initialize a vector of plot height values
gplot <- dget("gplot.R") # Load plotting function

# UI ----------------------------------------------------------------------

ui <- function(request) {
  fluidPage(
    title = "plotGrouper",
    # shinyjs::useShinyjs(),
    theme = shinythemes::shinytheme("cosmo"),
    navbarPage(
      (tags$img(src = "logo_white_small.png", width = "100px", height = "100px")),
      fluid = T,
      position = "fixed-top",
      tabPanel(
        h2("Plot", style = "margin-top: 30px; margin-bottom: 30px"),
        fluidPage(

          ##### Plot sidebarPanel ####
          sidebarPanel(
            tags$style(type = "text/css", "body {padding-top: 140px;}"),
            actionButton("sampleFile", "Iris"),

            #### File input ####
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

            #### Sheet selection ####
            fluidRow(
              column(8, selectInput("sheet",
                "Select sheet",
                multiple = T,
                choices = NULL
              ))
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

            #### Select comparison and grouping columns ####
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

            #### Save plot ####
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

            #### Set plot dimensions ####
            fluidRow(
              column(6, sliderInput("plotHeight",
                "Plot height (mm)",
                min = 20,
                max = 150,
                value = 40
              )),
              column(6, sliderInput("plotWidth",
                "Plot width (mm)",
                min = 20,
                max = 150,
                value = 30
              ))
            ),

            hr(),

            #### Modify Y label ####
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

            #### Modify group lables ####
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

            #### Transform Y axis ####
            selectInput("trans.y",
              "Transform y",
              choices = c("identity", "log2", "log10"),
              selected = "identity"
            ),

            #### Select error type to plot ####
            selectInput("errortype",
              "Select errorbar type",
              choices = c("mean_se", "mean_sdl"),
              selected = "mean_se"
            ),

            #### Select statistical method ####
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

            #### Format width and dodge ####
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
                value = 0.80
              ))
            ),

            hr(),

            #### Options for transforming counts ####
            selectInput("id",
              "ID column",
              choices = NULL
            ),

            checkboxInput("count",
              "# Beads/dilution factor included in data",
              value = F
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

            #### Plot type & legend options ####
            fluidRow(
              column(3, selectInput("geom",
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
              column(3, selectInput("legend",
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

              column(3,
                style = "margin-top: 30px;",
                actionButton("plt2rprt",
                  label = "Include in report",
                  class = "btn btn-primary btn-sm",
                  style = "color: #fff; 
                          background-color: #337ab7; 
                          border-color: #2e6da4"
                )
              ),
              column(3,
                style = "margin-top: 30px;",
                actionButton("loadLast", 
                  label = "Load last stored plot",
                  class = "btn btn-primary btn-sm",
                  style = "color: #fff; 
                          background-color: #337ab7; 
                          border-color: #2e6da4"
                )
              )
            ),

            #### Font, point, stroke ####
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
              )),

              column(3, selectInput("comps",
                "Order of comparisons",
                multiple = T,
                choices = NULL
              ))
            ),

            hr(),

            #### Plot ####
            fluidRow(
              column(12,
                align = "center",
                imageOutput("myPlot",
                  height = "100%",
                  width = "100%"
                )
              )
            ),

            hr(),

            #### Shape, color, fill UI ####
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

            hr()
          )
        )
      ),

      tabPanel(
        h2("Report", style = "margin-top: 30px; margin-bottom: 30px"),
        fluidPage(
          mainPanel(

            #### Clear report objects ####
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

            #### Save report ####
            fluidRow(
              column(5, textInput(
                "report",
                "Filename",
                "Report1"
              )),
              column(5,
                style = "margin-top: 25px;",
                downloadButton("downloadReport",
                  "Save",
                  class = "btn btn-primary btn-md",
                  style = "color: #fff; 
                                                 background-color: #337ab7; 
                                                 border-color: #2e6da4"
                )
              )
            ),

            #### Display report ####
            fluidRow(
              column(12,
                align = "center",
                imageOutput("myReport",
                  height = "100%",
                  width = "100%"
                )
              )
            )
          )
        )
      ),

      #### Statistics ####
      tabPanel(
        h2("Statistics", style = "margin-top: 30px; margin-bottom: 30px;"),
        fluidPage(
          mainPanel(
            dataTableOutput("stat_display"),

            downloadButton("save.stat",
              "Download",
              style = "color: #fff; 
                      background-color: #337ab7; 
                      border-color: #2e6da4;"
            )
          )
        )
      ),

      #### Table of plotting data ####
      tabPanel(
        h2("Plot Data", style = "margin-top: 30px; margin-bottom: 30px"),
        fluidPage(
          mainPanel(
            dataTableOutput("data_table_display")
          )
        )
      ),

      #### Raw import data ####
      tabPanel(
        h2("Raw Data", style = "margin-top: 30px; margin-bottom: 30px"),
        fluidPage(
          mainPanel(
            dataTableOutput("raw_data_table_display")
          )
        )
      )
    ), bookmarkButton()
  )
}

# Server ------------------------------------------------------------------


server <- function(input, output, session) {
  dataFrame <- reactiveVal(NULL)
  rawData <- reactiveVal(NULL)
  inFile <- reactiveVal(NULL)
  
  palette_cols <- reactiveVal(
    c("#000000", "#000000")
  )
  
  palette_fills <- reactiveVal(
    c("#444444", "#00000000")
  )
  
  observeEvent(input$sampleFile, {
    updateSelectInput(session,
      "sheet",
      "Select Sheet",
      choices = "iris",
      selected = "iris"
    )
  })

  #### Get file/read sheets ####
  observeEvent({
    input$file}, {
    print("File input changed")
    inFile(input$file)
    # Identify sheets and use them as choices to load file
    sheets <- readxl::excel_sheets(input$file$datapath)
    updateSelectInput(session,
      "sheet",
      "Select Sheet",
      choices = sheets,
      selected = sheets[1]
    )
  }, priority = 4)
  
  #### Make tibble from file ####
  observeEvent({
    input$sheet}, {
    req(input$sheet)
    print("Sheet changed")
    
    # Use iris data
    if (is.null(inFile())) {
      print("using iris data")
      f <- iris %>%
        mutate(Species = as.character(Species)) %>%
        group_by(Species) %>%
        mutate(
          Sample = paste0(Species, "_", row_number()),
          Sheet = input$sheet
        ) %>%
        select(Sheet, everything())
    }
    
    # Read excel file in
    if (!is.null(inFile())) {
      for (i in 1:length(input$sheet)) {
        a <- readxl::read_excel(input$file$datapath,
          sheet = input$sheet[i],
          col_names = T
        ) %>%
          mutate(Sheet = input$sheet[i]) %>%
          select(Sheet, everything())
        
        column_names <- names(a)
        column_names <- str_replace_all(column_names, c(
          ",Freq. of Parent" = " %",
          ",Count" = " #",
          "—" = "-",
          ",," = "",
          ",Median,<.*>," = " MFI "
        ))
        
        colnames(a) <- column_names
        
        if (i == 1) {
          f <- a
        } else {
          f <- bind_rows(f, a)
        }
        rm(a)
      }
    }
    print("dataframe created")
    
    rawData(f)
    
    vars <- names(f)
    columns_select <- c(
      "Experiment",
      "Sheet",
      "Genotype",
      "Sample",
      "Condition",
      "Mouse",
      "Target",
      "Species",
      "Dilution",
      "Total Bead"
    )
    variables <- vars[which(!vars %in% c(
      columns_select,
      "Bead %",
      "Bead #",
      "Beads %",
      "Beads #"
    ))]
    
    current_columns <- input$columns
    
    if (is.null(input$columns)) {
      current_columns <- "empty"
    }
    
    if (!all(current_columns %in% vars)) {
      print("updating columns")
      updateSelectInput(session, 
                        "columns",
                        choices = vars,
                        selected = vars[which(vars %in% columns_select)]
      )
    }
    
    current_variables <- input$variables
    
    if (is.null(input$variables)) {
      current_variables <- "empty"
    }
    
    if (!all(current_variables %in% variables)) {
      print("variables empty")
      updateSelectInput(session,
                        "variables",
                        choices = variables,
                        selected = variables[1]
      )
    }
    
    if (all(current_variables %in% variables)) {
      print("updating variable choices; keeping selected")
      updateSelectInput(session,
                        "variables",
                        choices = variables,
                        selected = input$variables
      )
    }
    
    current_comp <- input$comp
    
    if (is.null(input$comp)) {
      current_comp <- "empty"
    }
    
    if (!current_comp %in% vars) {
      print("updating comp")
      updateSelectInput(session, 
                        "comp",
                        choices = vars,
                        selected = vars[which(vars %in% c(
                          "Genotype", 
                          "Condition", 
                          "Species"
                          ))]
      )
    }
    
    current_id <- input$id
    
    if (is.null(input$id)) {
      current_id <- "empty"
    }
    
    if (!current_id %in% vars) {
      print("updating id")
      updateSelectInput(session, 
                        "id", 
                        choices = vars, 
                        selected = "Sample"
      )
    }
    
    current_group <- input$group
    
    if (is.null(input$group)) {
      current_group <- "empty"
    }
    
    if (!current_group %in% vars) {
      print("updating group")
      updateSelectInput(session, 
                        "group",
                        choices = c("variable", vars),
                        selected = "variable"
      )
    }
  }, priority = 3)
  
  #### Update comps ####
  observeEvent({
    input$sheet
    input$comp}, {
    req(input$sheet,
        input$comp)
    print("updating comps")
    vars <- unique(rawData()[[input$comp]])
    
    if (is.null(vars)) {
      vars <- character(0)
    }
    
    updateSelectInput(session,
      "comps",
      choices = vars,
      selected = vars
    )
  }, priority = 2)


  #### Filter tibble ####
  observeEvent({
    rawData()
    input$columns
    input$variables
    input$bead
    input$dilution
    input$comp
    input$group
    input$comps
    input$count
  }, {
    req(
      input$sheet,
      input$columns,
      input$variables,
      input$comp,
      input$group,
      length(input$comps) > 1
    )
    print("filtering dataframe")
    print(isolate(input$columns))
    print(isolate(input$variables))
    print(isolate(input$comp))
    print(isolate(input$comps))
    print(isolate(input$group))

    d <- gather(
      rawData(),
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
    } else if (input$count == T) {
      d <- d %>%
        group_by_(input$id) %>%
        mutate(value = ifelse(str_detect(variable, "#"),
          (value / value[variable == "Bead #"] * `Total Bead` * `Dilution`),
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
    
    dataFrame(d)
    print(d)
    print(dataFrame())
  }, priority = 10)

  #### Create plot object ####
  plotInput <- function() {
    variables <- c(input$variables)
    groups <- unique(dataFrame()[[input$group]])
    comparisons <- unique(dataFrame()[[input$comp]])
    comps <- c(input$comps)
    
    if (input$y.lab == "") {
      y.lab <- NULL
    } else {
      y.lab <- input$y.lab
    }
    
    ref.group <- NULL
    
    if (input$refGroup == T) {
      ref.group <- comps[1]
    }
    
    levs.comps <- order(factor(unique(dataFrame()[[input$comp]]),
      levels = comps
    ))

    if (input$group == "variable") {
      levs <- order(factor(unique(dataFrame()[[input$group]]),
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
    
    gplot(
      dataset = dataFrame(),
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
      plotWidth = input$plotWidth,
      plotHeight = input$plotHeight,
      trans.y = input$trans.y,
      split = input$split,
      trim = input$trim,
      angle = input$angle.x,
      y.lab = y.lab,
      leg.pos = input$legend,
      levs = levs,
      levs.comps = levs.comps,
      color.groups = cols,
      fill.groups = fills,
      shape.groups = shapes,
      sci = input$scientific
    )
  }

  #### Create current plot ####
  currentPlot <- reactive({
    req(!is.null(dataFrame()))
    print("Generating current plot")
    lapply(1:length(unique(dataFrame()[[input$comp]])), function(i) {
      req(
        input[[paste0("shape", i)]],
        input[[paste0("col", i)]],
        input[[paste0("fill", i)]]
      )
    })
    plotInput()
  })

  #### Store current plot height ####
  cpHeight <- reactive({
    if (is.null(dataFrame())) {
      return(1)
    }
    req(currentPlot(), leg)
    
    pheight <- sum(as.numeric(grid::convertUnit(currentPlot()$heights, "mm")))
    lheight <- ifelse(input$legend != "none",
      sum(as.numeric(grid::convertUnit(leg$height, "mm"))),
      0
    )
    if (input$legend %in% c("top", "bottom")) {
      total.height <- sum(pheight, lheight)
    } else {
      total.height <- pheight
    }
    # return total height in pixels
    return(total.height * 3.7795275591)
  })

  #### Store current plot width ####
  cpWidth <- reactive({
    if (is.null(dataFrame())) {
      return(1)
    }
    req(currentPlot(), leg)
    
    pwidth <- sum(as.numeric(grid::convertUnit(currentPlot()$widths, "mm")))
    lwidth <- ifelse(input$legend != "none",
      sum(as.numeric(grid::convertUnit(leg$width, "mm"))),
      0
    )
    if (input$legend %in% c("top", "bottom")) {
      total.width <- sum(pwidth, c(lwidth - pwidth))
    } else {
      total.width <- sum(pwidth, lwidth)
    }
    return(total.width * 3.7795275591)
  })


  #### Calculate stats ####
  stats <- function() {
    variables <- c(input$variables)
    groups <- unique(dataFrame()[[input$group]])
    comps <- c(input$comps)
    
    if (input$group == "variable") {
      levs <- order(factor(unique(dataFrame()[[input$group]]),
        levels = variables
      ))
    } else {
      levs <- order(factor(groups), levels = groups)
    }
    
    levs.comps <- order(factor(unique(dataFrame()[[input$comp]]),
      levels = comps
    ))
    
    gplot(
      dataset = dataFrame(),
      comparison = input$comp,
      group.by = input$group,
      errortype = input$errortype,
      method = input$method,
      paired = input$paired,
      levs = levs,
      levs.comps = levs.comps,
      stats = T
    )
  }

  #### Create shape picker ####
  output$shapes <- renderUI({
    req(input$sheet,
        input$variables, 
        input$comp, 
        length(input$comps) > 1,
        input$group)
    print("shape picker")
    
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

  #### Create color picker ####
  output$colors <- renderUI({
    req(input$sheet,
        input$variables, 
        input$comp, 
        length(input$comps) > 1,
        input$group)
    print("color picker")
    
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

  #### Create fill picker ####
  output$fills <- renderUI({
    req(input$sheet, 
        input$variables, 
        input$comp, 
        length(input$comps) > 1,
        input$group)
    print("fill picker")
    
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

  #### Plot the data ####
  output$myPlot <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext = ".png")
    
    # Generate the PNG
    png(outfile,
      width = cpWidth() * 3,
      height = cpHeight() * 3,
      res = 72 * 3
    )
    gridExtra::grid.arrange(currentPlot())
    dev.off()
    
    # Return a list containing the filename
    list(
      src = outfile,
      contentType = "image/png",
      width = cpWidth(),
      height = cpHeight(),
      alt = "This is alternate text"
    )
  }, deleteFile = TRUE)

  #### Save plot ####
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

  #### Create stats table ####
  output$stat_display <- renderDataTable({
    print("creating datatable of statistics")
    req(input$variables)
    stats()
  })

  #### Save stats table ####
  output$save.stat <- downloadHandler(
    filename = function() {
      paste0(input$filename, "_stats", ".csv")
    },
    content = function(file) {
      write_csv(stats(), file, col_names = T)
    }
  )

  #### Create table of plotted data ####
  output$data_table_display <- renderDataTable({
    print("creating datatable of plotted data")
    req(input$variables)
    dataFrame()
  })

  #### Create table with raw data ####
  output$raw_data_table_display <- renderDataTable({
    print("creating datatable of raw data")
    req(input$variables)
    rawData()
  })

  #### Add current plot to report ####
  observeEvent(input$plt2rprt, {
    print("plt2rprt was clicked")
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
    inputs <<- isolate(reactiveValuesToList(input))
  })

  #### Clear last report from report ####
  observeEvent(input$clear, {
    print("clear last plot from report was clicked")
    l <- length(plist)
    ncols <- length(wlist)
    nrows <- length(hlist)
    
    if (l > 0) {
      plist[[l]] <<- NULL
      new_ncols <- floor(sqrt(l))
      new_nrows <- floor((l) / new_ncols)
      
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

  #### Clear all plots from report ####
  observeEvent(input$clearAll, {
    print("clear all plots from report was clicked")
    plist <<- list()
    wlist <<- c()
    hlist <<- c()
    h <<- 10
    w <<- 10
  })

  reportHeight <- reactive({
    print("report height changed")
    pltrprt <- input$plt2rprt
    clearlast <- input$clear
    clearall <- input$clearAll
    return(h)
  })

  reportWidth <- reactive({
    print("report width changed")
    pltrprt <- input$plt2rprt
    clearlast <- input$clear
    clearall <- input$clearAll
    return(w)
  })

  #### Create report ####
  output$myReport <- renderImage({
    print("rendering report image")
    
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext = ".png")
    
    # Generate the PNG
    png(outfile,
      width = reportWidth() * 3,
      height = reportHeight() * 3,
      res = 72 * 3
    )
    if (length(plist) > 0) {
      numcol <- floor(sqrt(length(plist) + 1))
      gridExtra::grid.arrange(
        grobs = plist,
        ncol = numcol
      )
    }
    dev.off()

    # Return a list containing the filename
    list(
      src = outfile,
      contentType = "image/png",
      width = reportWidth(),
      height = reportHeight(),
      alt = "Nothing added to report yet."
    )
  }, deleteFile = TRUE)

  #### Download report ####
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste(input$report, "pdf", sep = ".")
    },
    content = function(file) {
      ggsave(file,
        plot =
          if (length(plist) > 0) {
            numcol <- floor(sqrt(length(plist) + 1))
            gridExtra::arrangeGrob(
              grobs = plist,
              ncol = numcol
            )
          },
        useDingbats = F,
        height = (h / 3.7795275591),
        width = (w / 3.7795275591),
        units = "mm",
        device = "pdf"
      )
    }
  )
  
  observeEvent(input$loadLast, {
    print("Loading last sheet")
    updateSelectInput(session,
                      "sheet",
                      selected = inputs$sheet
    )
  }, priority = -1)
  
  observeEvent(input$loadLast, {
    print("Loading last columns")
    updateSelectInput(session,
                      "columns",
                      selected = inputs$columns
    )
  }, priority = -2)
  
  observeEvent(input$loadLast, {
    print("Loading last variables")
    updateSelectInput(session,
                      "variables",
                      selected = inputs$variables
    )
  }, priority = -3)
  
  observeEvent(input$loadLast, {
    print("Loading last comparisons")
    updateSelectInput(session,
                      "comp",
                      selected = inputs$comp
    )
  }, priority = -4)
  
  observeEvent(input$loadLast, {
    
    print("Loading last id")
    updateSelectInput(session,
                      "id",
                      selected = inputs$id
    )
  }, priority = -5)
  
  observeEvent(input$loadLast, {
    print("Loading last group")
    updateSelectInput(session,
                      "group",
                      selected = inputs$group
    )
  }, priority = -6)


  onRestored(function(state) {
    browser()
    updateSelectInput(session,
      "sheet",
      selected = state$input$sheet
    )
  })
  
  onRestored(function(state) {
    browser()
    updateSelectInput(session,
      "columns",
      selected = state$input$columns
    )
    updateSelectInput(session,
      "variables",
      selected = state$input$variables
    )
    updateSelectInput(session,
      "comp",
      selected = state$input$comp
    )
    updateSelectInput(session,
      "id",
      selected = state$input$id
    )
    updateSelectInput(session,
      "group",
      selected = state$input$group
    )
  })

  #### Stop app on close ####
  session$onSessionEnded(function() {
    rm("leg", envir = globalenv())
    graphics.off()
  })
  session$onSessionEnded(stopApp)
}

shiny::shinyApp(ui, server, enableBookmarking = "server")
