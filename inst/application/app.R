
# Copyright 2017-2018 John Gagnon
# This program is distributed under the terms of the GNU General Public License

# UI ----------------------------------------------------------------------

ui <- function(request) {
  fluidPage(
    title = "plotGrouper",
    theme = shinythemes::shinytheme("cosmo"),
    navbarPage(
      (shiny::tags$img(src = "logo_white_small.png", width = "100px", height = "100px")),
      fluid = TRUE,
      position = "fixed-top",
      tabPanel(
        h2("Plot", style = "margin-top: 30px; margin-bottom: 30px"),
        fluidPage(
          ##### Plot sidebarPanel ####
          sidebarPanel(
            shiny::tags$style(type = "text/css", "body {padding-top: 140px;}"),
            bookmarkButton(),
            actionButton("sampleFile", "Iris"),
            hr(),
            #### File input ####
            fileInput("file",
              "Data file",
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
            h4("Select appropriate columns"),
            fluidRow(
              column(8, selectInput("sheet",
                "Sheet(s)",
                multiple = TRUE,
                choices = NULL
              ))
            ),

            selectInput("columns",
              "Organizational variables",
              multiple = TRUE,
              choices = NULL
            ),

            selectInput("variables",
              "Variables to plot",
              multiple = TRUE,
              choices = NULL
            ),

            #### Select comparison and grouping columns ####
            fluidRow(
              column(6, selectInput("comp",
                "Comparison",
                choices = NULL
              )),
              column(6, selectInput("group",
                "Independent variable",
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
                  value = FALSE
                )
              )
            ),
            hr(),
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
                  value = TRUE
                )
              ),
              column(3,
                style = "margin-top: 25px;",
                checkboxInput(
                  "angle.x",
                  "Angle",
                  value = FALSE
                )
              )
            ),
            textInput("trim",
              "Trim text from right side of group labels",
              value = "none"
            ),
            hr(),
            #### Transform Y axis ####
            fluidRow(
              column(
                4,
                selectInput("trans.y",
                  "Transform y",
                  choices = c("identity", "log2", "log10"),
                  selected = "identity"
                )
              ),
              column(
                4,
                numericInput("y.min",
                  "Y min",
                  value = NULL
                )
              ),
              column(
                4,
                numericInput("y.max",
                  "Y max",
                  value = NULL
                )
              )
            ),
            hr(),
            #### Select error type to plot ####
            selectInput("errortype",
              "Select errorbar type",
              choices = c("SE", "SD"),
              selected = "SD"
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
                  value = FALSE
                )
              ),
              column(4,
                style = "margin-top: 25px;",
                checkboxInput("paired",
                  "Paired",
                  value = FALSE
                )
              )
            ),
            hr(),
            #### Format width and dodge ####
            fluidRow(
              column(6, sliderInput("width",
                "Group width",
                min = 0,
                max = 1,
                step = 0.05,
                value = 0.80
              )),
              column(6, sliderInput("dodge",
                "Comparison Dodge",
                min = 0,
                max = 2,
                step = 0.05,
                value = 0.80
              ))
            ),
            hr(),
            #### Options for transforming counts ####
            h4("Select columns to transform count data"),
            fluidRow(
              column(4,
                selectInput("id",
                  "Unique ID",
                   choices = NULL
                )),
              column(4,
                selectInput("beadColumn",
                  "# Beads/Sample",
                  choices = NULL
                )),
              column(4,
                selectInput("dilutionColumn",
                  "Dilution (1/x)",
                  choices = NULL
                ))
            )
          ),

          mainPanel(
            #### Plot type & legend options ####
            fluidRow(
              column(3, sliderInput("plotHeight",
                "Plot height (mm)",
                step = 2.5,
                min = 10,
                max = 150,
                value = 30
              )),
              column(3, sliderInput("plotWidth",
                "Plot width (mm)",
                step = 2.5,
                min = 10,
                max = 150,
                value = 20
              )),
              column(2, selectInput("geom",
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
                multiple = TRUE
              )),
              column(2, selectInput("legend",
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

              column(2,
                style = "margin-top: 30px;",
                actionButton("refreshData",
                  label = "Refresh current plot",
                  class = "btn btn-primary btn-sm",
                  style = "color: #fff;
                  background-color: #337ab7;
                  border-color: #2e6da4"
                )
              )
            ),

            fluidRow(
              column(2,
                style = "margin-top: 30px;",
                actionButton("plt2rprt",
                  label = "Add plot to report",
                  class = "btn btn-primary btn-sm",
                  style = "color: #fff;
                          background-color: #337ab7;
                          border-color: #2e6da4"
                )
              ),
              column(
                2,
                selectInput("loadPlot",
                  "Report plot #",
                  choices = NULL
                )
              ),
              column(2,
                style = "margin-top: 30px;",
                actionButton("load",
                  label = "Load plot from report",
                  class = "btn btn-primary btn-sm",
                  style = "color: #fff;
                                  background-color: #337ab7;
                                  border-color: #2e6da4"
                )
              ),
              column(2,
                style = "margin-top: 30px;",
                actionButton("update",
                  label = "Update plot in report",
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
                value = 8,
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
                max = 1,
                value = 0.25,
                step = 0.25
              )),

              column(3, selectInput("comps",
                "Order of comparisons",
                multiple = TRUE,
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
              column(4, checkboxInput("lock.shapes", "Lock", FALSE)),
              column(4, checkboxInput("lock.cols", "Lock", FALSE)),
              column(4, checkboxInput("lock.fills", "Lock", FALSE))
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
                "Clear all",
                class = "btn btn-primary btn-md",
                style = "color: #fff;
                                        background-color: #337ab7;
                                        border-color: #2e6da4"
              )),
              column(4, actionButton("refresh",
                "Refresh plots",
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
    )
  )
}

# Server ------------------------------------------------------------------


server <- function(input, output, session) {
  options(shiny.maxRequestSize = 50 * 1024^2)
  setBookmarkExclude(c(
    "file",
    "loadPlot",
    "plt2rprt",
    "sampleFile",
    "clear",
    "clearAll",
    "load",
    "update"
  ))
  dataFrame <- reactiveVal(NULL)
  rawData <- reactiveVal(NULL)
  inFile <- reactiveVal(NULL)
  reportHeight <- reactiveVal(10)
  reportWidth <- reactiveVal(10)
  inputs <- reactiveValues()
  plotList <- reactiveValues()
  plotListLength <- reactiveVal(0)
  sheets <- reactiveVal(NULL)
  wlist <- reactiveValues()
  hlist <- reactiveValues()

  palette_cols <- reactiveVal(
    c("#000000", "#000000")
  )

  palette_fills <- reactiveVal(
    c("#444444", "#00000000")
  )

  observeEvent(input$sampleFile, {
    print("updating sheet with iris")
    inFile(NULL)
    updateSelectInput(
      session = session,
      inputId = "sheet",
      label = "Select Sheet",
      choices = "iris",
      selected = "iris"
    )
  }, priority = 100)

  #### Get file/read sheets ####
  observeEvent({
    input$file
  }, {
    print("File input changed")
    inFile(input$file)
    # Identify sheets and use them as choices to load file
    sheets(readxl::excel_sheets(input$file$datapath))
    updateSelectInput(
      session = session,
      inputId = "sheet",
      label = "Select Sheet",
      choices = sheets(),
      selected = sheets()[1]
    )
  }, priority = 1)

  #### Make tibble from file ####
  observeEvent({
    input$sheet
  }, {
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
      f <- readData(
        sheet = input$sheet,
        file = input$file$datapath
      )
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
    } else if (all(current_columns %in% vars)) {
      print("updating column choices; keeping selected")
      updateSelectInput(session,
        "columns",
        choices = vars,
        selected = input$columns
      )
    }

    current_variables <- input$variables
    if (is.null(input$variables)) {
      current_variables <- "empty"
    }
    if (!all(current_variables %in% variables)) {
      print("updating variables")
      updateSelectInput(session,
        "variables",
        choices = variables,
        selected = variables[1]
      )
    } else if (all(current_variables %in% variables)) {
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
    } else if (all(current_comp %in% vars)) {
      print("updating comp choices; keeping selected")
      updateSelectInput(session,
                        "comp",
                        choices = vars,
                        selected = input$comp
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
    } else if (current_group %in% vars) {
      print("updating group choices; keeping selected")
      updateSelectInput(session,
        "group",
        choices = c("variable", vars),
        selected = input$group
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
    } else if (current_id %in% vars) {
      print("updating id choices; keeping selected")
      updateSelectInput(session,
                        "id",
                        choices = vars,
                        selected = input$id
      )
    }

    updateSelectInput(session,
        "beadColumn",
        choices = c("none", vars),
        selected = "Total Bead"
      )

    updateSelectInput(session,
                        "dilutionColumn",
                        choices = c("none", vars),
                        selected = "Dilution")

  }, priority = 3)


  #### Update comps ####
  observeEvent({
    input$sheet
    input$comp
  }, {
    req(
      input$sheet,
      input$comp
    )
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
    # rawData()
    input$refreshData
    input$columns
    input$variables
    input$comp
    input$group
    input$comps
    input$id
    input$beadColumn
    input$dilutionColumn
  }, {
    req(
      input$sheet,
      input$columns,
      input$variables,
      input$comp,
      input$group,
      length(input$comps) > 1,
      input$id
    )
    print("organizing dataframe")

    d <- organizeData(
      data = rawData(),
      exclude = input$columns,
      comp = input$comp,
      comps = input$comps,
      variables = input$variables,
      id = input$id,
      beadColumn = input$beadColumn,
      dilutionColumn = input$dilutionColumn)
    dataFrame(d)
  }, priority = 1)

  #### Create plot object ####
  plotInput <- function() {
    print("running plotting function")
    variables <- c(input$variables)
    groups <- unique(dataFrame()[[input$group]])
    comparisons <- unique(dataFrame()[[input$comp]])
    comps <- c(input$comps)

    errortype <- ifelse(input$errortype == "SE", "mean_se", "mean_sdl")

    y.min <- ifelse(is.null(input$y.min), NA, input$y.min)
    y.max <- ifelse(is.null(input$y.max), NA, input$y.max)

    y.lim <- c(y.min, y.max)

    if (input$split.on == "") {
      split_str <- NULL
    } else {
      split_str <- input$split.on
    }

    if (input$y.lab == "") {
      y.lab <- NULL
    } else {
      y.lab <- input$y.lab
    }

    ref.group <- NULL

    if (input$refGroup == TRUE) {
      ref.group <- comps[1]
    }

    levs.comps <- order(factor(unique(dataFrame()[[input$comp]]),
      levels = comps
    ))

    levs <- order(factor(unique(dataFrame()[[input$group]]),
      levels = variables
    ))

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
      errortype = errortype,
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
      y.lim = y.lim,
      split = input$split,
      split_str = split_str,
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
    print("currentPlot triggered")
    req(!is.null(dataFrame()))
    lapply(1:length(unique(dataFrame()[[input$comp]])), function(i) {
      req(
        input[[paste0("shape", i)]],
        input[[paste0("col", i)]],
        input[[paste0("fill", i)]]
      )
    })
    print("Generating current plot")
    plotInput()
  })

  #### Store current plot height ####
  cpHeight <- reactive({
    print("calculating plot height")
    if (is.null(dataFrame())) {
      return(1)
    }
    req(currentPlot())
    pheight <- sum(as.numeric(grid::convertUnit(currentPlot()$heights, "mm")))
    # return total height in pixels
    return(pheight * 3.7795275591)
  })

  #### Store current plot width ####
  cpWidth <- reactive({
    print("calculating plot width")
    if (is.null(dataFrame())) {
      return(1)
    }
    req(currentPlot())
    pwidth <- sum(as.numeric(grid::convertUnit(currentPlot()$widths, "mm")))
    return(pwidth * 3.7795275591)
  })


  #### Calculate stats ####
  stats <- function() {
    print("calculating statistics")
    variables <- c(input$variables)
    groups <- unique(dataFrame()[[input$group]])
    comps <- c(input$comps)

    errortype <- ifelse(input$errortype == "SE", "mean_se", "mean_sdl")

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
      errortype = errortype,
      method = input$method,
      paired = input$paired,
      levs = levs,
      levs.comps = levs.comps,
      stats = TRUE
    )
  }

  #### Create shape picker ####
  output$shapes <- renderUI({
    req(
      input$sheet,
      input$comp,
      length(input$comps) > 1,
      input$group
    )
    print("shape picker")

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
      shiny::tags$div(
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
    req(
      input$sheet,
      input$comp,
      length(input$comps) > 1,
      input$group
    )
    print("color picker")

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
    req(
      input$sheet,
      input$comp,
      length(input$comps) > 1,
      input$group
    )
    print("fill picker")

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
        allowTransparent = TRUE
      )
    })
  })

  #### Plot the data ####
  # output$myPlot <- renderPlot({
  #   gridExtra::grid.arrange(currentPlot())
  # }, height = function() cpHeight(), width = function() cpWidth())

  output$myPlot <- renderImage({
    print("rendering plot")
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
    file = function() {
      paste0(input$filename, ".pdf")
    },
    content = function(file) {
      ggsave(file,
        plot = isolate(currentPlot()),
        useDingbats = FALSE,
        height = isolate(cpHeight()) / 3.7795275591,
        width = isolate(cpWidth()) / 3.7795275591,
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
    file = function() {
      paste0(input$file, "_stats", ".csv")
    },
    content = function(file) {
      write_csv(stats(), file, col_names = TRUE)
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
    req(input$sheet)
    print("plt2rprt was clicked")
    comparisons <- unique(dataFrame()[[input$comp]])
    previous_plotListLength <- plotListLength()
    plotListLength(previous_plotListLength + 1)
    current_plotListLength <- previous_plotListLength + 1
    prev_numcol <- ifelse(previous_plotListLength == 0, 1, floor(sqrt(previous_plotListLength)))
    current_numcol <- floor(sqrt(current_plotListLength))
    prev_numrow <- ifelse(previous_plotListLength == 0, 1, ceiling(previous_plotListLength / prev_numcol))
    current_numrow <- ceiling(current_plotListLength / current_numcol)
    wlist[[as.character(current_plotListLength)]] <- cpWidth()
    hlist[[as.character(current_plotListLength)]] <- cpHeight()
    widths <- unlist(reactiveValuesToList(wlist), use.names = FALSE)
    heights <- unlist(reactiveValuesToList(hlist), use.names = FALSE)
    length(widths) <- suppressWarnings(prod(dim(matrix(widths, ncol = current_numcol))))
    length(heights) <- suppressWarnings(prod(dim(matrix(heights, ncol = current_numcol))))
    widths[is.na(widths)] <- 0
    heights[is.na(heights)] <- 0
    wdims <- as.tibble(matrix(widths, ncol = current_numcol, byrow = TRUE)) %>%
      mutate(rowSums = rowSums(., na.rm = TRUE))
    hdims <- as.tibble(matrix(heights, ncol = current_numcol, byrow = TRUE))
    reportWidth(ceiling(max(wdims$rowSums, na.rm = TRUE)))
    reportHeight(ceiling(max(colSums(hdims, na.rm = TRUE), na.rm = TRUE)))
    plotList[[as.character(current_plotListLength)]] <- isolate(currentPlot())
    inputs[[as.character(current_plotListLength)]] <- isolate(reactiveValuesToList(input))
    reportPlots <- as.character(1:current_plotListLength)
    cols <- c()
    fills <- c()
    shapes <- c()
    lapply(1:length(comparisons), function(i) {
      cols[i] <<- input[[paste0("col", i)]]
      fills[i] <<- input[[paste0("fill", i)]]
      shapes[i] <<- as.numeric(input[[paste0("shape", i)]])
    })
    inputs[[as.character(current_plotListLength)]]$cols <- cols
    inputs[[as.character(current_plotListLength)]]$fills <- fills
    inputs[[as.character(current_plotListLength)]]$shapes <- shapes
    updateSelectInput(session,
      "loadPlot",
      choices = reportPlots,
      selected = reportPlots[current_plotListLength]
    )
  })

  #### Clear last plot from report ####
  observeEvent(input$clear, {
    print("clear last plot from report was clicked")

    previous_plotListLength <- plotListLength()
    if (previous_plotListLength > 1) {
      current_plotListLength <- previous_plotListLength - 1
      plotListLength(current_plotListLength)
      prev_numcol <- floor(sqrt(previous_plotListLength))
      current_numcol <- floor(sqrt(current_plotListLength))
      prev_numrow <- ceiling(previous_plotListLength / prev_numcol)
      current_numrow <- ceiling(current_plotListLength / current_numcol)
      wlist[[as.character(previous_plotListLength)]] <- NULL
      hlist[[as.character(previous_plotListLength)]] <- NULL
      plotList[[as.character(previous_plotListLength)]] <- grid::nullGrob(vp = NULL)
      inputs[[as.character(previous_plotListLength)]] <- NULL
      widths <- unlist(reactiveValuesToList(wlist), use.names = FALSE)
      heights <- unlist(reactiveValuesToList(hlist), use.names = FALSE)
      length(widths) <- suppressWarnings(prod(dim(matrix(widths, ncol = current_numcol))))
      length(heights) <- suppressWarnings(prod(dim(matrix(heights, ncol = current_numcol))))
      widths[is.na(widths)] <- 0
      heights[is.na(heights)] <- 0
      wdims <- as.tibble(matrix(widths, ncol = current_numcol, byrow = TRUE)) %>%
        mutate(rowSums = rowSums(., na.rm = TRUE))
      hdims <- as.tibble(matrix(heights, ncol = current_numcol, byrow = TRUE))
      reportWidth(ceiling(max(wdims$rowSums, na.rm = TRUE)))
      reportHeight(ceiling(max(colSums(hdims, na.rm = TRUE), na.rm = TRUE)))
      reportPlots <- as.character(1:current_plotListLength)
      updateSelectInput(session,
        "loadPlot",
        choices = reportPlots,
        selected = reportPlots[current_plotListLength]
      )
    }
    if (previous_plotListLength == 1) {
      plotList[[as.character(1)]] <- grid::nullGrob(vp = NULL)
      inputs[[as.character(1)]] <- NULL
      wlist[[as.character(1)]] <- NULL
      hlist[[as.character(1)]] <- NULL
      plotListLength(0)
      reportHeight(10)
      reportWidth(10)
      updateSelectInput(session,
        "loadPlot",
        choices = "",
        selected = ""
      )
    }
  })

  #### Clear all plots from report ####
  observeEvent(input$clearAll, {
    print("clear all plots from report was clicked")
    previous_plotListLength <- plotListLength()
    for (i in previous_plotListLength:1) {
      plotList[[as.character(i)]] <- grid::nullGrob(vp = NULL)
      inputs[[as.character(i)]] <- NULL
      wlist[[as.character(i)]] <- NULL
      hlist[[as.character(i)]] <- NULL
    }
    plotListLength(0)
    reportHeight(10)
    reportWidth(10)
    updateSelectInput(session,
      "loadPlot",
      choices = "",
      selected = ""
    )
  })

  #### Update plot in report ####
  observeEvent(input$update, {
    print("updating plot in report")
    req(plotListLength() >= 1)
    comparisons <- unique(dataFrame()[[input$comp]])
    current_plotListLength <- plotListLength()
    current_numcol <- floor(sqrt(current_plotListLength))
    wlist[[as.character(input$loadPlot)]] <- cpWidth()
    hlist[[as.character(input$loadPlot)]] <- cpHeight()
    widths <- unlist(reactiveValuesToList(wlist), use.names = FALSE)
    heights <- unlist(reactiveValuesToList(hlist), use.names = FALSE)
    length(widths) <- suppressWarnings(prod(dim(matrix(widths, ncol = current_numcol))))
    length(heights) <- suppressWarnings(prod(dim(matrix(heights, ncol = current_numcol))))
    widths[is.na(widths)] <- 0
    heights[is.na(heights)] <- 0
    wdims <- as.tibble(matrix(widths, ncol = current_numcol, byrow = TRUE)) %>%
      mutate(rowSums = rowSums(., na.rm = TRUE))
    hdims <- as.tibble(matrix(heights, ncol = current_numcol, byrow = TRUE))
    reportWidth(ceiling(max(wdims$rowSums, na.rm = TRUE)))
    reportHeight(ceiling(max(colSums(hdims, na.rm = TRUE), na.rm = TRUE)))
    plotList[[input$loadPlot]] <- isolate(currentPlot())
    inputs[[input$loadPlot]] <- isolate(reactiveValuesToList(input))
    cols <- c()
    fills <- c()
    shapes <- c()
    lapply(1:length(comparisons), function(i) {
      cols[i] <<- input[[paste0("col", i)]]
      fills[i] <<- input[[paste0("fill", i)]]
      shapes[i] <<- as.numeric(input[[paste0("shape", i)]])
    })
    inputs[[input$loadPlot]]$cols <- cols
    inputs[[input$loadPlot]]$fills <- fills
    inputs[[input$loadPlot]]$shapes <- shapes
  })

  #### Create report ####
  output$myReport <- renderImage({
    print("rendering report image")
    current_plotListLength <- plotListLength()
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext = ".png")

    # Generate the PNG
    png(outfile,
      width = reportWidth() * 3,
      height = reportHeight() * 3,
      res = 72 * 3
    )
    if (current_plotListLength > 0) {
      numcol <- floor(sqrt(current_plotListLength))
      gridExtra::grid.arrange(
        grobs = reactiveValuesToList(plotList)[1:current_plotListLength],
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
    file = function() {
      paste(input$report, "pdf", sep = ".")
    },
    content = function(file) {
      ggsave(file,
        plot =
          if (plotListLength() > 0) {
            numcol <- floor(sqrt(plotListLength()))
            gridExtra::arrangeGrob(
              grobs = reactiveValuesToList(plotList)[1:isolate(plotListLength())],
              ncol = numcol
            )
          },
        useDingbats = FALSE,
        height = reportHeight() / 3.7795275591,
        width = reportWidth() / 3.7795275591,
        units = "mm",
        device = "pdf",
        limitsize = FALSE
      )
    }
  )

  #### Load plot from report ####
  observeEvent(input$load, {
    req(plotListLength() >= 1)
    print("Loading last sheet")
    updateSelectInput(session,
      "sheet",
      selected = inputs[[input$loadPlot]]$sheet
    )
  }, priority = -1)

  observeEvent(input$load, {
    req(plotListLength() >= 1)
    print("Loading last columns")
    updateSelectInput(session,
      "columns",
      selected = inputs[[input$loadPlot]]$columns
    )
  }, priority = -2)

  observeEvent(input$load, {
    req(plotListLength() >= 1)
    print("Loading last variables")
    updateSelectInput(session,
      "variables",
      selected = inputs[[input$loadPlot]]$variables
    )
  }, priority = -3)

  observeEvent(input$load, {
    req(plotListLength() >= 1)
    print("Loading last comparisons")
    updateSelectInput(session,
      "comp",
      selected = inputs[[input$loadPlot]]$comp
    )
  }, priority = -4)

  observeEvent(input$load, {
    req(plotListLength() >= 1)
    print("Loading last id")
    updateSelectInput(session,
      "id",
      selected = inputs[[input$loadPlot]]$id
    )
  }, priority = -5)

  observeEvent(input$load, {
    req(plotListLength() >= 1)
    print("Loading last group")
    updateSelectInput(session,
      "group",
      selected = inputs[[input$loadPlot]]$group
    )
    updateSelectInput(session,
      "comps",
      selected = inputs[[input$loadPlot]]$comps
    )
  }, priority = -6)

  observeEvent(input$load, {
    req(plotListLength() >= 1)
    print("Loading independent inputs")
    updateSelectInput(session,
      "beadColumn",
      selected = inputs[[input$loadPlot]]$beadColumn
    )
    updateSelectInput(session,
      "dilutionColumn",
      selected = inputs[[input$loadPlot]]$dilutionColumn
    )
    updateTextInput(session,
      "y.lab",
      value = inputs[[input$loadPlot]]$y.lab
    )
    updateSelectInput(session,
      "geom",
      selected = inputs[[input$loadPlot]]$geom
    )
    updateSelectInput(session,
      "method",
      selected = inputs[[input$loadPlot]]$method
    )
    updateSelectInput(session,
      "trans.y",
      selected = inputs[[input$loadPlot]]$trans.y
    )
    updateSelectInput(session,
      "legend",
      selected = inputs[[input$loadPlot]]$legend
    )
    updateSelectInput(session,
      "errortype",
      selected = inputs[[input$loadPlot]]$errortype
    )
    updateSliderInput(session,
      "font",
      value = inputs[[input$loadPlot]]$font
    )
    updateSliderInput(session,
      "size",
      value = inputs[[input$loadPlot]]$size
    )
    updateSliderInput(session,
      "stroke",
      value = inputs[[input$loadPlot]]$stroke
    )
    updateSliderInput(session,
      "plotWidth",
      value = inputs[[input$loadPlot]]$plotWidth
    )
    updateSliderInput(session,
      "plotHeight",
      value = inputs[[input$loadPlot]]$plotHeight
    )
    updateSliderInput(session,
      "width",
      value = inputs[[input$loadPlot]]$width
    )
    updateSliderInput(session,
      "dodge",
      value = inputs[[input$loadPlot]]$dodge
    )
    updateTextInput(session,
      "trim",
      value = inputs[[input$loadPlot]]$trim
    )
    updateTextInput(session,
      "split.on",
      value = inputs[[input$loadPlot]]$split.on
    )
    updateCheckboxInput(session,
      "split",
      value = inputs[[input$loadPlot]]$split
    )
    updateCheckboxInput(session,
      "angle.x",
      value = inputs[[input$loadPlot]]$angle.x
    )
    updateCheckboxInput(session,
      "refGroup",
      value = inputs[[input$loadPlot]]$refGroup
    )
    updateCheckboxInput(session,
      "paired",
      value = inputs[[input$loadPlot]]$paired
    )
    updateCheckboxInput(session,
      "scientific",
      value = inputs[[input$loadPlot]]$scientific
    )
  }, priority = -7)

  observeEvent(input$load, {
    req(plotListLength() >= 1)
    print("updating colors, fills, shapes")
    for (i in 1:length(unique(dataFrame()[[input$comp]]))) {
      colourpicker::updateColourInput(session,
        paste0("col", i),
        value = inputs[[input$loadPlot]][[paste0("col", i)]]
      )
      colourpicker::updateColourInput(session,
        paste0("fill", i),
        value = inputs[[input$loadPlot]][[paste0("fill", i)]]
      )
      updateSelectInput(session,
        paste0("shape", i),
        selected = inputs[[input$loadPlot]][[paste0("shape", i)]]
      )
    }
  }, priority = -8)

  #### Refresh plots in report ####
  observeEvent(input$refresh, {
    req(input$file,
        plotListLength() >= 1)
    for (i in as.character(1:plotListLength())) {
      print(paste0("refreshing plot: ", i))

      rData <- readData(
        sheet = inputs[[i]]$sheet,
        file = input$file$datapath
      )

      oData <- organizeData(
        data = rData,
        exclude = inputs[[i]]$columns,
        comp = inputs[[i]]$comp,
        comps = inputs[[i]]$comps,
        variables = inputs[[i]]$variables,
        id = inputs[[i]]$id,
        beadColumn = inputs[[i]]$beadColumn,
        dilutionColumn = inputs[[i]]$dilutionColumn
      )

      errortype <- ifelse(inputs[[i]]$errortype == "SE", "mean_se", "mean_sdl")

      if (inputs[[i]]$split.on == "") {
        split_str <- NULL
      } else {
        split_str <- inputs[[i]]$split.on
      }

      if (inputs[[i]]$y.lab == "") {
        y.lab <- NULL
      } else {
        y.lab <- inputs[[i]]$y.lab
      }

      ref.group <- NULL

      if (inputs[[i]]$refGroup == TRUE) {
        ref.group <- inputs[[i]]$comps[1]
      }

      levs.comps <- order(factor(unique(oData[[inputs[[i]]$comp]]),
        levels = inputs[[i]]$comps
      ))

      levs <- order(factor(unique(oData[[inputs[[i]]$group]]),
        levels = inputs[[i]]$variables
      ))

      cols <- inputs[[i]]$cols
      fills <- inputs[[i]]$fills
      shapes <- inputs[[i]]$shapes

      if (inputs[[i]]$trim == "") {
        updateTextInput(session, "trim", value = "none")
      }

      cPlot <- gplot(
        dataset = oData,
        comparison = inputs[[i]]$comp,
        group.by = inputs[[i]]$group,
        geom = inputs[[i]]$geom,
        errortype = errortype,
        method = inputs[[i]]$method,
        paired = inputs[[i]]$paired,
        size = inputs[[i]]$size,
        stroke = inputs[[i]]$stroke,
        width = inputs[[i]]$width,
        dodge = inputs[[i]]$dodge,
        font_size = inputs[[i]]$font,
        ref.group = ref.group,
        plotWidth = inputs[[i]]$plotWidth,
        plotHeight = inputs[[i]]$plotHeight,
        trans.y = inputs[[i]]$trans.y,
        split = inputs[[i]]$split,
        split_str = split_str,
        trim = inputs[[i]]$trim,
        angle = inputs[[i]]$angle.x,
        y.lab = y.lab,
        leg.pos = inputs[[i]]$legend,
        levs = levs,
        levs.comps = levs.comps,
        color.groups = cols,
        fill.groups = fills,
        shape.groups = shapes,
        sci = inputs[[i]]$scientific
      )
      pheight <- sum(as.numeric(grid::convertUnit(cPlot$heights, "mm"))) * 3.7795275591
      pwidth <- sum(as.numeric(grid::convertUnit(cPlot$widths, "mm"))) * 3.7795275591
      wlist[[i]] <- pwidth
      hlist[[i]] <- pheight
      plotList[[i]] <- cPlot
    }
    current_plotListLength <- plotListLength()
    current_numcol <- floor(sqrt(current_plotListLength))
    widths <- unlist(reactiveValuesToList(wlist), use.names = FALSE)
    heights <- unlist(reactiveValuesToList(hlist), use.names = FALSE)
    length(widths) <- suppressWarnings(prod(dim(matrix(widths, ncol = current_numcol))))
    length(heights) <- suppressWarnings(prod(dim(matrix(heights, ncol = current_numcol))))
    widths[is.na(widths)] <- 0
    heights[is.na(heights)] <- 0
    wdims <- as.tibble(matrix(widths, ncol = current_numcol, byrow = TRUE)) %>%
      mutate(rowSums = rowSums(., na.rm = TRUE))
    hdims <- as.tibble(matrix(heights, ncol = current_numcol, byrow = TRUE))
    reportWidth(ceiling(max(wdims$rowSums, na.rm = TRUE)))
    reportHeight(ceiling(max(colSums(hdims, na.rm = TRUE), na.rm = TRUE)))
  })

  #### Bookmarking values ####
  onBookmark(function(state) {
    state$values$plotList <- reactiveValuesToList(plotList)
    state$values$inputs <- reactiveValuesToList(inputs)
    state$values$plotListLength <- isolate(plotListLength())
    state$values$hlist <- reactiveValuesToList(hlist)
    state$values$wlist <- reactiveValuesToList(wlist)
  })

  #### Restoring values ####
  onRestored(function(state) {
    print("Restoring report")
    plotListLength(state$values$plotListLength)
    for (i in 1:isolate(plotListLength())) {
      inputs[[as.character(i)]] <- state$values$inputs[[as.character(i)]]
      plotList[[as.character(i)]] <- state$values$plotList[[as.character(i)]]
      hlist[[as.character(i)]] <- state$values$hlist[[as.character(i)]]
      wlist[[as.character(i)]] <- state$values$wlist[[as.character(i)]]
    }
    plotListPlots <- as.character(1:isolate(plotListLength()))
    updateSelectInput(session,
      "loadPlot",
      choices = plotListPlots,
      selected = tail(plotListPlots, 1)
    )
    current_plotListLength <- plotListLength()
    current_numcol <- floor(sqrt(current_plotListLength))
    widths <- unlist(reactiveValuesToList(wlist), use.names = FALSE)
    heights <- unlist(reactiveValuesToList(hlist), use.names = FALSE)
    length(widths) <- suppressWarnings(prod(dim(matrix(widths, ncol = current_numcol))))
    length(heights) <- suppressWarnings(prod(dim(matrix(heights, ncol = current_numcol))))
    widths[is.na(widths)] <- 0
    heights[is.na(heights)] <- 0
    wdims <- as.tibble(matrix(widths, ncol = current_numcol, byrow = TRUE)) %>%
      mutate(rowSums = rowSums(., na.rm = TRUE))
    hdims <- as.tibble(matrix(heights, ncol = current_numcol, byrow = TRUE))
    reportWidth(ceiling(max(wdims$rowSums, na.rm = TRUE)))
    reportHeight(ceiling(max(colSums(hdims, na.rm = TRUE), na.rm = TRUE)))
  })

  #### Stop app on close ####
  session$onSessionEnded(function() {
    graphics.off()
  })
  session$onSessionEnded(stopApp)
}

shiny::shinyApp(ui, server, enableBookmarking = "server")
