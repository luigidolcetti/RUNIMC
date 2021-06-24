
###### UI ######

ui <- shinydashboard::dashboardPage(

  shinydashboard::dashboardHeader(title = 'RUNIMC::'
                  # shinydashboard::dropdownMenu(type = "task", badgeStatus = "success",
                  #              shinydashboard::taskItem(value = 20, color = "green",
                  #                       "load Stack"),
                  #              shinydashboard::taskItem(value = 20, color = "aqua",
                  #                       "load polygons"))
                  ),
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("File", tabName = "File"),
      shinydashboard::menuItem("Picture", tabName = "Picture"),
      shinydashboard::menuItem("Labels", tabName = "Labels"),
      shinydashboard::menuItem("Statistics", tabName = "Stats")
    )
  ),
  shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      shinydashboard::tabItem(tabName = "File",
              shiny::fluidRow(
                shinydashboard::box(
                  shiny::helpText(paste0('Study::: ',shiny::getShinyOption('studyName'),
                                        ' / ',
                                        'Analysis:::', shiny::getShinyOption('analysisName')))
                ),
                shinydashboard::box(
                  shiny::fileInput("fileRaster", "Upload one Raster Stack",
                            multiple = FALSE,

                            accept = ".stk"),

                  shiny::verbatimTextOutput('fileUpload',placeholder = T),
                  width = 12),
                shinydashboard::box(
                  shiny::fileInput("deftblUpload", "Upload label definitions",
                                   multiple = FALSE,
                                   accept = ".sqlite"),
                  width = 6),

                shinydashboard::box(
                  shiny::fileInput("dldtblUpload", "Upload Polygons",
                            multiple = FALSE,
                            accept = ".sqlite"),
                  width = 6),

                shinydashboard::box(
                  downloadButton('deftbl','Download label definitions'),
                  width = 6),


                shinydashboard::box(
                  downloadButton('dldtbl','Download Polygons'),
                  width = 6)
              )
      ),
      shinydashboard::tabItem(tabName = "Picture",
              shiny::fluidRow(
                shinyjs::useShinyjs(),
                keys::useKeys(),
                keys::keysInput("keys", shinyServiceEnv$hotKey),
                shinydashboard::box(
                  shiny::plotOutput("plotG",
                             click='plotG_click',
                             hover =  (NULL)),
                  width = 5),
                shinydashboard::box(
                  shiny::plotOutput("plotT",
                                    click = "plotT_click",
                                    hover = (NULL)),
                  width = 5
                ),
                shinydashboard::box(
                  textOutput('NofV'),
                  shiny::selectInput('cvrLabel','coverage label ss',""),
				  "On top-left image:", shiny::br(),
				  "Left Click - add point", shiny::br(),
				  "Return - complete polygon", shiny::br(),
				  "Shift-v - remove points", shiny::br(),
				  "Shift-c - remove point/polygon around point",
                  width = 2
                ),
                shinydashboard::box(
                  shiny::plotOutput("plotP"),
                  width = 5
                ),
                shinydashboard::box(
                  shiny::sliderInput("sTrls","Thresholds",
                              min = 0,
                              max = 1,
                              value = c(0,1),step = 0.001),
                  shiny::sliderInput("xTrls","X position",
                              min = shinyServiceEnv$rstStack[[1]]@extent[1],
                              max = shinyServiceEnv$rstStack[[1]]@extent[2],
                              value = round((shinyServiceEnv$rstStack[[1]]@extent[2]-shinyServiceEnv$rstStack[[1]]@extent[1])/2)),
                  shiny::sliderInput("yTrls","Y position",
                              min = shinyServiceEnv$rstStack[[1]]@extent[3],
                              max = shinyServiceEnv$rstStack[[1]]@extent[4],
                              value = round((shinyServiceEnv$rstStack[[1]]@extent[4]-shinyServiceEnv$rstStack[[1]]@extent[3])/2)),
                  shiny::sliderInput("zTrls","Zoom",
                              min = 1,
                              max = {
                                TEMP<-min(shinyServiceEnv$rstStack[[1]]@extent[2],shinyServiceEnv$rstStack[[1]]@extent[4])
                                if (TEMP<100) {TEMP} else {100}},
                              value = {
                                TEMP<-min(shinyServiceEnv$rstStack[[1]]@extent[2],shinyServiceEnv$rstStack[[1]]@extent[4])
                                if (TEMP<100) {TEMP/2} else {50}}),
                  width = 5
                ),
                shinydashboard::box(
                  shiny::selectInput("lTrls0","Layer 0",
                              choices = shinyServiceEnv$nmsRst,
                              selected = shinyServiceEnv$nmsRst[[1]]),
                  shiny::selectInput("lTrls1","Layer 1",
                              choices = shinyServiceEnv$nmsRst,
                              selected = shinyServiceEnv$nmsRst[[1]]),
                  shiny::selectInput("lTrls2","Layer 2",
                              choices = shinyServiceEnv$nmsRst,
                              selected = shinyServiceEnv$nmsRst[[1]]),
                  shiny::selectInput("lTrls3","Layer 3",
                              choices = shinyServiceEnv$nmsRst,
                              selected = shinyServiceEnv$nmsRst[[1]]),
                  shiny::selectInput("lTrls4","Layer 4",
                              choices = shinyServiceEnv$nmsRst,
                              selected = shinyServiceEnv$nmsRst[[1]]),
                  width = 2
                )
              )
      ),
      shinydashboard::tabItem(tabName = "Labels",
              shiny::fluidRow(
                shinydashboard::box(
                  shiny::actionButton("addLbl","Add label"),
                  shiny::actionButton("delLbl","delete label"),
                  shiny::actionButton("tggl","Toggle visibility")),
                shinydashboard::box(
                  shiny::actionButton("delPt","delete all plygons keep labels"),
                  shiny::actionButton('delLblPt',"delete all labels and polygons")),
                shiny::fluidRow(
                  DT::DTOutput('lblTbl')
                )
              )),
      shinydashboard::tabItem(tabName = 'Stats',
              shiny::fluidRow(
                shinydashboard::box(
                  shiny::actionButton('recalcStats','recalculate stats'),
                  DT::DTOutput('tableT'),
                  width=12
                ),
                width=12),
              shiny::fluidRow(
                shinydashboard::box(
                  shiny::actionButton('refreshPlots','update plots'),
                  shiny::plotOutput("Stata"),
                  width=12
                ),
                width=12)
      )
    )
  )
)
