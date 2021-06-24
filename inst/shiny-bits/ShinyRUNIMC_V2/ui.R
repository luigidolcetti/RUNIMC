
###### UI ######

ui <- shinydashboard::dashboardPage(

  shinydashboard::dashboardHeader(title = 'RUNIMC::'),
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("File", tabName = "File"),
      shinydashboard::menuItem("Picture", tabName = "Picture"),
      shinydashboard::menuItem("Labels", tabName = "Labels"),
      shinydashboard::menuItem("Statistics", tabName = "Stats")
    )),
  shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      shinydashboard::tabItem(tabName = "File",
                              uiOutput("fileControls")),
      shinydashboard::tabItem(tabName = "Picture",
                              shinyjs::useShinyjs(),
                              keys::useKeys(),
                              keys::keysInput("keys", c('p','d','n','g','shift+v','enter','shift+c','t')),
                              uiOutput("pictureControls")),
      shinydashboard::tabItem(tabName = "Labels",
                              uiOutput("labelControls")),
      shinydashboard::tabItem(tabName = 'Stats',
                              uiOutput("statsControls"))
    )
  )
)

