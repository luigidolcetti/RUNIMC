
###### UI ######

ui <- shinydashboard::dashboardPage(

  shinydashboard::dashboardHeader(title = "RUNIMC"),
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("File", tabName = "File")
    )
  ),
  shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      shinydashboard::tabItem(tabName = "File",
                              shiny::fluidRow(
                                shinydashboard::box(
                                  DT::DTOutput("defTable"),
                                  width = 12
                                )

                              )
      )
    )
  )
)

