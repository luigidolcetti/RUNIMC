
server <- function(input, output, session) {
browser()
  rv_exprsDataFrame<-shiny::reactiveValues(exprs = shinyServiceEnv$exprs)

  output$defTable<-DT::renderDT(rv_exprsDataFrame$exprs,
                              server = F,
                              editable = F,
                              option=list(bFilter=0,
                                          bInfo=0,
                                          bLengthChange=0,
                                          bAutoWidth=0))

  # session$onSessionEnded(function(){

  #   if (exists("shinyServiceEnv", mode="environment",where = .GlobalEnv)) rm(shinyServiceEnv,pos = .GlobalEnv)
  #
  # })

  ###### reactive values ######


}
