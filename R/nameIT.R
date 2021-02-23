#' @export
nameIT <- function(fn_exprs,
                   fn_Xclass,
                   fn_Yclass,
                   fn_dimToPlot,
                   fn_Newclass = 'newClass') {

  newExprs<-sf::st_drop_geometry(fn_exprs)

  Xlevels<-sort(unique(as.character(newExprs[,fn_Xclass])))
  Ylevels<-sort(unique(as.character(newExprs[,fn_Yclass])))

  newExprs[,fn_Xclass]<-factor(newExprs[,fn_Xclass],levels = Xlevels)
  newExprs[,fn_Yclass]<-factor(newExprs[,fn_Yclass],levels = Ylevels)

  labelClash<-newExprs[,c(fn_Xclass,fn_Yclass)]

  labelCross<-apply(table(labelClash),2,function(x){x/sum(x)})

  Rowv <- as.dendrogram(hclust(dist(labelCross)))
  rowInd <- order.dendrogram(Rowv)

  Colv <- as.dendrogram(hclust(dist(t(labelCross))))
  colInd <- order.dendrogram(Colv)

  labelCross<-labelCross[rowInd,colInd]

  labelCross_long<-expand.grid(rev(rownames(labelCross)),colnames(labelCross))
  numberCross_long<-expand.grid(seq_along(rownames(labelCross)),seq_along(colnames(labelCross)))

  colnames(labelCross_long)<-c(fn_Xclass,fn_Yclass)
  labelCross_long[,fn_Xclass]<-factor(labelCross_long[,fn_Xclass],levels = Xlevels)
  labelCross_long[,fn_Yclass]<-factor(labelCross_long[,fn_Yclass],levels = Ylevels)

  colnames(numberCross_long)<-c('Y','X')
  labelCross_long<-cbind.data.frame(labelCross_long,
                                    crossLabel = paste(labelCross_long[,fn_Xclass],
                                                       labelCross_long[,fn_Yclass],
                                                       sep = ' x '),
                                    numberCross_long,
                                    perc = as.vector(labelCross))

  labelCross_def<-cbind.data.frame(labelCross_long[,c(fn_Xclass,
                                                      fn_Yclass,
                                                      'crossLabel')],
                                   NC='UNDEF')
  colnames(labelCross_def)[4]<-fn_Newclass


  exprs_long<-newExprs[,c(fn_Xclass,fn_Yclass,fn_dimToPlot)]
  exprs_long<-as.data.frame(tidyr::pivot_longer(data = exprs_long,
                                                cols = fn_dimToPlot,
                                                names_to = 'marker',
                                                values_to = 'MFI'))

  exprs_long<-dplyr::left_join(exprs_long,
                               labelCross_long[,c(fn_Xclass,
                                                  fn_Yclass,
                                                  'crossLabel',
                                                  'Y',
                                                  'X')],
                               by = c(fn_Xclass,fn_Yclass))




  server <- function(input, output, session) {



    rv_plotMap<-shiny::reactiveValues(map = raster::raster(labelCross,
                                                           xmn=0,
                                                           xmx=0+ncol(labelCross),
                                                           ymn=0,
                                                           ymx=0+nrow(labelCross)),
                                      df = labelCross_long,
                                      newLab = labelCross_def,
                                      newLabSub =NULL,
                                      select = NULL,
                                      selectCol = NULL,
                                      selectCrossLabel = NULL)
    rv_plotProfile<-shiny::reactiveValues(original = as.data.frame(exprs_long),
                                          sub = NULL)


    shiny::observeEvent(input$defUp,{
      rv_plotMap$newLab<-read.table(input$defUp$datapath,
                                    header = T,
                                    as.is = T,
                                    check.names = F,
                                    stringsAsFactors = T)
    })


    output$defDown <- shiny::downloadHandler(

      filename =function() 'table.txt',
      content = function(file) {
        write.table(rv_plotMap$newLab,file,append = F,row.names = F)
      }
    )






    output$plotMap<-shiny::renderPlot({

      par(cex.axis = 0.8,
          las = 2)
      raster::image(rv_plotMap$map,
                    ann = F,
                    axes = F,
                    col = scales::colour_ramp(c('blue','red'))((0:255)/255))
      raster::plot(raster::rasterToPolygons(rv_plotMap$map),add=T)
      points(rv_plotMap$select-c(0.5,0.5),
             bg = rv_plotMap$selectCol,
             pch = 22,
             col = 'black',
             lwd=1,
             cex = 2)
      #
      axis(1, at=rv_plotMap$df[,5]-0.5, labels=rv_plotMap$df[,2])
      axis(2, at=rv_plotMap$df[,4]-0.5, labels=rv_plotMap$df[,1])
    })

    output$plotPlot<-shiny::renderPlot({

      selectCase<-apply(rv_plotMap$select,1,function(sC){
        rv_plotMap$df[(rv_plotMap$df$Y == sC[2] &
                         rv_plotMap$df$X == sC[1])
                      ,c(fn_Yclass,fn_Xclass)]
      })
      selectCase<-do.call(rbind.data.frame,selectCase)


      sub_plot<-apply(selectCase,1,function(sC){
        rv_plotProfile$original[rv_plotProfile$original[,fn_Yclass] == sC[fn_Yclass] &
                                  rv_plotProfile$original[,fn_Xclass] == sC[fn_Xclass],]
      })

      sub_plot<-do.call(rbind.data.frame,sub_plot)


      ggplot2::ggplot(rv_plotProfile$original)+
        ggplot2::geom_density(data = sub_plot,mapping = ggplot2::aes(x=MFI,y=..scaled..,col=crossLabel))+
        ggplot2::scale_x_continuous(trans=scales::modulus_trans(-1),)+
        ggplot2::scale_color_manual(values = rv_plotMap$selectCol,
                                    breaks = rv_plotMap$selectCrossLabel,
                                    guide = ggplot2::guide_legend(override.aes = list(fill = rv_plotMap$selectCol) ))+
        ggplot2::facet_wrap(facets='marker',
                            scales  = 'free_x')+
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = 'black'),
                       panel.grid = ggplot2::element_line(color = 'gray20',
                                                          linetype = 3))


    }
    # ,width = 300,height = 150*length(fn_dimToPlot),res = 72
    )

    shiny::observeEvent(rv_plotMap$selectCrossLabel,{
      rv_plotMap$newLabSub <- rv_plotMap$newLab[rv_plotMap$newLab$crossLabel %in% rv_plotMap$selectCrossLabel, ]
    })


    # output$dtNewDef<-DT::renderDT(rv_plotMap$newLabSub,
    #                               server = T,
    #                               editable = T,
    #                               selection = 'single',
    #
    #                               options = list(bFilter=0,
    #                                              bInfo=0,
    #                                              bLengthChange=0,
    #                                              bAutoWidth=0,
    #                                              pageLength=nrow(rv_plotMap$newLab)))


    output$dtNewDef<-DT::renderDT(DT::formatStyle(
      table = DT::datatable(rv_plotMap$newLabSub,
                            editable = 'cell',
                            selection = 'single'
      ),
      columns = 3,
      target = 'cell',
      backgroundColor = DT::styleEqual(levels = rv_plotMap$selectCrossLabel, values = rv_plotMap$selectCol),
      valueColumns = 3,

      # ,
      # options = list(bFilter=0,
      #                bInfo=0,
      #                bLengthChange=0,
      #                bAutoWidth=0,
      #                pageLength=nrow(rv_plotMap$newLab))
    ))








    proxy = DT::dataTableProxy('dtNewDef')

    shiny::observeEvent(input$dtNewDef_cell_edit,{
      info = input$dtNewDef_cell_edit
      i = info$row
      j = info$col
      v = info$value

      crossLabIndex<-rv_plotMap$newLabSub$crossLabel[i]
      rv_plotMap$newLab[,fn_Newclass]<-droplevels(rv_plotMap$newLab[,fn_Newclass])
      oldLevels<-levels(rv_plotMap$newLab[,fn_Newclass])
      if (any(v == oldLevels)){
        rv_plotMap$newLab[rv_plotMap$newLab$crossLabel == crossLabIndex,fn_Newclass] <- v} else {
          levels(rv_plotMap$newLab[,fn_Newclass])<-c(oldLevels,v)
          rv_plotMap$newLab[rv_plotMap$newLab$crossLabel == crossLabIndex,fn_Newclass] <- v
        }
      rv_plotMap$newLabSub <- rv_plotMap$newLab[rv_plotMap$newLab$crossLabel %in% rv_plotMap$selectCrossLabel, ]
    })

    output$dtTab<-DT::renderDT(as.data.frame(table(rv_plotMap$newLab[,fn_Newclass])),
                               server = T,
                               editable = F,
                               selection = 'none',

                               options = list(bFilter=0,
                                              bInfo=0,
                                              bLengthChange=0,
                                              bAutoWidth=0))


    output$dtFinal<-DT::renderDT(rv_plotMap$newLab,
                                 server = T,
                                 editable = T,
                                 selection = 'single',
                                 filter = 'top',

                                 options = list(bInfo=0,
                                                bLengthChange=0,
                                                bAutoWidth=0,
                                                pageLength=nrow(rv_plotMap$newLab)))
    proxy = DT::dataTableProxy('dtFinal')

    shiny::observeEvent(input$dtFinal_cell_edit,{
      info = input$dtFinal_cell_edit
      i = info$row
      j = info$col
      v = info$value
      crossLabIndex<-rv_plotMap$newLab$crossLabel[i]
      rv_plotMap$newLab[,fn_Newclass]<-droplevels(rv_plotMap$newLab[,fn_Newclass])
      oldLevels<-levels(rv_plotMap$newLab[,fn_Newclass])
      if (any(v == oldLevels)){
        rv_plotMap$newLab[rv_plotMap$newLab$crossLabel == crossLabIndex,fn_Newclass] <- v} else {
          levels(rv_plotMap$newLab[,fn_Newclass])<-c(oldLevels,v)
          rv_plotMap$newLab[rv_plotMap$newLab$crossLabel == crossLabIndex,fn_Newclass] <- v
        }
    })

    shiny::observeEvent(input$plotMap_click,{

      xSelect<-trunc(input$plotMap_click$x)+1
      ySelect<-trunc(input$plotMap_click$y)+1
      if (is.null(rv_plotMap$select)) {
        rv_plotMap$select<-matrix(c(xSelect,ySelect),ncol=2)

        rv_plotMap$selectCrossLabel<-paste(unlist(rv_plotMap$df[rv_plotMap$df$Y == ySelect &
                                                                  rv_plotMap$df$X == xSelect, c(fn_Xclass,fn_Yclass)]),collapse = ' x ')
      } else {
        whichIs<-rv_plotMap$select[,1]==xSelect & rv_plotMap$select[,2]==ySelect
        if (any(whichIs)){
          rv_plotMap$select<-rv_plotMap$select[!(whichIs),,drop=F]
          rv_plotMap$selectCrossLabel<-rv_plotMap$selectCrossLabel[!(whichIs)]
        } else {
          rv_plotMap$select<-rbind(rv_plotMap$select,c(xSelect,ySelect))
          rv_plotMap$selectCrossLabel<-c(rv_plotMap$selectCrossLabel,
                                         paste(unlist(rv_plotMap$df[rv_plotMap$df$Y == ySelect &
                                                                      rv_plotMap$df$X == xSelect, c(fn_Xclass,fn_Yclass)]),collapse = ' x '))
        }

      }
      rv_plotMap$selectCol <- rainbow(nrow(rv_plotMap$select))

    })



  }

  ui <- shinydashboard::dashboardPage(

    shinydashboard::dashboardHeader(title = "RUNIMC"),
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem("MAP", tabName = "MAP"),
        shinydashboard::menuItem("PLOT", tabName = "PLOT"),
        shinydashboard::menuItem("FINALIZE", tabName = "FINALIZE")
      )
    ),
    shinydashboard::dashboardBody(
      shinydashboard::tabItems(
        shinydashboard::tabItem(tabName = "MAP",
                                shiny::fluidRow(
                                  shinydashboard::box(
                                    shiny::plotOutput("plotMap",
                                                      click='plotMap_click',
                                                      hover =  (NULL)),
                                    width = 12
                                  )

                                )
        ),
        shinydashboard::tabItem(tabName = "PLOT",
                                shiny::fluidPage(

                                  shinydashboard::box(
                                    shiny::plotOutput("plotPlot",
                                                      click= NULL,
                                                      hover =  (NULL)),
                                    width = 12,
                                    height = NULL),

                                  shinydashboard::box(

                                    DT::DTOutput('dtNewDef'),
                                    width = 6,
                                    height = NULL
                                  ),
                                  shinydashboard::box(
                                    DT::DTOutput('dtTab'),
                                    width = 3,
                                    height = NULL)
                                )


        ),
        shinydashboard::tabItem(tabName = "FINALIZE",
                                shiny::fluidPage(

                                  shinydashboard::box(

                                    DT::DTOutput('dtFinal'),
                                    width = 6,
                                    height = NULL
                                  ),
                                  shinydashboard::box(

                                    shiny::fileInput("defUp", "Upload one table",
                                                     multiple = FALSE,

                                                     accept = ".stk"),

                                    shiny::downloadButton('defDown','Download one table'),

                                  )
                                )
        )
      )
    )
  )



  shiny::shinyApp(ui,server,)
}
