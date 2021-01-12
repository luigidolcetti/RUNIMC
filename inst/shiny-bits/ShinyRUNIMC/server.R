
server <- function(input, output, session) {

  session$onSessionEnded(function(){
    if (exists('rstStack')) rm(rstStack,envir = .GlobalEnv)
    if (exists('nmsRst')) rm(nmsRst,envir = .GlobalEnv)
    if (exists('plc')) rm(plc,envir = .GlobalEnv)
    if (exists('plcn')) rm(plcn,envir = .GlobalEnv)
    if (exists('pWidth')) rm(pWidth,envir = .GlobalEnv)
    if (exists('pHeight')) rm(pHeight,envir = .GlobalEnv)
    if (exists('hotKey')) rm(hotKey,envir = .GlobalEnv)
    if (exists('NAcol')) rm(NAcol,envir = .GlobalEnv)
  })

  ###### reactive values ######

  PAINTMODE<-shiny::reactiveValues(pntm='n',
                            ent=F,
                            del=F,
                            mark=F,
                            delV=F)
  VERTEXBUFFER<-shiny::reactiveValues(x=integer(0),
                               y=integer(0))

  GEOM<-shiny::reactiveValues(buffer=sf::st_sf(sf::st_sfc()),
                       bufferColor='red',
                       dataBase=sf::st_sf(uid=character(0),
                                      label=character(0),
                                      color=character(0),
                                      GEOMETRY=sf::st_sfc(),
                                      stringsAsFactors = F),
                       toShow=sf::st_sf(label=character(0),
                                    color=character(0),
                                    GEOMETRY=sf::st_sfc(),
                                    stringsAsFactors = F),
                       toBorder=character(0))

  GEOMETRY<-shiny::reactiveValues(index=1,
                           vertices=list())

  BGCOLOR<-shiny::reactiveValues(bgc='white',
                          lty=1)

  RSTMARC<-shiny::reactiveValues(x=0,y=0)

  ZBRAKES<-shiny::reactiveVal(rep(0,1,1/(plcn-1)))

  LBLTBLV <- shiny::reactiveValues(table = data.frame(label=c('NUC','JNC','BKG'),
                                               note=c('TEST','TEST','TEST'),
                                               color=c('red','green','cyan'),
                                               show=c(T,T,T),
                                               stringsAsFactors = FALSE))

  UPLOADFILENAME<-shiny::reactiveVal("demo....")
  DOWNLOADFILENAME<-shiny::reactiveVal("demo....")

  output$fileUpload<-shiny::renderText('Upload a raster Stack....')

  #### observers #####

  shiny::observeEvent(LBLTBLV$table,{
    shiny::updateSelectInput(
      session = session,
      inputId = 'cvrLabel',
      label = 'coverage label',
      choices = c(NA,LBLTBLV$table$label),
      selected = NULL
    )

    GEOM$toShow<-GEOM$dataBase$GEOMETRY[GEOM$dataBase$label %in% LBLTBLV$table$label[LBLTBLV$table$show==T]]
    GEOM$toBorder<-as.character(GEOM$dataBase$color[GEOM$dataBase$label %in% LBLTBLV$table$label[LBLTBLV$table$show==T]])
  })

  shiny::observeEvent(GEOM$dataBase,{
    GEOM$toShow<-GEOM$dataBase$GEOMETRY[GEOM$dataBase$label %in% LBLTBLV$table$label[LBLTBLV$table$show==T]]
    GEOM$toBorder<-as.character(GEOM$dataBase$color[GEOM$dataBase$label %in% LBLTBLV$table$label[LBLTBLV$table$show==T]])
  })

  shiny::observeEvent(input$cvrLabel,{
    GEOM$bufferColor<-LBLTBLV$table$color[LBLTBLV$table$label==input$cvrLabel]
    if(length(GEOM$bufferColor)==0){GEOM$bufferColor='white'}
  })


  shiny::observeEvent(input$delPt,{
    GEOM$dataBase<-sf::st_sf(uid=character(0),
                         label=character(0),
                         color=character(0),
                         GEOMETRY=sf::st_sfc(),
                         stringsAsFactors = F)
  })

  shiny::observeEvent(input$delLblPt,{
    GEOM$dataBase<-sf::st_sf(uid=character(0),
                         label=character(0),
                         color=character(0),
                         GEOMETRY=sf::st_sfc(),
                         stringsAsFactors = F)

    LBLTBLV$table<-data.frame(label=character(0),
                              note=character(0),
                              color=character(0),
                              show=logical(0),
                              stringsAsFactors = FALSE)

  })


  shiny::observeEvent(input$addLbl,{
    LBLTBLV$table<-rbind(LBLTBLV$table,
                         data.frame(label=paste0(LETTERS[sample(1:26,3)],collapse = ""),
                                    note='TEST',
                                    color=colors()[sample(1:657,1)],
                                    show=T,
                                    stringsAsFactors = FALSE))
  })


  shiny::observeEvent(input$delLbl,{
    if (!is.null(input$lblTbl_rows_selected)) {
      GEOM$dataBase<-GEOM$dataBase[!GEOM$dataBase$label==LBLTBLV$table$label[as.numeric(input$lblTbl_rows_selected)],]
      LBLTBLV$table <- LBLTBLV$table[-as.numeric(input$lblTbl_rows_selected),]}
  })

  shiny::observeEvent(input$tggl,{
    if (!is.null(input$lblTbl_rows_selected)) {
      LBLTBLV$table$show[as.numeric(input$lblTbl_rows_selected)] <- !LBLTBLV$table$show[as.numeric(input$lblTbl_rows_selected)]}
  })


  shiny::observeEvent(input$lblTbl_cell_edit,{
    info = input$lblTbl_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value
    # browser()
    if (j==3 && !any(v %in% colors())) {v<-'!!!! invalid color !!!!'}
    if (j==3 && any(v %in% LBLTBLV$table[,3])) {v<-'!!!! color in use !!!!'}
    if (j==1 && any(v %in% LBLTBLV$table[,1])) {v<-'!!!! label in use !!!!'}
    if (nrow(GEOM$dataBase)!=0){
      if (j==3 ) {
        if (nrow(GEOM$dataBase[as.data.frame(GEOM$dataBase$color)==LBLTBLV$table[i,j],])!=0){
          GEOM$dataBase$color[as.data.frame(GEOM$dataBase$color)==LBLTBLV$table[i,j]]<-v}}
      if (j==1 ) {
        if (nrow(GEOM$dataBase[as.data.frame(GEOM$dataBase$label)==LBLTBLV$table[i,j],])!=0){
          GEOM$dataBase$label[as.data.frame(GEOM$dataBase$label)==LBLTBLV$table[i,j]]<-v}}
    }
    LBLTBLV$table[i, j]<- v
  })

  shiny::observeEvent(input$keys, {


    if (input$keys=='enter') {PAINTMODE$ent= T}
    if (input$keys=='shift+c') {PAINTMODE$del= T}
    if (input$keys=='shift+v'){PAINTMODE$delV=T}

  })

  shiny::observeEvent(input$sTrls,{
    ZBRAKES(seq(input$sTrls[1],
                input$sTrls[2],
                1/(plcn-1)))

  })

  ###### Tables ######

  output$tableT<-DT::renderDT(LBLTBLV$polygons,
                          server = F,
                          editable = F,
                          option=list(bFilter=0,
                                      bInfo=0,
                                      bLengthChange=0,
                                      bAutoWidth=0,
                                      pageLength=nrow(LBLTBLV$polygons)))

  output$lblTbl<-DT::renderDT(LBLTBLV$table,
                          server = F,
                          editable = T,
                          selection = 'single',
                          options = list(bFilter=0,
                                         bInfo=0,
                                         bLengthChange=0,
                                         bAutoWidth=0,
                                         pageLength=nrow(LBLTBLV$table)))
  proxy = DT::dataTableProxy('lblTbl')


  ###### Plots ######

  output$text_out<-shiny::renderText(UPLOADFILENAME())
  output$plotG<-shiny::renderPlot(

    {
      input$sTrls
      par(oma=c(0,0,0,0),mar=c(3,3,1,1))
      RUNIMC:::plotsPoly(fn_rst = rstStack[[input$lTrls0]],
                 fn_xmin = input$xTrls-input$zTrls,
                 fn_xmax = input$xTrls+input$zTrls,
                 fn_ymin = input$yTrls-input$zTrls,
                 fn_ymax = input$yTrls+input$zTrls,
                 fn_plcn = plcn,
                 fn_plcRange = ZBRAKES(),
                 fn_xaxs = 'i',
                 fn_yaxs = 'i',
                 fn_xaxt = 's',
                 fn_yaxt = 's',
                 fn_colNA = NAcol,
                 fn_Bx = VERTEXBUFFER$x,
                 fn_By = VERTEXBUFFER$y,
                 fn_bgc = GEOM$bufferColor,
                 fn_geom = GEOM$toShow,
                 fn_geomB = GEOM$toBorder,
                 fn_title = input$lTrls0)
    })

  ####
  output$plotP<-shiny::renderPlot({
    input$sTrls
    par(oma=c(0,0,0,0),mar=c(0,0,1,0))
    layout.matrix <- matrix(c(1,2,3,4), nrow = 2, ncol = 2)
    layout(mat = layout.matrix,
           heights = c(2, 2),
           widths = c(2, 2))

    RUNIMC:::plotsPoly(fn_rst = rstStack[[input$lTrls1]],
               fn_xmin = input$xTrls-input$zTrls,
               fn_xmax = input$xTrls+input$zTrls,
               fn_ymin = input$yTrls-input$zTrls,
               fn_ymax = input$yTrls+input$zTrls,
               fn_plcn = plcn,
               fn_plcRange = ZBRAKES(),
               fn_xaxs = 'i',
               fn_yaxs = 'i',
               fn_xaxt = 'n',
               fn_yaxt = 'n',
               fn_colNA = NAcol,
               fn_Bx = VERTEXBUFFER$x,
               fn_By = VERTEXBUFFER$y,
               fn_bgc = GEOM$bufferColor,
               fn_geom = GEOM$toShow,
               fn_geomB = GEOM$toBorder,
               fn_title = input$lTrls1)
    RUNIMC:::plotsPoly(fn_rst = rstStack[[input$lTrls2]],
               fn_xmin = input$xTrls-input$zTrls,
               fn_xmax = input$xTrls+input$zTrls,
               fn_ymin = input$yTrls-input$zTrls,
               fn_ymax = input$yTrls+input$zTrls,
               fn_plcn = plcn,
               fn_plcRange = ZBRAKES(),
               fn_xaxs = 'i',
               fn_yaxs = 'i',
               fn_xaxt = 'n',
               fn_yaxt = 'n',
               fn_colNA = NAcol,
               fn_Bx = VERTEXBUFFER$x,
               fn_By = VERTEXBUFFER$y,
               fn_bgc = GEOM$bufferColor,
               fn_geom = GEOM$toShow,
               fn_geomB = GEOM$toBorder,
               fn_title = input$lTrls2)
    RUNIMC:::plotsPoly(fn_rst = rstStack[[input$lTrls3]],
               fn_xmin = input$xTrls-input$zTrls,
               fn_xmax = input$xTrls+input$zTrls,
               fn_ymin = input$yTrls-input$zTrls,
               fn_ymax = input$yTrls+input$zTrls,
               fn_plcn = plcn,
               fn_plcRange = ZBRAKES(),
               fn_xaxs = 'i',
               fn_yaxs = 'i',
               fn_xaxt = 'n',
               fn_yaxt = 'n',
               fn_colNA = NAcol,
               fn_Bx = VERTEXBUFFER$x,
               fn_By = VERTEXBUFFER$y,
               fn_bgc = GEOM$bufferColor,
               fn_geom = GEOM$toShow,
               fn_geomB = GEOM$toBorder,
               fn_title = input$lTrls3)
    RUNIMC:::plotsPoly(fn_rst = rstStack[[input$lTrls4]],
               fn_xmin = input$xTrls-input$zTrls,
               fn_xmax = input$xTrls+input$zTrls,
               fn_ymin = input$yTrls-input$zTrls,
               fn_ymax = input$yTrls+input$zTrls,
               fn_plcn = plcn,
               fn_plcRange = ZBRAKES(),
               fn_xaxs = 'i',
               fn_yaxs = 'i',
               fn_xaxt = 'n',
               fn_yaxt = 'n',
               fn_colNA = NAcol,
               fn_Bx = VERTEXBUFFER$x,
               fn_By = VERTEXBUFFER$y,
               fn_bgc = GEOM$bufferColor,
               fn_geom = GEOM$toShow,
               fn_geomB = GEOM$toBorder,
               fn_title = input$lTrls4)
  })

  ###
  output$plotT<-shiny::renderPlot({
    input$sTrls
    par(oma=c(0,0,0,0),mar=c(1,1,1,1))
    raster::plot(rstStack[[input$lTrls0]],
         col=plc(plcn),
         breaks= ZBRAKES(),
         asp=1,
         xaxs="i",
         yaxs="i",
         yaxt='n',
         legend=F,
         colNA=NAcol)
    title(input$lTrls0,adj=0,line=0.3)
    abline(h=c(input$yTrls-input$zTrls,
               input$yTrls+input$zTrls),
           v=c(input$xTrls-input$zTrls,
               input$xTrls+input$zTrls),
           col=GEOM$bufferColor,lty=1)
  })

  shiny::observeEvent(VERTEXBUFFER$x,{
    output$NofV<-shiny::renderText(paste0('Vertex N: ',length(VERTEXBUFFER$x)))
  })


  ###### Click plot ######

  shiny::observeEvent(input$plotG_click,{

    if(input$cvrLabel!='NA'){
      VERTEXBUFFER$x<-c(VERTEXBUFFER$x,input$plotG_click$x)
      VERTEXBUFFER$y<-c(VERTEXBUFFER$y,input$plotG_click$y)
    }

  })

  shiny::observeEvent(PAINTMODE$ent,{

    if (PAINTMODE$ent==T){

      PAINTMODE$ent<-F
      nVertex<-length(VERTEXBUFFER$x)
      if (nVertex==1){
        Vmat<-matrix(c(VERTEXBUFFER$x,VERTEXBUFFER$y),ncol=2,byrow = F)
        Vpoly<-sf::st_sfc(sf::st_point(Vmat))
      }
      if (nVertex==2){
        Vmat<-matrix(c(VERTEXBUFFER$x,VERTEXBUFFER$y),ncol=2,byrow = F)
        Vpoly<-sf::st_sfc(sf::st_linestring(Vmat))
      }
      if (nVertex>=3){
        VERTEXBUFFER$x<-c(VERTEXBUFFER$x,VERTEXBUFFER$x[1])
        VERTEXBUFFER$y<-c(VERTEXBUFFER$y,VERTEXBUFFER$y[1])
        Vmat<-matrix(c(VERTEXBUFFER$x,VERTEXBUFFER$y),ncol=2,byrow = F)
        Vpoly<-sf::st_sfc(sf::st_polygon(list(Vmat)))
      }

      if (nVertex!=0){

        lblSelect<-input$cvrLabel
        clSelect<-LBLTBLV$table$color[LBLTBLV$table$label==lblSelect]
        GEOM$buffer<-sf::st_sf(uid=rstStack@uid,
                           label=lblSelect,
                           color=clSelect,
                           GEOMETRY = sf::st_sfc(Vpoly))
        GEOM$dataBase<-rbind(GEOM$dataBase,GEOM$buffer)
        VERTEXBUFFER$x=integer(0)
        VERTEXBUFFER$y=integer(0)
      }
    }
  })

  shiny::observeEvent(PAINTMODE$del,{

    if (PAINTMODE$del==T){
      PAINTMODE$del<-F
      Vmat<-matrix(c(VERTEXBUFFER$x,VERTEXBUFFER$y),
                   ncol=2,
                   byrow = F)
      if (nrow(Vmat)==0) {return(0)}
      if (nrow(GEOM$dataBase)==0) {return(0)}
      vpoly<-sf::st_sfc(sf::st_point(Vmat))
      intersection<-which(unlist(lapply(sf::st_intersects(GEOM$dataBase,vpoly),function(x){length(x)==0})))
      GEOM$dataBase<-GEOM$dataBase[intersection,]
      VERTEXBUFFER$x=integer(0)
      VERTEXBUFFER$y=integer(0)
    }
  })

  shiny::observeEvent(PAINTMODE$delV,{
    if (PAINTMODE$delV==T){
      PAINTMODE$delV<-F
      VERTEXBUFFER$x<-integer(0)
      VERTEXBUFFER$y<-integer(0)}
  })


  shiny::observeEvent(input$plotT_click,{

    shiny::updateSliderInput(session,"xTrls",NULL,
                             min = rstStack[[1]]@extent[1],
                             max = rstStack[[1]]@extent[2],
                             value = round(input$plotT_click$x))
    shiny::updateSliderInput(session,"yTrls",NULL,
                             min = rstStack[[1]]@extent[3],
                             max = rstStack[[1]]@extent[4],
                             value = round(input$plotT_click$y))
  })

  ###### uopload Raster#####
  shiny::observeEvent(input$fileRaster,{

    rstStack<<-IMCstackOpen(input$fileRaster$datapath)

    nmsRst<-names(rstStack)
    for (lyr in nmsRst){
      rstStack[[lyr]]<<-quantNorm(rstStack[[lyr]],0.996)
    }

    fnm<-rstStack@IMC_text_file
    fnm<-strsplit(fnm,'/')
    fnm<-fnm[[1]][length(fnm[[1]])]
    UPLOADFILENAME(paste0('file:',fnm,'\n uid: ',rstStack@uid))
    DOWNLOADFILENAME(fnm)

    textDetails<-paste0('uid: ',rstStack@uid,'\n',
                        'file: ',rstStack@IMC_text_file,'\n',
                        'layers: ',paste(nmsRst,collapse = ', '))
    output$fileUpload<-shiny::renderText(textDetails)

    rm(fnm)


    isolate({


      shiny::updateSliderInput(session,"xTrls",NULL,
                        min = rstStack[[1]]@extent[1],
                        max = rstStack[[1]]@extent[2],
                        value = round((rstStack[[1]]@extent[2]-rstStack[[1]]@extent[1])/2))
      shiny::updateSliderInput(session,"yTrls",NULL,
                        min = rstStack[[1]]@extent[3],
                        max = rstStack[[1]]@extent[4],
                        value = round((rstStack[[1]]@extent[4]-rstStack[[1]]@extent[3])/2))
      shiny::updateSliderInput(session,"zTrls",NULL,
                        min = 1,
                        max = {
                          TEMP<-min(rstStack[[1]]@extent[2],rstStack[[1]]@extent[4])
                          if (TEMP<100) {TEMP} else {100}},
                        value = {
                          TEMP<-min(rstStack[[1]]@extent[2],rstStack[[1]]@extent[4])
                          if (TEMP<100) {TEMP/2} else {50}})

      shiny::updateSelectInput(session,"lTrls0",NULL,
                        choices = nmsRst,
                        selected = nmsRst[[1]])
      shiny::updateSelectInput(session,"lTrls1",NULL,
                        choices = nmsRst,
                        selected = nmsRst[[1]])
      shiny::updateSelectInput(session,"lTrls2",NULL,
                        choices = nmsRst,
                        selected = nmsRst[[1]])
      shiny::updateSelectInput(session,"lTrls3",NULL,
                        choices = nmsRst,
                        selected = nmsRst[[1]])
      shiny::updateSelectInput(session,"lTrls4",NULL,
                        choices = nmsRst,
                        selected = nmsRst[[1]])
    })
  })

  ###### uopload Table#####

  shiny::observeEvent(input$dldtblUpload,{
    GEOM$dataBase<-sf::st_read(input$dldtblUpload$datapath,stringsAsFactors = F)
    TEMPloc<-as.data.frame(GEOM$dataBase)[,c('label','color')]
    TEMPloc<-unique(TEMPloc)
    LBLTBLV$table<-data.frame(label=TEMPloc$label,
                              note=rep('TEST',nrow(TEMPloc)),
                              color=TEMPloc$color,
                              show=rep(T,nrow(TEMPloc)),
                              stringsAsFactors = FALSE)
    GEOM$toShow<-GEOM$dataBase$GEOMETRY[GEOM$dataBase$label %in% LBLTBLV$table$label[LBLTBLV$table$show==T]]
    GEOM$toBorder<-as.character(GEOM$dataBase$color[GEOM$dataBase$label %in% LBLTBLV$table$label[LBLTBLV$table$show==T]])
    # invalidateLater(10000)
  })


  ###### Download table ######



  output$dldtbl <- shiny::downloadHandler(

    filename = function(){


      paste0(rstStack@IMC_text_file,".sqlite")},
    content = function(file) {
      sf::st_write(GEOM$dataBase,file,append=F)

    }
  )

  ###### uopload def Table#####

  shiny::observeEvent(input$deftblUpload,{

    GEOM$dataBase<-sf::st_sf(uid=character(0),
                             label=character(0),
                             color=character(0),
                             GEOMETRY=sf::st_sfc(),
                             stringsAsFactors = F)

    LBLTBLV$table<-data.frame(label=character(0),
                              note=character(0),
                              color=character(0),
                              show=logical(0),
                              stringsAsFactors = FALSE)

    LBLTBLV$table<-utils::read.table(file = input$deftblUpload$datapath,
                                     header = T,
                                     sep = '\t',
                                     as.is = T,
                                     check.names = F,
                                     stringsAsFactors = F)

  })


  ###### Download def table ######



  output$deftbl <- shiny::downloadHandler(

    filename = function(){


      "labels.txt"},
    content = function(file) {
      utils::write.table(LBLTBLV$table,file,append=F,sep = '\t',row.names = F,col.names = T,quote = F)

    }
  )





  ##### recalculate stats #####

  shiny::observeEvent(input$recalcStats,{

    isolate({
      TEMP_label<-GEOM$dataBase$label
      TEMP_area<-sf::st_area(GEOM$dataBase)
      TEMP_perimeter<-lwgeom::st_perimeter(GEOM$dataBase)
      TEMP_roundness<-4*pi*TEMP_area/(TEMP_perimeter^2)
      TEMP_dataFrame<-data.frame(label=TEMP_label,
                                 area = signif(TEMP_area,4),
                                 perimeter = signif(TEMP_perimeter,4),
                                 roundness = signif(TEMP_roundness,4))
      TEMP_totalArea<-aggregate(area~label,TEMP_dataFrame,sum)
      names(TEMP_totalArea)[2]<-'total area'
      TEMP_medianArea<-aggregate(area~label,TEMP_dataFrame,median)
      names(TEMP_medianArea)[2]<-'median area'
      TEMP_medianPerimeter<-aggregate(perimeter~label,TEMP_dataFrame,median)
      names(TEMP_medianPerimeter)[2]<-'median perimeter'
      TEMP_medianRoundness<-aggregate(roundness~label,TEMP_dataFrame,median)
      names(TEMP_medianRoundness)[2]<-'median roundness'
      out_dataFrame<-plyr::join_all(list(TEMP_totalArea,
                                   TEMP_medianArea,
                                   TEMP_medianPerimeter,
                                   TEMP_medianRoundness),by='label')

    })

    LBLTBLV$polygons<-out_dataFrame
  })

  shiny::observeEvent(input$refreshPlots,{
    TEMP_rst<-list(rstStack)
    names(TEMP_rst)<-unique(GEOM$dataBase$uid)
    pixelValue<-extractFeatures(TEMP_rst,GEOM$dataBase,fn_coverage = c(1,0.5),fn_coverage_label = c("X"))
    rm(TEMP_rst)

    pixelValue<-tidyr::pivot_longer(pixelValue$value,names_to='layer',values_to='value',cols=names(rstStack))

    pall<-cbind(paste0('X_',LBLTBLV$table$label),LBLTBLV$table$color)

    ggp<-ggplot2::ggplot(pixelValue)+
      ggplot2::geom_density(ggplot2::aes(x=value,y=..scaled..,color=label),alpha=0.8,position = 'identity',lwd=1)+
      ggplot2::scale_color_manual(values = pall[,2],breaks = pall[,1])+
      ggplot2::scale_x_continuous(trans = scales::modulus_trans(-1))+
      ggplot2::facet_wrap(facets='layer',scales = "free")+
      ggplot2::theme (panel.background = ggplot2::element_rect(fill='black'),
             panel.grid = ggplot2::element_blank(),
             strip.background = ggplot2::element_rect(fill="gray5"),
             strip.text = ggplot2::element_text(colour="white"),
             axis.title = ggplot2::element_blank(),
             axis.text = ggplot2::element_blank(),
             axis.line = ggplot2::element_blank(),
             axis.ticks = ggplot2::element_blank(),
             legend.position = 'bottom',
             legend.key = ggplot2::element_rect(fill = "black"))

    output$Stata<-shiny::renderPlot(ggp)
  })

}
