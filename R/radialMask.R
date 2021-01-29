
radialMask<-function(fn_radius,
                     fn_Nspikes){
  rds<-seq(1,fn_radius)
  ang<-seq(0,2*pi,2*pi/fn_Nspikes)
  ang<-ang[1:(length(ang)-1)]
  mat<-expand.grid(rds,ang)
  sinMatrix<-matrix(apply(mat,1,function(x){round(sin(x[2])*x[1])}),ncol=(length(rds)),nrow=(length(ang)),byrow = T)
  cosMatrix<-matrix(apply(mat,1,function(x){round(cos(x[2])*x[1])}),ncol=(length(rds)),nrow=(length(ang)),byrow = T)

  centerPoint<-sf::st_point(x=c(0,0),dim='XY')
  segmentRadius<-lapply(ang,function(ang){
    sf::st_linestring(matrix(c(c(0,sin(ang)*(fn_radius+1)),
                               c(0,cos(ang)*(fn_radius+1))),
                             ncol=2),
                      dim='XY')})

  pixelBox<-lapply(1:nrow(mat),function(x){
    xPxCenter<-round(sin(mat[x,2])*mat[x,1])
    yPxCenter<-round(cos(mat[x,2])*mat[x,1])
    pixelBox<-matrix(c(c(xPxCenter-0.5,xPxCenter+0.5,xPxCenter+0.5,xPxCenter-0.5,xPxCenter-0.5),
                       c(yPxCenter+0.5,yPxCenter+0.5,yPxCenter-0.5,yPxCenter-0.5,yPxCenter+0.5)),ncol=2)
    pixelBox<-sf::st_linestring(pixelBox,dim='XY')
  })

  pixelBox<-matrix(pixelBox,ncol=fn_radius,byrow = T)
  intersectionMatrix<-matrix(list(),ncol=fn_radius,nrow = fn_Nspikes*2)
  for (spike in 1:(fn_Nspikes)){
    for (rad in 1:(fn_radius)){
      pB<-pixelBox[spike,rad][[1]]
      sG<-segmentRadius[[spike]]
      intersectionsCoords<-sf::st_intersection(sG,pB,flatten=F)
      intersectionsCoords<-matrix(unlist(lapply(intersectionsCoords,function(x) x)),ncol=2)

      distEstimate<-apply(intersectionsCoords,1,function (x){
        sf::st_distance(centerPoint,sf::st_point(x=as.vector(x)),which='Euclidean')})
      if (length(distEstimate)==1){
        intersectionsCoords<-rbind(intersectionsCoords,intersectionsCoords)
        distEstimate<-c(distEstimate,distEstimate)
      }
      if (length(distEstimate)==0){intersectionsCoords=NULL}
      intersectionMatrix[spike,rad]<-list(intersectionsCoords[order(distEstimate),])
    }
  }
  out<-list(sinMatrix=sinMatrix,
            cosMatrix=cosMatrix,
            intersectionMatrix=intersectionMatrix,
            pixelBox=pixelBox)
}
