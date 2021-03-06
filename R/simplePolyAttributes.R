.dist<-function(x){
  sqrt((x[1,1]-x[2,1])^2+(x[1,2]-x[2,2])^2)
}

.perimeter<-function(x){
  dd<-sapply(1:(nrow(x)-1),function(y){
    .dist(x[y:(y+1),])},simplify = T,USE.NAMES = F)
  sum(dd)
}

.area<-function(x){
  nr<-nrow(x)
  down<-sum(x[1:(nr-1),1]*x[2:nr,2])
  up<-sum(x[1:(nr-1),2]*x[2:nr,1])
  abs(down-up)/2
}

.roundness<-function(area,perimeter){
  4*pi*area/(perimeter^2)
}
