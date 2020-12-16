#'Some BullShit
#'
#'
#' @export
autoTreshold<-function(fn_df,
                        fn_marker,
                        fn_maxNOfEvents=100,
                        fn_tol=1e-5,
                        fn_expansion=1,
                        fn_seed=1234){
  fn_df<-as.data.frame(fn_df)
  TEMP_x=fn_df[,fn_marker]
  TEMP_y=rank(fn_df[,fn_marker])
  trns<-scales::modulus_trans(2)
  df<-data.frame(x=trns$transform(TEMP_x),y=TEMP_y)
  df<-df[df$x!=-Inf,]

  if (fn_maxNOfEvents>nrow(df)) {fn_maxNOfEvents<-nrow(df)}
  set.seed(fn_seed)
  df<-df[(sample(1:nrow(df),fn_maxNOfEvents)),]

  #
  m <- stats::nls(df$y ~ a + ((b - a)/(1 + exp(-c * (df$x - d)))), data=df,
           start = list(a = min(df$y),
                        b = max(df$y),
                        c = 1,
                        d = round(median(df$x))),
           trace = TRUE,nls.control(maxiter=1000,minFactor = 1e-10))

  ex<-expression(a + ((b - a)/(1 + exp(-c * (x - d)))))
  dd<-D(ex,"x")
  dd2<-D(dd,"x")
  cc<-coef(m)
  a=unname(cc[1])
  b=unname(cc[2])
  c=unname(cc[3])
  d=unname(cc[4])

  ff<-function(x)eval(dd2)
  #
  set.seed(fn_seed)
  minMax<-stats::optimise(ff,lower = d,upper = d+(fn_expansion*abs((max(df$x)))),tol = fn_tol)
  tresholdSuspect<-trns$inverse(minMax[[1]])
  #
  plot(df$x,df$y,ylim=c(-max(df$y),max(df$y)))
  points(df$x,predict(m,df$x),col='red')
  points(df$x,ff(df$x),col='green')
  abline(v=minMax$minimum,col='orange')
  text(x=minMax$minimum,y=-max(df$y)/2,as.character(signif(tresholdSuspect,3)))

  return(list(trsh=tresholdSuspect,
              mdl=m,
              dd1=dd,
              dd2=dd2,
              cc=cc))
}
