#'Some nice thing
#'
#'
XMLparseObject<-function(x=NULL,objectType=NULL){

  if (is.null(x)) stop(mError('Specify an object to serialise'))
  if (is.null(objectType)) stop(mError('Specify an object Type'))
  if (!any(objectType %in% c('study','analysis'))) stop(mError('Object type must be either study or analysis'))

  slts<-ls(x)
  xmlDoc<-XML::newXMLDoc()
  root<-XML::newXMLNode(objectType,
                        attrs = c(class=class(x),
                                  crtnTimeStmp=attr(x,'crtnTimeStmp'),
                                  mdtnTimeStmp=attr(x,'mdtnTimeStmp'),
                                  artnTimeStmp=attr(x,'artnTimeStmp'),
                                  fileArchive=attr(x,'fileArchive')),
                        doc = xmlDoc)
  objectS<-sapply(slts,function(s){

    instObj<-eval(parse(text=s),envir = x)
    sltClass<-class(instObj)
    sltTypeOf<-typeof(instObj)
    crtnTimeStmp<-attr(instObj,'crtnTimeStmp')
    mdtnTimeStmp<-attr(instObj,'mdtnTimeStmp')
    artnTimeStmp<-attr(instObj,'artnTimeStmp')
    fileArchive<-attr(instObj,'fileArchive')
    if (any(class(instObj)=='character')) content<-paste0(instObj,collapse = ",") else content<-fileArchive
    XML::newXMLNode(s,
                    content,
                    attrs = c(class=sltClass,
                              type=sltTypeOf,
                              crtnTimeStmp=crtnTimeStmp,
                              mdtnTimeStmp=mdtnTimeStmp,
                              artnTimeStmp=artnTimeStmp),
                    parent=root)

  },USE.NAMES = T)
  return(xmlDoc)
}
