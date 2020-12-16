#'Some BullShit
#'
#'
XMLparseObject<-function(x){

  slts<-ls(x)
  xmlDoc<-XML::newXMLDoc()
  root<-XML::newXMLNode('object',
                        attrs = c(class=class(x),
                                  crtnTimeStmp=attr(x,'crtnTimeStmp'),
                                  mdfnTimeStmp=attr(x,'mdfnTimeStmp'),
                                  arcnTimeStmp=attr(x,'arcnTimeStmp')),
                        doc = xmlDoc)
  objectS<-sapply(slts,function(s){

    instObj<-eval(parse(text=s),envir = x)
    sltClass<-class(instObj)
    sltTypeOf<-typeof(instObj)
    crtnTimeStmp<-attr(instObj,'crtnTimeStmp')
    mdfnTimeStmp<-attr(instObj,'mdtnTimeStmp')
    arcnTimeStmp<-attr(instObj,'artnTimeStmp')
    fileArchive<-attr(instObj,'fileArchive')
    if (any(class(instObj)=='character')) content<-instObj else content<-'xFile'
    XML::newXMLNode(s,
                    content,
                    attrs = c(class=sltClass,
                              type=sltTypeOf,
                              crtnTimeStmp=crtnTimeStmp,
                              mdfnTimeStmp=mdfnTimeStmp,
                              arcnTimeStmp=arcnTimeStmp),
                    parent=root)

  },USE.NAMES = T)
  return(xmlDoc)
}
