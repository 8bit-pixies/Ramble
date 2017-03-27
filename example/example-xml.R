#' XML parser example

xml = '<complexType name="SubjectType">
    <choice>
        <sequence>
            <choice>
                <element ref="saml:BaseID"/>
                <element ref="saml:NameID"/>
                <element ref="saml:EncryptedID"/>
            </choice>
            <element ref="saml:SubjectConfirmation" minOccurs="0" maxOccurs="unbounded"/>
        </sequence>
        <element ref="saml:SubjectConfirmation" maxOccurs="unbounded"/>
    </choice>
</complexType>'


xmlParser <- (many(startTag %alt% singleTag) %then%
          many(endTag %alt% singleTag))


startTag <- (
  symbol("<") %then%
    identifier() %then% 
    many(attributes) %then%
  symbol(">") %using% function(x) {
    els <- unlist(c(x))
    #return(unlist(c(x)))
    return(list(name=els[2], all=els))
  }
)

endTag <- (
  symbol("</") %then%
    identifier() %then% 
  symbol(">") %using% function(x) {
    return(unlist(c(x)))
  }
  )

singleTag <- (
  symbol("<") %then%
    identifier() %then% 
    many(attributes) %then%
    symbol("/>") %using% function(x) {
      els <- unlist(c(x))
      return(unlist(c(x)))
    }
)

attributes <- (
  identifier() %then% 
    symbol("=") %then%
    quoteString
  )

quoteString <- (
  symbol('"') %then%
    many(satisfy(function(x) {return(!!length(grep('[^"]+', x)))})) %then%
    symbol('"') %using% function(x) {
      return(paste0(unlist(c(x)), collapse=""))
    } 
  )

xmlParser(xml)


