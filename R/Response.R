Response <- setRefClass(
   'Response',
   fields = c('body','status','headers','length'),
   methods = list(
      initialize = function(body='',status=200,headers=list(),...){
         .self$status <- as.integer(status)
         .self$headers <- as.environment(list('Content-Type'='text/html'))
         if (length(headers) > 0)
            .self$headers <- as.environment(c(as.list(.self$headers),headers))
         if (!is.character(body) && !is.raw(body)){
            base::stop('Body must be a character or raw vector, not',typeof(body))
         }
         .self$body <- body 
         .self$length <- Utils$bytesize(.self$body)
         callSuper(...)
      },
      header = function(key,value) {
         if (missing(value)) headers[[key]]
         else headers[[key]] <<- value
      },
      set_cookie = function(key,value){
         Utils$set_cookie_header(headers,key,value)
      },
      delete_cookie = function(key,value){
         Utils$delete_cookie_header(headers, key, value)
      },
      redirect = function(target,status=302){
         status <<- as.integer(status)
         header('Location',target)
      },
      finish = function(){
         list(
            status=status,
            headers = as.list(headers),
            body = body
         )
      },
      write = function(str){
         s <- paste(as.character(str),collapse='')
         length <<- length + Utils$bytesize(s)
         header('Content-Length',length)
         body <<- paste(body,s,sep='')
      }

   )
)
