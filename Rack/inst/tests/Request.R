library(Rack)
env <- new.env()
env[['CONTENT_TYPE']] <- 'text/html;charset=utf-8'
x <- Rack::Request$new(env)
x$media_type()
x$media_type_params()
x$content_charset()
env[['HTTP_X_FORWARDED_HOST']] <- 'foo, 127.0.0.1:8080'
env[['SERVER_PORT']] <- '8080'
x$host_with_port()
rm('HTTP_X_FORWARDED_HOST',envir=env)
env[['HTTP_HOST']] <- 'localhost:80'
x$host_with_port()
rm('HTTP_HOST',envir=env)
env[['SERVER_NAME']] <- 'localhost'
x$host_with_port()
rm('SERVER_NAME',envir=env)
env[['SERVER_ADDR']] <- '127.1.1.1'
x$host_with_port()
x$host()
