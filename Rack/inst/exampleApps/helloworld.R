app <- function(env){
    req <- Rack::Request$new(env)
    res <- Rack::Response$new()
    friend <- 'World'
    if (!is.null(req$GET()[['friend']]))
	friend <- req$GET()[['friend']]
    res$write(paste('<h1>Hello',friend,'</h1>\n'))
    res$write('What is your name?\n')
    res$write('<form method="GET">\n')
    res$write('<input type="text" name="friend">\n')
    res$write('<input type="submit" name="Submit">\n</form>\n<br>')
    res$finish()
}
