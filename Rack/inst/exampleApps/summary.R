app <- function(env) {
    req <- Rack::Request$new(env)
    res <- Rack::Response$new()
    res$write('Choose a CSV file:\n')
    res$write('<form method="POST" enctype="multipart/form-data">\n')
    res$write('<input type="file" name="data">\n')
    res$write('<input type="submit" name="Upload">\n</form>\n<br>')

    if (!is.null(req$POST())){
	data <- req$POST()[['data']]
	res$write("<h3>Summary of Data</h3>");
	res$write("<pre>")
	res$write(paste(capture.output(summary(read.csv(data$tempfile,stringsAsFactors=FALSE)),file=NULL),collapse='\n'))
	res$write("</pre>")
	res$write("<h3>First few lines (head())</h3>");
	res$write("<pre>")
	res$write(paste(capture.output(head(read.csv(data$tempfile,stringsAsFactors=FALSE)),file=NULL),collapse='\n'))
	res$write("</pre>")
    }
    res$finish()
}
