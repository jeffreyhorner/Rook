# Server object available to web servers to set how they please.
# Must be set in onLoad. After that, they are locked.
Server <- NULL

.onLoad <- function(libpath, pkgname){
    if ('(embedding)' %in% names(getLoadedDLLs()) && 'rapache' %in% search()){
	sys.source(
	    file.path(libpath,pkgname,'servers','rApache.R'),
	    envir = asNamespace('Rook')
	)
    }
}
