flatlist <- function(mylist){
	lapply(rapply(mylist, base::enquote, how="unlist"), eval)
}

records2df <- function(recordlist, columns) {
	if(length(recordlist)==0 && !missing(columns)){
		return(as.data.frame(matrix(ncol=length(columns), nrow=0, dimnames=list(NULL,columns))))
	}	
	
	un <- lapply(recordlist, flatlist)
	if(!missing(columns)){
		ns <- columns;
	} else {
		ns <- unique(unlist(lapply(un, names)))
	}

	un <- lapply(un, function(x) {
		y <- as.list(x)[ns]
		names(y) <- ns
		lapply(y, function(z) if(is.null(z)) NA else z)})
	s <- lapply(ns, function(x) sapply(un, "[[", x))
	names(s) <- ns
	data.frame(s, stringsAsFactors=FALSE)
}

rename <- function(mydf, from, to){
	index <- which(names(mydf) == from)
	if(length(index) == 0) stop("Name:", from, "not found in dataframe.")
	names(mydf)[index] <- to;
	mydf
}