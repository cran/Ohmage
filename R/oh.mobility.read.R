#' Read Mobility data
#' 
#' @param date date in ISO format
#' @param username name of user to query (only works when server allows to see others data). 
#' @param column_list variables to be returned
#' @param ... stuff passed to oh.call
#' @return a dataframe with mobility data
#' @export
oh.mobility.read <- function(date = today(), username=getOption("ohmage_username"), column_list, ...){
	if(missing(column_list)){
		column_list <- c("mobility:id", "location", "mobility:mode", "mobility:timestamp", "mobility:classifier_data");
	}
	if(is.character(date) && nchar(date) != "10"){
		stop("Date has to be in format YYYY-mm-dd");
	} 
	if("Date" %in% class(date)){
		date <- as.character(date);
	}
	if("POSIXt" %in% class(date)){
		date <- as.character(as.Date(date));
	}

	xhr <- oh.call("/mobility/read", date=date, username=username, column_list=paste(column_list, collapse=","), ...);		
	
	output <- records2df(xhr$data, c("id", "m", "ts", "l.lo", "l.la", "l.ac", "cd.m"))
	
	output <- rename(output, "l.lo", "lo")
	output <- rename(output, "l.la", "la")
	output <- rename(output, "l.ac", "ac")
	output$ts <- strptime(output$ts, format="%Y-%m-%d %H:%M:%S");

	#if("mobility:sensor_data" %in% column_list){
	#	output$t  <- structure(as.numeric(unlist(lapply(xhr$data, "[[", "t")))/1000, class=class(Sys.time()));		
	#	output$speed <- as.numeric(unlist(lapply(lapply(xhr$data, "[[", "data"), "[[", "sp")));
	#}
	
	#sort
	if(nrow(output) > 0){
		output <- output[order(output$ts),];
	}
	
	return(output);
}
