#' @export
#' 

.get <- function(strurl, oauthKey, params = character(), curl = getCurlHandle()) 
{
	params <- lapply(params, FUN = function(X) if (inherits(X, "name") || inherits(X, "call")) eval(X) else X)
	params["access_token"] <- oauthKey
	params <- params[order(names(params))]
	if (!is.null(params[["status"]])) params[["status"]] <- .cntoUTF8(curlPercentEncode(params[["status"]]))
	if (!is.null(params[["comment"]])) params[["comment"]] <- .cntoUTF8(curlPercentEncode(params[["comment"]]))
	if (!is.null(params[["screen_name"]])) params[["screen_name"]] <- .cntoUTF8(curlPercentEncode(params[["screen_name"]]))
	
	fields <- paste(names(params), sapply(params, curlPercentEncode), sep = "=", collapse = "&") 
	strurl <- paste(strurl, fields, sep ="?")

	OUT <- getURL(strurl, ssl.verifypeer = FALSE, curl = curl, .encoding = "UTF-8")
	OUT <- .fromJSON(OUT)
	
	return(OUT)
}

.post <- function(strurl, oauthKey, params = character(), curl = getCurlHandle(), transjson = TRUE) 
{
	params <- lapply(params, FUN = function(X) if (inherits(X, "name") || inherits(X, "call")) eval(X) else X)
	
	if(is.null(curl)) curl <- getCurlHandle()
	
	reader <- dynCurlReader(curl, baseURL = strurl, verbose = FALSE)
	
	params <- lapply(params, as.character)
	params[["access_token"]] <- oauthKey
	params <- params[order(names(params))]
	if (!is.null(params[["status"]])) params[["status"]] <- .cntoUTF8(curlPercentEncode(params[["status"]]))
	if (!is.null(params[["comment"]])) params[["comment"]] <- .cntoUTF8(curlPercentEncode(params[["comment"]]))
	if (!is.null(params[["screen_name"]])) params[["screen_name"]] <- .cntoUTF8(curlPercentEncode(params[["screen_name"]]))
	
	fields <- paste(names(params), unlist(params), sep = "=", collapse = "&")  
	
	curlPerform(curl = curl, URL = strurl, postfields = fields, writefunction = reader$update, ssl.verifypeer = FALSE)
	OUT <- reader$value()
	if (transjson) OUT <- .fromJSON(OUT)
	
	return(OUT)
}

.cntoUTF8 <- function(strcn) {
	OUT <- paste(strcn, "Rweibo", sep = "")
	OUT <- gsub("Rweibo$", "", OUT)
	return(OUT)
}

.setCallback <- function() {
	config <- readLines(file.path(system.file(package = "Rweibo"), "config", "Rweibo.txt"))
	port <- try(as.numeric(gsub("port:", "", config[grep("port", config)])), silent = TRUE)
	if (inherits(port, "try-error") || is.na(port)) {
		port <- 80
		cat(paste("Format of", 
						file.path(system.file(package = "Rweibo"), "config", "Rweibo.txt"),
						"is wrong!\n"))
	}
	if (port == 80) {
		struri <- "http://sjhfx.cc/callback.html"
	} else {
		struri <- paste("http://sjhfx.cc:", port, "/callback.html", sep = "")
	}
	options(redirect_uri = struri)
	cat(paste("# The port of help server was set to", port, "\n"))
	
	.setHttpPort(port)
		
	if (!file.exists(file.path(system.file(package = "Rweibo"), "doc", "callback.html"))) {
		file.copy(file.path(system.file(package = "Rweibo"), "config", "callback.html"), 
				file.path(system.file(package = "Rweibo"), "doc"))
	}

}

.setHttpPort <- function(port) {
	port <- try(as.integer(port), silent = TRUE)
	if (is.na(port)) stop("Not integer!")
	options(help.ports = port)
	try(startDynamicHelp(start = FALSE), silent = TRUE)
	try(startDynamicHelp(), silent = TRUE)
}



.strextract <- function(string, pattern, invert = FALSE,
		ignore.case = FALSE, perl = FALSE, useBytes = FALSE) 
{
	expr <- gregexpr(pattern = pattern, text = string, ignore.case = ignore.case, 
			perl = perl, fixed = FALSE, useBytes = useBytes)
	OUT <- regmatches(x = string, m = expr, invert = invert)
	return(OUT)
}

.strtrim <- function(string, side = c("both", "left", "right")) {
	side <- match.arg(side)
	pattern <- switch(side, left = "^\\s+", right = "\\s+$", both = "^\\s+|\\s+$")
	OUT <- gsub(pattern, "", string)
	return(OUT)
}

.hourtime <- function(hours) {
	outTimeN <- (as.numeric(Sys.time()) %/% 3600 + hours) * 3600
	outTime <- as.POSIXct(outTimeN, origin="1970-01-01 08:00:00")
	return(format(outTime, "%Y-%m-%d %H:%M:%S"))
}

.inttodate <- function(intD, type = c("String", "Time"), origin = "1970-01-01 08:00:00") {
	type = match.arg(type)
	intD <- as.numeric(intD)
	intD[nchar(intD) == 13] <- intD[nchar(intD) == 13] / 1000
	T <- as.POSIXct(intD, origin=origin)
	if (type == "Time") return(T)
	if (type == "String") return(format(T, "%Y-%m-%d %H:%M:%S"))
}

.fromJSON <- function(json, api = c("rjson", "RJSONIO"), ...) {
	api <- match.arg(api)
	iscontent <- inherits(json, "AsIs") || (!file.exists(json) && length(grep("^[[:space:]]*[[{]", json)))
	
	if (api == "rjson") {
		if (iscontent) {
			OUT <- rjson:::fromJSON(json_str = json)
		} else {
			OUT <- rjson:::fromJSON(file = json)
		}
	}
	
	if (api == "RJSONIO") {
		OUT <- RJSONIO:::fromJSON(content = json, ...)
	}
	
	return(OUT)
}

.parseUserList <- function(UserList, prefix = "") {
	ColName <- c("idstr", "screen_name", "province", "city", "location", "description", 
			"gender", "followers_count", "friends_count", "statuses_count",
			"favourites_count", "geo_enabled", "created_at", 
			"following", "follow_me", "bi_followers_count", 					
			"verified", "verified_type", "verified_reason")

	if (is.null(UserList)) {
		OUT <- as.data.frame(t(rep(NA, length(ColName))), stringsAsFactors = FALSE)
		names(OUT) <- ColName
	} else {
		OUT <- as.data.frame(t(unlist(UserList[ColName])), stringsAsFactors = FALSE)
	}
	
	if (nzchar(prefix)) names(OUT) <- paste(prefix, names(OUT), sep = "_")
	return(OUT)
}

.parseRepostList <- function(RepostList, prefix = "") {
	ColName <- c("created_at", "mid", "text", "reposts_count", "comments_count", 
			"attitudes_count" , "in_reply_to_status_id", "in_reply_to_user_id", 
			"in_reply_to_screen_name")
	
	if (is.null(RepostList)) {
		OUT <- as.data.frame(t(rep(NA, length(ColName))), stringsAsFactors = FALSE)
		names(OUT) <- ColName
	} else {
		tmp.list <- RepostList[ColName]
		names(tmp.list) <- ColName
		tmp.list <- lapply(tmp.list, FUN = function(X) if (is.null(X)) NA else X)
		OUT <- as.data.frame(t(unlist(tmp.list)), stringsAsFactors = FALSE)
	}
	
	if (nzchar(prefix)) names(OUT) <- paste(prefix, names(OUT), sep = "_")
	return(OUT)
}

.parseCommentList <- function(CommentList, prefix = "") {
	ColName <- c("created_at", "mid", "text", "source")
	
	if (is.null(CommentList)) {
		OUT <- as.data.frame(t(rep(NA, length(ColName))), stringsAsFactors = FALSE)
		names(OUT) <- ColName
	} else {
		tmp.list <- CommentList[ColName]
		names(tmp.list) <- ColName
		tmp.list <- lapply(tmp.list, FUN = function(X) if (is.null(X)) NA else X)
		OUT <- as.data.frame(t(unlist(tmp.list)), stringsAsFactors = FALSE)
	}
	
	if (nzchar(prefix)) names(OUT) <- paste(prefix, names(OUT), sep = "_")
	return(OUT)
}

.encryptPwd <- function(oripwd, servertime, nonce, pubkey) {
	#OUT <- digest(oripwd, algo= "sha1", serialize = FALSE)
	#OUT <- digest(OUT, algo= "sha1", serialize = FALSE)
	#OUT <- paste(OUT, as.character(servertime), nonce, sep = "")
	#OUT <- digest(OUT, algo= "sha1", serialize = FALSE)
	#return(OUT)
	rsaPublickey = .hextoint(pubkey)
	key.pub = PKI:::PKI.mkRSApubkey(rsaPublickey, exponent=65537L, format = "key")
	raw.message = charToRaw(paste(servertime, "\t", nonce, "\n", oripwd, sep = ""))
	str.pwd <- PKI:::PKI.encrypt(raw.message, key.pub)
	PKI:::raw2hex(str.pwd, sep = "")
	
}

.hextoint <- function(h) {
	xx = strsplit(tolower(h), "")[[1L]]
	pos = match(xx, c(0L:9L, letters[1L:6L]))
	sum((pos - 1L) * 16^gmp:::as.bigz(rev(seq_along(xx) - 1)))
}


