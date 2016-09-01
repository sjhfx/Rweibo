##' Search content by word.
##' 
##' @title Search content by word.
##' @param sword The word to be searched.
##' @param page The number of the total pages of the search result. The default value is 1. The limited value is 50.
##' @param combinewith A data.frame of the previous search result. The default value is NULL.
##' @param since If not NULL, restricts weibos to those since the given date. Date should be an object of class \code{\link{POSIXlt}} or string formatted as YYYY-MM-DD
##' @param sinceID If not NULL, returns weibos with IDs greater (ie newer) than the specified ID.
##' @param sleepmean Mean of the sleeping time before each searching.
##' @param sleepsd Standard deviations of the sleeping time before each searching.
##' @param roauth A OAuth object created by \code{\link{createOAuth}}.
##' @param ... Other request parameters for this API.
##' @export
##' @return 
##'  A data.frame of weibos, each weibo contains: 
##'  \item{MID}{Weibo ID of string}
##'  \item{Author}{Nick name of the author}
##'  \item{Weibo}{Weibo content}
##'  \item{Forward}{Forward content}
##'  \item{Time_Weibo}{Time of this weibo}
##'  \item{Time_Search}{Time of this search}
##'  \item{Count_Forward}{Count of replys}
##'  \item{Count_Reply}{Count of forwards}
##'  
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @keywords Search
##' @examples \dontrun{
##' web.search.content("Rweibo")$Weibo
##' web.search.content("Rweibo", sinceID = "3508307023192146")$Weibo
##' web.search.content("Rweibo", since = "2012-10-01")$Weibo
##' }

web.search.content <- function(sword, page = 1, combinewith = NULL, 
		since = NULL, sinceID = NULL, sleepmean = 3, sleepsd = 1, roauth = NULL, ...) {
	if (length(page) == 1) page = 1:page
	page <- page[page > 0 & page <= 50]
	page <- sort(page)
	if (length(page) > 25) page <- page[1:25]
	Search <- TRUE
	ipage <- 1
	if (!is.null(combinewith)) {
		if (all(c("MID", "Author", "Weibo", "Forward", "Time_Weibo", "Time_Search", "Count_Forward", "Count_Reply") %in% names(combinewith))) {
			OUT <- combinewith[, c("MID", "Author", "Weibo", "Forward", "Time_Weibo", "Time_Search", "Count_Forward", "Count_Reply")]
			maxid <- max(as.numeric(OUT$MID))
		} else {
			OUT <- data.frame(stringsAsFactors = FALSE)
			maxid <- 0
			warning("Ignored 'combinewith' because of wrong format!")
		}
	} else {
		OUT <- data.frame(stringsAsFactors = FALSE)
		maxid <- 0
	}
	
	if (!is.null(sinceID)) {
		maxid <- max(maxid, as.numeric(sinceID))
	}
	
	if (is.null(since)) {
		maxdate <- -Inf
	} else {
		if (inherits(since, "character")) {
			since <- strptime(since, format = "%Y-%m-%d")
			if (is.na(since)) {
				warning("Ignore 'since' because of the wrong format!")
				maxdate <- -Inf
			}
		}
		if (inherits(since, "POSIXlt")) maxdate <- since
	}
	
	while (Search && ipage <= length(page)) {
		Sys.sleep(abs(rnorm(1, sleepmean, sleepsd)))
		if (is.null(roauth)) {
			tmp.search <- try(.search.content.anon(sword, page[ipage]), silent = TRUE)
		} else {
			tmp.search <- try(.search.content.curl(sword, page[ipage], roauth$webCurl), silent = TRUE)
		}
		ipage <- ipage + 1
		if (is.null(tmp.search)) {
			cat(paste(ipage - 2, " pages was stored!\n", sep = ""))
			Search <- FALSE
		} else if (inherits(tmp.search, "try-error")){
			warning(paste("Error in page ", ipage - 1, sep = ""))
		} else {
			if (min(as.numeric(tmp.search$MID)) <= maxid || min(tmp.search$Time_Weibo) < maxdate) {
				Search <- FALSE
				tmp.search <- tmp.search[as.numeric(tmp.search$MID) > maxid & tmp.search$Time_Weibo >= maxdate, ]
			}
			OUT <- rbind(tmp.search, OUT)
		}
	}
	OUT <- OUT[order(as.numeric(OUT$MID), decreasing = TRUE), ]
	return(OUT)
}


.search.content.anon <- function(sword, page = 1, ...) {
	requestURL <- "http://s.weibo.com/weibo/"
	sword <- curlEscape(.cntoUTF8(sword))
	strurl <- paste(requestURL, sword, "&xsort=time&page=", page, sep = "") # time sorting 
	pagetree <- htmlParse(strurl)
	
	pagenode <- getNodeSet(pagetree, "//script")
	
	pagescript <- sapply(pagenode, xmlValue)
	weiboline <- pagescript[grep("\"pid\":\"pl_weibo_direct\"", pagescript)]
	weibojson <- gsub("\\)$", "", gsub("^.*STK.pageletM.view\\(", "", weiboline))
	if (length(weibojson) == 0) {
		warning("Can not crawl any page now. May be forbidden by Sina temporarily.", call. = FALSE)
		return(NULL)
	}
	weibolist <- .fromJSON(weibojson)
	
	weibopage <- htmlParse(weibolist[["html"]], asText=TRUE, encoding = "UTF-8")
	
	weiboitem.attr <- getNodeSet(weibopage, "//div[@action-type = 'feed_list_item']")
	weiboitem.name <- getNodeSet(weibopage, "//div[@class = 'feed_content wbcon']//a[@class = 'W_texta W_fb']")
	weiboitem.date <- getNodeSet(weibopage, "//div[@class = 'feed_from W_textb']//a[@title]")
	weiboitem.stat <- getNodeSet(weibopage, "//div[@class = 'feed_action clearfix']//span")
	weiboitem.nores <- getNodeSet(weibopage, "//div[@class = 'pl_noresult']")
	
	if (length(weiboitem.nores) == 0) {
		res.mid <- sapply(weiboitem.attr, function(X) xmlGetAttr(X, "mid"))
		res.name <- sapply(weiboitem.name, function(X) xmlGetAttr(X, "nick-name"))
		Encoding(res.name) <- "UTF-8"
		res.con <- sapply(getNodeSet(weibopage, "//div[@class = 'feed_content wbcon']/p"), xmlValue)
		res.date <- sapply(weiboitem.date, function(X) xmlGetAttr(X, "title"))
		res.stat <- sapply(weiboitem.stat, xmlValue)
		res.con <- .strtrim(res.con)
		res.forward <- .strtrim(res.forward)
		res.date <- strptime(res.date, format = "%Y-%m-%d %H:%M")
		res.stat.f <- as.numeric(gsub("[^0-9]", "", sapply(res.stat, FUN = function(X) X[grep("\u8F6C\u53D1", X)])))
		res.stat.r <- as.numeric(gsub("[^0-9]", "", sapply(res.stat, FUN = function(X) X[grep("\u8BC4\u8BBA", X)])))
		res.stat.f[is.na(res.stat.f)] <- 0
		res.stat.r[is.na(res.stat.r)] <- 0
		
		res.stat.f <- res.stat.f[(1:length(res.mid))*4-2]
		res.stat.r <- res.stat.r[(1:length(res.mid))*4-1]
		res.forward <- as.numeric(gsub("[^0-9]", "", res.stat[(1:length(res.mid))*4]))
		# like counts
		res.forward[is.na(res.forward)] <- 0
		
		OUT <- data.frame(MID = res.mid, Author = res.name, Weibo = res.con, Forward = res.forward, Time_Weibo = res.date,
				Time_Search = Sys.time(), Count_Forward = res.stat.f, Count_Reply = res.stat.r, stringsAsFactors = FALSE)
		OUT$Weibo <- sapply(seq_along(OUT$Weibo), FUN = function(X) 
					gsub(paste("^ *", OUT$Author[X], "\uFF1A *", sep = ""), "", OUT$Weibo[X]))
	} else {
		OUT <- NULL
	}
	
	return(OUT)
}


.search.content.curl <- function(sword, page = 1, ...) {
	requestURL <- "http://s.weibo.com/weibo/"
	sword <- curlEscape(.cntoUTF8(sword))
	strurl <- paste(requestURL, sword, "&xsort=time&page=", page, sep = "") # time sorting 
	
	resXML <- getURL(strurl, .encoding = 'UTF-8')
	resHTMLs <- .strextract(resXML, "<script>.+?</script>")[[1]]
	resHTML <- resHTMLs[grep("\"pid\":\"pl_weibo_direct\"", resHTMLs)][1]
	if (is.na(resHTML)) {
		warning("Can not crawl any page now. May be forbidden by Sina temporarily.", call. = FALSE)
		return(NULL)
	}

	weibojson <- gsub("\\)</script>$", "", gsub("^.*STK.pageletM.view\\(", "", resHTML))
	weibolist <- .fromJSON(weibojson)
	
	weibopage <- htmlParse(weibolist[["html"]], asText=TRUE, encoding = "UTF-8")
	
	weiboitem.attr <- getNodeSet(weibopage, "//div[@action-type = 'feed_list_item']")
	weiboitem.name <- getNodeSet(weibopage, "//div[@class = 'feed_content wbcon']//a[@class = 'W_texta W_fb']")
	weiboitem.date <- getNodeSet(weibopage, "//div[@class = 'feed_from W_textb']//a[@title]")
	weiboitem.stat <- getNodeSet(weibopage, "//div[@class = 'feed_action clearfix']//span")
	weiboitem.nores <- getNodeSet(weibopage, "//div[@class = 'pl_noresult']")
	
	if (length(weiboitem.nores) == 0) {
		res.mid <- sapply(weiboitem.attr, function(X) xmlGetAttr(X, "mid"))
		res.name <- sapply(weiboitem.name, function(X) xmlGetAttr(X, "nick-name"))
		Encoding(res.name) <- "UTF-8"
		res.con <- sapply(getNodeSet(weibopage, "//div[@class = 'feed_content wbcon']/p"), xmlValue)
		res.date <- sapply(weiboitem.date, function(X) xmlGetAttr(X, "title"))
		res.stat <- sapply(weiboitem.stat, xmlValue)
		res.con <- .strtrim(res.con)
		res.forward <- .strtrim(res.forward)
		res.date <- strptime(res.date, format = "%Y-%m-%d %H:%M")
		res.stat.f <- as.numeric(gsub("[^0-9]", "", sapply(res.stat, FUN = function(X) X[grep("\u8F6C\u53D1", X)])))
		res.stat.r <- as.numeric(gsub("[^0-9]", "", sapply(res.stat, FUN = function(X) X[grep("\u8BC4\u8BBA", X)])))
		res.stat.f[is.na(res.stat.f)] <- 0
		res.stat.r[is.na(res.stat.r)] <- 0
		
		res.stat.f <- res.stat.f[(1:length(res.mid))*4-2]
		res.stat.r <- res.stat.r[(1:length(res.mid))*4-1]
		res.forward <- as.numeric(gsub("[^0-9]", "", res.stat[(1:length(res.mid))*4]))
		# like counts
		res.forward[is.na(res.forward)] <- 0
		
		OUT <- data.frame(MID = res.mid, Author = res.name, Weibo = res.con, Forward = res.forward, Time_Weibo = res.date,
				Time_Search = Sys.time(), Count_Forward = res.stat.f, Count_Reply = res.stat.r, stringsAsFactors = FALSE)
		OUT$Weibo <- sapply(seq_along(OUT$Weibo), FUN = function(X) 
					gsub(paste("^ *", OUT$Author[X], "\uFF1A *", sep = ""), "", OUT$Weibo[X]))
	} else {
		OUT <- NULL
	}
	
	return(OUT)
}
