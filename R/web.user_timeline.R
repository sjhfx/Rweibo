


##' Return the weibos of one user by web parsing.
##' 
##' @title Return the weibos of one user by web parsing.
##' @param roauth A OAuth object created by \code{\link{createOAuth}}.
##' @param uid User ID to be queried.
##' @param pages The pages of the search result. The default value is 1.
##' @param wv The default value is 5.
##' @param sleepmean Mean of the sleeping time before each searching. The default value is 3.
##' @param sleepsd Standard deviations of the sleeping time before each searching. The default value is 1.
##' @export
##' @return 
##'  A data.frame of weibos, each weibo contains: 
##'  \item{MID}{Weibo ID of string}
##'  \item{Author}{Nick name of the author}
##'  \item{UID}{User ID}
##'  \item{Weibo}{Weibo content}
##'  \item{Forward}{Forward content}
##'  \item{Time_Weibo}{Time of this weibo}
##'  \item{Time_Search}{Time of this search}
##'  \item{Count_Forward}{Count of replys}
##'  \item{Count_Reply}{Count of forwards}
##'  \item{Client}{Client of the weibo}
##'  
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @keywords Search
##' @examples \dontrun{
##' u1 <- web.search.user("lijian001")
##' w1 <- web.user_timeline(roauth, uid = u1$uid, pages = 1:2)
##' }

web.user_timeline <- function(roauth, uid, pages = 1, wv = 5, 
		sleepmean = 3, sleepsd = 1) 
{
	uid <- as.character(uid)
	if (uid == roauth$webUser) stop("Can't search the current user, please change an account to login.")
	ftype <- "weibo"
	page0 <- min(pages)
	time1 <- Sys.time()
	res0 <- .web.user.get0(uid, roauth$webCurl, wv = wv, page = page0, ftype = ftype)
	time2 <- Sys.time()
	if (is.null(res0$df)) stop("The pages were out of range!", call. = FALSE)
	timecost0 <-  as.numeric(difftime(time2, time1, units="secs"))
	endid0 <- res0$df$MID[1]
	weibonum0 <- res0$info$profile
	name0 <- res0$info$name
	pages <- sort(intersect(pages, 1:ceiling(weibonum0 / 45)))
	estimatedtime <- round(length(pages) * (sleepmean + timecost0) / 60, 2)
	cat(paste(length(pages), " pages will be crawled in ", estimatedtime, " minutes ...\n", sep = ""))
	outDf <- res0$df
	
	for (i in seq_along(pages)) {
		ipage <- pages[i]
		# part1
		if (i > 1) {
			tmphtml <- .web.user.get1(uid, roauth$webCurl, endid = endid0, wv = wv, 
					page = ipage, prepage = pages[i - 1], ftype = ftype)
			tmpDf <- .web.user_timeline(tmphtml)
			if (is.null(tmpDf)) return(outDf)
			outDf <- rbind(outDf, tmpDf)
		}
		# part2
		tmphtml <- .web.user.get2(uid, roauth$webCurl, maxid = outDf$MID[length(outDf$MID)], 
				endid = endid0, wv = wv, page = ipage, ftype = ftype)
		tmpDf <- .web.user_timeline(tmphtml)
		if (is.null(tmpDf)) return(outDf)
		outDf <- rbind(outDf, tmpDf)
		# part3
		tmphtml <- .web.user.get3(uid, roauth$webCurl, maxid = outDf$MID[length(outDf$MID)], 
				endid = endid0, wv = wv, page = ipage, ftype = ftype)
		tmpDf <- .web.user_timeline(tmphtml)
		if (is.null(tmpDf)) return(outDf)
		outDf <- rbind(outDf, tmpDf)
		Sys.sleep(abs(rnorm(1, sleepmean, sleepsd)))
	}
	
	outDf$Author <- name0
	outDf$UID <- as.character(uid)
	time3 <- Sys.time()
	actualtime <- round(as.numeric(difftime(time3, time1, units="secs")) / 60, 2)
	cat(paste(length(pages), " pages were crawled in ", actualtime, " minutes!\n", sep = ""))
	
	outDf$Client <- sapply(strsplit(outDf$Client, split = " "), FUN = function(X) X[1])
	return(outDf)
}



.web.user.get0 <- function(uid, curl, wv = 5, page = 1, ftype = c("all", "weibo")) {
	ftype <- match.arg(ftype)
	profile_ftype <- ifelse(ftype == "all", "", "?profile_ftype=1")
	strurl <- paste("http://weibo.com/", uid, profile_ftype, "?page&page=", page, sep = "")
	resXML <- getURL(strurl, curl = curl, .encoding = 'UTF-8')
	resHTMLs <- .strextract(resXML, "<script>.+?</script>")[[1]]
	resHTML <- resHTMLs[grep("\"domid\":\"Pl_Core_OwnerFeed", resHTMLs)][1]
	nameHTML <- resHTMLs[grep("pl.header.head.index", resHTMLs)][1]
	infoHTML <- resHTMLs[grep("pl.header.head.index", resHTMLs)][1]
	if (is.na(resHTML)) {
		warning("Can not crawl any page now. May be forbidden by Sina temporarily.", call. = FALSE)
		return(NULL)
	}
	weibojson <- gsub("\\)</script>$", "", gsub("^.*FM\\.view\\(", "", resHTML))
	weibolist <- .fromJSON(weibojson)
	
	infojson <- gsub("\\)</script>$", "", gsub("^.*FM\\.view\\(", "", infoHTML))
	infolist <- .fromJSON(infojson)
	
	#namejson <- gsub("\\)</script>$", "", gsub("^.*STK.pageletM.view\\(", "", nameHTML))
	#namelist <- .fromJSON(namejson)
	
	OUT <- list()
	OUT[["df"]] <- .web.user_timeline(weibolist[["html"]])
	
	infopage <- htmlParse(infolist[["html"]], asText=TRUE, encoding = "UTF-8")
	#infonum <- as.numeric(gsub("[^0-9]", "", sapply(getNodeSet(infopage, "//ul/li"), xmlValue)))
	#namepage <- htmlParse(namelist[["html"]], asText=TRUE, encoding = "UTF-8")
 
	OUT[["info"]] <- list(
			name = xmlValue(getNodeSet(infopage, "//span[@class='name']")[[1]])[1],
			follow =  as.numeric(.strextract(xmlValue(getNodeSet(infopage, "//li[@class='S_line1']")[[1]])[1], "[0-9]+")[[1]][1]),
			fan = as.numeric(.strextract(xmlValue(getNodeSet(infopage, "//li[@class='follower S_line1']")[[1]])[1], "[0-9]+")[[1]][1]),
			profile = as.numeric(.strextract(xmlValue(getNodeSet(infopage, "//li[@class='W_no_border']")[[1]])[1], "[0-9]+")[[1]][1])
	)

	return(OUT)
}


.web.user.get1 <- function(uid, curl, endid, wv = 5, page = 2, prepage = 1, ftype = c("all", "weibo")) {
	ftype <- match.arg(ftype)
	profile_ftype <- ifelse(ftype == "all", "", "&profile_ftype=1")
	strurl <- "http://weibo.com/aj/mblog/mbloglist"
	strurl <- paste(strurl, "?_wv=", wv, profile_ftype, "&page=", page, "&uid=", uid, 
			"&count=50&pre_page=", prepage, "&end_id=", endid,
			sep = "")
	resJson <- getURL(strurl, curl = curl, .encoding = 'UTF-8')
	weibolist <- .fromJSON(resJson)
	return(weibolist[["data"]])
}


.web.user.get2 <- function(uid, curl, maxid, endid, wv = 5, page = 1, ftype = c("all", "weibo")) {
	ftype <- match.arg(ftype)
	profile_ftype <- ifelse(ftype == "all", "", "&profile_ftype=1")
	strurl <- "http://weibo.com/aj/mblog/mbloglist"
	strurl <- paste(strurl, "?_wv=", wv, profile_ftype, "&page=", page, "&uid=", uid, 
			"&count=15&pre_page=", page, "&pagebar=0&max_id=", maxid, "&end_id=", endid,
			sep = "")
	resJson <- getURL(strurl, curl = curl, .encoding = 'UTF-8')
	weibolist <- .fromJSON(resJson)
	return(weibolist[["data"]])
}


.web.user.get3 <- function(uid, curl, maxid, endid, wv = 5, page = 1, ftype = c("all", "weibo")) {
	ftype <- match.arg(ftype)
	profile_ftype <- ifelse(ftype == "all", "", "&profile_ftype=1")
	strurl <- "http://weibo.com/aj/mblog/mbloglist"
	strurl <- paste(strurl, "?_wv=", wv, profile_ftype, "&page=", page, "&uid=", uid, 
			"&count=15&pre_page=", page, "&pagebar=1&max_id=", maxid, "&end_id=", endid,
			sep = "")
	resJson <- getURL(strurl, curl = curl, .encoding = 'UTF-8')
	weibolist <- .fromJSON(resJson)
	return(weibolist[["data"]])
}



.web.user_timeline <- function(strhtml) {
		
	weibopage <- htmlParse(strhtml, asText=TRUE, encoding = "UTF-8")
	weiboitem.con <- getNodeSet(weibopage, "//div[@action-type='feed_list_item']")
	
	if( length(weiboitem.con) == 0) return(NULL)
	
	res.mid <- sapply(weiboitem.con, function(X) xmlGetAttr(X, "mid"))
	res.con <- sapply(weiboitem.con, FUN = function(X) xmlValue(getNodeSet(X, "div/div/div[@node-type='feed_list_content']")[[1]]))
	res.name <- ""
	res.uid <- ""
	res.date <- sapply(weiboitem.con, FUN = function(X) xmlGetAttr(getNodeSet(X, "div/div/div/div[@class = 'WB_from']/a")[[1]], "title"))
	res.stat <- lapply(weiboitem.con, FUN = function(X) sapply(getNodeSet(X, "div/div/div/div[@class='WB_handle']/a"), xmlValue))
	res.forward <- sapply(weiboitem.con, FUN = function(X) {
				tmp.node <- getNodeSet(X, "div/div/div/div/div[@node-type='feed_list_reason']")
				if (length(tmp.node) == 0) {
					NA
				} else {
					xmlValue(tmp.node[[1]])
				}
			}
	)
	res.client <- sapply(weiboitem.con, FUN = function(X) xmlValue(getNodeSet(X, "div/div/div/div[@class = 'WB_from']")[[1]]))
	res.client <- .strtrim(gsub("\t.*$", "", gsub("^.*?\u6765\u81EA", "", res.client)))
	res.con <- .strtrim(res.con)
	res.forward <- .strtrim(res.forward)
	res.date <- strptime(res.date, format = "%Y-%m-%d %H:%M")
	res.stat.f <- as.numeric(gsub("[^0-9]", "", sapply(res.stat, FUN = function(X) X[grep("\u8F6C\u53D1", X)])))
	res.stat.r <- as.numeric(gsub("[^0-9]", "", sapply(res.stat, FUN = function(X) X[grep("\u8BC4\u8BBA", X)])))
	res.stat.f[is.na(res.stat.f)] <- 0
	res.stat.r[is.na(res.stat.r)] <- 0
		
	OUT <- data.frame(MID = res.mid, Author = res.name, UID = res.uid, Weibo = res.con, Forward = res.forward, 
			Time_Weibo = res.date, Time_Search = Sys.time(), Count_Forward = res.stat.f, Count_Reply = res.stat.r, 
			Client = res.client, stringsAsFactors = FALSE
	)

	return(OUT)
}


