


##' Search user by nick name.
##' 
##' @title Search user by nick name.
##' @param screen_name Nick name of a user.
##' @param roauth A OAuth object created by \code{\link{createOAuth}}.
##' @export
##' @return 
##'  A data.frame of weibos, each weibo contains: 
##'  \item{uid}{User ID}
##'  \item{url}{URL of user}
##'  \item{follow}{Number of followers}
##'  \item{fan}{Number of funs}
##'  \item{profile}{Number of weibos}
##'  
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @keywords Search
##' @examples \dontrun{
##' web.search.user("lijian001")
##' }

web.search.user <- function(screen_name, roauth = NULL) 
{
	requestURL <- "http://s.weibo.com/user/"
	sword <- curlEscape(.cntoUTF8(screen_name))
	strurl <- paste(requestURL, sword, sep = "")
	
	if (is.null(roauth)) {
		pagetree <- htmlParse(strurl)
		pagenode <- getNodeSet(pagetree, "//script")
		pagescript <- sapply(pagenode, xmlValue)
		weiboline <- pagescript[grep("\"pid\":\"pl_user_feedList", pagescript)]
		weibojson <- gsub("\\)$", "", gsub("^.*STK.pageletM.view\\(", "", weiboline))
		
	} else {
		resXML <- getURL(strurl, curl = roauth$webCurl, .encoding = 'UTF-8')
		resHTMLs <- .strextract(resXML, "<script>.+?</script>")[[1]]
		resHTML <- resHTMLs[grep("\"pid\":\"pl_user_feedList\"", resHTMLs)][1]
		if (is.na(resHTML)) {
			warning("Can not crawl any page now. May be forbidden by Sina temporarily.", call. = FALSE)
			return(NULL)
		}
		weibojson <- gsub("\\)</script>$", "", gsub("^.*STK.pageletM.view\\(", "", resHTML))
	}
	
	if (length(weibojson) == 0) {
		warning("Can not crawl any page now. May be forbidden by Sina temporarily.", call. = FALSE)
		return(NULL)
	}
	weibolist <- .fromJSON(weibojson)
	weibopage <- try(htmlParse(weibolist[["html"]], asText=TRUE, encoding = "UTF-8"), silent = TRUE)
	if (inherits(weibopage, "try-error")) stop("please check your nick name.", call. = FALSE)
	weiboitem <- getNodeSet(weibopage, "//div[@class='person_detail']")[[1]]
	out.uid <- xmlGetAttr(getNodeSet(weiboitem, "p[@class='person_name']/a")[[1]], "uid")
	out.url <- xmlGetAttr(getNodeSet(weiboitem, "p[@class='person_name']/a")[[1]], "href")
	out.title <- xmlGetAttr(getNodeSet(weiboitem, "p[@class='person_name']/a")[[1]], "title")
	out.num <- sapply(getNodeSet(weiboitem, "p[@class='person_num']/span/a"), xmlValue)
	OUT <- list(
			uid = out.uid,
			url = out.url,
			follow = as.integer(out.num[1]),
			fan = as.integer(out.num[2]),
			profile = as.integer(out.num[3])
	)
	return(OUT)
}

