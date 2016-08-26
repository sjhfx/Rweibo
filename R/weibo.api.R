##' Invoke an API directly by it's URL.
##' 
##' @title Invoke an API directly by it's URL.
##' @param roauth A OAuth object created by \code{\link{createOAuth}}.
##' @param URL The url of this API.
##' @param paramlist Parameters list. 
##' @param httpmethod HTTP request method, 'GET' or 'POST'.
##' @export
##' @return A R object. 
##'  
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @references \url{http://open.weibo.com/wiki}
##' @keywords Weibo
##' @examples \dontrun{
##' 
##' weibo.api(roauth, URL = "https://api.weibo.com/2/statuses/public_timeline.json", 
##'     paramlist = list(count = 5), httpmethod = "GET")
##' }

weibo.api <- function(roauth, URL, paramlist = list(), httpmethod = c("GET", "POST")) {
	
	if (!is.list(paramlist)) stop('paramlist must be list!')
	paramlist[["source"]] <- NULL
	paramlist$access_token <- NULL
	if (length(paramlist) == 0) stop("There is no parameters list!")
	
	httpmethod <- toupper(httpmethod)
	httpmethod <- match.arg(httpmethod)
	
	if (httpmethod == "GET") {
		returnthis <- .get(URL, roauth$oauthToken, params = paramlist)
	}
	if (httpmethod == "POST") {
		returnthis <- .post(URL, roauth$oauthToken, params = paramlist)
	}
	
	return(returnthis)
}
