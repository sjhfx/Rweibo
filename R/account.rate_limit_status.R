


##' Return the authenticating user's API access rate limitation.
##' 
##' @title Return the authenticating user's API access rate limitation.
##' @param roauth A OAuth object created by \code{\link{createOAuth}}.
##' @export
##' @return limit list
##'  
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @references \url{http://open.weibo.com/wiki/2/account/rate_limit_status}
##' @keywords Accounts
##' @examples \dontrun{
##' 
##' account.rate_limit_status(roauth)
##' }

account.rate_limit_status <- function(roauth) {
	requestURL <- "https://api.weibo.com/2/account/rate_limit_status.json"
	funCall <- match.call()
	params <- as.list(funCall)
	params[[1]] <- NULL
	params[["roauth"]] <- NULL

	returnthis <- .get(requestURL, roauth$oauthToken, params=params)
	return(returnthis)
}
