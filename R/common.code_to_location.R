


##' Return address names by address codes.
##' 
##' @title Return address names by address codes.
##' @param roauth A OAuth object created by \code{\link{createOAuth}}.
##' @param codes Address codes to be queried.These are separated by comma.
##' @export
##' @return 
##'  A list of address name.
##'  
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @references \url{http://open.weibo.com/wiki/2/common/code_to_location}
##' @keywords Accounts
##' @examples \dontrun{
##' 
##' common.code_to_location(roauth, codes = 100)
##' }

common.code_to_location <- function(roauth, codes) {
	requestURL <- "https://api.weibo.com/2/common/code_to_location.json"
	funCall <- match.call()
	params <- as.list(funCall)
	params[[1]] <- NULL
	params[["roauth"]] <- NULL

	returnthis <- .get(requestURL, roauth$oauthToken, params=params)
	return(returnthis)
}
