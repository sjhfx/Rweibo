


##' Return cities list.
##' 
##' @title Return cities list.
##' @param roauth A OAuth object created by \code{\link{createOAuth}}.
##' @param province Province code.
##' @param capital The first letter of the provinces, a-z. Return all of the provinces 
##'        if the parameter is not provided.Default is not provided.
##' @param language The returned language version. zh-cn: Simplified Chinese; 
##'        zh-tw: Traditional Chinese; english: English; Default is zh-cn.
##' @export
##' @return 
##'  A list of cities.
##'  
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @references \url{http://open.weibo.com/wiki/2/common/get_city}
##' @keywords Accounts
##' @examples \dontrun{
##' 
##' common.get_city(roauth, province = "001042")
##' }

common.get_city <- function(roauth, province, capital, language) {
	requestURL <- "https://api.weibo.com/2/common/get_city.json"
	funCall <- match.call()
	params <- as.list(funCall)
	params[[1]] <- NULL
	params[["roauth"]] <- NULL

	returnthis <- .get(requestURL, roauth$oauthToken, params=params)
	return(returnthis)
}
