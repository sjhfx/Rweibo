


##' Return all of the schools.
##' 
##' @title Return all of the schools.
##' @param roauth A OAuth object created by \code{\link{createOAuth}}.
##' @param province Province ID
##' @param city City ID
##' @param area Area ID
##' @param type Return the schools by the school type. 1 : university; 2 : high school; 3 : technical secondary school; 4 : junior high school; 5 : elementary school; Default is 1.
##' @param capital The first letter of the school name.Default is A.
##' @param keyword The key word of the school name.
##' @param count The returned count.Default is 10.
##' @export
##' @note You must provide a parameter either keyword or capital. You must provide patameter "province" if you query by "capital".
##' @return 
##'  A list of: 
##'  \item{id}{ID of the school}
##'  \item{name}{Name of the school}
##'  
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @references \url{http://open.weibo.com/wiki/2/account/profile/school_list}
##' @keywords Accounts
##' @examples \dontrun{
##' 
##' account.profile.school_list(roauth, keyword = "China")
##' }

account.profile.school_list <- function(roauth, province, city, area, type = 1, 
		capital = "A", keyword, count = 10) {
	requestURL <- "https://api.weibo.com/2/account/profile/school_list.json"
	funCall <- match.call()
	params <- as.list(funCall)
	params[[1]] <- NULL
	params[["roauth"]] <- NULL

	returnthis <- .get(requestURL, roauth$oauthToken, params=params)
	roauth$oauthLimits$RemainingHits[6] = roauth$oauthLimits$RemainingHits[6] - 1
	roauth$oauthLimits$RemainingHits[7] = roauth$oauthLimits$RemainingHits[7] - 1
	return(returnthis)
}
