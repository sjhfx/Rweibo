


##' Return the latest comments of the authenticating user including the sending and receiving.
##' 
##' @title Return the latest comments of the authenticating user including the sending and receiving.
##' @param roauth A OAuth object created by \code{\link{createOAuth}}.
##' @param count The returned count for every page.The default value is 20.
##' @param page The returned page.The default value is 1.
##' @param ... Other request parameters for this API.
##' @export
##' @return 
##'  A list of weibos, each weibo contains: 
##'  \item{idstr}{Weibo ID of string}
##'  \item{created_at}{Created time}
##'  \item{id}{Weibo ID}
##'  \item{text}{Weibo content}
##'  \item{source}{Weibo source}
##'  \item{mid}{Weibo MID}
##'  \item{user}{User profile that posted the weibo}
##'  \item{status}{The weibo that is commented}
##'  \item{reply_comment}{Replied comment information}
##'  
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @references \url{http://open.weibo.com/wiki/2/comments/timeline}
##' @keywords Comments
##' @examples \dontrun{
##' 
##' comments.timeline(roauth, count = 5)
##' }

comments.timeline <- function(roauth, count = 20, page = 1, ...) {
	requestURL <- "https://api.weibo.com/2/comments/timeline.json"
	funCall <- match.call()
	params <- as.list(funCall)
	params[[1]] <- NULL
	params[["roauth"]] <- NULL
	
	returnthis <- .get(requestURL, roauth$oauthToken, params=params)
	roauth$oauthLimits$RemainingHits[6] = roauth$oauthLimits$RemainingHits[6] - 1
	roauth$oauthLimits$RemainingHits[7] = roauth$oauthLimits$RemainingHits[7] - 1
	return(returnthis$comments)
}
