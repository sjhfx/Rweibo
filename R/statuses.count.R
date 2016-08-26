


##' Return the comment counts and repost counts of a batch of weibos.
##' 
##' @title Return the comment counts and repost counts of a batch of weibos.
##' @param roauth A OAuth object created by \code{\link{createOAuth}}.
##' @param ids The ID of the weibo to be returned.
##' @param ... Other request parameters for this API.
##' @export
##' @return 
##'  A list of weibos, each weibo contains: 
##'  \item{id}{The ID of the weibo}
##'  \item{comments}{The count of comments}
##'  \item{reposts}{The count of reposts}
##'  
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @references \url{http://open.weibo.com/wiki/2/statuses/count}
##' @keywords Weibo
##' @examples \dontrun{
##' 
##' statuses.count(roauth, ids = "3543748358960699")
##' }

statuses.count <- function(roauth, ids, ...) {
	requestURL <- "https://api.weibo.com/2/statuses/count.json"
	funCall <- match.call()
	params <- as.list(funCall)
	params[[1]] <- NULL
	params[["roauth"]] <- NULL

	returnthis <- .get(requestURL, roauth$oauthToken, params=params)
	roauth$oauthLimits$RemainingHits[6] = roauth$oauthLimits$RemainingHits[6] - 1
	roauth$oauthLimits$RemainingHits[7] = roauth$oauthLimits$RemainingHits[7] - 1
	return(returnthis )
}
