


##' Return the latest N reposted weibos of a original weibo.
##' 
##' @title Return the latest N reposted weibos of a original weibo.
##' @param roauth A OAuth object created by \code{\link{createOAuth}}.
##' @param id The original weibo ID.
##' @param count The returned count for every page.The default value is 20.
##' @param page The returned page.The default value is 1.
##' @param filter_by_author Return the weibos by the owner type. 0 : all types; 1 : my friend; 2 : stranger; The default value is 0.
##' @param ... Other request parameters for this API.
##' @export
##' @return 
##'  A list of weibos, each weibo contains: 
##'  \item{idstr}{Weibo ID of string}
##'  \item{created_at}{Created time}
##'  \item{id}{Weibo ID}
##'  \item{text}{Weibo content}
##'  \item{source}{Weibo source}
##'  \item{favorited}{Whether it is favorited}
##'  \item{truncated}{Whether it is truncated}
##'  \item{in_reply_to_status_id}{The reply's ID}
##'  \item{in_reply_to_user_id}{Replyer ID}
##'  \item{in_reply_to_screen_name}{Replyer nickname}
##'  \item{mid}{Weibo MID}
##'  \item{bmiddle_pic}{Medium picture}
##'  \item{original_pic}{Original picture}
##'  \item{thumbnail_pic}{Thumbnail picture}
##'  \item{reposts_count}{Repost count}
##'  \item{comments_count}{Comment count}
##'  \item{annotations}{Weibo annotation information}
##'  \item{geo}{Geography information}
##'  \item{user}{User profile that posted the weibo}
##'  \item{retweeted_status}{Reposted weibo content}
##'  
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @references \url{http://open.weibo.com/wiki/2/statuses/repost_timeline}
##' @keywords Weibo
##' @examples \dontrun{
##' 
##' statuses.repost_timeline(roauth, id = "3575234466298494", count = 5)
##' }

statuses.repost_timeline <- function(roauth, id, count = 20, page = 1, filter_by_author = 0, ...) {
	requestURL <- "https://api.weibo.com/2/statuses/repost_timeline.json"
	funCall <- match.call()
	params <- as.list(funCall)
	params[[1]] <- NULL
	params[["roauth"]] <- NULL

	returnthis <- .get(requestURL, roauth$oauthToken, params=params)
	roauth$oauthLimits$RemainingHits[6] = roauth$oauthLimits$RemainingHits[6] - 1
	roauth$oauthLimits$RemainingHits[7] = roauth$oauthLimits$RemainingHits[7] - 1
	return(returnthis$reposts)
}
