


##' Return the latest weibos of one user.
##' 
##' @title Return the latest weibos of one user.
##' @param roauth A OAuth object created by \code{\link{createOAuth}}.
##' @param uid Specified ID of a user.
##' @param screen_name The nickname of the user that ID is specified.
##' @param count The returned count for every page.The default value is 20.
##' @param page The returned page.The default value is 1.
##' @param base_app Whether get the data based on current application only. 1 is only current application; 0 is no limitation. The default value is 0.
##' @param feature Return the weibos by weibo type. 0 : all types; 1 : original; 2 : picture; 3 : video; 4 : music; The default value is 0.
##' @param trim_user The control of the user information. 0 : Return the whole user informatio; 1 : Return the user_id only. The default value is 0.
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
##'  
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @references \url{http://open.weibo.com/wiki/2/statuses/user_timeline}
##' @keywords Weibo
##' @examples \dontrun{
##' 
##' statuses.user_timeline(roauth, screen_name = "lijian001", count = 5)
##' }

statuses.user_timeline <- function(roauth, uid, screen_name, count = 20, 
		page = 1, base_app = 0, feature = 0, trim_user = 0, ...) {
	requestURL <- "https://api.weibo.com/2/statuses/user_timeline.json"
	funCall <- match.call()
	params <- as.list(funCall)
	params[[1]] <- NULL
	params[["roauth"]] <- NULL

	returnthis <- .get(requestURL, roauth$oauthToken, params=params)
	roauth$oauthLimits$RemainingHits[6] = roauth$oauthLimits$RemainingHits[6] - 1
	roauth$oauthLimits$RemainingHits[7] = roauth$oauthLimits$RemainingHits[7] - 1
	return(returnthis$statuses)
}
