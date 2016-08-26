


##' Post a new weibo.
##' 
##' @title Post a new weibo.
##' @param roauth A OAuth object created by \code{\link{createOAuth}}.
##' @param status The weibo content to be posted.It must be encoded by URLEncode within 140 Chinese characters.
##' @param lat Latitude: from -90.0 to +90.0; +: North latitude; Default is 0.0.
##' @param long Longitude: from -180.0 to +180.0; +: East longitude;Default is 0.0.
##' @param ... Other request parameters for this API.
##' @export
##' @note The second weibo content can not be the same as the first one.
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
##' @references \url{http://open.weibo.com/wiki/2/statuses/update}
##' @keywords Weibo
##' @examples \dontrun{
##' 
##' statuses.update(roauth, status = "hello world!")
##' }

statuses.update <- function(roauth, status, lat = "0.0", long = "0.0", ...) {
	requestURL <- "https://api.weibo.com/2/statuses/update.json"
	funCall <- match.call()
	params <- as.list(funCall)
	params[[1]] <- NULL
	params[["roauth"]] <- NULL

	returnthis <- .post(requestURL, roauth$oauthToken, params=params)
	roauth$oauthLimits$RemainingHits[6] = roauth$oauthLimits$RemainingHits[6] - 1
	roauth$oauthLimits$RemainingHits[7] = roauth$oauthLimits$RemainingHits[7] - 1
	roauth$oauthLimits$RemainingHits[1] = roauth$oauthLimits$RemainingHits[1] - 1
	return(returnthis)
}
