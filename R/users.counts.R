


##' Return the followers counts,friends counts, and weibo counts of a batch of users.
##' 
##' @title Return the followers counts,friends counts, and weibo counts of a batch of users.
##' @param roauth A OAuth object created by \code{\link{createOAuth}}.
##' @param uids The ID of the user to be returned.These should be separated by simeangle comma, and are limited by 100 ID per call.
##' @param ... Other request parameters for this API.
##' @export
##' @return 
##'  A list of: 
##'  \item{id}{Weibo ID}
##'  \item{followers_count}{Count of followers}
##'  \item{friends_count}{Count of friends}
##'  \item{statuses_count}{Count of Weibos}
##'  
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @references \url{http://open.weibo.com/wiki/2/users/counts}
##' @keywords Users
##' @examples \dontrun{
##' 
##' users.counts(roauth, uids = c("1318558807", "1869170057"))
##' }

users.counts <- function(roauth, uids) {
	requestURL <- "https://api.weibo.com/2/users/counts.json"
	if (any(grepl(",", uids))) uids <- .strtrim(unlist(strsplit(uids, split = ",")))
	params <- list(uids = paste(uids, collapse = ","))

	returnthis <- .get(requestURL, roauth$oauthToken, params=params)
	roauth$oauthLimits$RemainingHits[6] = roauth$oauthLimits$RemainingHits[6] - 1
	roauth$oauthLimits$RemainingHits[7] = roauth$oauthLimits$RemainingHits[7] - 1
	return(do.call("rbind", lapply(returnthis, as.data.frame, stringsAsFactors = FALSE)))
}
