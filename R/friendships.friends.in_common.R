


##' Return the common friends list between two users.
##' 
##' @title Return the common friends list between two users.
##' @param roauth A OAuth object created by \code{\link{createOAuth}}.
##' @param uid User ID to be queried.
##' @param suid The other user ID of the two users to be returned.Default is the authenticating user.
##' @param count The returned count for every page.The default value is 20.
##' @param page The returned page.Default is 1.
##' @param ... Other request parameters for this API.
##' @export
##' @return 
##'  A list of user, which contains: 
##'  \item{id}{User ID}
##'  \item{screen_name}{User nickname}
##'  \item{name}{Friendly displayed name}
##'  \item{province}{Province ID user located}
##'  \item{city}{City ID user located}
##'  \item{location}{Address user located}
##'  \item{description}{User description}
##'  \item{url}{Url of the user's blog}
##'  \item{profile_image_url}{Profile image}
##'  \item{domain}{The user's personalized weibo url}
##'  \item{gender}{Gender, m : male; f : female; n : unknown}
##'  \item{followers_count}{Followers count}
##'  \item{friends_count}{Friends count}
##'  \item{statuses_count}{Weibo count}
##'  \item{favourites_count}{Favorites count}
##'  \item{created_at}{Created time}
##'  \item{following}{Whether the current user is following the user}
##'  \item{allow_all_act_msg}{Whether all the people can send direct message to the user}
##'  \item{geo_enabled}{Whether can have the geography information}
##'  \item{verified}{Whether the user is verified by his real identity, marked with "V"}
##'  \item{allow_all_comment}{Whether all the people can comment the user's weibo}
##'  \item{avatar_large}{Profile large image}
##'  \item{verified_reason}{verification reason}
##'  \item{follow_me}{Whether the user is following the current user}
##'  \item{online_status}{Whether the user is online, 0 : offline, 1 : online}
##'  \item{bi_followers_count}{The count of the users that are following the user and are being followed by the user}
##'  \item{status}{The latest weibo of the user}
##'  
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @references \url{http://open.weibo.com/wiki/2/friendships/friends/in_common}
##' @keywords Friendships
##' @examples \dontrun{
##' 
##' friendships.friends.in_common(roauth, uid = "1318558807")
##' }

friendships.friends.in_common <- function(roauth, uid, suid, count = 20, page = 1, ...) {
	requestURL <- "https://api.weibo.com/2/friendships/friends/in_common.json"
	funCall <- match.call()
	params <- as.list(funCall)
	params[[1]] <- NULL
	params[["roauth"]] <- NULL

	returnthis <- .get(requestURL, roauth$oauthToken, params=params)
	roauth$oauthLimits$RemainingHits[6] = roauth$oauthLimits$RemainingHits[6] - 1
	roauth$oauthLimits$RemainingHits[7] = roauth$oauthLimits$RemainingHits[7] - 1
	return(returnthis)
}
