


##' Return the authenticating user's privacy settings.
##' 
##' @title Return the authenticating user's privacy settings.
##' @param roauth A OAuth object created by \code{\link{createOAuth}}.
##' @export
##' @return 
##'  A list of: 
##'  \item{comment}{Who can comment this account's status. 0 : all users; 1 : followings; 2 : credible users.}
##'  \item{geo}{Whether allow saving and displaying geo info in status. 0 : not allow; 1 : allow.}
##'  \item{message}{Who can send direct message to this account. 0 : all users; 1 : followings; 2 : credible users.}
##'  \item{realname}{Whether allow other user find me by real name though the search engine. 0 : not allow; 1 : allow.}
##'  \item{badge}{Medal status. 0 : private. 1 : public.}
##'  \item{mobile}{Whether allow other user find me by mobile phone number though the search engine. 0 : not allow; 1 : allow.}
##'  \item{webim}{Whether enable webim. 0 : no; 1 : yes.}
##'  
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @references \url{http://open.weibo.com/wiki/2/account/get_privacy}
##' @keywords Accounts
##' @examples \dontrun{
##' 
##' account.get_privacy(roauth)
##' }

account.get_privacy <- function(roauth) {
	requestURL <- "https://api.weibo.com/2/account/get_privacy.json"
	funCall <- match.call()
	params <- as.list(funCall)
	params[[1]] <- NULL
	params[["roauth"]] <- NULL

	returnthis <- .get(requestURL, roauth$oauthToken, params=params)
	roauth$oauthLimits$RemainingHits[6] = roauth$oauthLimits$RemainingHits[6] - 1
	roauth$oauthLimits$RemainingHits[7] = roauth$oauthLimits$RemainingHits[7] - 1
	return(returnthis)
}
