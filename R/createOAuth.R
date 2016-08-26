##' Create an authorized OAuth object
##' 
##' @title create an OAuth object
##' @param app_name name of the application.
##' @param access_name a string of your access name.
##' @param authorize whether to authorize the oauth to use sina API.
##' @param login whether to login to impersonate the login.
##' @param username user name of the account to impersonate the login.
##' @param password password of the account to impersonate the login.
##' @export
##' @return An reference object of \code{\link{weibo2.0}}.
##' @note There is only one OAuth object needed.
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @seealso \code{\link{registerApp}}
##' @references \url{http://open.weibo.com/wiki/OAuth/en}
##' @keywords authorization
##' @examples \dontrun{
##' 
##' roauth <- createOAuth("sinademo", "user1")
##' }
createOAuth <- function(app_name, access_name, authorize = TRUE, 
		login = FALSE, username = "", password = "") {
	oauthobj <- new("weibo2.0", appName = app_name, oauthName = access_name)
	if (authorize) {
		if (oauthobj$expiresIn() < 0) {
			oauthobj$authorize(forcelogin = FALSE)
			oauthobj$save()
		}
	}
	
	if (file.exists(file.path(oauthobj$appPath, paste(oauthobj$oauthName, ".cookie", sep = "")))) {
		testresult <- oauthobj$testcookie(TRUE)
		if (testresult) login <- FALSE
	}
	
	if(login) {
		oauthobj$login(username, password) 
	}
	
	limitDf <- try(oauthobj$getLimits(TRUE), silent = TRUE)
	if (is.data.frame(limitDf)) {
		oauthobj$oauthMsg <- "oauth was authorized! (expires in HOURS hours)"
		oauthobj$oauthLimits <- limitDf
		oauthobj$oauthResetTime <- .hourtime(1)
	} else {
		warning("oauth test failed, please check the connection or your application.", call. = FALSE)
		oauthobj$oauthLife <- "-1"
	}
	
	return(oauthobj)
}



