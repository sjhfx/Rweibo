

##' Return the data.frame of weibos of one user.
##' 
##' @title Return the data.frame of weibos of one user.
##' @param roauth A OAuth object created by \code{\link{createOAuth}}.
##' @param uid User ID.
##' @param screen_name The nickname of the user that ID is specified.
##' @param count The total count of reposts.
##' @param startcount The start number of reposts.
##' @export
##' @return A data.frame.
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @keywords Analysis
##' @examples \dontrun{
##' 
##' a1 <- analysis.getUserTimeline(roauth, screen_name = "rweibo")
##' }

analysis.getUserFollowers <- function(roauth, uid = NULL, screen_name = NULL, count = -1, startcount = 1) {
	
	limitDf <- roauth$getLimits(TRUE)
	if (limitDf$RemainingHits[7] <= 1) stop("Please try again in the next hour!")
	
	if (!is.null(uid) && !is.null(screen_name)) {
		warning("'screen_name' was ignored!")
		screen_name <- NULL
	}
	
	if (is.null(uid) && is.null(screen_name)) stop("You must provide a parameter either 'uid' or 'screen_name'!")
	
	userList <- do.call("users.show", do.call("c", list(roauth = roauth, uid = uid, screen_name = screen_name)))
	pagemax <- limitDf$RemainingHits[7] - 1
	if (count <= 0) count <- userList$statuses_count
	if (startcount <= 0) stop("'startcount' should be gerater than 0!")
	counttotal <- count + startcount - 1
	counttotal <- min(counttotal, userList$statuses_count)
	if (counttotal <= startcount) stop("'startcount' is greater than the total count!")
	pagestart <- ceiling(startcount/100)
	pagenum <- ceiling(counttotal/100)
	if ((pagenum - pagestart + 1) > pagemax) {
		warning(paste("you will exceed the limits in this hour, only ", 
						pagemax * 100 , " reposts will be saved. Please try again in the next hour.", 
						sep = ""))
		pagenum <- pagemax + pagestart - 1
	}
	
	OUT <- data.frame()
	for (ipage in pagestart:pagenum) {
		res.tmp <- do.call("statuses.user_timeline", do.call("c", 
						list(roauth = roauth, uid = uid, screen_name = screen_name, count = 100, page = ipage)))
		df.tmp <- do.call("rbind", lapply(res.tmp, FUN = function(X) 
							cbind(.parseRepostList(X), 
									.parseRepostList(X$retweeted_status, prefix = "retweeted"),
									.parseUserList(X$retweeted_status$user, prefix = "retweeted_user")
							)))
		OUT <- rbind(OUT, df.tmp)
	}
	return(OUT)
}
