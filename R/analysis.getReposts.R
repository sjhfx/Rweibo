

##' Return the data.frame of reposted weibos of a original weibo.
##' 
##' @title Return the data.frame of reposted weibos of a original weibo.
##' @param roauth A OAuth object created by \code{\link{createOAuth}}.
##' @param mid Weibo ID.
##' @param count The total count of reposts.
##' @param startcount The start number of reposts.
##' @export
##' @return A data.frame.
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @keywords Analysis
##' @examples \dontrun{
##' 
##' a1 <- analysis.getReposts(roauth, mid = "3575234466298494")
##' }

analysis.getReposts <- function(roauth, mid, count = -1, startcount = 1) {
	limitDf <- roauth$getLimits(TRUE)
	if (limitDf$RemainingHits[7] <= 1) stop("Please try again in the next hour!")
	weiboList <- do.call("statuses.show", list(roauth = roauth, id = mid))
	pagemax <- limitDf$RemainingHits[7] - 1
	if (count <= 0) count <- weiboList$reposts_count
	if (startcount <= 0) stop("'startcount' should be gerater than 0!")
	counttotal <- count + startcount - 1
	counttotal <- min(counttotal, weiboList$reposts_count)
	if (counttotal <= startcount) stop("'startcount' is greater than the total count!")
	pagestart <- ceiling(startcount/200)
	pagenum <- ceiling(counttotal/200)
	if ((pagenum - pagestart + 1) > pagemax) {
		warning(paste("you will exceed the limits in this hour, only ", 
						pagemax * 200 , " reposts will be saved. Please try again in the next hour.", 
						sep = ""))
		pagenum <- pagemax + pagestart - 1
	}
	
	OUT <- data.frame()
	for (ipage in pagestart:pagenum) {
		res.tmp <- do.call("statuses.repost_timeline", list(roauth = roauth, id = mid, count = 200, page = ipage))
		df.tmp <- do.call("rbind", lapply(res.tmp, FUN = function(X) 
				cbind(.parseRepostList(X), .parseUserList(X$user, prefix = "User"))
		))
		OUT <- rbind(OUT, df.tmp)
	}
	return(OUT)
}
