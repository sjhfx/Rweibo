


##' Check whether there is a new version of Rweibo.
##' 
##' @title Check whether there is a new version of Rweibo.
##' @return Invisible TRUE or FALSE. 
##'  
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @keywords Weibo
##' @export
##' @examples \dontrun{
##' 
##' weibo.checkUpdate()
##' }

weibo.checkUpdate <- function() {
	
	groupid <- "1054"
	pkg <- "Rweibo"
	strurl <- paste("http://r-forge.r-project.org/R/?group_id=", groupid, "&pkg=", pkg, sep = "")
	pagetree <- htmlParse(strurl)
	rweiboinfo <- sapply(getNodeSet(pagetree, "//tr/td[@valign='center']"), xmlValue)
	newversion <- .strtrim(.strextract(rweiboinfo[1], " ([0-9]|\\.|-)+? ")[[1]][1])
	curversion <- as.character(packageVersion("Rweibo"))
	comp <- compareVersion(curversion, newversion)
	if (comp < 0) {
		cat(paste("There is a new version of Rweibo (", newversion, ") available!\n", sep = ""))
		cat("You can install it from R-forge:\n")
		cat("install.packages(\"Rweibo\", repos=\"http://R-Forge.R-project.org\")\n")
		invisible(TRUE)
	} else {
		cat("The current 'Rweibo' package is the latest!\n")
		invisible(FALSE)
	}
}
