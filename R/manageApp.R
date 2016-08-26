##' Reigster, modify and delete application.
##' 
##' Use the key and secret you get from "open.weibo.com"
##' 
##' @title Manage application
##' @rdname registerApp
##' @param app_name name of an application
##' @param app_key key of an application
##' @param app_secret secret of an application
##' @export
##' @return a logical value 
##' @note You should register an application on sina firstly.
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @seealso See Also as \code{\link{createOAuth}}
##' @references \url{http://open.weibo.com/}
##' @keywords authorization
##' @examples
##' registerApp(app_name, app_key, app_secret)
##' listApp("sinademo")
##' modifyApp("sinademo", "t1", "t2")
##' deleteApp("sinademo")
##'

registerApp <- function(app_name, app_key, app_secret) {
	apppath <- file.path(system.file(package = "Rweibo"), "oauth")
	if (!file.exists(apppath)) dir.create(apppath)
	if (file.exists(file.path(apppath, app_name))) {
		warning(paste("The App", app_name, "has been registered, please use 'modifyApp' to make change."))
		invisible(FALSE)
	} else {
		applist <- list(app_key = app_key, app_secret = app_secret, app_token = list())
		appfile <- file(file.path(apppath, app_name) , open = "w" )
		writeLines(toJSON(applist), appfile)
		close(appfile)
		invisible(TRUE)
	}
}

##' @rdname registerApp
##' @export
##' @return a logical value 	
modifyApp <- function(app_name, app_key, app_secret) {
	apppath <- file.path(system.file(package = "Rweibo"), "oauth")
	if (!file.exists(apppath)) dir.create(apppath)
	if (app_name %in% list.files(apppath)) {
		applist <- .fromJSON(file.path(apppath, app_name))
		applist$app_key <- app_key
		applist$app_secret <- app_secret
		appfile <- file(file.path(apppath, app_name) , open = "w" )
		writeLines(toJSON(applist), appfile)
		close(appfile)
	} else {
		stop(paste(app_name, "doesn't exist, please use 'registerApp' to create"))
	}
	return(TRUE)
}

##' @rdname registerApp
##' @export
##' @return a logical value 
deleteApp <- function(app_name) {
	apppath <- file.path(system.file(package = "Rweibo"), "oauth")
	if (!file.exists(apppath)) dir.create(apppath)
	if (file.exists(file.path(apppath, app_name))) {
		unlink(file.path(apppath, app_name))
	} else {
		stop(paste(app_name, "doesn't exist"))
	}
	return(TRUE)
}

##' @rdname registerApp
##' @export
##' @return a list with components as below
##'   \item{app_key }{key of the application} 
##'   \item{app_secret }{secret of the application} 
##'   \item{app_token }{authorization information of OAuth} 
listApp <- function(app_name) {
	apppath <- file.path(system.file(package = "Rweibo"), "oauth")
	if (!file.exists(apppath)) dir.create(apppath)
	if (app_name %in% list.files(apppath)) {
		applist <- .fromJSON(file.path(apppath, app_name))
		return(applist)
	} else {
		stop(paste(app_name, "doesn't exist, please use 'registerApp' to create"))
	}
}

.addAccess <- function(app_name, access_name, access_token, access_user, access_time, access_expires) {
	apppath <- file.path(system.file(package = "Rweibo"), "oauth")
	if (!file.exists(apppath)) dir.create(apppath)
	if (app_name %in% list.files(apppath)) {
		applist <- .fromJSON(file.path(apppath, app_name))
		if (access_name %in% names(applist$app_token)) {
			stop(paste("The access", access_name, "has existed, please use '.modifyAccess' to make change."))
		} else {
			applist$app_token[[access_name]] <- list(token_key = access_token, 
					token_user = access_user, token_time =  access_time, 
					expires_in = access_expires)
			appfile <- file(file.path(apppath, app_name) , open = "w" )
			writeLines(toJSON(applist), appfile)
			close(appfile)
		}
	} else {
		stop(paste(app_name, "doesn't exist, please use 'registerApp' to create"))
	}
	return(TRUE)
}

.modifyAccess <- function(app_name, access_name, access_token, access_user, access_time, access_expires) {
	apppath <- file.path(system.file(package = "Rweibo"), "oauth")
	if (!file.exists(apppath)) dir.create(apppath)
	if (app_name %in% list.files(apppath)) {
		applist <- .fromJSON(file.path(apppath, app_name))
		if (access_name %in% names(applist$app_token)) {
			applist$app_token[[access_name]] <- list(token_key = access_token, 
					token_user = access_user, token_time =  access_time, 
					expires_in = access_expires)
			appfile <- file(file.path(apppath, app_name) , open = "w" )
			writeLines(toJSON(applist), appfile)
			close(appfile)
		} else {
			stop(paste(access_name, "doesn't exist, please use '.addAccess' to create"))
		}
	} else {
		stop(paste(app_name, "doesn't exist, please use 'registerApp' to create"))
	}
	return(TRUE)
}

.deleteAccess <- function(app_name, access_name) {
	apppath <- file.path(system.file(package = "Rweibo"), "oauth")
	if (!file.exists(apppath)) dir.create(apppath)
	if (app_name %in% list.files(apppath)) {
		applist <- .fromJSON(file.path(apppath, app_name))
		if (access_name %in% names(applist$app_token)) {
			applist$app_token[[access_name]] <- NULL
			appfile <- file(file.path(apppath, app_name) , open = "w" )
			writeLines(toJSON(applist), appfile)
			close(appfile)
		} else {
			stop(paste(access_name, "doesn't exist, please use '.addAccess' to create"))
		}
	} else {
		stop(paste(app_name, "doesn't exist, please use 'registerApp' to create"))
	}
	return(TRUE)
}
