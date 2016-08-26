# TODO: Add comment
#' @export
# Author: jli
###############################################################################

.onAttach <- function(libname, pkgname ){
	packageStartupMessage( paste("# Rweibo Version:", packageDescription("Rweibo", fields = "Version")) )
}

