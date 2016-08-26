\name{weibo2.0-class}
\docType{class}
\alias{weibo2.0-class}
\alias{weibo2.0}
\alias{expiresIn}
\alias{getLimits}
\alias{authorize}
\alias{login}

\title{Class "weibo2.0" }
\description{To use the APIs of Sina Weibo open platform, you have to register a application first. 
The open platform will provide a unique App Key and App Secret for every application. You can use these
key and secret to invoke the authorize interface of OAuth2. All the authorization information will be 
stored in this object of OAuth2.}
\section{Objects from the Class}{
Objects can be created by calls of the function \code{\link{createOAuth}}. 
}
\section{Fields}{
	\describe{
	\item{\code{appPath}:}{Object of class \code{"character"}. Path of the application folder.}
	\item{\code{appName}:}{Object of class \code{"character"}. Name of the application.}
    \item{\code{appKey}:}{Object of class \code{"character"}. Key of the application.}
    \item{\code{appSecret}:}{Object of class \code{"character"}. Secret of the application. }
    \item{\code{oauthName}:}{Object of class \code{"character"}. The access name.}
    \item{\code{oauthToken}:}{Object of class \code{"character"}. Token of the oauth.}
    \item{\code{oauthUser}:}{Object of class \code{"character"}. User ID.}
    \item{\code{oauthTime}:}{Object of class \code{"character"}. Created time.}
    \item{\code{oauthResetTime}:}{Object of class \code{"character"}. Reset time.}
    \item{\code{oauthLife}:}{Object of class \code{"character"}. Expected life (in sec) of the oauth.}
    \item{\code{authURL}:}{Object of class \code{"character"}. URL of authorization.}
    \item{\code{accessURL}:}{Object of class \code{"character"}. URL of accessing.}
    \item{\code{oauthLimits}:}{Object of class \code{"ANY"}. data.frame of oauth limits.}
    \item{\code{oauthMsg}:}{Object of class \code{"character"}. oauth massage.}
    \item{\code{webMsg}:}{Object of class \code{"character"}. web login massage.}
    \item{\code{webCurl}:}{Object of class \code{"ANY"}. Curl handle.}
    \item{\code{webName}:}{Object of class \code{"character"}. Nick name of the login account.}
    \item{\code{webUser}:}{Object of class \code{"character"}. User id of the login account.}
    }
}

\section{Methods}{
  \describe{
    \item{\code{expiresIn(byAPI = FALSE)}:}{Will return the expired
     time in seconds.}
    \item{\code{getLimits(byAPI = FALSE)}:}{Will return a data.frame 
     of the oauth limits.}
    \item{\code{authorize(forcelogin = FALSE)}:}{Will do the authorization.}
    \item{\code{login(username, password)}:}{Will impersonate the login 
     with \code{username} and \code{password}.}
    \item{\code{save()}:}{Save the authorization information to file.}
    \item{\code{list()}:}{List the fields of the object.}
	\item{\code{show()}:}{Show all the fields of the object.}
  }
}


\author{Jian li <rweibo@sina.com>}

\examples{
	showClass("weibo2.0")
}
\keyword{classes}
