#' Launch CytoDRAV.
#'
#' Function requires no parameters but accepts optional parameters
#' that can be passed in \code{\link[shiny]{runApp}}, such as \code{launch.browser}
#' @param launch.browser TRUE or FALSE. Launch default browser.
#' @examples
#' launch_application()
#' launch_application(launch.browser=F)
#' @export
launch_application <- function(launch.browser=T,...)
{
  shiny::runApp(appDir = system.file("application", package = 'CytoDRAV'), launch.browser = launch.browser,
                ...)
}
