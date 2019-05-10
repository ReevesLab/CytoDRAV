#' Launch CytoDRAV.
#'
#' Function requires no parameters but accepts optional parameters
#' that can be passed in \code{\link[shiny]{runApp}}, such as \code{launch.browser}
#' @examples
#' launch_application()
#' launch_application(launch.browser=TRUE)
#' @export
launch_application <- function(...)
{
  shiny::runApp(appDir = system.file("application", package = 'CytoDRAV'),
                ...)
}
