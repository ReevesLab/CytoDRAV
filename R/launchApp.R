#' @export
launch_application <- function(x, ...)
{
  shiny::runApp(appDir = system.file("application", package = 'CytoDRAV'),
                ...)
}