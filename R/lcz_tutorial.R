#' LCZ4r tutorials
#'
#' Opens LCZ4r tutorial on a web browser. This function requires internet connection
#'
#' @param article String. Default is "general" for general functions. It can be either "local" for local functions
#'
#' @return Opens LCZ4r tutorial on a web browser
#'
#' @export
#'
#' @examples
#' lcz_tutorial(article="general")


lcz_tutorial <- function(article = "general") {

  if (article == "general") {
    utils::browseURL("https://bymaxanjos.github.io/LCZ4r/articles/Introd_general_LCZ4r.html")
  } else if (article == "local") {
    utils::browseURL("https://bymaxanjos.github.io/LCZ4r/articles/Introd_local_LCZ4r.html")
  }
  invisible(NULL)
}
