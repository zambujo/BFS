#' Get the Swiss Federal Statistical Office data catalog in a given language
#'
#' This function scraps the RSS Feed of the Swiss Federal Statistical Office data catalog.
#'
#' @param language character The language of a BFS catalog.
#'
#' @return A data frame
#'
#' @importFrom tidyRSS tidyfeed
#' @importFrom dplyr select mutate
#'
#' @seealso \code{\link{bfs_get_metadata}}
#'
#' @examples
#' \donttest{bfs_get_catalog(language = "de")}
#'
#' @export

bfs_get_catalog <- function(language) {
  if (missing(language))
    stop("must choose a language, either 'de', 'fr', 'it' or 'en'", 
         call. = FALSE)
  language <- match.arg(arg = language, choices = c("de", "fr", "it", "en"))
  # url definition  
  feed <- 
    "https://www.bfs.admin.ch/bfs/" %+%
    language %+%
    "/home/statistiken/kataloge-datenbanken/daten/_jcr_content/par/" %+%
    "ws_catalog.rss.xml?skipLimit=true"
  base_url_bfs <- 
    "https://www.bfs.admin.ch/content/bfs/" %+%
    language %+%
    "/home/statistiken/kataloge-datenbanken/daten.assetdetail."
  base_url_px <- 
    "https://www.bfs.admin.ch/bfsstatic/dam/assets/"
  # download feed as tibble -------
  df <- tidyfeed(feed)
  df <- df %>%
    mutate(
      language = language,
      url_px = gsub(base_url_bfs, base_url_px, item_link),
      url_px = gsub(".html$", "/master", url_px)) %>%
    select(title = item_title, 
           language, 
           published = item_pub_date, 
           url_bfs = item_link, 
           url_px)
  return(df)
}
