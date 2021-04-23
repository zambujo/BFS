#' Get the Swiss Federal Statistical Office data catalog in a given language
#'
#' This function scraps the RSS Feed of the Swiss Federal Statistical Office data catalog.
#'
#' @param language character The language of a BFS catalog.
#'
#' @return A data frame
#'
#' @importFrom httr RETRY stop_for_status
#' @importFrom xml2 read_xml
#' @importFrom dplyr select mutate
#' @importFrom tibble tibble
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
  language <-
    match.arg(arg = language, choices = c("de", "fr", "it", "en"))
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
  response <- RETRY("GET", feed, pause_cap = 15)
  stop_for_status(response)
  
  xml_obj <-  read_xml(response)
  df <- tibble(
    title = rss_item_parse("title", xml_obj),
    published = rss_item_parse("pubDate", xml_obj),
    url_bfs = rss_item_parse("link", xml_obj)
  ) %>%
    mutate(
      language = language,
      url_px = gsub(base_url_bfs, base_url_px, url_bfs),
      url_px = gsub(".html$", "/master", url_px)
    ) %>%
    select(title, language, published, url_bfs, url_px)
  return(df)
}
