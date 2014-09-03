gist_to_url <- function(url) {
  pattern <- "^gist://(.*)/(.*)/(.*)"
  if (regexpr(pattern, url) == -1) return(url)
  gsub(pattern, "https://gist.githubusercontent.com/\\1/\\2/raw/\\3", url)
}

url_to_gist <- function(url) {
  pattern <- "^https://gist.githubusercontent.com/(.*)/(.*)/raw/(.*)"
  if (regexpr(pattern, url) == -1) return(url)
  gsub(pattern, "gist://\\1/\\2/\\3", url)
}

findURIs <- function(url=NULL) {
  # Don't assume 'utils' is attached
  URLdecode <- utils::URLdecode

  if (is.null(url)) {
    urls <- names(findSourceTraceback())
    pattern <- ".*/rfile#"
    urls <- grep(pattern, urls, value=TRUE)
    urls <- gsub(pattern, "", urls)
    url <- urls[1]
  }
  if (is.na(url)) {
    # Local testing?
    url <- getOption("rfile#")
    if (is.null(url)) return(data.frame(name=character(0L), flags=c()))
    print(url)
  }

  url <- URLdecode(url)

  uris <- unlist(strsplit(url, split=",", fixed=TRUE))

  # Translater gist:// URIs
  uris <- sapply(uris, FUN=gist_to_url)

  uris
} # findURIs()
