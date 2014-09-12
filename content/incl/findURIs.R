gist_to_url <- function(url) {
  pattern <- "^gist://(.*)/(.*)/(.*)"
  if (regexpr(pattern, url) == -1L) return(url)
  gsub(pattern, "https://gist.githubusercontent.com/\\1/\\2/raw/\\3", url)
}

url_to_gist <- function(url) {
  pattern <- "^https://gist.githubusercontent.com/(.*)/(.*)/raw/(.*)"
  if (regexpr(pattern, url) == -1L) return(url)
  gsub(pattern, "gist://\\1/\\2/\\3", url)
}

findURIs <- function() {
  # Don't assume 'utils' is attached
  URLdecode <- utils::URLdecode

  url <- parseUrl()
  uris <- url$fragment

  # Nothing to do?
  if (length(uris) == 0L) {
    return(character(0L))
  }

  uris <- URLdecode(uris)
  uris <- unlist(strsplit(uris, split="[,#]"))

  # Translater gist:// URIs
  uris <- sapply(uris, FUN=gist_to_url)

  uris
} # findURIs()
