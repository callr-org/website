splitUrl <- function(url, ...) {
  stopifnot(length(url) == 1L)
  if (!hasUrlProtocol(url)) stop("Not a valid URL: ", url)

  # Get the protocol
  pattern <- "^([abcdefghijklmnopqrstuvwxyz]+)(://)(.*)"
  protocol <- gsub(pattern, "\\1", url, ignore.case=TRUE)
  tail <- gsub(pattern, "\\3", url, ignore.case=TRUE)

  # Allocate results
  res <- list(
    url=url,
    protocol=protocol,
    hierarchical_part=NULL,
    host=NULL, path=NULL,
    query=NULL, query_parsed=NULL,
    fragment=NULL
  )

  # hierarchical_part
  pos <- regexpr("[?#]", tail)
  if (pos != -1L) {
    sep <- substring(tail, first=pos, last=pos)
    res$hierarchical_part <- substring(tail, first=1L, last=pos-1L)
    tail <- substring(tail, first=pos+1L)
  } else {
    res$hierarchical_part <- tail
    tail <- ""
    sep <- NULL
  }

  # Get the host and the path
  parts <- strsplit(res$hierarchical_part, split="/", fixed=TRUE)[[1L]]
  if (length(parts) > 0L) {
    res$host <- parts[1L]
    res$path <- paste(parts[-1L], collapse="/")
  } else {
    res$host <- NULL
    res$path <- res$hierarchical_part
  }

  # Done?
  if (nchar(tail) == 0L) {
    return(res)
  }

  if (sep == "?") {
    # Get the query then fragments
    res$query <- gsub("#.*", "", tail)
    res$fragment <- gsub(".*#", "", tail)
  } else if (sep == "#") {
    res$fragment <- gsub("[?].*", "", tail)
    res$query <- gsub(".*[?]", "", tail)
  }

  # Parse query
  if (length(res$query) > 0L) {
    parts <- strsplit(res$query, split="&", fixed=TRUE)[[1L]]
    if (length(parts) > 0L) {
      parts <- strsplit(parts, split="=", fixed=TRUE)
      names <- unlist(lapply(parts, FUN=function(x) x[1L]), use.names=FALSE)
      args <- lapply(parts, FUN=function(x) paste(x[-1L], collapse="="))
      names(args) <- names
      res$query_parsed <- args
    }
  }

  res
} # splitUrl()
