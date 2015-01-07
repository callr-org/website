findPackages <- function() {
  url <- parseUrl()
  pkgs <- url$fragment

  # Nothing to do?
  if (length(pkgs) == 0L) {
    return(data.frame(name=character(0L), fullname=character(0)))
  }

  pkgs <- unlist(strsplit(pkgs, split="[,#]"))

  pkgs <- lapply(pkgs, FUN=function(pkg) {
    pkg <- trim(pkg)
    pattern <- "([^[]*)[ ]*([[](.*)[]])?"
    name <- trim(gsub(pattern, "\\1", pkg))
    flags <- gsub(pattern, "\\3", pkg)
    flags <- unlist(strsplit(flags, split="", fixed=TRUE))

    if (regexpr("^[a-z]+://.+[.](tar[.]gz|tgz|zip)$", pkg) != -1) {
      # Install directly from URL (and force it)
      flags <- c(flags, "D", "!")
    } else if (regexpr("[^/]+[/][^/]+", pkg) != -1) {
      # Install from GitHub? (and force it)
      flags <- c(flags, "G", "!")
    } else {
      # Install from repositories (default)
    }

    flags <- unique(flags[nzchar(flags)])
    list(name=name, flags=flags)
  })
  flags <- lapply(pkgs, FUN=`[[`, "flags")
  pkgs <- sapply(pkgs, FUN=`[[`, "name")
  pkgnames <- gsub(".*/", "", pkgs)
  pkgnames <- gsub("[@#].*", "", pkgnames)
  pkgs <- data.frame(name=I(pkgnames), fullname=I(pkgs))
  pkgs$flags <- flags
  pkgs <- subset(pkgs, nzchar(name))
  pkgs
} # findPackages()
