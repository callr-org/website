findPackages <- function(cmd=NULL) {
  trim <- function(x) gsub("(^[ ]|[ ]$)", "", x)

  if (is.null(cmd)) {
    urls <- names(findSourceTraceback())
    pattern <- ".*/install#"
    urls <- grep(pattern, urls, value=TRUE)
    urls <- gsub(pattern, "", urls)
    cmd <- urls[1]
  }
  if (is.na(cmd)) {
    # Local testing?
    cmd <- getOption("install#")
    if (is.null(cmd)) return(data.frame(name=character(0L), flags=c()))
    print(cmd)
  }

  cmd <- URLdecode(cmd)

  pkgs <- unlist(strsplit(cmd, split=",", fixed=TRUE))
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
  pkgs <- data.frame(name=I(gsub(".*/", "", pkgs)), fullname=I(pkgs))
  pkgs$flags <- flags
  pkgs <- subset(pkgs, nzchar(name))
  pkgs
} # findPackages()
