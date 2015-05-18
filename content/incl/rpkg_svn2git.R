git <- function(..., mustWork=FALSE) {
  args <- c(...)
  bin <- Sys.which("git")
  stopifnot(nchar(bin) > 0L)
  res <- system2(bin, args=args, stdout=TRUE)

  if (mustWork) {
    status <- attr(res, "status")
    if (is.numeric(status) && status != 0) {
      stop(sprintf("Error (code=%d) while calling 'git %s'.",
                   status, paste(args, collapse=" ")))
    }
  }

  res
}

git_version <- function() {
  ver <- tryCatch({
    ver <- git("--version")
    ver <- gsub("(git|version|msysgit.*)", "", ver)
    ver <- gsub("(^[ ]+|[ .]+$)", "", ver)
    numeric_version(ver)
  }, error = function(ex) numeric_version("0.0.0"))
  ver
}

git_svn_authors_r_forge <- function() {
  # R-Forge admins
  authors <- c(
    "mpacala = Martin Pacala <mpacala[SPAM-at-SPAM]wu.ac.at>",
    "stefan7th = Stefan Theuss <stefan.theussl[SPAM-at-SPAM]R-project.org>"
  )
  gsub("[SPAM-at-SPAM]", "@", authors, fixed=TRUE)
}

git_svn_authors_bioc <- function() {
  # Bioconductor admins
  authors <- c(
    "hpages[SPAM-at-SPAM]fhcrc.org = Hervé Pagès <hpages[SPAM-at-SPAM]fhcrc.org>",
    "p.aboyoun = Patrick Aboyoun <paboyoun[SPAM-at-SPAM]fhcrc.org>",
    "n.gopalakrishnan = Nishant Gopalakrishnan <ngopalak[SPAM-at-SPAM]fhcrc.org>",
    "c.wong = Chao-Jen Wong <cwon2[SPAM-at-SPAM]fhcrc.org>",
    "d.tenenbaum = Dan Tenenbaum <dtenenba[SPAM-at-SPAM]fhcrc.org>",
    "mtmorgan[SPAM-at-SPAM]fhcrc.org = Martin Morgan <mtmorgan[SPAM-at-SPAM]fhcrc.org>",
    "sethf = Seth Falcon <seth[SPAM-at-SPAM]userprimary.net>",
    "tliu[SPAM-at-SPAM]fhcrc.org = Ting-Yuan Liu <tliu[SPAM-at-SPAM]fhcrc.org>",
    "rgentlem = Robert Gentleman <rgentlem[SPAM-at-SPAM]fhcrc.org>",
    "m.carlson = Mark Carlson <mcarlson[SPAM-at-SPAM]fhcrc.org>"
  )
  gsub("[SPAM-at-SPAM]", "@", authors, fixed=TRUE)
}

git_svn_authors_known <- function() {
  c(git_svn_authors_r_forge(), git_svn_authors_bioc())
}

git_svn_authorsfile <- function() {
  pathname <- git("config", "svn.authorsfile")
  status <- attr(pathname, "status")
  if (!is.null(status))
    stop("The git svn authors file not set, e.g. git config --global svn.authorsfile ~/.gitauthors")
  if (!file_test("-f", pathname))
    stop("The git svn authors file not found: ", pathname)
  pathname
}

git_svn_authors <- function(authors=NULL) {
  if (is.null(authors)) authors <- readLines(git_svn_authorsfile())
  authors <- grep("^[ ]*#", authors, invert=TRUE, value=TRUE)
  authors <- trim(authors)
  authors <- authors[nzchar(authors)]
  authors <- sort(unique(authors))
  pattern <- "(.*)[ \t]*=[ \t]*(.+)[<](.*)[>]"
  unknown <- grep(pattern, authors, invert=TRUE, value=TRUE)
  if (length(unknown) > 0) {
    stop("Malformed entry in authors file: ", unknown[1])
  }
  user <- trim(gsub(pattern, "\\1", authors))
  name <- trim(gsub(pattern, "\\2", authors))
  email <- trim(gsub(pattern, "\\3", authors))

  data.frame(user=user, email=email, name=name)
}

git_svn <- function(url, path=NULL, authors=NULL, ...) {
  ## Change working directory?
  if (!is.null(path)) {
    if (!file_test("-d", path)) dir.create(path, recursive=TRUE)
    opwd <- getwd()
    on.exit(setwd(opwd))
    setwd(path)
  }

  if (is.data.frame(authors) && nrow(authors) > 0L) {
    # Create authors file
    authfile <- ".svn2git_git_svn_authorsfile.txt"
    bfr <- sprintf("%s = %s <%s>", authors$user, authors$name, authors$email)
    writeLines(bfr, con=authfile)
    fetch_args <- sprintf("--authors-file=%s", authfile)
  } else {
    fetch_args <- NULL
  }

  git("svn", "init", "--prefix=svn/", "--no-metadata",
                          sprintf("--trunk=%s", url), mustWork=TRUE)
  git("svn", "fetch", fetch_args, mustWork=TRUE)
  git("gc")

  mcat("\n\nThe 3 most recent commits:\n")
  log <- git("log", "-3")
  mcat(log, sep="\n")

  mcat("\nSVN-to-Git export completed.\n")
  mprintf("Path to local Git repository: %s\n\n", getwd())
}


rpkg_svn2git <- function(pkg, from=NULL, ...) {
  pkg <- as.character(pkg)
  stopifnot(length(pkg) == 1L, grepl("(.*):(.*)", pkg))
  if (!is.null(from)) from <- as.integer(from)

  ## Parse package specification
  repos <- tolower(gsub("(.*):(.*)", "\\1", pkg))
  if (!is.element(repos, c("bioc-devel", "r-forge"))) {
    stop("Unknown repository: ", repos)
  }
  pkg <- gsub("(.*):(.*)", "\\2", pkg)

  if (git_version() < "1.8.0") {
    stop("Your version of git is too old: ", git_version())
  } else if (git_version() < "1.9.0") {
    warning("Your version of git is old and may fail the SVN-to-Git export: ", git_version())
  }

  ## Build authors map
  authors_local <- readLines(git_svn_authorsfile())
  authors_known <- git_svn_authors_known()
  authors <- git_svn_authors(c(authors_known, authors_local))
  mcat("svn-to-git map of authors:\n")
  mprint(authors)

  ## URL for the SVN repository
  if (repos == "bioc-devel") {
    package <- pkg
    svnURL <- sprintf("https://hedgehog.fhcrc.org/bioconductor/trunk/madman/Rpacks/%s", package)
  } else if (repos == "r-forge") {
    project <- gsub("(.*)/(.*)", "\\1", pkg)
    package <- gsub("(.*)/(.*)", "\\2", pkg)
    mprintf("R-Forge project: %s\n", project)
    mprintf("R-Forge package: %s\n", package)
    svnURL <- sprintf("svn://svn.r-forge.r-project.org/svnroot/%s/pkg/%s",
                                                         project, package)
  }
  mprintf("SVN URL (translated): %s\n", svnURL)
  mprintf("R package name: %s\n", package)


  ## Download complete SVN history
  git_svn(url=svnURL, path=package, authors=authors)
} # rpkg_svn2git()
