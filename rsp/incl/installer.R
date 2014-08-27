installer <- function(pkgs=NULL, ...) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Local functions
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  isInstalled <- function(pkgs) {
     unlist(sapply(pkgs, FUN=function(pkg) {
       nzchar(suppressWarnings(system.file(package=pkg)))
     }))
  }

  knownRepos <- function() {
    p <- file.path(Sys.getenv("HOME"), ".R", "repositories")
    if (!file.exists(p)) p <- file.path(R.home("etc"), "repositories")
    ns <- getNamespace("tools")
    .read_repositories <- get(".read_repositories", envir=ns)
    a <- .read_repositories(p)
    repos <- a$URL
    names(repos) <- rownames(a)
	repos
  } # knownRepos()


  # DEBUG
  mcat("Argument 'pkgs':\n")
  mprint(pkgs)

  repos <- getOption("repos")

  # Append other known repositories, iff missing
  repos <- c(repos, knownRepos())

  # Append other known repositories, iff missing
  repos <- c(repos, "AROMA"="http://braju.com/R")

  # Keep only unique ones
  names <- names(repos)
  dups <- (nzchar(names) & duplicated(names))
  repos <- repos[!dups]

  # Set temporarily
  oopts <- options(repos=repos)
  on.exit(options(oopts))

  # DEBUG
  mcat("Repositories:\n")
  mprint(repos)

  # Record package state before
  pkgs0 <- installed.packages()[,"Version"]
  pkgs0c <- paste(names(pkgs0), pkgs0)

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Install requested packages
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Nothing to do.
  if (nrow(pkgs) == 0L) message("No packages specified.")

  quiet <- any(sapply(pkgs$flags, FUN=function(x) is.element("Q", x)))

  pkgs$isInstalled <- isInstalled(pkgs$name)

  pkgs$force <- sapply(pkgs$flags, FUN=function(x) is.element("!", x))
  pkgs$suggests <- sapply(pkgs$flags, FUN=function(x) is.element("S", x))
  pkgs$recommends <- !sapply(pkgs$flags, FUN=function(x) is.element("r", x))

  # Install packages from URL?
  pkgs$url <- any(sapply(pkgs$flags, FUN=function(x) is.element("D", x)))

  # Install packages from GitHub?
  pkgs$github <- any(sapply(pkgs$flags, FUN=function(x) is.element("G", x)))

  # To install
  pkgsT <- subset(pkgs, !isInstalled | force)

  # DEBUG
  mcat("Packages to install:\n")
  mprint(pkgsT)

  warns <- list()


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # (a) Install packages from regular repositories
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  pkgsR <- subset(pkgsT, (!url & !github))
  if (nrow(pkgsR) > 0L) {
    message("Installing packages from package repositories:")
    # For some reason withCallingHandlers() fails to capture messages
    # on "Warning: unable to access index for repository ...".
    suppressWarnings({
      # Install all at once?
      if (all(pkgsR$suggests == pkgsR$suggests[1])) {
        deps <- if (pkgsR$suggests[1]) TRUE else NA
        install.packages(pkgsR$name, dependencies=deps, quiet=quiet, ...)
      } else {
        mapply(pkgsR$name, pkgsR$suggests, FUN=function(pkg, deps) {
          deps <- if (deps) TRUE else NA
          install.packages(pkg, dependencies=deps, quiet=quiet, ...)
        })
      }
    })
  }


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # (b) Install packages from URL
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  pkgsU <- subset(pkgsT, (url & !github))
  if (nrow(pkgsU) > 0L) {
    message("Installing packages from URLs:")
    suppressWarnings({
      if (!isInstalled("devtools")) {
        install.packages("devtools", quiet=quiet, ...)
      }
      # Install one by one
      for (kk in seq_len(nrow(pkgsU))) {
        pkg <- pkgsU[kk,]
        url <- pkg$fullname
        devtools::install_url(url)
      }
    })
  }


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # (c) Install packages from GitHub
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  pkgsG <- subset(pkgsT, (!url & github))
  if (nrow(pkgsG) > 0L) {
    message("Installing packages from GitHub:")
    suppressWarnings({
      if (!isInstalled("devtools")) {
        install.packages("devtools", quiet=quiet, ...)
      }
      # Install one by one
      for (kk in seq_len(nrow(pkgsG))) {
        pkg <- pkgsG[kk,]
        devtools::install_github(pkg$fullname)
      }
    })
  }


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # (d) Install "Recommended" packages (only from package repositories)
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  pkgsT <- subset(pkgs, recommends)
  if (nrow(pkgsT) > 0L) {
    pkgNames <- function(s) {
      s <- na.omit(s)
      if (length(s) == 0L) return(s)
      s <- unlist(strsplit(s, split=",", fixed=TRUE))
      s <- gsub("[(].*", "", s)
      gsub("^[ \n]+|[ \n]+$", "", s)
    }

    pkgDeps <- function(names, fields=c("Depends", "Imports", "LinkingTo"), recursive=FALSE) {
      if (recursive) {
        pkgsDepsDone <- NULL
        res <- NULL
        while(length(names) > 0L) {
          resT <- pkgDeps(names, fields=fields, recursive=FALSE)
          res <- c(res, resT)
          pkgsDepsDone <- c(pkgsDepsDone, names)
          names <- setdiff(resT, pkgsDepsDone)
        }
      } else {
        suppressWarnings({
          res <- unlist(lapply(names, FUN=function(pkg) {
            deps <- try(packageDescription(pkg, fields=fields, drop=TRUE))
            deps <- unlist(deps)
            # Handle 'SuggestsNote' field with 'Recommended:' specially
            deps <- grep("Recommended:[ ]*", deps, value=TRUE)
            deps <- gsub("Recommended:[ ]*", "", deps)
            pkgNames(deps)
          }))
        })
      }
      res <- unlist(res)
      res <- setdiff(res, c("R", "base", "methods", "tools", "utils"))
      res
    }

    # Find all package dependencies
    deps <- pkgDeps(pkgsT$name, recursive=TRUE)
    deps <- sort(unique(c(pkgsT$name, deps)))

    field <- "SuggestsNote"
    recs <- pkgDeps(deps, field=field)
    if (length(recs) > 0L) {
      recs <- recs[!isInstalled(recs)]
      if (length(recs) > 0L) {
        mprintf("Installing packages (according to DESCRIPTION field '%s'): %s\n", field, paste(recs, collapse=", "))
        for (rec in recs) {
          try(install.packages(rec, quiet=quiet, ...))
        }
      }
    }
  }


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Update?
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  update <- !any(sapply(pkgs$flags, FUN=function(x) is.element("u", x)))
  if (update) {
    ask <- !any(sapply(pkgs$flags, FUN=function(x) is.element("U", x)))
    # For some reason withCallingHandlers() fails to capture messages
    # on "Warning: unable to access index for repository ...".
    suppressWarnings({
      update.packages(..., quiet=quiet, ask=ask)
    })
  }

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Present what and how packages changed
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  pkgs1 <- installed.packages()[,"Version"]
  pkgs1c <- paste(names(pkgs1), pkgs1)

  # Identify difference
  d32 <- match(setdiff(pkgs1c, pkgs0c), table=pkgs1c)
  p32 <- pkgs1[d32]
  d23 <- match(setdiff(pkgs0c, pkgs1c), table=pkgs0c)
  p23 <- pkgs0[d23]
  added <- setdiff(names(p32), names(p23))
  removed <- setdiff(names(p23), names(p32))
  updated <- intersect(names(p23), names(p32))

  # Compile into a data frame
  pmods <- unique(c(pkgs$name, added, removed, updated))
  if (length(pmods) > 0L) pmods <- sort(pmods)
  mods <- data.frame(Previous=unname(pkgs0[pmods]), Current=unname(pkgs1[pmods]), stringsAsFactors=FALSE)
  rownames(mods) <- pmods
  if (length(pmods) > 0L) {
    if (length(rr <- c(added, updated)) > 0) mods[rr,"Current"] <- p32[rr]
    if (length(rr <- c(removed, updated)) > 0) mods[rr,"Previous"] <- p23[rr]
    msg <- capture.output(print(mods))
    msg <- paste(c("", "Package updates:", msg), collapse="\n")
  } else {
    msg <- "\nNo packages were installed or updates."
  }
  message(msg)


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Failure or success?
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  failed <- rownames(mods)[is.na(mods$Current)]
  if (length(failed) > 0L) {
    msg <- "\nFailed to install requested package"
    if (length(failed) > 1L) msg <- sprintf("%ss", msg)
    msg <- sprintf("%s: %s", msg, paste(sQuote(failed), collapse=", "))
    message(msg)
    if (length(warns) > 0L) {
      wmsgs <- sapply(warns, FUN=function(w) w$message)
      wmsgs <- unique(wmsgs)
      wmsgs <- paste(sprintf(" (W%d) %s", seq_along(wmsgs), wmsgs), collapse="\n")
      mprintf("\nThe reason may be explained by one of the following warnings caught:\n\n%s\n---\n", wmsgs)
    }
  } else {
    # Add library() commands to the R command line history
    cmds <- sprintf("library('%s')", pkgs$name)
    inHistory <- tryCatch({
      tf <- tempfile(pattern=".Rhistory-")
      savehistory(tf)
      hist <- readLines(tf)
      cmdAll <- paste(cmds, collapse="; ")
      hist <- c(hist, cmdAll)
      writeLines(con=tf, hist)
      loadhistory(tf)
      TRUE
    }, error = function(ex) { FALSE })

    if (nrow(pkgs) == 1L) {
      msg <- "Package installed/updated. Load it by:"
    } else {
      msg <- "Packages installed/updated. Load them by:"
    }
    vers <- sapply(pkgs$name, FUN=packageVersion)
    msg <- c(msg, "", sprintf(" %s", cmds))
    msg <- c("", msg, "")
    if (inHistory) {
      msg <- c(msg, "(These commands have been added to your commands history.)",  "")
    }
    msg <- paste(msg, collapse="\n")
    message(msg)
  }
} # installer()
