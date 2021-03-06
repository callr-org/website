installer <- function(pkgs=NULL, recursive=FALSE, update=FALSE, ...) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Local functions
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Don't assume 'utils' is attached
  install.packages <- utils::install.packages
  installed.packages <- utils::installed.packages
  update.packages <- utils::update.packages
  packageDescription <-  utils::packageDescription
  packageVersion <-  utils::packageVersion
  savehistory <- utils::savehistory


  knownRepos <- function() {
    p <- file.path(Sys.getenv("HOME"), ".R", "repositories")
    if (!file.exists(p)) p <- file.path(R.home("etc"), "repositories")
    ns <- getNamespace("tools")
    .read_repositories <- get(".read_repositories", envir=ns)
    a <- .read_repositories(p)
    repos <- a$URL
    names(repos) <- rownames(a)

    ## Bioconductor
    ## Note: R tries to keep track of the current Bioconductor version
    ## internally.  However, it is hard coded in the R version installed,
    ## which means that it is only updated when the R version is updated.
    ## This means that unless you're running an updated R patched version,
    ## you're stuck with the Bioconductor version that was at the time
    ## of the R stable release.
    
    isPatched <- grepl("Patched", R.version$status, ignore.case = TRUE)
    if (!isPatched) {
      ## FIXME: Look up Bioc version for current version of R.
    }
    
    ## WORKAROUND: RCurl 1.96.0 is buggy / HB 2015-05-08
    ## Don't install from Omegahat, because it provides
    ## RCurl 1.96.0 (1.95-4.6 is on CRAN), which despite
    ## installing properly, causes httr::GET() -> ... ->
    ## RCurl::curlPerform() to core dump R.
    names <- names(repos)
    names <- setdiff(names, "Omegahat")
    repos <- repos[names]

    repos
  } # knownRepos()

  pkgNames <- function(s) {
    s <- na.omit(s)
    if (length(s) == 0L) return(s)
    s <- unlist(strsplit(s, split=",", fixed=TRUE))
    s <- gsub("[(].*", "", s)
    gsub("^[ \n]+|[ \n]+$", "", s)
  }

  pkgDeps <- function(names, fields=c("Depends", "Imports", "LinkingTo"), recursive=FALSE) {
    withVerbose({
      message(sprintf("pkgDeps(%s, recursive=%s):\n", paste(sQuote(names), collapse=", "), recursive))
    })

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
          deps <- try(packageDescription(pkg, fields=fields, drop=FALSE))
          if (!is.list(deps)) return(character(0L))

          depsS <- deps$SuggestsNote
          deps$SuggestsNote <- NULL
          deps <- unlist(deps)
          deps <- pkgNames(deps)

          if (length(depsS) > 0L) {
            # Handle 'SuggestsNote' field with 'Recommended:' specially
            recs <- grep("^(.*|)Recommended:[ ]*", depsS, value=TRUE)
            recs <- gsub("^(.*|)Recommended:[ ]*", "", recs)
            recs <- pkgNames(recs)
            deps <- sort(unique(c(deps, recs)))
          }

          deps
        }))
      })
    }
    res <- unlist(res)
    res <- setdiff(res, c("R", "base", "methods", "tools", "utils"))

    withVerbose({
      message("  Packages:")
      mstr(res)
    })

    res
  }


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Setup
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  withVerbose({
    message("Argument 'pkgs':\n")
    mprint(pkgs)
  })

  repos <- getOption("repos")

  # Append other known repositories, iff missing
  repos <- c(repos, knownRepos())

  # Append other known repositories, iff missing
  repos <- c(repos, "AROMA"="https://braju.com/R")

  # Keep only unique ones
  names <- names(repos)
  dups <- (nzchar(names) & duplicated(names))
  repos <- repos[!dups]

  # FIXME: Drop non-functional 'R-Forge[https]' /2015-08-25
  repos <- repos[!grepl("R-Forge[https]", names(repos), fixed=TRUE)]

  # FALLBACK: Iff CRAN is not set, use predefined repository
  if (identical(repos[["CRAN"]], "@CRAN@")) {
     repos[["CRAN"]] <- "https://cloud.r-project.org"
  }

  # Set temporarily
  oopts <- options(repos=repos)
  on.exit(options(oopts), add=TRUE)

  withVerbose({
    message("Repositories:\n")
    mprint(repos)
  })

  # Record package state before
  pkgs0 <- installed.packages()[,"Version"]
  pkgs0c <- paste(names(pkgs0), pkgs0)


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (is.character(pkgs)) {
    pkgs <- data.frame(
      name=pkgs,
      fullname=pkgs,
      flags="",
    stringsAsFactors=FALSE)
  } else if (!is.data.frame(pkgs)) {
    stop("Argument 'pkgs' must be a data frame or a character vector: ", mode(pkgs))
  }

  # Nothing to do?
  if (nrow(pkgs) == 0L) message("No packages specified.")


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Flags
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## Global flags
  # (u) Disable update of all installed packages
  update <- update && !any(sapply(pkgs$flags, FUN=function(x) is.element("u", x)))

  # (U) If update, ask user to confirm?
  update_ask <- !any(sapply(pkgs$flags, FUN=function(x) is.element("U", x)))

  # (Q) Super quiet installation
  quiet <- any(sapply(pkgs$flags, FUN=function(x) is.element("Q", x)))

  ## Package specific flags
  # (!) Force installation
  pkgs$force <- sapply(pkgs$flags, FUN=function(x) is.element("!", x))

  # (s) Install requested packages from source tar.gz
  pkgs$source <- sapply(pkgs$flags, FUN=function(x) is.element("s", x))

  # (D) Install packages directly from URLs
  pkgs$url <- any(sapply(pkgs$flags, FUN=function(x) is.element("D", x)))

  # (G) Install packages directly from GitHub
  pkgs$github <- any(sapply(pkgs$flags, FUN=function(x) is.element("G", x)))

  # (S) Also install suggested packages
  pkgs$suggests <- sapply(pkgs$flags, FUN=function(x) is.element("S", x))

  # (r) Also install "recommended" packages.  Recommended packages
  #     are those listed in DESCRIPTION field 'SuggestsNote:' after
  #     string "Recommended:".  This particular format is not official
  #     (CRAN, Bioconductor, ...).  It was invented by callr.org and
  #     is accepted by CRAN rules.
  pkgs$recommends <- !sapply(pkgs$flags, FUN=function(x) is.element("r", x))


  withVerbose({
    message("Requested packages (with parsed flags):\n")
    mprint(pkgs)
  })


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Install from 'source' as well even if not specified by options
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  pkgType <- getOption("pkgType")
  if (grepl("binary", pkgType) && getRversion() >= "3.1.3") {
    msg <- sprintf("NOTE: Detected option pkgType='%s'. Temporarily changed to 'both' during this installation process in order to make sure the most up-to-date versions of packages are installed in case they are only available as source.", pkgType)
    mcat(msg)
    warning(msg)
    oopts <- options(pkgType="both")
    on.exit(options(oopts), add=TRUE)
  }

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Install requested packages
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  pkgs$isInstalled <- isPackageInstalled(pkgs$name)
  pkgsT <- subset(pkgs, !isInstalled | force)

  withVerbose({
    message("Packages to install:\n")
    mprint(pkgsT)
  })

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
      oneByOne <- (length(pkgsR$suggests) == 1L) &&
                   any(pkgsR$suggests != pkgsR$suggests[1])
      if (!oneByOne) {
        deps <- if (pkgsR$suggests[1]) TRUE else NA
        res <- try(install.packages(pkgsR$name, dependencies=deps, quiet=quiet, ...))
        ## Failed to install one or more of the packages?
        ## Then retry to install them one-by-one
        oneByOne <- (nrow(pkgsR) > 1L) && !inherits(res, "try-error")
        if (oneByOne) {
          message('At least one of the packages failed to install. Will fall back to install each of them one by one: ', paste(sQuote(pkgsR$name), collapse=", "))
        }
      }
      if (oneByOne) {
        mapply(pkgsR$name, pkgsR$suggests, FUN=function(pkg, deps) {
          deps <- if (deps) TRUE else NA
          try(install.packages(pkg, dependencies=deps, quiet=quiet, ...))
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
      requireRemotes()

      # Install one by one
      for (kk in seq_len(nrow(pkgsU))) {
        pkg <- pkgsU[kk,]
        url <- pkg$fullname
        remotes::install_url(url)
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
      requireRemotes()

      # Install one by one
      for (kk in seq_len(nrow(pkgsG))) {
        pkg <- pkgsG[kk,]
        remotes::install_github(pkg$fullname)
      }
    })
  }


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # (d) Install "Recommended" packages (only from package repositories)
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  pkgsT <- subset(pkgs, recommends)
  if (nrow(pkgsT) > 0L) {
    # Find all package dependencies
    deps <- pkgDeps(pkgsT$name, recursive=TRUE)
    deps <- sort(unique(c(pkgsT$name, deps)))

    withVerbose({
    })
    field <- "SuggestsNote"
    recs <- pkgDeps(deps, fields=field)
    if (length(recs) > 0L) {
      recs <- recs[!isPackageInstalled(recs)]
      if (length(recs) > 0L) {
        message(sprintf("Installing packages (according to DESCRIPTION field '%s'): %s", field, paste(recs, collapse=", ")))
        for (rec in recs) {
          try(install.packages(rec, quiet=quiet, ...))
        }
      }
    }
  }


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Scan recursively for "recommended" package?
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (recursive && FALSE) {  ### NOT NEEDED /HB 2015-01-28
    withVerbose({
      message("Scanning recursively dependent packages for \"recommended\" packages:\n")
    })
    deps <- pkgDeps(pkgs$name, fields=c("Depends", "Imports", "LinkingTo", "SuggestsNote"), recursive=TRUE)
    if (length(deps) > 0L) {
      withVerbose({
        message(sprintf("  Found %d dependent packages.\n", length(deps)))
      })
      recs <- character(0L)
      for (pkg in deps) {
        pkgsR <- pkgDeps(pkg, fields="SuggestsNote", recursive=TRUE)
        recs <- c(recs, pkgsR)
      }
      ## AD HOC: Drop anything that doesn't look like a package name
      recs <- grep(" ", recs, invert=TRUE, value=TRUE)
      recs <- sort(unique(recs))
      if (length(recs) > 0L) {
        withVerbose({
          message(sprintf("  Found %d \"recommended\" packages.\n", length(recs)))
        })
        recs <- recs[!isPackageInstalled(recs)]
        if (length(recs) > 0L) {
          message(sprintf("  Found %d \"recommended\" packages not already installed.\n", length(recs)))
          mprint(recs)
          message("Will try to install those as well:")
          installer(recs, recursive=FALSE, update=FALSE)
        }
      }
    }
  } # if (recursive)


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Update?
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (update) {
    # For some reason withCallingHandlers() fails to capture messages
    # on "Warning: unable to access index for repository ...".
    suppressWarnings({
      try(update.packages(..., quiet=quiet, ask=update_ask))
    })
  } else {
    # Always try to update the requested packages, unless installed above
    pkgsTT <- setdiff(pkgsT$name, c(pkgsR$name, pkgsU$name, pkgsG$name))
    if (length(pkgsTT) > 0L) {
      suppressWarnings({
        try(update.packages(oldPkgs=pkgsTT, ..., quiet=quiet, ask=update_ask))
      })
    }
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
      msg <- sprintf("\nThe reason may be explained by one of the following warnings caught:\n\n%s\n---\n", wmsgs)
      message(msg)
    }
  } else {
    msg <- if (nrow(pkgs) == 1L) "Package" else "Packages"
    msg <- sprintf("%s installed/updated.", msg)

    if (interactive()) {
      # Add library() commands to the R command line history
      tryCatch({
        tf <- tempfile(pattern=".Rhistory-")
        savehistory(tf)
        hist <- readLines(tf)
        cmds <- sprintf("library('%s')", pkgs$name)
        cmdAll <- paste(cmds, collapse="; ")
        hist <- c(hist, cmdAll)
        writeLines(con=tf, hist)
        loadhistory(tf)

        msg <- c(
          sprintf("%s Load %s by:\n", msg,
                  if (nrow(pkgs) == 1L) "it" else "them"),
          "",
          sprintf(" %s", cmds),
          "",
          "(These commands have been added to your commands history.)"
        )
      }, error = function(ex) {})
    }

    msg <- paste(c("", msg), collapse="\n")
    message(msg)
  }
} # installer()
