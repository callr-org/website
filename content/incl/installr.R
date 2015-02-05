installr.install <- function(tool=NULL, ...) {
  ## Use an up-to-date version of 'installr'
  pkg <- "installr"
  if (!isPackageInstalled(pkg)) {
    install.packages(pkg)
  } else {
    suppressWarnings({
      update.packages(oldPkgs=pkg)
    })
  }

  suppressPackageStartupMessages(library("installr"))
  ns <- getNamespace("installr")

  ## Install from a menu?
  if (tool == "menu") {
    return(installr(use_GUI=FALSE))
  }

  ## Check if it's one of the known software tools
  known <- ls(pattern="^install[.]", envir=ns)
  known <- sort(gsub("^install[.]", "", known))
  if (tool == "list") {
    message("Software tools that can be installed by 'installr':")
    message(paste(sQuote(known), collapse=", "))
    return(FALSE)
  }

  msg <- "Installing software:"
  message(msg, " ", paste(sQuote(tool), collapse=", "))

  if (!is.element(tool, known)) {
    similar <- agrep(tool, known, max.distance=0.3, fixed=TRUE, value=TRUE)
    msg <- sprintf("Unknown software tool ('%s') specified; there is no installr::install.%s() function.", tool, tool)
    if (length(similar) > 0L) {
      msg <- paste(msg, "Did you mean:", paste(similar, collapse=", "))
    }
    stop(msg)
  }

  ## Get the installr installation function
  fcn <- get(sprintf("install.%s", tool), mode="function", envir=ns)
  stopifnot(is.function(fcn))

  ## Install tool
  fcn()
} # installr.install()
