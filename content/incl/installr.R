installr.use <- function(update=TRUE, skip=TRUE) {
  updated <- getOption("callr.org/installr::updated", FALSE)
  update <- update && !(skip && updated)

  ## Use an up-to-date version of 'installr'
  if (!isPackageInstalled("installr")) {
    install.packages("installr")
  } else if (update) {
    suppressWarnings({
      update.packages(oldPkgs="installr")
    })
    options("callr.org/installr::updated"=TRUE)
  }

  suppressPackageStartupMessages(library("installr"))
}

installr.list <- function(update=TRUE) {
  installr.use(update=update)
  ns <- getNamespace("installr")
  known <- ls(pattern="^install[.]", envir=ns)
  known <- gsub("^install[.]", "", known)
  known <- tolower(known)
  known <- setdiff(known, c("packages.zip", "url"))
  known <- sort(unique(known))
  known
}

installr.install <- function(tool=NULL, update=TRUE, ...) {
  installr.use(update=update)
  ns <- getNamespace("installr")

  ## Install from a menu?
  if (tool == "menu") {
    return(installr(use_GUI=FALSE))
  }

  msg <- "Installing software:"
  message(msg, " ", paste(sQuote(tool), collapse=", "))

  ## Check if it's one of the known software tools
  known <- installr.list(update=FALSE)

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
