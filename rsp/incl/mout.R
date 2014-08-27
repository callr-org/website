if (getOption("install.debug", FALSE)) {
  message <- base::message
  captureOutput <- utils::capture.output
} else {
  message <- function(...) {}
  captureOutput <- function(...) ""
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# From R.utils 1.33.0
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
mout <- function(..., appendLF=FALSE) {
  bfr <- captureOutput(..., envir=parent.frame())
  bfr <- paste(c(bfr, ""), collapse="\n")
  message(bfr, appendLF=appendLF)
}

mprint <- function(..., appendLF=FALSE) {
  bfr <- captureOutput(print(...), envir=parent.frame())
  bfr <- paste(c(bfr, ""), collapse="\n")
  message(bfr, appendLF=appendLF)
}

mcat <- function(..., appendLF=FALSE) {
  bfr <- captureOutput(cat(...), envir=parent.frame())
  bfr <- paste(c(bfr, ""), collapse="\n")
  message(bfr, appendLF=appendLF)
}

mstr <- function(..., appendLF=FALSE) {
  bfr <- captureOutput(str(...), envir=parent.frame())
  bfr <- paste(c(bfr, ""), collapse="\n")
  message(bfr, appendLF=appendLF)
}

mshow <- function(..., appendLF=FALSE) {
  bfr <- captureOutput(show(...), envir=parent.frame())
  bfr <- paste(c(bfr, ""), collapse="\n")
  message(bfr, appendLF=appendLF)
}

mprintf <- function(..., appendLF=FALSE) {
  bfr <- sprintf(...)
  message(bfr, appendLF=appendLF)
}
