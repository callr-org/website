captureOutput <- capture.output

# From R.utils 1.33.0
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

mprintf <- function(..., appendLF=FALSE) {
  bfr <- sprintf(...)
  message(bfr, appendLF=appendLF)
}

# Create debug output functions
for (mname in c("mprint", "mcat", "mstr", "mprintf")) {
  dname <- sub("m", "d", mname, fixed=TRUE)
  if (getOption("install.debug", FALSE)) {
    assign(dname, get(mname, mode="function"))
  } else {
    # No debug output
    assign(dname, function(...) {})
  }
}
