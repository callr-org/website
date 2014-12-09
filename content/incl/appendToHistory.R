appendToHistory <- function(cmds, verbose=FALSE) {
  tf <- tempfile(pattern=".Rhistory-")
  on.exit({
    try(unlink(tf))
  })

  success <- tryCatch({
    savehistory(tf)
    hist <- readLines(tf)
    cmdAll <- paste(cmds, collapse="; ")
    hist <- c(hist, cmdAll)
    writeLines(con=tf, hist)
    loadhistory(tf)
    TRUE
  }, error = function(ex) { FALSE })

  if (success && verbose) {
    msg <- c("The following have been appended to the command history:",
             "", cmds, "")
    msg <- paste(msg, collapse="\n")
    message(msg)
  }
} # appendToHistory()
