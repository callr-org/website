printCode <- function(name, class=NULL, mode="function", envir=NULL, ...) {
  fullname <- paste(c(name, class), collapse=".")
  if (is.character(envir)) envir <- getNamespace(envir)
  obj <- get(fullname, mode=mode, envir=envir)
  attributes(obj) <- NULL
  environment(obj) <- parent.frame()
  print(obj)
} # printCode()

# HISTORY:
# 2014-09-03
# o Created.
