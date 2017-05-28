printCode <- function(name, class=NULL, mode="function", envir=NULL, tweak=NULL, ...) {
  fullname <- paste(c(name, class), collapse=".")
  if (is.character(envir)) envir <- getNamespace(envir)
  obj <- get(fullname, mode=mode, envir=envir)
  attributes(obj) <- NULL
  output <- capture.output(print(obj, useSource=TRUE))
  if (length(output) == 0) return(output)
  idxs <- grep("<environment:", output, fixed=TRUE)
  if (length(idxs) > 0L) output <- output[1:(idxs[1]-1)]
  if (is.function(tweak)) output <- tweak(output)
  cat(output, sep="\n")
} # printCode()

# HISTORY:
# 2014-09-03
# o Created.
