printCode <- function(name, class=NULL, mode="function", envir=NULL, tweak=NULL, ...) {
  fullname <- paste(c(name, class), collapse=".")
  if (is.character(envir)) envir <- getNamespace(envir)
  obj <- get(fullname, mode=mode, envir=envir)
  attributes(obj) <- NULL
  output <- capture.output(print(obj, useSource=TRUE))
  if (length(output) == 0) return(output)
  idxs <- grep("<(bytecode|environment):", output, fixed=FALSE)
  if (length(idxs) > 0L) output <- output[-idxs]
  if (is.function(tweak)) output <- tweak(output)
  res <- tryCatch(parse(text = output), error = function(e) e)
  if (inherits(res, "error")) {
    stop("printCode() produced R code that does not parse: ", conditionMessage(res))
  }
  cat(output, sep="\n")
} # printCode()

# HISTORY:
# 2014-09-03
# o Created.
