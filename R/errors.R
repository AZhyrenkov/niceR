# basic error handler and slack logger ----
errorHandler <- function(code, prefix) {
  require(slackr)
  tryCatch(
    code,
    error = function(c){
      errorMessage = paste0(now(),', ', 'Error ', prefix, c$message)
      slackr_bot(errorMessage)
      stop()
    },
    warning = function(c){
      warningMessage = paste0(now(),', ', 'Warning ', prefix, '\n', c$message)
      slackr_bot(warningMessage)
    })
}
