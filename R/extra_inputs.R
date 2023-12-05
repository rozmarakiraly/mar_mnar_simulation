quantileSliderInput <- function(...){
  
  args <- list(...)
  
  # browser()
  
  if(!('min' %in% names(args)) & 'step' %in% names(args)){
    args$min <- args$step
  }
  
  if(!('max' %in% names(args)) & 'step' %in% names(args)){
    args$max <- 100-args$step
  }
  
  sliderInput(
    inputId = args[[1]],
    label = paste0(args[[2]], " (expressed in quantiles)"),
    min = args$min,
    max = args$max,
    value = args$value,
    step = args$step
  )
}