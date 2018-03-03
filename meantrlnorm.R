
# use truncated lognormal
meantrlnorm <- function(meanlog, sdlog, lower, upper){
  b <- (log(upper) - meanlog)/sdlog
  a <- (log(lower) - meanlog)/sdlog
  if(!is.na(upper) & !is.na(lower)){  # if truncated at both left and right
    out <- exp(meanlog + (sdlog**2)/2)*(pnorm(sdlog - a) - pnorm(sdlog - b))/(pnorm(b) - pnorm(a))  
  } 
  if(!is.na(upper) & is.na(lower)){
    out <- exp(meanlog + (sdlog**2)/2)*pnorm(-sdlog + b)/pnorm(b)
  }
  if(is.na(upper) & !is.na(lower)){
    out <- exp(meanlog + (sdlog**2)/2)*pnorm(sdlog - a)/pnorm(-a)
  }
  if(is.na(upper) & is.na(lower)){
#     out <- exp(meanlog + (sdlog**2)/2)
    out <- NA
  }
  return(out)
}