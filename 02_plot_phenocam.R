##' Plot Phenocam data
##' 
##' @param dat  dataframe of date, gcc_mean, gcc_std
##' @param ...  additional graphing parameters
plot_phenocam <- function(dat, pred = NULL){
  
  if(!is.null(dat)){
    
    dat <- dat %>% 
      mutate(ylo = gcc_mean - 1.96 * gcc_std,
             yhi = gcc_mean + 1.96 * gcc_std)
    
    ## 
    p <- ggplot(dat, aes(x = date)) +
      geom_ribbon(aes(ymin = ylo, ymax = yhi),
                  alpha = 0.70,
                  fill = "lightblue") +
      geom_line(aes(y = gcc_mean))
    
    if(!is.null(pred)){
      p <- p  +
        geom_line(data = pred, aes(x = date, y = pred))
      
    }
    
    p
    
  } else {
    print("plot_phenocam: input data not provided")
  }
  
}