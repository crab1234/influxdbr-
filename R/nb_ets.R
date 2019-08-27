#' @rdname nb_ets
#' @export
nb_ets <- function(data,freq,predlenth,force_positive=FALSE,debug=FALSE){
  t_pre <- 0
  t_ets <- 0
  t_write <- 0
  for(j in 1:length(data$results.series_values)){
    t1 = Sys.time()
    series_tags <- as.data.frame(data$results.series_tags[j])
    series_names <- paste(data$results.series_names[j],'pred',sep="_")
    
    temp<-as.data.frame(data$results.series_values[j])
    temp<-temp[complete.cases(temp),]
    
    y = ts(temp$y,frequency=freq)
    t2 = Sys.time()
    res_y = stlm(y, s.window = freq, robust = FALSE, method = "ets", etsmodel = "ZZN")
    pred_y = forecast(res_y, method = "ets", etsmodel = "ZZN", forecastfunction = NULL,
                      h = predlenth, level = 80,
                      fan = FALSE, lambda = res_y$lambda, biasadj = NULL, xreg = NULL,
                      newxreg = NULL)
    
    history <- history(data = temp,pred_y = pred_y,freq = freq, width = 2.0)
    future <- future(data = temp,pred_y = pred_y,freq = freq, pred_length = predlenth)
    output = rbind(history,future)
    if(force_positive){
      output[output<0] <- 0
    }
    output <- cbind(output,series_tags) 
    t3 = Sys.time()
    influx_write(con = con, 
                 db = db,
                 x = output,
                 time_col = "time", 
                 tag_cols = colnames(output)[-c(1:4)],
                 measurement = series_names)
    t4 = Sys.time()
    t_pre <- t_pre + t2 -t1
    t_ets <- t_ets + t3 -t2
    t_write <- t_write + t4 -t3
  }
  if(debug){
    print("---time for pre---")
    print(t_pre)
    print("---time for ets---")
    print(t_ets)
    print("---time for write---")
    print(t_write)
  }
  
}

#' @keywords internal
history <- function(data,pred_y,freq,width){
  h = 0
  sum_value = 0
  mean_value = 0
  sum_square = 0
  mean_square = 0
  data2<-data
  data2$upper <- 0
  data2$lower <- 0
  data2$fitted <- as.numeric(pred_y$fitted)
  data2$sigma <- 0
  sigma <- 0
  for(i in 1:length(data2$y)){
    if(h < freq){
      h = h + 1
      sum_value = sum_value + data2$y[i]
      mean_value = sum_value/h
      sum_square = sum_square + data2$y[i]**2
      mean_square = sum_square/h
      sigma <- sqrt(mean_square - mean_value**2)
      data2$sigma[i] <- sigma
    }
    else if(h == freq){
      sum_value = sum_value + data2$y[i]-data2$y[i-h]
      mean_value = sum_value/h
      sum_square = sum_square + data2$y[i]**2 - data2$y[i-h]**2
      mean_square = sum_square/h
      sigma <- sqrt(mean_square - mean_value**2)
      data2$sigma[i] <- sigma
    }
    #if first several values are equal,sigma will be 0
    #need fix
    
  }
  data2$upper<-data2$fitted+data2$sigma*width
  data2$lower<-data2$fitted-data2$sigma*width
  history <- data.frame(time=data2$time, fitted=data2$fitted, upper=data2$upper,
                        lower=data2$lower)
  colnames(history) = c("time","yhat","y_upper","y_lower")
  return(history)
}

#' @keywords internal
future <- function(data,pred_y,freq,pred_length){
  time_delta = 86400 / freq
  future = time_delta*(1:pred_length)+data$time[length(data$time)]
  forecast <- data.frame(time=future, fitted=as.numeric(pred_y$mean),
                         upper=as.numeric(pred_y$upper), lower=as.numeric(pred_y$lower))
  colnames(forecast) = c("time","yhat","y_upper","y_lower")
  return(forecast)
}
