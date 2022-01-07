std_cutp <- function (data, method, upper_per, lower_per) { 
  
  f <- switch(method,
              mean =  mean(data),
              median = median(data),
              percentile=quantile(data, probs=c(lower_per, upper_per)),
              cutp <- opt_cutp(data)
  )
  return(f)
}
opt_cutp <- function (data, index, type) { 
      #### optimal cut-point finder Contal et al 1999 & Mandrekar et al 2003

    if(type=="Overall"){
 	data$Overall.Survival.Time.in.Months = (data$Overall.Survival.Time.in.Days)/30.42
      	data$os = Surv(time=data$Overall.Survival.Time.in.Months, event=data$Vital.Status=="Dead")
    } 
	else {
 	data$Event.Free.Survival.Time.in.Months = (data$Event.Free.Survival.Time.in.Days)/30.42
      	data$os = Surv(time=data$Event.Free.Survival.Time.in.Months, event=data$Event.Status=="Event")
	}
      gx <- data[,index]
      cox.os = coxph(os ~ gx, data=data) 
      c1 <- cutp(cox.os)$gx
      data.table::setorder(c1, gx)
      percentile <- ecdf(c1$gx)
      low <- as.numeric(c1[order(c1$Q), ][nrow(c1), 1])
}