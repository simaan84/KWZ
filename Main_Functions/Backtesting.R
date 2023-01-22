main_run_portfolio_fun <- function(choose_data,gamma,sample_size,TC) {
  
  # choose the data - in total we have six different sets
  ds <- ds_list[[choose_data]]
  months_tot <- sort(unique(ds$date))
  
  # store data
  results_all <- data.frame()
  
  # store portfolio weights - relevant for portfolio turnover and TC
  W_ALL_list <- list()
  
  # run a loop from T until the end-1 of data
  for(i in sample_size:(length(months_tot) - 1)) {
    
    # current month
    month_i <- months_tot[i]
    # next month, which is unknown during portfolio construction
    month_i_plus <- months_tot[i+1]
    
    # for tracking backtesting
    if( month(month_i_plus) == 12) {
      cat("this is month ", as.character(month_i_plus),"\n")
    }
    
    # define the in-sample data
    R_sub <- ds[ds$date <= month_i,]
    run_DR_fun <- DR_function(R_sub,gamma,sample_size,TC)
    
    
    # keep track of weights and stack in matrix
    W_1_list <- lapply(run_DR_fun,t)
    W_1_list <- lapply(W_1_list,function(x) data.frame(month_i_plus,x) )
    W_ALL_list <- c(W_ALL_list,list(W_1_list))
    
    R_next <- ds[ds$date == month_i_plus,-1]
    R_port <- lapply(W_1_list, function(x)  sum(x[,-1]*R_next)  )
    R_port2 <- data.frame(date = month_i_plus,Reduce(cbind,R_port))
    names(R_port2)[-1] <- names(R_port)
    results_all <- rbind(results_all,R_port2)
  }
  
  list(port_ret = results_all, weights = W_ALL_list)
}


