library(tidyverse)

sample_size_mean <- function(sigma, alpha, power){
  
  if(length(c(sigma, alpha, power))==3){
    n = round((2*(qnorm(alpha/2) - qnorm(power))^2)/(sigma^2))
    print(paste("The sample size for a two-sample t-test of mean is", n))
  }
  
  else{
    
    for(i in 1:3){
      if(length(list(sigma, alpha, power)[[i]]) > 1){
        multiple = i
      }
    }
    
    parameters = c("sigma", "alpha", "power")
    
    print(paste("You have selected multiple values for", parameters[multiple], ":",
                paste(list(sigma, alpha, power)[[multiple]], collapse = ", ")))
    
    n = c()
    
    if(parameters[multiple]=="sigma"){
      
      
      for(i in 1:length(sigma)){
        n[i] = round((2*(qnorm(alpha/2) - qnorm(power))^2)/(sigma[i]^2))
        
        print(paste("For sigma of", sigma[i], "the sample size is", n[i]))
        
      }
      data = data_frame(n,sigma)
      ggplot(data, aes(x=sigma, y=n)) + geom_line() +
        ggtitle(paste('Sample Size vs Sigma with Alpha of',
                      alpha, 'and Power of', power)) +
        xlab("Sigma") + ylab("Sample Size (n)")
      
      
    }
    
    
    else if(parameters[multiple]=="alpha"){
      for(i in 1:length(alpha)){
        n[i] = round((2*(qnorm(alpha[i]/2) - qnorm(power))^2)/(sigma^2))
        print(paste("For alpha of", alpha[i], ", the sample size is", n[i]))
      }
      
      data = data_frame(n,alpha)
      ggplot(data, aes(x=alpha, y=n)) + geom_line() +
        ggtitle(paste('Sample Size vs Alpha with Sigma of ',
                      sigma, 'and Power of', power)) +
        xlab("alpha") + ylab("Sample Size (n)")
      
    }
    
    
    else if(parameters[multiple]=="power"){
      for(i in 1:length(power)){
        n[i] = round((2*(qnorm(alpha/2) - qnorm(power[i]))^2)/(sigma^2))
        print(paste("For power of", power[i], "the sample size is", n[i]))
      }
      data = data_frame(n,power)
      ggplot(data, aes(x=power, y=n)) + geom_line() +
        ggtitle(paste('Sample Size vs Power with Sigma of',
                      sigma, 'and Alpha of', alpha)) +
        xlab("Power") + ylab("Sample Size (n)")
      }
    
  
  }
}

sample_size_mean(sigma =  .5, alpha = .05, power = .8)
sample_size_mean(sigma = c(.5, 1), alpha = .05, power = .8)
sample_size_mean(sigma = .5, alpha = c(.01, .05), power = .8)
sample_size_mean(sigma = .5, alpha = .05, power = c(.8, .9))

