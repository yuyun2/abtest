library(tidyverse)

sample_size_proportion <- function(p1, delta, alpha, power){
  p2 = p1 - delta
  p1 = p2 + delta
  
  if(length(c(p1, delta, alpha, power))==4){
  z_alpha_beta = (qnorm(alpha/2, lower.tail = F) + qnorm(1 - power, lower.tail = F))^2
  n = round(z_alpha_beta * (p1*(1-p1) + p2*(1-p2)) / (p1-p2)^2)
  print(paste("The sample size for a two-sample Z-test of proportions is", n))
  }
  
  else{
    
    for(i in 1:4){
      if(length(list(p1, delta, alpha, power)[[i]]) > 1){
        multiple = i
      }
    }
    
    parameters = c("p1", "delta", "alpha", "power")
    
    print(paste("You have selected multiple values for", parameters[multiple], ":",
                paste(list(p1, delta, alpha, power)[[multiple]], collapse = ", ")))
    
    n = c()
    
     if(parameters[multiple]=="p1"){
       p1 = seq(p1[1], p1[2], .01)
       p2 = p1 - delta
       
       
      for(i in 1:length(p1)){
        z_alpha_beta = (qnorm(alpha/2, lower.tail = F) + qnorm(1 - power, lower.tail = F))^2
        n[i] = round(z_alpha_beta * (p1[i]*(1-p1[i]) + p2[i]*(1-p2[i])) / (p1[i]-p2[i])^2)
       
      print(paste("For p1 of", p1[i], "the sample size is", n[i]))
       

          
      }
       data = data_frame(n,p1)
       ggplot(data, aes(x=p1, y=n)) + geom_line() +
         ggtitle(paste('Sample Size vs Proportion A with alpha of',
                       alpha, 'and power of', power)) +
         xlab("Proportion A") + ylab("Sample Size (n)")
       
         
    }
    
    
    else if(parameters[multiple]=="delta"){
      
      p2 = p1 - delta
      p1 = p2 + delta
      
      for(i in 1:length(delta)){
        z_alpha_beta = (qnorm(alpha/2, lower.tail = F) + qnorm(1 - power, lower.tail = F))^2
        n[i] = round(z_alpha_beta * (p1[i]*(1-p1[i]) + p2[i]*(1-p2[i])) / (p1[i]-p2[i])^2)
        print(paste("For delta of", p1[i] - p2[i], "the sample size is", n[i]))
      }
      
      
      data = data_frame(n,delta)
      ggplot(data, aes(x=delta, y=n)) + geom_line() +
        ggtitle(paste('Sample Size vs delta with alpha of',
                      alpha, 'and power of', power)) +
        xlab("delta") + ylab("Sample Size (n)")
    }
    
    
    else if(parameters[multiple]=="alpha"){
      alpha = seq(alpha[1], alpha[2], .01)
      for(i in 1:length(alpha)){
    z_alpha_beta = (qnorm(alpha[i]/2, lower.tail = F) + qnorm(1 - power, lower.tail = F))^2
    n[i] = round(z_alpha_beta * (p1*(1-p1) + p2*(1-p2)) / (p1-p2)^2)
    print(paste("For alpha of", alpha[i], ", the sample size is", n[i]))
      }
      
      data = data_frame(n,alpha)
      ggplot(data, aes(x=alpha, y=n)) + geom_line() +
        ggtitle(paste('Sample Size vs alpha with Proportion A of',
                      p1, 'and power of', power)) +
        xlab("alpha") + ylab("Sample Size (n)")
      
    }
    
    
    else if(parameters[multiple]=="power"){
      power = seq(power[1], power[2], .01)
      
      for(i in 1:length(power)){
        z_alpha_beta = (qnorm(alpha/2, lower.tail = F) + qnorm(1 - power[i], lower.tail = F))^2
        n[i] = round(z_alpha_beta * (p1*(1-p1) + p2*(1-p2)) / (p1-p2)^2)
        print(paste("For power of", power[i], "the sample size is", n[i]))
      }
      
      data = data_frame(n,power)
      ggplot(data, aes(x=power, y=n)) + geom_line() +
        ggtitle(paste('Sample Size vs Power with alpha of',
                      alpha, 'and delta of', delta)) +
        xlab("Power") + ylab("Sample Size (n)")
      
      
      
    }
    
    
    
  
  }
}

sample_size_proportion(p1 = 0.1,delta =  .05, alpha = .05, power = .8)
sample_size_proportion(p1 = c(.1, .2),delta =  .05, alpha = .05, power = .8)
sample_size_proportion(p1 = .1,delta =  c(.05,.1), alpha = .05, power = .8)
sample_size_proportion(p1 = .1,delta =  .05, alpha = c(.05, .1), power = .8)
sample_size_proportion(p1 = .3, delta =  .2, alpha = .01, power = c(.8,.9))
