CoinTosses <- function (n){
  return (sample(c(0,1), n, replace = TRUE))
}

UpdatedCoinTosses <- function (n){
  m=length(n)
  result=c()
  for (x in 1:m){
      temp=c()
      for (y in 1:100){
        vector=sample(c(0,1), n[x], replace = TRUE)
        check=sum(vector)/n[x]
        check=ifelse(check<0.4,0,ifelse(check<=0.6,1,0))
        temp=c(temp,check)
      }
      temp=sum(temp)
      result=c(result,temp)
  }  
  return(result)
}

trial_vector=seq(1,250)
temp=as.data.frame(cbind(trial_vector,UpdatedCoinTosses(trial_vector)))
colnames(temp)=c("Number_of_tosses","Proportion_of_Heads")

library(ggplot2)
ggplot(data=temp,aes(x=Number_of_tosses,y=Proportion_of_Heads,color=ifelse(Proportion_of_Heads>94,"red","blue"))) +
   geom_point() + geom_hline(yintercept=95) + ylab("Proportion of heads (%) in 100 trials") + theme(legend.position="none")