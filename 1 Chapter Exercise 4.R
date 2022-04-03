volley <- function (){
  return (sample(c(0:1), 1, replace = TRUE, prob = c(0.4,0.6)))
}

raquetball <- function (){
  score_a=0
  score_b=0
  volley_status_a=1
  volley_status_b=0
  while (score_a<21 & score_b<21){
    if(volley_status_a==1){
      temp_score=volley()
      if(temp_score==1){
        score_a=score_a+1
      }
      else{
        volley_status_a=0
        volley_status_b=1
      }
    }
    else{
      temp_score=sample(c(0:1),1)
      if(temp_score==1){
        score_b=score_b+1
      }
      else{
        volley_status_b=0
        volley_status_a=1
      }
    }  
    
  }
  if (score_a==21){
    return(1)
  }
  else{
   return (0)
  }  
}

number_of_trials=100000

trial_vector=seq(1,number_of_trials)
results=rep(0,number_of_trials)
for (x in 1:number_of_trials){
  results[x]=raquetball()
}
temp=as.data.frame(cbind(trial_vector,results))
colnames(temp)=c("TrialNO","Winner")
temp$Winner=ifelse(temp$Winner==1,"A","B")

library(ggplot2)
library(dplyr)
Labels <- temp %>% group_by(Winner) %>% summarise(Sum=n()) 

ggplot(data=temp, aes(x=Winner,fill=Winner)) + geom_histogram(binwidth=0.5,stat="count") +
   ylab("Number of Trials won by each player") + xlab("Player") + 
    geom_text(data = Labels,aes(x=Winner,y=Sum,label=paste0(Sum/number_of_trials*100,"%")))
