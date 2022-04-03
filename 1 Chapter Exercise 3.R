DiceTosses <- function (n){
  return (sample(c(1:6), n, replace = TRUE))
}

GalileoExperiment <- function (n){
  temp=c()
  for (x in 1:n){
    temp=c(temp,sum(DiceTosses(3)))
  }
  return (temp)
}

temp=as.data.frame(GalileoExperiment(1000000))
colnames(temp)="DiceSum"
ggplot(data=temp, aes(x=DiceSum)) + geom_histogram(binwidth=0.5) +ylab("Freqeuncy of Sum of Three Dices") + xlab("Sum of Three Dices")

