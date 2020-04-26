# Libraries
library(ggplot2)
library(hrbrthemes)
library(RColorBrewer)

# for control: N = 914
inputs <- c(21.77,16.74,14.22,12.47,10.94,8.75,7.99,7.11)
#inputs <- c(75,25)
simulationRange <- seq(10, 1000, by=1)
deviations <- matrix(nrow=length(simulationRange), ncol=length(inputs))
for (c in 1:dim(deviations)[1]) {
  for (r in 1:dim(deviations)[2]) {
    tmp = (simulationRange[c] * (inputs[r] / 100)) %% 1
    if (tmp > 0.5)
      tmp = 1 - tmp;
    deviations[c,r] <- tmp 
  }
}

results <- apply(deviations, 1, mean)

data <- data.frame(simulationRange,results)
ggplot(data, aes(x=simulationRange, y=results)) +
  geom_line()

# How accruate are inputs
sum(inputs)
# output N
data$simulationRange[data$results == min(data[,2])]
