# This is the simplest code for applying the DbS model using the DbS.R
# library to the Kahneman and Tversky (1979) data. 
#
# R CMD BATCH Kahneman_Tversky_1979_example.R should produce example output
source("DbS.R")

source("Kahneman_Tversky_1979_data.R")

# Thresholds for discriminating amounts and probabilities
thresholds=list(x=10, p=.1)

# Set the context with samples from the Stewart, Brown, and Chater (2006)
# distributions of gains, losses, and probabilities
exchange.rate <- 2.06044 # 1 British pound = 2.14 4 Israeli pounds
context <- list(
	x=exchange.rate * scan("amounts.csv"),
	p=scan("probs.csv")
)

# Or uncomment the line below for a uniform distribution for the background
# context of amounts and probabilities
# context <- list(x=seq(-6000,6000,1), p=seq(-1,1,.01))

# Probability of selecting each gamble for each choice
predictions <- DbS(choices=choices, 
	thresholds=thresholds, 
	context=context, 
	prob.sample.context=.5, 
	prob.sample.amount=.5, 
	self.comparison=F, 
	choice.mechanism="random walk",
	choice.threshold=5
)
predictions
# Probability of selecting the first gamble in each choice
sapply(predictions,function(z) {z[[1]]})
