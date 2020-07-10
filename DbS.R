# Version 1.0.2
# Copyright 2010 Neil Stewart
# The program is distributed under the terms of the GNU General Public License
#
# DbS.R is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free
# Software Foundation, either version 3 of the License, or (at your option)
# any later version.
#
# DbS.R is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
#
# <http://www.gnu.org/licenses/>
#
# Version history 
#
# 1.0.1: Added code for accumulators to complement random walk code, thus
# extending the model beyond binary choice. Moved normalization of the
# probability of a favorable comparison from prob.random.walk() into DbS().
# This avoids duplicating normalization in prob.random.walk() and
# prob.accumulator(), and correctly moves conversion of the probability of a
# favorable comparison into the _relative_ probability of a favorable
# comparison into the DbS() function.
#
# 1.0.2: Revised prob.favourable() to correct for machine precision errors 
#
# 1.0.3: Added choice.threshold <- round(choice.threshold) to DbS() to ensure 
# that the choice threshold is an integer

####################################################################################################
#
# Code for dealing with gambles (matrices of branches by attributes) and choices (lists of gambles)
#
####################################################################################################

gamble <- function(x, p) {
#
# gamble(x=c(x1, x2, ...), p=c(p1, p2, ...)) returns a matrix with rows for
# each branch (pi chance of xi) and columns labeled "x" and "p" for each
# attribute
#
    g <- matrix(c(x,p), ncol=2)   
    colnames(g) <- c("x", "p")
    g
}

attributes.from.gamble <- function(gamble, attribute.type) {
#
# attributes.from.gamble(gamble=g, attribute.type="x"|"p") returns either
# the amounts or the probabilities from a gamble in a one column matrix
#
	gamble[ ,attribute.type, drop=F]
}

attributes.from.choice <- function(choice, attribute.type) {
#
# attributes.from.choice(choice=c, attribute.type="x"|"p") returns a vector
# of either all the amounts or all the probabilities in a choice
#
    z <- unlist(
		lapply(choice,attributes.from.gamble, attribute.type=attribute.type)
	)
    sort(z)
}

####################################################################################################
#
# Code for valencing a gamble, where the probabilities of bad things become negative numbers
#
####################################################################################################

valence.branch <- function(branch, absolute.threshold) {
#
# valence.branch(branch=c(x=x, p=p), absolute.threshold=t) returns the
# branch c(x=x, p=p) (i.e., a row from a gamble matrix), making p negative
# if x is less than t
#
	if(branch["x"] < absolute.threshold) {
		branch["p"] <- -branch["p"]
	}
	branch
}

valence.gamble <- function(gamble, absolute.threshold) {
#
# valence.gamble(gamble=g, absolute.threshold=t) returns the gamble g,
# making the probability negative for a branch if the corresponding amount
# is less than t
#

	t(apply(gamble, 1, valence.branch, absolute.threshold=absolute.threshold))
}

valence.choice <- function(choice, threshold, valence.rule) {
#
# valence.choice(choice=choice, threshold=threshold) valences each gamble in
# a choice. For gains, probabilities associated with amounts less than the
# minimum amount plus threshold are negated. For losses, the threshold point is
# the maximum gain less the threshold. For mixed gambles, the threshold point is
# zero
#
	xs <- attributes.from.choice(choice, "x")
	if(sum(xs<0) == 0) {
		# Gains
		if(valence.rule=="within threshold of min/max")
			t <- min(xs + threshold)
		else if(valence.rule=="within threshold of zero")
			t <- min( + threshold)
		else {
			print("Uncaught threshold rule")
			stop()
		}
	} else if(sum(xs>0) == 0) {
		# Losses
		if(valence.rule=="within threshold of min/max")
			t <- max(xs - threshold)
		else if(valence.rule=="within threshold of zero")
			t <- max( - threshold)
		else {
			print("Uncaught threshold rule")
			stop()
		}
	} else {
		# Mixed gamble
		t <- 0	
	}
	lapply(choice, valence.gamble, absolute.threshold=t)		
}

####################################################################################################
#
# Code for calculating the probability that a given attribute value will
# compare favorably with another member of the decision sample (i.e., with
# another of the attribute values in the choice or an attribute value from
# the wider context)
#
####################################################################################################

prob.favorable <- function(target.attribute, other.attributes, threshold) {
#
# prob.favorable(target.attribute=x,
#	other.attributes=c(x1, x2, ...),
#	threshold=threshold
# )
# returns the proportion of x1, x2, ... for which x is better by at least
# threshold
#
# The +1e-10 deals with problems due to limited machine precision
#
    sum(target.attribute-threshold+1e-10 >= other.attributes) /
        length(other.attributes)
}


prob.favorable.against.choice.and.context <- function(attribute,
	other.attributes, context, threshold, prob.sample.context) {
#
# prob.favorable.against.choice.and.context(attribute=x,
#	other.attributes=c(x1, x2, ...),
#	context=c(y1, y2, ...),
#	threshold=threshold,
#	prob.sample.context=p
# )
# returns the probability that x will be favorable (better by at least
# threshold) when compared with the x1, x2, ... under consideration in the
# choice and the attributes available from the context y1, y2, ... .
# prob.sample.context provides the relative weighting of attributes from the
# choice and from the context.
#
	prob.other.favorable <- prob.favorable(attribute,
		other.attributes, threshold)
	prob.context.favorable <- prob.favorable(attribute, context, threshold)
	(1-prob.sample.context)*prob.other.favorable +
		prob.sample.context*prob.context.favorable
}

prob.each.attribute.in.gamble.favorable <- function(gamble,
	other.gambles, thresholds, context, prob.sample.context) {
#
# prob.each.attribute.in.gamble.favorable(gamble=g,
#	other.gambles=list(g1,g2, ...),
#	thresholds=list(x=x.threshold, p=p.threshold),
#	context=list(x=c(y1, y2, ...), p=c(q1, q2, ...),
#	prob.sample.context=prob.sample.context
# )
# returns a matrix the same shape as gamble where each element is the
# probability that the corresponding attribute in gamble is favorable when
# compared to the other.gambles and context.
#
	amounts <- attributes.from.gamble(gamble, "x")
	other.amounts <- attributes.from.choice(other.gambles, "x")
	amounts.favorable <- sapply(amounts,
		prob.favorable.against.choice.and.context,
		other.attributes=other.amounts,
		threshold=thresholds[["x"]],
		context=context[["x"]],
		prob.sample.context=prob.sample.context
	)
	probs <- attributes.from.gamble(gamble, "p")
	other.probs <- attributes.from.choice(other.gambles, "p")
	probs.favorable <- sapply(probs,
		prob.favorable.against.choice.and.context,
		other.attributes=other.probs,
		threshold=thresholds[["p"]],
		context=context[["p"]],
		prob.sample.context=prob.sample.context
	)
	favorable <- matrix(c(amounts.favorable,probs.favorable), ncol=2)
	colnames(favorable) <- c("x", "p")
	favorable	
}

prob.gamble.favorable <- function(gamble, other.gambles, thresholds, context,
	prob.sample.context, prob.sample.amount) {
#
# prob.gamble.favorable(gamble=g,
#	other.gambles==list(g1,g2, ...),
#	thresholds=list(x=x.threshold, p=p.threshold),
#	context=list(x=c(y1, y2, ...), p=c(q1, q2, ...),
#	prob.sample.context=prob.sample.context,
#	prob.sample.amount=prob.sample.amount
# )
#
# returns the probability of a favorable comparison, averaged over all of
# the attributes in a gamble, with prob.sample.amount giving the relative
# weighting of amounts compared to probabilities
#
	each.attribute.probs <- prob.each.attribute.in.gamble.favorable(
		gamble=gamble,
		other.gambles=other.gambles,
		thresholds=thresholds,
		context=context,
		prob.sample.context=prob.sample.context
	)
	mean(prob.sample.amount *
		attributes.from.gamble(each.attribute.probs,"x")
		+ (1-prob.sample.amount) *
		attributes.from.gamble(each.attribute.probs,"p")
	)
}

prob.each.gamble.in.choice.favorable <- function(choice, thresholds, context,
	prob.sample.context, prob.sample.amount, self.comparison=F) {
#
# prob.each.gamble.in.choice.favorable(choice=c,
#	thresholds=list(x=x.threshold, p=p.threshold),
#   context=list(x=c(y1, y2, ...), p=c(q1, q2, ...),
#   prob.sample.context=prob.sample.context,
#   prob.sample.amount=prob.sample.amount 
#	self.comparison=T|F
# )
# returns a vector, where each element is the average probability than an
# attribute from a gamble will compare favorably against attributes from the
# other gambles and from the context. With self.comparison=F an attribute is
# compared with attributes from only other gambles but with
# self.comparison=T an attribute is compared with all other attributes,
# including those from the same gamble.
#
	if(self.comparison) {
		sapply(choice,
			prob.gamble.favorable,
			other.gambles=choice,
			thresholds=thresholds,
            context=context,
            prob.sample.context=prob.sample.context,
            prob.sample.amount=prob.sample.amount
		)
	} else {
		drop.ith <- function(i, x) {x[-i]}
		other.gambles.for.each.gamble <- lapply(1:length(choice), drop.ith, x=choice)
		mapply(prob.gamble.favorable,
			choice,
			other.gambles.for.each.gamble,
			MoreArgs=list(thresholds=thresholds,
				context=context,
				prob.sample.context=prob.sample.context,
				prob.sample.amount=prob.sample.amount
			)
		)
	}
}

####################################################################################################
#
# Code for the random walk choice mechanism
#
####################################################################################################

prob.random.walk <- function(prob.favorable, choice.threshold) {
#
# prob.random.walk(prob.favorable=c(p1, p2), choice.threshold=t) returns a
# vector with the probablity that a random walk will result in a choice for
# each gamble. p1 and p2 give the probability of a unit step in each
# direction. t is a positive integer. Only works for binary choices.
#
	if(length(prob.favorable)!=2) {
		print("probLeftRandomWalk() only works for binary choices")
		return (-1)
	}
	1/(1+(-1+1/prob.favorable)^choice.threshold)
}

####################################################################################################
#
# Code for the accumulator choice mechanism
#
####################################################################################################

combinations <- function(N, n ,i=n) {
#
# Returns a matrix with a row for each way of making n draws from N symbols
# (with replacement). Works using recursion. Used in accumulator()
#
    if(i==1) {
        matrix(rep(0:(N-1), each=N^(i-1), times=N^(n-i)), 
			ncol=1)
    } else {
        cbind(combinations(N=N, n=n, i=i-1), 
			rep(0:(N-1), each=N^(i-1), times=N^(n-i))
		)  
    }
}

term <- function(i, p, K) {
#	
# Gives the probability of a particular combination of accumulators where
# the left accumulator has won (i.e., has reached K) and the others have
# values in vector i less than K. 
# Equivalent to p[1] * dmultinom(c(K-1, i), prob=p), but faster
#
	factorial(K+sum(i)-1) / (prod(factorial(i)) * factorial(K-1)) * prod(p^c(K,i))
}

prob.left <- function(p, T) {
#
# For an set of accumulators with increment probabilities p, this gives the
# probability that the left accumulator will reach T first
#
	other.accumulator.combinations <- combinations(N=T, n=length(p)-1)
	sum(apply(other.accumulator.combinations, 1, term, p=p, K=T))
}

rotate <- function(n, x) {
# 
# rotate() shifts the elements of x n places to the left for positive n and
# n places to the right for negative n
#
	x[(((1:length(x))-1+n) %% length(x))+1]
}

prob.accumulator <- function(prob.favorable, choice.threshold) {
#
# prob.accumulator(prob.favorable=c(p1, p2, ...), choice.threshold=T)
#
# prob.accumulator() returns a vector of the probabilities of choosing each
# gamble in a simple accumulator model with one accumulator for each gamble
# where p1, p2, ... give the probabilities of incrementing each accumulator.
# The accumulation finished when the first gamble reaches threshold T.
#
	rotations <- sapply(0:(length(prob.favorable)-1), rotate, x=prob.favorable)	
	rotations
	apply(rotations, 1, prob.left, T=choice.threshold)
}

####################################################################################################
#
# The core DbS function
#
####################################################################################################

DbS <- function(choices, thresholds, context, prob.sample.context,
	prob.sample.amount, self.comparison=F, choice.mechanism="random walk", choice.threshold, valence.rule="within threshold of min/max") {
#
# DbS(choices=list(c1, c2, ...),
#   thresholds=list(x=x.threshold, p=p.threshold),
#   context=list(x=c(y1, y2, ...), p=c(q1, q2, ...),
#   prob.sample.context=prob.sample.context,
#   prob.sample.amount=prob.sample.amount,
#   self.comparison=T|F,
#	choice.mechanism="random walk"|"accumulators",
#	choice.threshold=t,
#	valence.rule="within threshold of min/max"|"within threshold of zero"
# )
#
# DbS() applies the DbS model to a list of choices. DbS returns a list of
# vectors, one for each choice. Each element in a vector gives the
# probability that the corresponding gamble in the choice will be chosen
#
	valenced.choices <- lapply(choices,
		valence.choice,
		threshold=thresholds[["x"]],
		valence.rule=valence.rule
	)
	prob.favorable <- lapply(valenced.choices,
		prob.each.gamble.in.choice.favorable,
		thresholds=thresholds, context=context,
		prob.sample.context=prob.sample.context,
		prob.sample.amount=prob.sample.amount,
		self.comparison=self.comparison
	)
	# Normalize
	normalized.prob.favorable <- lapply(prob.favorable, function(x) {x/sum(x)})
	if(choice.mechanism=="random walk") 
		choice.function <- prob.random.walk
	else if(choice.mechanism=="accumulators")
		choice.function <- prob.accumulator
	else {
		print("Uncaught choice mechanism in DbS()")
		return -1
	}
	choice.threshold <- round(choice.threshold)
	prob.chosen <- lapply(normalized.prob.favorable,
		choice.function,
		choice.threshold=choice.threshold
	)
	prob.chosen
}
