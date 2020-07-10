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

# 16 gambles from Kahneman and Tversky (1979) used in Stewart and Simpson (2008)
c1 <- list(g1A=gamble(x=c(2500, 2400, 0), p=c(0.33, 0.66, 0.01)), 
	g1B=gamble(x=c(2400), p=c(1))
)
c2 <- list(g2A=gamble(x=c(2500, 0), p=c(0.33, 0.67)),
	g2B=gamble(x=c(2400, 0), p=c(0.34, 0.66))
)
c3 <- list(g3A=gamble(x=c(4000, 0), p=c(0.8, 0.2)),
	g3B=gamble(x=c(3000), p=c(1))
)
c4 <- list(g4A=gamble(x=c(4000, 0), p=c(0.2, 0.8)),
	g4B=gamble(x=c(3000, 0), p=c(0.25, 0.75))
)
c7 <- list(g7A=gamble(x=c(6000, 0), p=c(0.45, 0.55)),
	g7B=gamble(x=c(3000, 0), p=c(0.9, 0.1))
)
c8 <- list(g8A=gamble(x=c(6000, 0), p=c(0.001, 0.999)),
	g8B=gamble(x=c(3000, 0), p=c(0.002, 0.998))
)
c3p <- list(g3pA=gamble(x=c(-4000, 0), p=c(0.8, 0.2)),
	g3pB=gamble(x=c(-3000), p=c(1))
)
c4p <- list(g4pA=gamble(x=c(-4000, 0), p=c(0.2, 0.8)),
	g4pB=gamble(x=c(-3000, 0), p=c(0.25, 0.75))
)
c7p <- list(g7pA=gamble(x=c(-6000, 0), p=c(0.45, 0.55)),
	g7pB=gamble(x=c(-3000, 0), p=c(0.9, 0.1))
)
c8p <- list(g8pA=gamble(x=c(-6000, 0), p=c(0.001, 0.999)),
	g8pB=gamble(x=c(-3000, 0), p=c(0.002, 0.998))
)
c13 <- list(g13A=gamble(x=c(6000, 0), p=c(0.25, 0.75)),
	g13B=gamble(x=c(4000, 2000, 0), p=c(0.25, 0.25, 0.5))
)
c13p <- list(g13pA=gamble(x=c(-6000, 0), p=c(0.25, 0.75)),
	g13pB=gamble(x=c(-4000, -2000, 0), p=c(0.25, 0.25, 0.5))
)
c14 <- list(g14A=gamble(x=c(5000, 0), p=c(0.001, 0.999)),
	g14B=gamble(x=c(5), p=c(1))
)
c14p <- list(g14pA=gamble(x=c(-5000, 0), p=c(0.001, 0.999)),
	g14pB=gamble(x=c(-5), p=c(1))
)
c11 <- list(g11A=gamble(x=c(1000, 0), p=c(0.5, 0.5)),
	g11B=gamble(x=c(500), p=c(1))
)
c12 <- list(g12A=gamble(x=c(-1000, 0), p=c(0.5, 0.5)),
	g12B=gamble(x=c(-500), p=c(1))
)

choices <- list(c1=c1, c2=c2, c3=c3, c4=c4, c7=c7, c8=c8, c3p=c3p, c4p=c4p,
	c7p=c7p, c8p=c8p, c13=c13, c13p=c13p, c14=c14, c14p=c14p, c11=c11,
	c12=c12
)

rm(c1, c2, c3, c3p, c4, c4p, c7, c7p, c8, c8p, c11, c12, c13, c13p, c14, c14p)

# Proportion of A choices for each of the 16 gambles
proportion.A.chosen.data <- c(c1=0.18, c2=0.83, c3=0.2, c4=0.65, c7=0.14,
	c8=0.73, c3p=0.92, c4p=0.42, c7p=0.92, c8p=0.3, c13=0.18, c13p=0.7,
	c14=0.72, c14p=0.17, c11=0.16, c12=0.69
)

# Number of people asked each choice
N.data<-c(c1=72, c2=72, c3=95, c4=95, c7=66, c8=66, c3p=95, c4p=95, c7p=66, c8p=66, 
	c13=68, c13p=64, c14=72, c14p=72, c11=70, c12=68
)
