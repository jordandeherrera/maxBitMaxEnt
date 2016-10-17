# Calculate probability of a micro-state
prob <- function(n,N){
  log(1 + (1/n)) / log(N+1)
}

# Calculate bits needed to describe a micro-state
bit <- function(n,N){
  log2(1/prob(n,N))
}

# Calculate entropy of a micro-state
ent <- function(n,N){
  bit(n,N)*prob(n,N)
}

count <- for (i in 1:9){
  prob(i,9)*10
}

# Entropy in a number system
freqDF <- data.frame(N = 1:9, Count = round(prob(1:9,9)*10,0),
                     EntropyPerDigit = ent(1:9,9),
                     Bits = bit(1:9,9))

freqDF$Entropy <- freqDF$Count * freqDF$EntropyPerDigit

# Maximum Entropy in 10 Digits:
maxEnt <- round(sum(freqDF$Entropy),0)
maxBit <- round(sum(freqDF$Bits),0)

# Maximum number of digits
maxPicks <- 10
