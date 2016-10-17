library(Rglpk)

#################################
##
## LOAD DATA
##
#################################

# objective:
obj <- freqDF$Entropy

# the vars are represented as booleans
var.types <- rep("B", num.picks)

# the constraints
matrix <- rbind(#diag(final$TeamELO),
  diag(final$ELODiff)
)

# Create empty matrix
#matrix <- matrix(,nrow=0, ncol=num.picks)

for (i in 1:length(teams)){
  newrow <- as.numeric(final$Team == teams[i])
  matrix <- rbind(matrix,newrow)
}

for (i in min(unique(final$Week)):max(unique(final$Week))){
  newrow <- as.numeric(final$Week == i)
  matrix <- rbind(matrix,newrow)
}

matrix <- as.matrix(matrix)

rownames(matrix) <- 1:nrow(matrix)

colnames(matrix) <- 1:ncol(matrix)

direction <- c(#rep(">=", num.picks),
  rep(">=", num.picks),
  rep("<=", length(teams)),
  rep("==", length(unique(final$Week)))
)
rhs <- c(#rep(0, num.picks), Minimum ELO for picked team
  rep(0, num.picks),
  rep(1, length(teams)),
  rep(1, length(unique(final$Week))))

sol <- Rglpk_solve_LP(obj = obj, mat = matrix, dir = direction, rhs = rhs,
                      types = var.types, max = TRUE)

finalSelection <- final[sol$solution == 1,]

finalSelection <- finalSelection[order(finalSelection$Week),]

write.csv(finalSelection,file="SurvivorPicks.csv")
