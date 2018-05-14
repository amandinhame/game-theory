# Shapley "percentage"
# v.1.0.0

# Libraries
# install.packages("combinat")
library(combinat)

# Check if g1 and g2 are equals
# g1 and g2: vectors
same_group <- function(g1, g2) {
  if(length(g1) != length(g2)) {
    return(FALSE)
  } else if (length(g1) == length(intersect(g1, g2))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Get index of item (vector) in list
# l: list
# item: item to be searched in list
# return index of item in l or 0 otherwise
index_group <- function(l, item) {
  for (i in c(1:length(l))) {
    sub <- unlist(l[i])
    if (same_group(sub, item)) {
      return (i)
    }
  }
  return(0)
}

# Remove left and right spaces from string
# x: string
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# Generate combinations of p with n_ini to n_end number of elements
combinations <- function(p, n_ini, n_end) {
  comb <- c()
  for(i in c(n_ini:n_end)) {
    comb <- c(comb, combn(p, i, simplify = FALSE))
  }
  return(comb)
}

# Compute percentage to each player based on shapley value
# p: vector of players
# s: list with vector combinations of players
# v: vector of rewards (same order and size of list s)
perc_shapley <- function(p, s, v) {
  
  perc <- c()
  phi <- c()
  comb <- combinations(p, 1, 4)
  
  for (i in c(1:length(p))) {
    
    phi[i] <- 0
    
    for (j in c(1:length(comb))) {
      sub <- unlist(comb[j])
      # subgroups without i
      if (length(sub[sub == p[i]]) == 0) {
        
        vS  <- 0 # value without i
        iVS <- index_group(s, c(sub))
        if (iVS != 0) {
          vS <- v[iVS]
        }
        
        vSI <- 0 # value with i
        iVSI <- index_group(s, c(sub, p[i]))
        if (iVSI != 0) {
          vSI <- v[iVSI]
        }
        
        # Modified
        if (vSI - vS > 0) {
          phi[i] <- phi[i] + (((factorial(length(sub)) * factorial(length(p) - length(sub) -1)) / factorial(length(p))) *  (vSI - vS))
        }
      }
    }
    
    # empty group
    vSI <- 0
    iVSI <- index_group(s, p[i])
    if (iVSI != 0) {
      vSI <- v[iVSI]
    }
    phi[i] <- phi[i] + vSI # alterar formula
    
  }
  perc <- phi / sum(phi)
  return (perc)
}

# tests / example
p <- c("a", "b", "c", "d")                # i in N (Players)
s <- list(c("a"), c("b","c","a"), c("c")) # Player coalitions
v <- c(10, 10, 30)                        # Rewards by coalition (same size and order as s)
print(perc_shapley(p, s, v))