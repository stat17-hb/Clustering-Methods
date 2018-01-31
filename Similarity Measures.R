########################################################################################
####--------------------------------- Euclidean metric ---------------------------- ####
########################################################################################

x <- c(1,2,2,4,5)
y <- c(1,1,4,3,4)

df <- data.frame(cbind(x,y))

dist(df, method="euclidean")

euclidean <- function(df, m){
  dm <- matrix(0, nrow(df), nrow(df))
  for (r in 1:nrow(df)){
    for (c in 1:nrow(df)){
      dm[r,c] <- sqrt(sum((df[r, ]-df[c, ])^2))
    }
  }
  return(dm)
}

euclidean(df)

as.dist(euclidean(df))


########################################################################################
####--------------------------------- Minkowski metric ---------------------------- ####
########################################################################################

dist(df, method="minkowski", p=2) # It is same with the euclidean distance
dist(df, method="minkowski", p=1)


minkowski <- function(df, m){
  dm <- matrix(0, nrow(df), nrow(df))
  for (r in 1:nrow(df)){
    for (c in 1:nrow(df)){
      dm[r,c] <- (sum((abs(df[r, ] - df[c, ]))^m))^(1/m)
    }
  }
  return(dm)
}

minkowski(df,1)
minkowski(df,2)
minkowski(df,3)



