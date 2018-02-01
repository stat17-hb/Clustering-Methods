########################################################################################
####--------------- Aggolomerative Hierachical Clustering, "Bottom-up" ------------ ####
########################################################################################

### Single linkage method(Graph based method) => MIN

x <- c(1,2,2,4,5)
y <- c(1,1,4,3,4)

df <- data.frame(cbind(x,y))
df

# scatter plot of df
plot(df, pch=19, xlim=c(0,6), ylim=c(0,6))
text(df[,1], df[,2], labels=paste0("p",1:5), pos=1)

# proximity matrix : squares of euclidian distance matrix for 5 points
d <- dist(df, method="euclidian")
d

temp <- as.matrix(d)
temp

diag(temp) <- 999
# 군집 간 최소 거리를 찾아야 하는데 diagonal elements는 군집내 거리라서 값이 모두 0이라서
# 이게 최솟값으로 안잡히게 하려고 임의의 큰 수를 넣어줌
temp


### [step 1]

# find minimum distance

which(temp==min(temp)) # 2번째, 6번째 값이 최솟값으로 잡히는데 symmetric matrix라서 그러는 것
# (1,2), (2,1) 성분인데, 1과 2가 거리가 가장 가까운 data point여서 군집으로 묶인다는 의미

## 거리 데이터의 위치(pos) 표현 방식
# 1 6  11 ...
# 2 7  12 ...
# 3 8  13 ...
# 4 9  14 ...
# 5 10 15 ...

pos <- which(temp==min(temp))[1] # 그 중 하나만 선택해서 위치 저장
pos
if (pos %% ncol(temp)!=0){ # column 수로 위치(pos)를 나눈 나머지가 0이 아닌 경우

  c <- pos %/% ncol(temp) + 1 # column 수로 위치(pos)를 나눈 몫 + 1을 거리데이터의 column 위치로 저장
  r <- pos %% ncol(temp) # column 수로 위치(pos)를 나눈 나머지를 거리데이터의 row 위치로 저장

} else { # column 수로 위치(pos)를 나눈 나머지가 0인 경우

  c <- pos %/% ncol(temp) # column 수로 위치(pos)를 나눈 몫을 거리데이터의 column 위치로 저장
  r <- ncol(temp) # column 수를 거리데이터의 row 위치로 저장

  # row 위치를 저장할 때 column 수(ncol(temp))로 저장하는 것은 symmetric matrix이기 때문, nrow(temp)로 저장해도 됨

}
r
c

temp[r,c]
min(temp)

group <- c(2,2,3,4,6)

plot(df, pch=19, xlim=c(0,6), ylim=c(0,6), col=group)
text(df[,1], df[,2], labels=paste0("p",1:5), pos=1, col=group)


### [step 2]

# adjusted distance matrix by single linkage method
temp.1 <- temp[-c(r,c),-c(r,c)]
temp.1 <- rbind(temp.1, `12`=0)
temp.1 <- cbind(temp.1, `12`=0)
temp.1

# d[(1, 2), 3] = min[ d(1, 3), d(2, 3)]
min(temp[1,3], temp[2,3])

# d[(1, 2), 4] = min[ d(1, 4), d(2, 4)]
min(temp[1,4], temp[2,4])

# d[(1, 2), 5] = min[ d(1, 5), d(2, 5)]
min(temp[1,5], temp[2,5])

setdiff(1:ncol(temp), c(r,c))

adj_dist <- c()
for (i in setdiff(1:ncol(temp), c(r,c))){
  adj_dist <- c(adj_dist, min(temp[r,i], temp[c,i]))
}
adj_dist <- c(adj_dist, 999)
temp.1[nrow(temp.1), ] <- adj_dist
temp.1[ ,ncol(temp.1)] <- adj_dist
temp.1


# find minimum distance
pos <- which(temp.1==min(temp.1))[1]
pos
if (pos %% ncol(temp.1)!=0){
  c <- pos %/% ncol(temp.1) + 1
  r <- pos %% ncol(temp.1)
} else {
  c <- pos %/% ncol(temp.1)
  r <- ncol(temp.1)
}
r
c

group <- c(2,2,3,4,4)

plot(df, pch=19, xlim=c(0,6), ylim=c(0,6), col=group)
text(df[,1], df[,2], labels=paste0("p",1:5), pos=1, col=group)


### [step 3]

# adjusted distance matrix by single linkage method
temp.2 <- temp.1[-c(r,c),-c(r,c)]
temp.2 <- rbind(temp.2, `45`=0)
temp.2 <- cbind(temp.2, `45`=0)
temp.2

# d[(1, 2), (4, 5)] = min[d{(1, 2), 4}, d{(1, 2), 5}]
min(temp.1[2,4], temp.1[3,4])

# d[(4, 5), 3] = min[d(4, 3), d(5, 3)]
min(temp.1[2,1], temp.1[3,1])


setdiff(1:ncol(temp.1), c(r,c))

adj_dist <- c()
for (i in setdiff(1:ncol(temp.1), c(r,c))){
  adj_dist <- c(adj_dist, min(temp.1[r,i], temp.1[c,i]))
}
adj_dist <- c(adj_dist, 999)
temp.2[nrow(temp.2), ] <- adj_dist
temp.2[ ,ncol(temp.2)] <- adj_dist
temp.2


# find minimum distance
pos <- which(temp.2==min(temp.2))[1]
pos
if (pos %% ncol(temp.2)!=0){
  c <- pos %/% ncol(temp.2) + 1
  r <- pos %% ncol(temp.2)
} else {
  c <- pos %/% ncol(temp.2)
  r <- ncol(temp.2)
}
r
c

group <- c(2,2,3,4,4)

plot(df, pch=19, xlim=c(0,6), ylim=c(0,6), col=group)
text(df[,1], df[,2], labels=paste0("p",1:5), pos=1, col=c(2,2,6,6,6))


### [step4]

# adjusted distance matrix by single linkage method
temp.3 <- temp.2[-c(r,c),-c(r,c), drop=F]
temp.3 <- rbind(temp.3, `345`=0)
temp.3 <- cbind(temp.3, `345`=0)
temp.3

# d(12, 345) = min[d(12, 3), d(12, 45)]
min(temp.2[3,2], temp.2[1,2])

setdiff(1:ncol(temp.2), c(r,c))

adj_dist <- c()
for (i in setdiff(1:ncol(temp.2), c(r,c))){
  adj_dist <- c(adj_dist, min(temp.2[r,i], temp.2[c,i]))
}
adj_dist <- c(adj_dist, 999)
temp.3[nrow(temp.3), ] <- adj_dist
temp.3[ ,ncol(temp.3)] <- adj_dist
temp.3


### cluster 패키지의 hclust함수의 결과와 같은지 확인

library(cluster)
hc <- hclust(d, method="single")
hc
plot(hc, ylab="distance")



########################################################################################
####------------------------------ Drug company example --------------------------- ####
########################################################################################

## data loading
data <- read.csv("drug_comp.csv")

## calculate distance matrix
d <- dist(data[,-1], method = "euclidian")
class(d)

## change dist into matrix and choose 5 company to show all data on the screen at once
d_mat <- as.matrix(d)
temp <- d_mat[1:5, 1:5]

rownames(temp) <- data$company[1:5]
colnames(temp) <- data$company[1:5]
temp

## replace diagonal elements to random large number to make calculation easy
diag(temp) <- 9999

## find min distance
pos <- which(temp==min(temp))
pos # 2nd and 6th elment of matrix have min distance (elements are compared by column)

pos[1] %/% nrow(temp) + 1 # 1st row
pos[1] %% nrow(temp)

