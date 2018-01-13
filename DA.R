# 1 summarising data
# input data

data1a <- read.csv(file = 'data1a.csv')
data1b <- read.csv(file = 'data1b.csv')
data1c <- read.csv(file = 'data1c.csv')
data1d <- read.csv(file = 'data1d.csv')
data1e <- read.csv(file = 'data1e.csv')

# analysis 1

data1a <- data1a[!is.na(data1a)]
a.boxplot <- boxplot(data1a)
a.stats <- a.boxplot[["stats"]]
for (i in 1:length(data1a)) {
  if (data1a[i] <= a.stats[1] | data1a[i] >= a.stats[5] )
    data1a[i] <- NA
}
data1a <- data1a[!is.na(data1a)]
a.median <- median(data1a)# the avearge value of variable A

# analysis 2

b.median <- by(data1b$B, data1b$groups, median)
b.IQR <- by(data1b$B, data1b$groups, IQR) 

# analysis 3 

plot(data1c)
c.cor <- cor(x = data1c$C, y = data1c$D, method = "pearson")

# analysis 4

E1 <- data1d[data1d$groups == "group1",1]
E2 <- data1d[data1d$groups == "group2",1]
F1 <- data1d[data1d$groups == "group1",2]
F2 <- data1d[data1d$groups == "group2",2]
data1d <- cbind(E1,E2,F1,F2)
pairs(data1d[,1:4])

d.cor <- cor(data1d, method = "pearson")

# analysis 5

e.table <- table(data1e)
barplot(e.table)

# 2 Correlation
# input data
data1f <- read.csv(file = 'data1f.csv')
data1g <- read.csv(file = 'data1g.csv')
data1h <- read.csv(file = 'data1h.csv')

# analysis 1

?cor

# analysis 2

pairs(data1f)
pairs(data1g)
f.cor <- cor(data1f, method = "pearson")
g.cor <- cor(data1g, method = "pearson")

# analysis 3

?cor.test()
f.cor.test <- cor.test(data1f$X, data1f$Y)
#  It is significant.

# analysis 4

h.cor.test <- cor.test(data1h$x, data1h$y)

# 3 T-tests
# input data
data1i <- read.csv(file = 'data1i.csv')
data1j <- read.csv(file = 'data1j.csv')

# analysis 1

?t.test()

# analysis 2

i.t.test <- t.test(data1i$A, mu = 10)
# NO

i2.t.test <- t.test(data1i$B, data1i$C)
# NO

j.t.test <- t.test(data1i$D, data1j$E)
# cannot reject
