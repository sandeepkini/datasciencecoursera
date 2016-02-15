q1 <- read.csv(file.choose(), header=T)
sum(is.na(q1))

q1b <- q1[which(q1$ACR==3 & q1$AGS==6),]

head(q1b)

img <- readJPEG(file.choose(), native = TRUE)
quantile(img,probs=c(0.3,0.8))

GDP <- read.csv(file.choose(), header=T)
FED <- read.csv(file.choose(), header=T)

mergedata= merge(GDP,FED,by.x="X",by.y="CountryCode",all=FALSE)

order <- mergedata[order(mergedata$Gross.domestic.product.2012, decreasing=TRUE),]

order$Gross.domestic.product.2012 <- as.numeric(as.character(order$Gross.domestic.product.2012))
neworder <- order [which(order$Gross.domestic.product.2012 >= 1),]

library(plyr)
neworder <- arrange(neworder, Gross.domestic.product.2012, desc(-Gross.domestic.product.2012))

HiOECD <- neworder[neworder$Income.Group %in% c("High income: OECD"),]

HinonOECD <- neworder[neworder$Income.Group %in% c("High income: nonOECD"),]

mean(HiOECD$Gross.domestic.product.2012)
mean(HinonOECD$Gross.domestic.product.2012)

library(hmisc)
group <- (neworder,g=5)

HinonOECD

head(order,13)