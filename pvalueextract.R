df = read.csv("C:/Users/Bharat Jambhulkar/ISPS2024/exc_115.csv")
View(df)
plot(df$Mumbai)
index = 1:7
testM = df$Bengaluru
testM =y= testM[-1]

y = scale(testM)
plot(y)
regM=lm(y~index)
abline(regM)
sum=summary(regM)
round(sum$coefficients[2,4],4)

pvalue = c()

for(i in 2:50){
  y = df[,i]
  reg = lm(y~index)
  sum=summary(reg)
  pvalue[i]=round(sum$coefficients[2,4],4)
}

pvaluedf=matrix(c(colnames(df),pvalue),nrow=51,ncol=2)

#####
df1 = df[-1]/10
pvalue1=c()
for(i in 1:50){
  y = df1[,i]
  reg = lm(y~index)
  sum=summary(reg)
  pvalue1[i]=round(sum$coefficients[2,4],4)
}
length(pvalue1[which(pvalue1<0.05)])
