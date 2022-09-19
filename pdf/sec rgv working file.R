SEC <- read.csv("D:/OU/Fall 2021/Quantitative Methods/Final/RGV Demographics.csv")


#bar chart
SECcolor = table(SEC$County)
barplot(SECcolor, main = "Total Tracts per County", xlab = "County", ylab = "Frequency", col = c("paleturquoise", "olivedrab2", "thistle1"))


#pareto chart
barplot(SECcolor[order(SECcolor,decreasing = TRUE)],main="Total Tracts per County", xlab = "County", 
        ylab = "Frequency", col = c("olivedrab2","paleturquoise","thistle1"))


#confidence intervals
stat.desc(SEC$Median.Income,basic=F,p=0.95)
stat.desc(SEC$Median.Age,basic=F,p=0.95)
stat.desc(SEC$Hispanic.Percentage,basic=F,p=0.95)
stat.desc(SEC$public.health.insurance,basic=F,p=0.95)
stat.desc(SEC$total.hs,basic=F,p=0.95)
stat.desc(SEC$total.college,basic=F,p=0.95)



# One-sample t-test 
t.test(SEC$Median.Income,mu = 40000, alternative ="two.sided")
t.test(SEC$Median.Age,mu = 35, alternative = "two.sided")
t.test(SEC$Hispanic.Percentage,mu = 40, alternative ="two.sided")
t.test(SEC$public.health.insurance,mu = 5000000, alternative ="two.sided")
t.test(SEC$total.hs,mu = 340000, alternative ="two.sided")
t.test(SEC$total.college,mu = 460000, alternative ="two.sided")



#Two-sample t-test
t.test(SEC$Median.Income ~ SEC$serv.under)
t.test(SEC$Median.Age ~ SEC$serv.under)
t.test(SEC$Hispanic.Percentage ~ SEC$serv.under)
t.test(SEC$public.health.insurance ~ SEC$serv.under)
t.test(SEC$total.hs ~ SEC$serv.under)
t.test(SEC$total.college ~ SEC$serv.under)



#central tendency
hist (SEC$Median.Income, main = "Median Income", xlab = "Median Income", ylab = "Frequency", col = "blue")
boxplot (SEC$Median.Income, main = "Median Income", col = "blue")
summary(SEC$Median.Income)
IQR = 60079 - 39236
describe (SEC$Median.Income)
scale (SEC$Median.Income)

hist (SEC$Median.Age, main = "Median Age", xlab = "Median Age", ylab = "Frequency", col = "blue")
boxplot (SEC$Median.Age, main = "Median Age", col = "blue")
summary(SEC$Median.Age)
IQR = 33.33 - 26.50
describe (SEC$Median.Age)
scale (SEC$Median.Age)

hist (SEC$Hispanic.Percentage, main = "Hispanic Percentage", xlab = "Hispanic Percentage", ylab = "Frequency", col = "blue")
boxplot (SEC$Hispanic.Percentage, main = "Hispanic Percentage", col = "blue")
summary(SEC$Hispanic.Percentage)
IQR = 97.66 - 85.30
describe (SEC$Hispanic.Percentage)
scale (SEC$Hispanic.Percentage)

hist (SEC$public.health.insurance, main = "Access to Public Health Care", xlab = "People Covered", ylab = "Frequency", col = "blue")
boxplot (SEC$public.health.insurance, main = "Access to Public Health Care", col = "blue")
summary(SEC$public.health.insurance)
IQR = 2339 - 1148
describe (SEC$public.health.insurance)
scale (SEC$public.health.insurance)

hist (SEC$total.hs, main = "Achieved High School Level Education", xlab = "Total People", ylab = "Frequency", col = "blue")
boxplot (SEC$total.hs, main = "Achieved High School Level Education", col = "blue")
summary(SEC$total.hs)
IQR = 1871 - 922.1
describe (SEC$total.hs)
scale (SEC$total.hs)

hist (SEC$total.college, main = "Achieved College Level Education", xlab = "Total People", ylab = "Frequency", col = "blue")
boxplot (SEC$total.college, main = "Achieved College Level Education", col = "blue")
summary(SEC$total.college)
IQR = 1685.6 - 649.2
describe (SEC$total.college)
scale (SEC$total.college)



#Normality Tests
shapiro.test(SEC$total.hs)
#not normally
shapiro.test(SEC$total.college)
#not normally
shapiro.test(SEC$Median.Income)
#not normally
shapiro.test(SEC$Median.Age)
#not normally
shapiro.test(SEC$Hispanic.Percentage)
#not normally
shapiro.test(SEC$public.health.insurance)
#not normally



#Bivariate correlations
cor.test(SEC$total.hs, SEC$public.health.insurance, method = "spearman")
plot(SEC$total.hs, SEC$public.health.insurance, xlab = "People with a High School Level Education", ylab = "No Public Health Care Access")
abline(lm(SEC$public.health.insurance ~ SEC$total.hs), col = "red")

cor.test(SEC$total.college, SEC$public.health.insurance, method = "spearman")
plot(SEC$total.college, SEC$public.health.insurance, xlab = "People with a College Level Education", ylab = "No Public Health Care Access")
abline(lm(SEC$public.health.insurance ~ SEC$total.college), col = "red")

cor.test(SEC$Median.Age, SEC$public.health.insurance, method = "spearman")
plot(SEC$Median.Age, SEC$public.health.insurance, xlab = "Median Age", ylab = "No Public Health Care Access")
abline(lm(SEC$public.health.insurance ~ SEC$Median.Age), col = "red")

cor.test(SEC$Median.Income, SEC$public.health.insurance, method = "spearman")
plot(SEC$Median.Income, SEC$public.health.insurance, xlab = "Mean Income", ylab = "No Public Health Care Access")
abline(lm(SEC$public.health.insurance ~ SEC$Median.Income), col = "red")

cor.test(SEC$Hispanic.Percentage, SEC$public.health.insurance, method = "spearman")
plot(SEC$Hispanic.Percentage, SEC$public.health.insurance, xlab = "Total Hispanic Population", ylab = "No Public Health Care Access")
abline(lm(SEC$public.health.insurance ~ SEC$Hispanic.Percentage), col = "red")



#Multivariate regression
publichealth <- lm(formula = SEC$public.health.insurance ~ SEC$total.college + SEC$total.hs + SEC$Median.Age + SEC$Hispanic.Percentage)
summary(publichealth)
par(mfrow = c(2,2))
plot(publichealth)

lm.beta(publichealth)



#ANOVA difference test
publiccount <- aov(formula = SEC$public.health.insurance ~ SEC$County)
summary(publiccount)
TukeyHSD(publiccount)
boxplot(SEC$public.health.insurance ~ SEC$County,  xlab = "County", ylab = "People with no Public Health Care Access", col = "blue")




#coefficient of variations
sd(SEC$total.hs)/mean(SEC$total.hs)
#0.4898461
sd(SEC$total.college)/mean(SEC$total.college)
#0.7379716
sd(SEC$Median.Age)/mean(SEC$Median.Age)
#0.1994196
sd(SEC$Median.Income)/mean(SEC$Median.Income)
#0.3311659
sd(SEC$Hispanic.Percentage)/mean(SEC$Hispanic.Percentage)
#0.1219281
sd(SEC$public.health.insurance)/mean(SEC$public.health.insurance)
#0.6032399



#chi-square test
rgvtab <- table(SEC$County, SEC$serv.under)
rgvtab

round(prop.table(rgvtab,1),2)*100 #row percentage
# of the total counties: 
  #93% of cameron is serviced, 7% is underserviced 
  #93% of hidalgo is serviced, 7% is underserviced
  #53% of starr is serviced, 47% is underserviced
round(prop.table(rgvtab,2),2)*100 #column percentage
# of the total serviced 
  #41% of serviced comes from cameron
  #55% of serviced comes from hidalgo
  #4% of serviced comes from starr
#of the total underserviced 
  #29% of underserviced comes from cameron
  #38% of underserviced comes from hidalgo 
  #33% of underserviced comes from starr

rgvchi <- chisq.test(rgvtab)
rgvchi



#PCA
rgvvar <- SEC[, c(12:18)]
rgvpc <- prcomp(rgvvar, center = TRUE, scale. = TRUE)
summary(rgvpc)
#Scree plot
plot(rgvpc, main = "Rio Grande Valley Scree Plot", col = "purple")
#component loadings
rgvpc
#save component loadings
rgvloadings <- predict(rgvpc)



#Cluster Analysis 
dismatrixrgv <- dist(rgvvar)

clusterrgv <- hclust(dismatrixrgv)
clusterrgv

plot(clusterrgv)

groupsrgv <- cutree(clusterrgv, k = 3)
groupsrgv

rect.hclust(clusterrgv, k= 3, border = "red")

kmeansrgv <- kmeans(rgvvar, 3)
kmeansrgv

require(cluster)
clusplot(rgvvar, kmeansrgv$cluster, color = TRUE, shade = TRUE, lines = 3, labels = 3, main = "RGV Cluster Plot")

kmeansrgv$centers
