library(foreign)
library(haven)
library(ggplot2)

# import data
data <- read.dta("microvan.dta")

str(data)
summary(data)
# exploratory analysis

variables <- colnames(data[,3:32])

p = ggplot(data)
p1 = p+geom_bar(aes_string(x=variables[1]))
p2 = p+geom_bar(aes_string(x=variables[2]))
p3 = p+geom_bar(aes_string(x=variables[3]))
p4 = p+geom_bar(aes_string(x=variables[4]))
p5 = p+geom_bar(aes_string(x=variables[5]))
p6 = p+geom_bar(aes_string(x=variables[6]))
p7 = p+geom_bar(aes_string(x=variables[7]))
p8 = p+geom_bar(aes_string(x=variables[8]))
p9 = p+geom_bar(aes_string(x=variables[9]))
p10 = p+geom_bar(aes_string(x=variables[10]))
p11 = p+geom_bar(aes_string(x=variables[11]))
p12 = p+geom_bar(aes_string(x=variables[12]))
p13 = p+geom_bar(aes_string(x=variables[13]))
p14 = p+geom_bar(aes_string(x=variables[14]))
p15 = p+geom_bar(aes_string(x=variables[15]))
p16 = p+geom_bar(aes_string(x=variables[16]))
p17 = p+geom_bar(aes_string(x=variables[17]))
p18 = p+geom_bar(aes_string(x=variables[18]))
p19 = p+geom_bar(aes_string(x=variables[19]))
p20 = p+geom_bar(aes_string(x=variables[20]))
p21 = p+geom_bar(aes_string(x=variables[21]))
p22 = p+geom_bar(aes_string(x=variables[22]))
p23 = p+geom_bar(aes_string(x=variables[23]))
p24 = p+geom_bar(aes_string(x=variables[24]))
p25 = p+geom_bar(aes_string(x=variables[25]))
p26 = p+geom_bar(aes_string(x=variables[26]))
p27 = p+geom_bar(aes_string(x=variables[27]))
p28 = p+geom_bar(aes_string(x=variables[28]))
p29 = p+geom_bar(aes_string(x=variables[29]))
p30 = p+geom_bar(aes_string(x=variables[30]))


library(gridExtra)
grid.arrange(p1,p2,p3,p4,p5,p6)
grid.arrange(p7,p8,p9,p10,p11,p12)
grid.arrange(p13,p14,p15,p16,p17,p18)
grid.arrange(p19,p20,p21,p22,p23,p24)
grid.arrange(p25,p26,p27,p28,p29,p30)

#create regression against 30 explanatory variables
z.full <- lm(mvliking ~ kidtrans+miniboxy+lthrbetr
           +secbiggr+safeimpt+buyhghnd
           +pricqual+prmsound+perfimpt
           +tkvacatn+noparkrm+homlrgst
           +envrminr+needbetw+suvcmpct
           +next2str+carefmny+shdcarpl
           +imprtapp+lk4whldr+kidsbulk
           +wntguzlr+nordtrps+stylclth
           +strngwrn+passnimp+twoincom
           +nohummer+aftrschl+accesfun
           , data=data)
summary(z.full)

##################
# factor analysis

library(REdaS)

# Bartlett test of sphericity
# chi square test
# H0: there is no significant correlation between variables
bart_spher(data[,3:32])

# Kaiser-Meyer-Olkin Measure of sampling adequacy
# wheter samples are adequate or not
# looking for critical value > 0.6
KMOS(data[,3:32])

# calculate eigen values
ev <- eigen(cor(data[,3:32]))$values
e <- data.frame(Eigenvalue = ev, PropOfVar = ev / length(ev), CumPropOfVar = cumsum(ev / length(ev)))

round(e, 4)

# Draw a scree plot
p <- ggplot()
p <- p + geom_line(aes(x = 1:length(ev), y = ev))
p <- p + geom_point(aes(x = 1:length(ev), y = ev))
p <- p + geom_hline(yintercept = 1, colour = "red")
p <- p + labs(x = "Number", y = "Eigen values", title = "Scree Plot of Eigen values")
p <- p + scale_x_continuous(breaks = 1:length(ev), minor_breaks = NULL)
p <- p + theme_bw()
p

# Select number of factors
n <- 5

library(psych)

# Do factor analysis using principal component
pc <- principal(data[,3:32], nfactors = n, rotate="varimax")

# Create a factor loadings table; Sort based on uniqueness
fl <- cbind.data.frame(pc$loadings[,], Uniqueness = pc$uniquenesses)
round(fl[order(pc$uniquenesses),], 4)

# Check how the factors predict the overall preference ratings
factor_scores <- cbind(data[,1:2],pc$scores)
head(factor_scores)

# regression using factor scores

z.fscore <- lm(mvliking ~ RC1 + RC2 + RC3 + RC4 + RC5, data=factor_scores)
summary(z.fscore)

###############
# Hierarchical Clustering

# Calculate Euclidian distances between rows, considering factors 1 to 5
d <- dist(factor_scores[,3:7])

# Apply Ward's linkage clustering
h <- hclust(d, method = "ward.D2")

# view dendogram
plot(h, xlab = "Respondent")

######################
# k-means clustering

# First, standardize the input variables (z-scores)
z <- scale(factor_scores[,3:7], center = TRUE, scale = TRUE)

# Since the k-means algorithm starts with a random set of centers, setting the seed helps ensure the results are reproducible
set.seed(7)

# Cluster based on factor scores
#k <- kmeans(factor_scores[,3:7], centers = 4)
k <- kmeans(z,centers=3)

# Cluster sizes
k$size

# Cluster means
k$centers

# add cluster back into dataset
factor_scores$cluster <- k$cluster
data$cluster <- k$cluster

#####################
# Regression by Segments

z.clust.reg <- lm(mvliking ~ as.factor(cluster), data=factor_scores)
summary(z.clust.reg)

# Plot the score distribution by segments

# Reshape from wide to long format (required for use of ggplot2)
plotdata <- reshape(factor_scores, varying = c("RC1", "RC2", "RC3", "RC4", "RC5"), v.names = "score", timevar = "attribute", times = c("Price Premium for Quality", "Medium Car Size", "Kid's Needs for Vehicle", "Safety", "Environmental Impact"), direction = "long")

head(plotdata)

# Build plot
p <- ggplot(data = plotdata)
p <- p + geom_density(aes(x = score, colour = as.factor(cluster), fill = as.factor(cluster)), size = 1, alpha = 0.3)
p <- p + facet_wrap(~ attribute, ncol = 3)
p <- p + labs(title = "Cluster histogram diagnostics")
#p <- p + xlim(c(0,8))
p <- p + theme_bw()
p

######################
# Cross Tab

library(gmodels)
CrossTable(x = data$cluster, y = data$mvliking, expected = TRUE, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE)


######################
# Demographic Profile
# Reshape data into long form
plotdata2 <- reshape(data[,33:40],varying = c("age", "income", "miles", "numkids", "educ","recycle"), v.names = "value", timevar = "demographic", times = c("Age", "Income", "Miles", "Number of Kids", "Education","Recycle"), direction = "long")

head(plotdata2)

# plot demographic distribution
p <- ggplot(data = plotdata2)
p <- p + geom_density(aes(x = value, colour = as.factor(cluster), fill = as.factor(cluster)), size = 1, alpha = 0.3)
p <- p + facet_wrap(~ demographic, ncol = 3,scales="free")
p <- p + labs(title = "Cluster Demographics")
p <- p + theme_bw()
p

#separate demographic data into each clusters
cluster1.demo <- data[data$cluster==1,c("age","income","miles","numkids","female","educ","recycle","cluster")]
cluster2.demo <- data[data$cluster==2,c("age","income","miles","numkids","female","educ","recycle","cluster")]
cluster3.demo <- data[data$cluster==3,c("age","income","miles","numkids","female","educ","recycle","cluster")]

#Summary for each cluster demographic
summary(cluster1.demo)
summary(cluster2.demo)
summary(cluster3.demo)

