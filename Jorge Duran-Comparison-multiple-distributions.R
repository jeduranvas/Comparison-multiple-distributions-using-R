#*** Jorge Eduardo Duran Vasquez***
#*** Introductory Case Studies SS 2021 ***
#*** Project 2: Comparison of multiple distributions

# Import the data from the csv
data_height <- read.csv("/Users/jeduranvas/Desktop/Dortmund/2. Semester 2/1. Introductury Case Studies/Proyect 2/Height_Data.csv", sep = ";")
#we present a summary of the data for the description of the data set.
summary(data_height)

data_height

# Description of variables in the data set
#First the box plot of each group in the data set is plotted
par(mfrow=c(1,1))
boxplot(data_height$Height~data_height$Sport, col = c("gold","gray","royalblue1", "Orange1", "palegreen4", "mediumpurple1"), xaxt="n", xlab="" , ylab=("Height [cm]"))
grid(NA, 6, col = "gray", lty = "dotted",lwd = par("lwd"))
axis(1, at = c(1,2,3,4,5,6),  c("Basketball", "Handball", "Ice hockey", "Soccer", "Volleyball","Water polo") , tick=FALSE , cex=0.1)
abline(v=1.5,lty=1, col="grey")
abline(v=2.5,lty=1, col="grey")
abline(v=3.5,lty=1, col="grey")
abline(v=4.5,lty=1, col="grey")
abline(v=5.5,lty=1, col="grey")

#Now a summary of the distribution of each block is obtained

summary_basketball <- list("observations"=length(data_height$Height[data_height$Sport=='basketball']),
                      "min"= min(data_height$Height[data_height$Sport=='basketball']),
                      "median"= median(data_height$Height[data_height$Sport=='basketball']),
                      "mean"=mean(data_height$Height[data_height$Sport=='basketball']),
                      "max"=max(data_height$Height[data_height$Sport=='basketball']),
                      "sd"=sd(data_height$Height[data_height$Sport=='basketball'])
                        )

summary_handball <- list("observations" = length(data_height$Height[data_height$Sport=='handball']),
                    "min"= min(data_height$Height[data_height$Sport=='handball']),
                    "median"= median(data_height$Height[data_height$Sport=='handball']),
                    "mean"=mean(data_height$Height[data_height$Sport=='handball']),
                    "max"= max(data_height$Height[data_height$Sport=='handball']),
                    "sd"= sd(data_height$Height[data_height$Sport=='handball'])
                      )

summary_icehockey <- list("observations" = length(data_height$Height[data_height$Sport=='ice hockey']),
                     "min"= min(data_height$Height[data_height$Sport=='ice hockey']),
                     "median"= median(data_height$Height[data_height$Sport=='ice hockey']),
                     "mean"=mean(data_height$Height[data_height$Sport=='ice hockey']),
                     "max"= max(data_height$Height[data_height$Sport=='ice hockey']),
                     "sd"= sd(data_height$Height[data_height$Sport=='ice hockey'])
                      )

summary_soccer <- list("observations" = length(data_height$Height[data_height$Sport=='soccer']),
                  "min"= min(data_height$Height[data_height$Sport=='soccer']),
                  "median"= median(data_height$Height[data_height$Sport=='soccer']),
                  "mean"=mean(data_height$Height[data_height$Sport=='soccer']),
                  "max"= max(data_height$Height[data_height$Sport=='soccer']),
                  "sd"= sd(data_height$Height[data_height$Sport=='soccer'])
                    )

summary_volleyball <- list("observations" = length(data_height$Height[data_height$Sport=='volleyball']),
                      "min"= min(data_height$Height[data_height$Sport=='volleyball']),
                      "median"= median(data_height$Height[data_height$Sport=='volleyball']),
                      "mean"=mean(data_height$Height[data_height$Sport=='volleyball']),
                      "max"= max(data_height$Height[data_height$Sport=='volleyball']),
                      "sd"= sd(data_height$Height[data_height$Sport=='volleyball'])
                        )


summary_waterpolo <- list("observations" = length(data_height$Height[data_height$Sport=='water polo']),
                    "min"= min(data_height$Height[data_height$Sport=='water polo']),
                    "median"= median(data_height$Height[data_height$Sport=='water polo']),
                    "mean"=mean(data_height$Height[data_height$Sport=='water polo']),
                    "max"= max(data_height$Height[data_height$Sport=='water polo']),
                    "sd"= sd(data_height$Height[data_height$Sport=='water polo'])
                      )

print(as.matrix(summary_basketball))
print(as.matrix(summary_handball))
print(as.matrix(summary_icehockey))
print(as.matrix(summary_soccer))
print(as.matrix(summary_volleyball))
print(as.matrix(summary_waterpolo))

# The tests that are used (T-test and F-test) have the assumption of normality, for that reason the qq-plot for each group is obtained.

par(mfrow=c(2,3))
qqnorm(data_height$Height[data_height$Sport=="soccer"], pch = 1, frame = FALSE, main = "Soccer", ylab = "Heights of the Players [cm]")
qqline(data_height$Height[data_height$Sport=="soccer"], col = "steelblue", lwd = 2)

qqnorm(data_height$Height[data_height$Sport=="basketball"], pch = 1, frame = FALSE, main = "Basketball", ylab = "Heights of the Players [cm]", xlim = c(-2,2))
qqline(data_height$Height[data_height$Sport=="basketball"], col = "steelblue", lwd = 2)

qqnorm(data_height$Height[data_height$Sport=="handball"], pch = 1, frame = FALSE, main = "Handball", ylab = "Heights of the Players [cm]")
qqline(data_height$Height[data_height$Sport=="handball"], col = "steelblue", lwd = 2)

qqnorm(data_height$Height[data_height$Sport=="water polo"], pch = 1, frame = FALSE, main = "Water Polo", ylab = "Heights of the Players [cm]" )
qqline(data_height$Height[data_height$Sport=="water polo"], col = "steelblue", lwd = 2)

qqnorm(data_height$Height[data_height$Sport=="volleyball"], pch = 1, frame = FALSE,  main = "Volleyball", ylab = "Heights of the Players [cm]", xlim = c(-2,2))
qqline(data_height$Height[data_height$Sport=="volleyball"], col = "steelblue", lwd = 2)

qqnorm(data_height$Height[data_height$Sport=="ice hockey"], pch = 1, frame = FALSE, main = "Ice Hockey", ylab = "Heights of the Players [cm]")
qqline(data_height$Height[data_height$Sport=="ice hockey"], col = "steelblue", lwd = 2)


# The F-test is used as a global test of the means of each group in the data set.
anova <- aov(Height ~ Sport, data = data_height)
summary(anova)

# Finally the pairwise t-test is used to compare the differences between highs of players, the parameter 
# pooled.sd is true by default.

pwc_T <- pairwise.t.test(data_height$Height, data_height$Sport,p.adjust.method="none")

pwc_T_bonferroni <- pairwise.t.test(data_height$Height, data_height$Sport, p.adjust.method = "bonferroni")

# The P-values of the pairwise t-test are printed
print(pwc_T)

# The adjusted P-values of the pairwise t-test are printed
print(pwc_T_bonferroni)




