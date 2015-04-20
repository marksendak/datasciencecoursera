#### This code is for Quiz 4 of the Coursera course "Statistical Inference"
#### Date: July 27, 2014
#### Author: Mark Dakkak

######### Q1) In a random sample of 100 patients at a clinic, you would like to test whether the mean RDI is x or more using a one sided 5% type 1 error rate. The sample mean RDI had a mean of 12 and a standard deviation of 4. What is the largest value of x (H0:μ=x versus Ha:μ>x) would you reject for?

q1mean <- 12
q1sd <- 4
q1n <- 100
q1threshold <- qnorm(.95)
q1mean - (q1threshold * sqrt((q1sd^2)/q1n))

######### Q2) Consider testing the hypothesis that there was a mean reduction in blood pressure? Give the P-value for the associated two sided test.

q2a <- seq(1, 5, 1)
q2b <- c(140, 138, 150, 148, 135)
q2c <- c(132, 135, 151, 146, 130)

q2DF <- as.data.frame(cbind(q2a, q2b, q2c))
colnames(q2DF) <- c("Subject", "Baseline", "Week2")

q2DF$Mean_Diff <- q2DF$Week2 - q2DF$Baseline
colMeans(q2DF)

q2n <- 5
q2meanBaseline <- mean(q2DF$Baseline)
q2sdBaseline <- sd(q2DF$Baseline)
q2meanWeek2 <- mean(q2DF$Week2)
q2sdWeek2 <- sd(q2DF$Week2)

q2pooledSD <- sqrt((q2n-1) * q2sdWeek2^2 + (q2n-1) * q2sdBaseline^2) / ((q2n-1) + (q2n-1))
q2md <- q2meanWeek2 - q2meanBaseline
q2semd <- q2pooledSD * sqrt(1/q2n + 1/q2n)
q2CI <- q2md + c(-1, 1) * qt(0.975, q2n + q2n - 2) * q2semd
        ## -7.50 to 0.70

t.test(q2DF$Baseline, q2DF$Week2, paired=TRUE, var.equal = TRUE)$p.value
        ## Use p.value to get the p-value
        ## Could also use conf to get the confidence interval

######### Q3) A sample of 9 men yielded a sample average brain volume of 1,100cc and a standard deviation of 30cc. What is the complete set of values of μ0 that a test of H0:μ=μ0 would fail to reject the null hypothesis in a two sided 5% Students t-test?

q3n <- 9
q3mean <- 1100
q3sd <- 30
q3error <- qt(0.975, df = q3n - 1) * q3sd / sqrt(q3n)
q3left <- q3mean - q3error
q3right <- q3mean + q3error
q3left  ## 1076.94
q3right  ## 1123.06

######### Q4) Researchers conducted a blind taste test of Coke versus Pepsi. Each of four people was asked which of two blinded drinks given in random order that they preferred. The data was such that 3 of the 4 people chose Coke. Assuming that this sample is representative, report a P-value for a test of the hypothesis that Coke is preferred to Pepsi using a one sided exact test.

q4DF <- matrix(c(3, 7500000, 1, 2500000), nrow = 2, dimnames = list(c("Blind", "Population"), c("Coke", "Pepsi")))

fisher.test(q4DF, alternative = "greater")

#### Alternative
binom.test(3, 4, 1/2, alternative="greater")
        ## ref: http://www.instantr.com/2012/11/06/performing-a-binomial-test/

######### Q5) Infection rates at a hospital above 1 infection per 100 person days at risk are believed to be too high and are used as a benchmark. A hospital that had previously been above the benchmark recently had 10 infections over the last 1,787 person days at risk. About what is the one sided P-value for the relevant test of whether the hospital is *below* the standard?

1787/100
        ## 17.87

ppois(10, 17.87, lower.tail = TRUE)

######### Q6) Suppose that 18 obese subjects were randomized, 9 each, to a new diet pill and a placebo. Subjects’ body mass indices (BMIs) were measured at a baseline and again after having received the treatment or placebo for four weeks. The average difference from follow-up to the baseline (followup - baseline) was −3 kg/m2 for the treated group and 1 kg/m2 for the placebo group. The corresponding standard deviations of the differences was 1.5 kg/m2 for the treatment group and 1.8 kg/m2 for the placebo group. Does the change in BMI over the two year period appear to differ between the treated and placebo groups? Assuming normality of the underlying data and a common population variance, give a pvalue for a two sided t test.

q6n <- 9
q6mean_placebo <- 1
q6mean_treatment <- -3
q6sd_placebo <- 1.8
q6sd_treatment <- 1.5

q6pooledSD <- sqrt(((q6n-1) * q6sd_placebo^2 + (q6n-1) * q6sd_treatment^2) / ((q6n-1) + (q6n-1)))
        ## pooled variance
q6pooledSD^2
q6md <- q6mean_treatment - q6mean_placebo
q6pooledSE <- q6pooledSD * sqrt(1/q6n + 1/q6n)
q6CI <- q6md + c(-1, 1) * qt(0.975, q6n + q6n - 2) * q6pooledSE
        ## -5.655699 -2.344301

pt(q6CI, df = 16)

######### Q8) Researchers would like to conduct a study of 100 healthy adults to detect a four year mean brain volume loss of .01 mm3. Assume that the standard deviation of four year volume loss in this population is .04 mm3. About what would be the power of the study for a 5% one sided test versus a null hypothesis of no volume loss?

power.t.test(n = 100, delta = .01, sd = .04, sig.level = .05, type = "one.sample", alternative = "one.sided")

######### Q9) Researchers would like to conduct a study of n healthy adults to detect a four year mean brain volume loss of .01 mm3. Assume that the standard deviation of four year volume loss in this population is .04 mm3. About what would be the value of n needded for 90% power of type one error rate of 5% one sided test versus a null hypothesis of no volume loss?

power.t.test(delta = .01, sd = .04, sig.level = .05, power = .90, type = "one.sample", alternative = "one.sided")

######### Q11) The Daily Planet ran a recent story about Kryptonite poisoning in the water supply after a recent event in Metropolis. Their usual field reporter, Clark Kent, called in sick and so Lois Lane reported the story. Researchers sampled 288 individuals and found mean blood Kryptonite levels of 44, both measured in Lex Luthors per milliliter (LL/ml). They compared this to 288 sampled individuals from Gotham city who had an average level of 42.04. About what is the Pvalue for a two sided Z test of the relevant hypothesis? Assume that the standard deviation is 12 for both groups.

q11n <- 288
q11mean_Metro <- 44
q11mean_Gotham <- 42.04
q11sd_Metro <- 12
q11sd_Gotham <- 12

q11pooledSD <- sqrt(((q11n-1) * q11sd_Metro^2 + (q11n-1) * q11sd_Gotham^2) / ((q11n-1) + (q11n-1)))
        ## pooled variance
q11pooledSD^2
q11md <- q11mean_Metro - q11mean_Gotham
q11pooledSE <- q11pooledSD * sqrt(1/q11n + 1/q11n)

q11t.statistic <- q11md/q11pooledSE
