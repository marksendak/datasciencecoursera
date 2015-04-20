#### This code is for Quiz 3 of the Coursera course "Statistical Inference"
#### Date: July 27, 2014
#### Author: Mark Dakkak

######### Q1) In a population of interest, a sample of 9 men yielded a sample average brain volume of 1,100cc and a standard deviation of 30cc. What is a 95% Student's T confidence interval for the mean brain volume in this new population?

q1mean <- 1100
q1sd <- 30
q1n <- 9
q1error <- qt(0.975, df = q1n - 1) * q1sd / sqrt(q1n)
q1left <- q1mean - q1error
q1right <- q1mean + q1error
q1left  ## 1076.94
q1right  ## 1123.06

######### Q2) A diet pill is given to 9 subjects over six weeks. The average difference in weight (follow up - baseline) is -2 pounds. What would the standard deviation have to be for the upper endpoint of the 95% T confidence interval to touch 0?

q2mean <- -2
q2n <- 9
q2error <- 2
q2sd <- (q2error * sqrt(q2n)) / qt(0.975, df = q2n - 1)
q2sd  ## 2.601903

######### Q4) Refer to the setting of the previous question. To further test the system, administrators selected 20 nights and randomly assigned the new triage system to be used on 10 nights and the standard system on the remaining 10 nights. They calculated the nightly median waiting time (MWT) to see a physician. The average MWT for the new system was 3 hours with a variance of 0.60 while the average MWT for the old system was 5 hours with a variance of 0.68. Consider the 95% confidence interval estimate for the differences of the mean MWT associated with the new system. Assume a constant variance. What is the interval? Subtract in this order (New System - Old System).

meanMWT_new <- 3
varMWT_new <- .6
meanMWT_old <- 5
varMWT_old <- .68
q4n <- 10
q4sp <- sqrt((9*varMWT_new + 9*varMWT_old)/(2*(q4n-1)))
        ## pooled variance estimate
q4md <- meanMWT_new - meanMWT_old
        ## difference in means
q4semd <- q4sp * sqrt(1/q4n + 1/q4n)
        ## standard error of means
q4md + c(-1,1) * qt(.975, 2*(q4n-1)) * q4semd
        ## confidence interval

######### Q6) To further test a hospital triage system, administrators selected 200 nights and randomly assigned a new triage system to be used on 100 nights and a standard system on the remaining 100 nights. They calculated the nightly median waiting time (MWT) to see a physician. The average MWT for the new system was 4 hours with a standard deviation of .5 hours while the average MWT for the old system was 6 hours with a standard deviation of 2 hours. Consider the hypothesis of a decrease in the mean MWT associated with the new treatment. What does the interval suggest vis a vis this hyptothesis?

meanMWT_new <- 4
sdMWT_new <- .5
meanMWT_old <- 6
sdMWT_old <- 2
q4n <- 100
q4sp <- sqrt(((q4n-1)*(sdMWT_new^2) + (q4n-1)*(sdMWT_old^2))/(2*(q4n-1)))
        ## pooled variance estimate
q4md <- meanMWT_old - meanMWT_new
        ## difference in means
q4semd <- q4sp * sqrt(1/q4n + 1/q4n)
        ## standard error of means
q4md + c(-1,1) * qt(.975, 2*(q4n-1)) * q4semd
        ## confidence interval: 1.593458 2.406542

######### Q7) Suppose that 18 obese subjects were randomized, 9 each, to a new diet pill and a placebo. Subjects’ body mass indices (BMIs) were measured at a baseline and again after having received the treatment or placebo for four weeks. The average difference from follow-up to the baseline (followup - baseline) was −3 kg/m2 for the treated group and 1 kg/m2 for the placebo group. The corresponding standard deviations of the differences was 1.5 kg/m2 for the treatment group and 1.8 kg/m2 for the placebo group. Does the change in BMI over the four week period appear to differ between the treated and placebo groups? Assuming normality of the underlying data and a common population variance, calculate the relevant *90%* t confidence interval. Subtract in the order of (Treated - Placebo) with the smaller (more negative) number first.

meanBMI_treated <- -3
sdBMI_treated <- 1.5
meanBMI_placebo <- 1
sdBMI_placebo <- 1.8
q5n <- 9
q5sp <- sqrt(((q5n-1)*(sdBMI_treated^2) + (q5n-1)*(sdBMI_placebo^2))/(2*(q5n-1)))
        ## pooled variance estimate
q5md <- meanBMI_treated - meanBMI_placebo
        ## difference in means
q5semd <- q5sp * sqrt(1/q5n + 1/q5n)
        ## standard error of means
q5md + c(-1,1) * qt(.95, 2*(q5n-1)) * q5semd
        ## confidence interval: 1.593458 2.406542
