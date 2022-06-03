Stanislaw & Todorov (1999) have a good discussion of this under the heading Hit and False-Alarm Rates of Zero or One.

They discuss the pros and cons of several methods for dealing with these extreme values, including:

    Use a non-parametric statistic such as A′

instead of d′

(Craig, 1979)

Aggregate data from multiple subjects before calculating the statistic (Macmillan & Kaplan, 1985)

add 0.5 to both the number of hits and the number of false alarms, and add 1 to both the number of signal trials and the number of noise trials; dubbed the loglinear approach (Hautus, 1995) (see note below)

Adjust only the extreme values by replacing rates of 0 with 0.5/n
and rates of 1 with (n−0.5)/n where n

    is the number of signal or noise trials (Macmillan & Kaplan, 1985)

The choice is ultimately up to you. Personally I prefer the third approach. The first approach has the drawback that A′
is less interpretable to your readers who are much more familiar with d′

. The second approach may not be suitable if you are interested in single-subject behavior. The fourth approach is biased because you are not treating your data points equally.

Note: the loglinear method calls for adding 0.5 to all cells under the assumption that there are an equal number of signal and noise trials. If this is not the case, then the numbers will be different. If there are, say, 60% signal trials and 40% noise trials, then you would add 0.6 to the number of Hits, and 2x0.6 = 1.2 to the number of signal trials, and then 0.4 to the number of false alarms, and 2x0.4 = 0.8 to the number of noise trials, etc.


Accdata$SP_d = qnorm((Accdata$samepair_cor+.5)/(Accdata$samepair_cor+Accdata$samepair_incor+1)) - 
               qnorm((Accdata$totalNEW_incor+.5)/(Accdata$totalNEW_cor+Accdata$totalNEW_incor+1))  # Dprime method( Hautus,1995)