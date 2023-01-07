# Jump-Hypothesis-Tests

# Overview:  
   This repository contains an application of hypothesis testing on time series modeled by Lévy processes. The method is used in  
   
   [_Infinitesimal generators for two-dimensional Lévy process-driven hypothesis testing_](https://link.springer.com/article/10.1007/s10436-019-00355-y),  
   [_Sequential Hypothesis Testing in Machine Learning, and Crude Oil Price Jump Size Detection_](https://www.tandfonline.com/doi/full/10.1080/1350486X.2020.1859943), and  
   _Hypothesis tests on high dimensional data streams, with finance application_ (to be published).  
   
   The goal is to identify periods of time when the jump parameters of the underlying stochastic processes are large relative to the training data set's and then use those results to predict the size of the jumps for future time periods. Specifically, I have applied it to oil prices and stock prices. This repository focuses on the application to 6 time series of stock close prices.  
   
# The Data:  
  The raw data set is sourced from Bloomberg Terminal and Yahoo Finance. S &P 500 is an
index measuring the performances of 500 large companies from 11 business sectors based
on their value and market capital. We utilize S &P 500 as our data set since the main focus
in this paper is on large companies from different industries. The data set contains 503
individual stocks from S & P 500, and we pulled the daily closing prices of each S & P 500
company for the period August 8, 2017 to September 6, 2020. We index the available dates
from 0 (August 8, 2017) to 1279 (September 6, 2020).

  We primarily focus on six stocks from different industries with relatively lower correlations compared to others. As illustrated in Figure 4, we pick XOM, VTR, BA, ECL, NWS, and KR from the raw data set based on their low stock price correlations. We do so in
order to minimize the correlations of the diffusion terms in the underlying Lévy processes.
In Figure 5, we further explore the trends of each stock and the S & P 500 index. In general,
there was a huge spike at the beginning of 2020 due to the COVID-19 Pandemic, and then
most companies started to bounce back and steadily grew.

# The Algorithm:  

Through the following algorithm, I use the theorems and decision rules in the previous papers to detect an important parameter to classify Lévy processes as having small or large jumps.

Given stock close prices in length- $m$ work day periods, we do the following: 

1. An inverse Gaussian density representing $\nu$ is fit to the distribution of negative percent daily jumps for the entire (training) data set.  

2. We then fit the density of the L\'evy measure $\nu^*(dx)=(1+ax)\nu(dx)$ to the distribution of the negative percent daily jumps for the $m$-length period. This gives the statistic $a$ approximating the parameter in the hypothesis test.  

3. Using the density $\nu^*$ and the standard deviation of percent daily changes $\sigma$, we calculate $\gamma$, $\beta$, $C$, $K$ in the super- and sub-solutions.  

4. We then compare the drift and the subordinator, weighing them both against the volatility of the log-likelihood process -- for each time period of $m$ days, we assign a value $$z=\frac{\gamma-\int x K(dx)}{\beta}.$$ 

5. We take the time periods with $z$ above some threshold and classify those time periods as having large jumps.  

The result of this algorithm is a list of 0's and 1's, representing whether the time period that begins on each day has large or small jumps. Graphically, I have highlighted the large jump time periods in red below. This is slightly misleading because the time periods overlap. Note that, in actuality, each period is 10 days.

![Results](https://github.com/mjroberts543/Jump-Hypothesis-Tests/blob/main/red.png)

# Machine Learning Predictions:  

We present four algorithms that can be used in the prediction of large jump time periods. Classification reports and confusion matrices are evaluated for each algorithm after applying various machine-learning techniques. First, we have __Algorithm I__ that seeks to predict whether the majority of the 6 stocks will have a significantly larger Lévy density using previous time periods' classifications:

1. For each of the 6 stocks, we run the previous algorithm to identify big jump time periods with $m=10$.
2. We take the mean of the 6 stocks' classification lists, where 1 signifies big jumps and 0 signifies small jumps, and create a staggering data frame with each set of 10 consecutive days' means as attributes.  
3. We create a target column that is $0$ if the mean of the 6 stocks' classification lists for the next disjoint $10$ days is above 1/2.  
4. We run various classification algorithms using the 10 consecutive days' means as features. The output is $1$ to represent large jumps or $0$ to represent small jumps in the next disjoint $10$ consecutive days for a majority of the stocks.  

__Algorithm II__ seeks to predict whether the majority of the 6 stocks will have a significantly larger Lévy density using previous days' mean percent daily changes:

1. For each of the 6 stocks, we calculate the percent daily changes for each day.  
2. We take the mean of the 6 stocks' percent daily changes and create a staggering data frame with each set of 10 consecutive days' means as attributes.  
3. We create a target column that is $0$ if the mean of the 6 stocks' classification lists for the next disjoint $10$ days is above 1/2.  
4. We run various classification algorithms using the 10 consecutive days' mean percent daily changes as features. The output is $1$ to represent large jumps or $0$ to represent small jumps in the next disjoint $10$ consecutive days for a majority of the stocks.  

Next, __Algorithm III__ aims to predict the big jump periods in the S \& P 500 index based on previous time periods' big/small classifications:

1. For each of the 6 stocks and S\& P 500 index, we run the previous algorithm to identify big jump time periods.  
2. We take the mean of the 6 stocks' classification lists, where 1 signifies big jumps and 0 signifies small jumps, and create a staggering data frame with each set of 10 consecutive days' means as attributes.  
3. We create a target column that is $0$ if the S\& P 500 index of the next disjoint $10$ days has small jumps and is $1$ if it has big jumps.  
4. We run various classification algorithms using the 10 consecutive days' means as features. The output is $1$ to represent large jumps or $0$ to represent small jumps in the next disjoint $10$ consecutive days for the S \& P 500 index.  

Finally, __Algorithm IV__ aims to predict the big jump periods in the S \& P 500 index based on previous days' mean percent daily changes:

1. For each of the 6 stocks, we calculate the percent daily changes for each day.  
2. We take the mean of the 6 stocks' percent daily changes and create a staggering data frame with each set of 10 consecutive days' means as attributes.  
3. We create a target column that is $0$ if the S\& P 500 index of the next disjoint $10$ days has small jumps and is $1$ if it has big jumps.  
4. We run various classification algorithms using the 10 consecutive days' means as features. The output is $1$ to represent large jumps or $0$ to represent small jumps in the next disjoint $10$ consecutive days for the S \& P 500 index.  





Here, we present the results of the predictions using various machine learning techniques.
We first use logistic regression (LR). Next, random forests (RF). Two neural net algorithms
are also employed: one is a classical neural net (NN), and the other is a long short-term
memory net with a batch normalizer (LSTM). The results are as follows:

![Results](https://github.com/mjroberts543/Jump-Hypothesis-Tests/blob/main/ml.png)
  
