---
title: "Chapter 4"
subtitle: "Poisson Regression"
output:
  pdf_document:
    number_sections: yes
  html_document: default
---
# Poisson Regression {#ch-poissonreg}

## Learning Objectives

After finishing this chapter, you should be able to:  

- Describe why  simple linear regression is not ideal for Poisson data.
- Write out a Poisson regression model and identify the assumptions for inference.
- Write out the likelihood for a Poisson regression and describe how it could be used to estimate coefficients for a model.
- Interpret estimated coefficients from a Poisson regression and construct confidence intervals for them.
- Use deviances for Poisson regression models to compare and assess models.
- Use an offset to account for varying effort in data collection.
- Fit and use a zero-inflated Poisson (ZIP) model.


```r
# Packages required for Chapter 4
library(gridExtra)
library(knitr)
library(kableExtra)
library(mosaic)
library(xtable)
library(pscl) 
library(multcomp)
library(pander)
library(MASS)
library(tidyverse)
```



## Introduction to Poisson Regression

Consider the following questions:

1. Are the number of motorcycle deaths in a given year related to a state's helmet laws?
2. Does the number of employers conducting on-campus interviews during a year differ for public and private colleges? 
3. Does the daily number of asthma-related visits to an Emergency Room differ depending on air pollution indices?
4. Has the number of deformed fish in randomly selected Minnesota lakes been affected by changes in trace minerals in the water over the last decade? 

Each example involves predicting a response using one or more explanatory variables, although these examples have response variables that are counts per some unit of time or space.  A Poisson random variable is often used to model counts; see Chapter \@ref(ch-distthry) for properties of the Poisson distribution.  Since a Poisson random variable is a count, its minimum value is zero and, in theory, the maximum is unbounded. We'd like to model our main parameter $\lambda$, the average number of occurrences per unit of time or space, as a function of one or more covariates.  For example, in the first question above, $\lambda_i$ represents the average number of motorcycle deaths in a year for state $i$, and we hope to show that state-to-state variability in $\lambda_i$ can be explained by state helmet laws.  

For a linear least squares regression model, the parameter of interest is the average response, $\mu_i$, for subject $i$, and $\mu_i$ is modeled as a line in the case of one explanatory variable. By analogy, it might seem reasonable to try to model the Poisson parameter $\lambda_i$ as a linear function of an explanatory variable, but there are some problems with this approach.  In fact, a model like $\lambda_i=\beta_0+\beta_1x_i$ doesn't work well for Poisson data.  A line is certain to yield negative values for certain $x_i$, but $\lambda_i$ can only take on values from 0 to $\infty$. In addition, the equal variance assumption in linear regression inference is violated because as the mean rate for a Poisson variable increases, the variance also increases (recall from Chapter \@ref(ch-distthry) that if $Y$ is the observed count, then $E(Y)=Var(Y)=\lambda$).  

One way to avoid these problems is to model log($\lambda_i$) instead of $\lambda_i$ as a function of the covariates. The log($\lambda_i$) takes on values from $-\infty$ to $\infty$. We can also take into account the increase in the variance with an increasing mean using this approach. (Note that throughout *Beyond Multiple Linear Regression* we use log to represent the natural logarithm.)  Thus, we will consider the **Poisson regression** \index{Poisson regression} model:

\begin{equation*}
log(\lambda_i)=\beta_0+\beta_1 x_i
\end{equation*}
where the observed values $Y_i \sim$ Poisson with $\lambda=\lambda_i$ for a given $x_i$. For example, each state $i$ can potentially have a different $\lambda$ depending on its value of $x_i$, where $x_i$ could represent presence or absence of a particular helmet law.  Note that the Poisson regression model contains no separate error term like the $\epsilon$ we see in linear regression, because $\lambda$ determines both the mean and the variance of a Poisson random variable.

### Poisson Regression Assumptions

Much like linear least squares regression (LLSR), using Poisson regression to make inferences requires model assumptions.

1. __Poisson Response__ The response variable is a count per unit of time or space, described by a Poisson distribution.
2. __Independence__ The observations must be independent of one another.
3. __Mean=Variance__ By definition, the mean of a Poisson random variable must be equal to its variance.
4. __Linearity__ The log of the mean rate, log($\lambda$), must be a linear function of x.

### A Graphical Look at Poisson Regression

<div class="figure" style="text-align: center">
<img src="04-Poisson-Regression_files/figure-epub3/OLSpois-1.png" alt="Regression models: Linear regression (left) and Poisson regression (right)." width="60%" />
<p class="caption">(\#fig:OLSpois)Regression models: Linear regression (left) and Poisson regression (right).</p>
</div>

Figure \@ref(fig:OLSpois) illustrates a comparison of the LLSR model for inference to Poisson regression using a log function of $\lambda$.

1. The graphic displaying the LLSR inferential model appears in the left panel of Figure \@ref(fig:OLSpois). It shows that, for each level of X, the responses are approximately normal.  The panel on the right side of Figure \@ref(fig:OLSpois) depicts what a Poisson regression model looks like. For each level of X, the responses follow a Poisson distribution (Assumption 1). For Poisson regression, small values of $\lambda$ are associated with a distribution that is noticeably skewed with lots of small values and only a few larger ones. As $\lambda$ increases the distribution of the responses begins to look more and more like a normal distribution.
2. In the LLSR model, the variation in $Y$ at each level of X, $\sigma^2$, is the same. For Poisson regression the responses at each level of X become more variable with increasing means, where variance=mean (Assumption 3). 
3. In the case of LLSR, the mean responses for each level of X, $\mu_{Y|X}$, fall on a line. In the case of the Poisson model, the mean values of $Y$ at each level of $X$, $\lambda_{Y|X}$, fall on a curve, not a line, although the logs of the means should follow a line (Assumption 4).


## Case Studies Overview

We take a look at the Poisson regression model in the context of three case studies. Each case study is based on real data and real questions. Modeling household size in the Philippines introduces the idea of regression with a Poisson response along with its assumptions. A quadratic term is added to a model to determine an optimal size per household, and methods of model comparison are introduced. The campus crime case study introduces two big ideas in Poisson regression modeling: offsets, to account for sampling effort, and overdispersion, when actual variability exceeds what is expected by the model.  Finally, the weekend drinking example uses a modification of a Poisson model to account for more zeros than would be expected for a Poisson random variable. These three case studies also provide context for some of the familiar concepts related to modeling such as exploratory data analysis (EDA), estimation, and residual plots.

## Case Study: Household Size in the Philippines {#cs-philippines}

How many other people live with you in your home? The number of people sharing a house differs from country to country and often from region to region. International agencies use household size when determining needs of populations, and the household sizes determine the magnitude of the household needs.

The Philippine Statistics Authority (PSA) spearheads the Family Income and Expenditure Survey (FIES) nationwide. The survey, which is undertaken every three years, is aimed at providing data on family income and expenditure, including levels of consumption by item of expenditure. Our data, from the 2015 FIES, is a subset of 1500 of the 40,000 observations [@PSA].  Our data set focuses on five regions: Central Luzon, Metro Manila, Ilocos, Davao, and Visayas (see Figure \@ref(fig:philippinesmap)). 

<div class="figure" style="text-align: center">
<img src="data/map_of_philippines.jpg" alt="Regions of the Philippines." width="50%" />
<p class="caption">(\#fig:philippinesmap)Regions of the Philippines.</p>
</div>

At what age are heads of households in the Philippines most likely to find the largest number of people in their household? Is this association similar for poorer households (measured by the presence of a roof made from predominantly light/salvaged materials)? We begin by explicitly defining our response, $Y=$ number of household members other than the head of the household. We then define the explanatory variables: age of the head of the household, type of roof (predominantly light/salvaged material or predominantly strong material), and location (Central Luzon, Davao Region, Ilocos Region, Metro Manila, or Visayas). Note that predominantly light/salvaged materials are a combination of light material, mixed but predominantly light material, and mixed but predominantly salvaged material, and salvaged matrial.  Our response is a count, so we consider a Poisson regression where the parameter of interest is $\lambda$, the average number of people, other than the head, per household. We will primarily examine the relationship between household size and age of the head of household, controlling for location and income.



### Data Organization {#organizedata4}
The first five rows from our data set `fHH1.csv` are illustrated in Table \@ref(tab:fHH1table1). Each line of the data file refers to a household at the time of the survey:

- `location` = where the house is located (Central Luzon, Davao Region, Ilocos Region, Metro Manila, or Visayas)
- `age` = the age of the head of household
- `total` = the number of people in the household other than the head
- `numLT5` = the number in the household under 5 years of age 
- `roof` = the type of roof in the household (either Predominantly Light/Salvaged Material, or Predominantly Strong Material, where stronger material can sometimes be used as a proxy for greater wealth)

<table>
<caption>(\#tab:fHH1table1)The first five observations from the Philippines Household case study.</caption>
 <thead>
  <tr>
   <th style="text-align:right;"> X1 </th>
   <th style="text-align:left;"> location </th>
   <th style="text-align:right;"> age </th>
   <th style="text-align:right;"> total </th>
   <th style="text-align:right;"> numLT5 </th>
   <th style="text-align:left;"> roof </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> CentralLuzon </td>
   <td style="text-align:right;"> 65 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> Predominantly Strong Material </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> MetroManila </td>
   <td style="text-align:right;"> 75 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> Predominantly Strong Material </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> DavaoRegion </td>
   <td style="text-align:right;"> 54 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> Predominantly Strong Material </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:left;"> Visayas </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> Predominantly Strong Material </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> MetroManila </td>
   <td style="text-align:right;"> 74 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> Predominantly Strong Material </td>
  </tr>
</tbody>
</table>


### Exploratory Data Analyses {#exploreHH}



For the rest of this case study, we will refer to the number of people in a household as the total number of people in that specific household *besides* the head of household. The average number of people in a household is 3.68 (Var = 5.53), and there are anywhere from 0 to 16 people in the houses. Over 11.1\% of these households are made from predominantly light and salvaged material. The mean number of people in a house for houses with a roof made from predominantly strong material is 3.69 (Var=5.55), whereas houses with a roof made from predominantly light/salvaged material average 3.64 people (Var=5.41). Of the various locations, Visayas has the largest household size, on average, with a mean of 3.90 in the household, and the Davao Region has the smallest with a mean of 3.39.  

<div class="figure" style="text-align: center">
<img src="04-Poisson-Regression_files/figure-epub3/nhouse-1.png" alt="Distribution of household size in 5 Philippine regions." width="60%" />
<p class="caption">(\#fig:nhouse)Distribution of household size in 5 Philippine regions.</p>
</div>

Figure \@ref(fig:nhouse) reveals a fair amount of variability in the number in each house; responses range from 0 to 16 with many of the respondents reporting between 1 and 5 people in the house. Like many Poisson distributions, this graph is right skewed. It clearly does not suggest that the number of people in a household is a normally distributed response.

<div class="figure" style="text-align: center">
<img src="04-Poisson-Regression_files/figure-epub3/totalPoisByAge-1.png" alt="Distribution of household sizes by age group of the household head." width="60%" />
<p class="caption">(\#fig:totalPoisByAge)Distribution of household sizes by age group of the household head.</p>
</div>

Figure \@ref(fig:totalPoisByAge) further shows that responses can be reasonably modeled with a Poisson distribution when grouped by a key explanatory variable: age of the household head.  These last two plots together suggest that Assumption 1 (Poisson Response) is satisfactory in this case study.

For Poisson random variables, the variance of $Y$ (i.e., the square of the standard deviation of $Y$), is equal to its mean, where $Y$ represents the size of an individual household. As the mean increases, the variance increases. So, if the response is a count and the mean and variance are approximately equal for each group of $X$, a Poisson regression model may be a good choice. In Table \@ref(tab:table1chp4) we display age groups by 5-year increments, to check to see if the empirical means and variances of the number in the house are approximately equal for each age group. This provides us one way in which to check the Poisson Assumption 3 (mean = variance).

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:table1chp4)Compare mean and variance of household size within each age group.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Age Groups </th>
   <th style="text-align:right;"> Mean </th>
   <th style="text-align:right;"> Variance </th>
   <th style="text-align:right;"> n </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> (15,20] </td>
   <td style="text-align:right;"> 1.666667 </td>
   <td style="text-align:right;"> 0.6666667 </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> (20,25] </td>
   <td style="text-align:right;"> 2.166667 </td>
   <td style="text-align:right;"> 1.5588235 </td>
   <td style="text-align:right;"> 18 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> (25,30] </td>
   <td style="text-align:right;"> 2.918367 </td>
   <td style="text-align:right;"> 1.4098639 </td>
   <td style="text-align:right;"> 49 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> (30,35] </td>
   <td style="text-align:right;"> 3.444444 </td>
   <td style="text-align:right;"> 2.1931464 </td>
   <td style="text-align:right;"> 108 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> (35,40] </td>
   <td style="text-align:right;"> 3.841772 </td>
   <td style="text-align:right;"> 3.5735306 </td>
   <td style="text-align:right;"> 158 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> (40,45] </td>
   <td style="text-align:right;"> 4.234286 </td>
   <td style="text-align:right;"> 4.4447947 </td>
   <td style="text-align:right;"> 175 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> (45,50] </td>
   <td style="text-align:right;"> 4.489691 </td>
   <td style="text-align:right;"> 6.3962662 </td>
   <td style="text-align:right;"> 194 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> (50,55] </td>
   <td style="text-align:right;"> 4.010638 </td>
   <td style="text-align:right;"> 5.2512231 </td>
   <td style="text-align:right;"> 188 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> (55,60] </td>
   <td style="text-align:right;"> 3.806897 </td>
   <td style="text-align:right;"> 6.5318966 </td>
   <td style="text-align:right;"> 145 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> (60,65] </td>
   <td style="text-align:right;"> 3.705882 </td>
   <td style="text-align:right;"> 6.1958204 </td>
   <td style="text-align:right;"> 153 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> (65,70] </td>
   <td style="text-align:right;"> 3.339130 </td>
   <td style="text-align:right;"> 7.9980168 </td>
   <td style="text-align:right;"> 115 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 2.549738 </td>
   <td style="text-align:right;"> 5.5435657 </td>
   <td style="text-align:right;"> 191 </td>
  </tr>
</tbody>
</table>

If there is a problem with this assumption, most often we see variances much larger than means.  Here, as expected, we see more variability as age increases.  However, it appears that the variance is smaller than the mean for lower ages, while the variance is greater than the mean for higher ages.  Thus, there is some evidence of a violation of the mean=variance assumption (Assumption 3), although any violations are modest.  

The Poisson regression model also implies that log($\lambda_i$), not the mean household size $\lambda_i$, is a linear function of age; i.e., $log(\lambda_i)=\beta_0+\beta_1\textrm{age}_i$. Therefore, to check the linearity assumption (Assumption 4) for Poisson regression, we would like to plot log($\lambda_i$) by age.  Unfortunately, $\lambda_i$ is unknown. Our best guess of $\lambda_i$ is the observed mean number in the household for each age (level of $X$). Because these means are computed for observed data, they are referred to as **empirical** means. Taking the logs of the empirical means and plotting by age provides a way to assess the linearity assumption. The smoothed curve added to Figure \@ref(fig:ageXnhouse) suggests that there is a curvilinear relationship between age and the log of the mean household size, implying that adding a quadratic term should be considered. This finding is consistent with the researchers' hypothesis that there is an age at which a maximum household size occurs. It is worth noting that we are not modeling the log of the empirical means, rather it is the log of the *true* rate that is modeled. Looking at empirical means, however, does provide an idea of the form of the relationship between log($\lambda)$ and $x_i$.

<div class="figure" style="text-align: center">
<img src="04-Poisson-Regression_files/figure-epub3/ageXnhouse-1.png" alt="The log of the mean household sizes, besides the head of household, by age of the head of household, with loess smoother." width="60%" />
<p class="caption">(\#fig:ageXnhouse)The log of the mean household sizes, besides the head of household, by age of the head of household, with loess smoother.</p>
</div>

We can extend Figure \@ref(fig:ageXnhouse) by fitting separate curves for each region (see Figure \@ref(fig:byregion)).  This allows us to see if the relationship between mean household size and age is consistent across region.  In this case, the relationships are pretty similar; if they weren't, we could consider adding an age-by-region interaction to our eventual Poisson regression model.

<div class="figure" style="text-align: center">
<img src="04-Poisson-Regression_files/figure-epub3/byregion-1.png" alt="Empirical log of the mean household sizes vs. age of the head of household, with loess smoother by region." width="60%" />
<p class="caption">(\#fig:byregion)Empirical log of the mean household sizes vs. age of the head of household, with loess smoother by region.</p>
</div>

Finally, the independence assumption (Assumption 2) can be assessed using knowledge of the study design and the data collection process.  In this case, we do not have enough information to assess the independence assumption with the information we are given. If each household was not selected individually in a random manner, but rather groups of households were selected from different regions with differing customs about living arrangements, the independence assumption would be violated. If this were the case, we could use a multilevel model like those discussed in later chapters with a village term.


### Estimation and Inference {#sec-PoisInference}

We first consider a model for which log($\lambda$) is linear in age. We then will determine whether a model with a quadratic term in age provides a significant improvement based on trends we observed in the exploratory data analysis.

R reports an estimated regression equation for the linear Poisson model as:

\begin{equation*}
log(\hat{\lambda}) = 1.55 - 0.0047 \textrm{age}
\end{equation*}


```r
modela = glm(total ~ age, family = poisson, data = fHH1)
```


```
##                 Estimate   Std. Error   z value
## (Intercept)  1.549942225 0.0502754106 30.829032
## age         -0.004705881 0.0009363388 -5.025832
##                                                                                                                                                                                                                             Pr(>|z|)
## (Intercept) 0.0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001070156
## age         0.0000005012547544342413452620084435285008339633350260555744171142578125000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
```

```
##  Residual deviance =  2337.089  on  1498 df 
##  Dispersion parameter =  1
```

How can the coefficient estimates be interpreted in terms of this example? As done when interpreting slopes in the LLSR models, we consider how the estimated mean number in the house, $\lambda$, changes as the age of the household head increases by an additional year. But in place of looking at change in the mean number in the house, with a Poisson regression we consider the log of the mean number in the house and then convert back to original units. For example, consider a comparison of two models---one for a given age ($x$) and one after increasing age by 1 ($x+1$):

\begin{equation}
\begin{split}
log(\lambda_X) &= \beta_0 + \beta_1X \\
log(\lambda_{X+1}) &= \beta_0 + \beta_1(X+1) \\
log(\lambda_{X+1})-log(\lambda_X) &=  \beta_1 \\
log \left(\frac{\lambda_{X+1}}{\lambda_X}\right)   &= \beta_1\\
\frac{\lambda_{X+1}}{\lambda_X} &= e^{\beta_1}
\end{split}
(\#eq:rateRatio)
\end{equation}

These results suggest that by exponentiating the coefficient on age we obtain the multiplicative factor by which the mean count changes. In this case, the mean number in the house changes by a factor of $e^{-0.0047}=0.995$ or decreases by 0.5\% (since $1-.995 = .005$) with each additional year older the household head is; or, we predict a 0.47\% *increase* in mean household size for a 1-year *decrease* in age of the household head (since $1/.995=1.0047$). The quantity on the left-hand side of Equation \@ref(eq:rateRatio) is referred to as a __rate ratio__  or __relative risk__, \index{relative risk (rate ratio)} and it represents a percent change in the response for a unit change in X.  In fact, for regression models in general, whenever a variable (response or explanatory) is logged, we make interpretations about multiplicative effects on that variable, while with unlogged variables we can reach our usual interpretations about additive effects.  



Typically, the standard errors for the estimated coefficients are included in Poisson regression output. Here the standard error for the estimated coefficient for age is 0.00094. We can use the standard error to construct a confidence interval for $\beta_1$. A 95\% CI provides a range of plausible values for the `age` coefficient and can be constructed:

\[(\hat\beta_1-Z^*\cdot SE(\hat\beta_1), \quad \hat\beta_1+Z^*\cdot SE(\hat\beta_1))\]
\[(-0.0047-1.96*0.00094, \quad -0.0047+1.96*0.00094)\]
\[ (-0.0065, -0.0029).
 \]

Exponentiating the endpoints yields a confidence interval for the relative risk; i.e., the percent change in household size for each additional year older.  Thus $(e^{-0.0065},e^{-0.0029})=(0.993,0.997)$ suggests that we are 95\% confident that the mean number in the house decreases between 0.7\% and 0.3\% for each additional year older the head of household is. It is best to construct a confidence interval for the coefficient and then exponentiate the endpoints because the estimated coefficients more closely follow a normal distribution than the exponentiated coefficients. There are other approaches to constructing intervals in these circumstances, including profile likelihood, the delta method, and bootstrapping, and we will discuss some of those approaches later.  In this case, for instance, the profile likelihood interval is nearly identical to the Wald-type (normal theory) confidence interval \index{Wald-type confidence interval} above.


```r
# CI for betas using profile likelihood
confint(modela)
```

```
##                    2.5 %       97.5 %
## (Intercept)  1.451170100  1.648249185
## age         -0.006543163 -0.002872717
```

```r
exp(confint(modela))
```

```
##                 2.5 %    97.5 %
## (Intercept) 4.2681057 5.1978713
## age         0.9934782 0.9971314
```

If there is no association between age and household size, there is no change in household size for each additional year, so $\lambda_X$ is equal to $\lambda_{X+1}$ and the ratio $\lambda_{X+1}/\lambda_X$ is 1.  In other words, if there is no association between age and household size, then $\beta_1=0$ and $e^{\beta_1}=1$.  Note that our interval for $e^{\beta_1}$, (0.993,0.997), does not include 1, so the model with age is preferred to a model without age; i.e., age is significantly associated with household size.  Note that we could have similarly confirmed that our interval for $\beta_1$ does not include 0 to show the significance of age as a predictor of household size.  

Another way to test the significance of the age term is to calculate a __Wald-type statistic__. \index{Wald-type test} A Wald-type test statistic is the estimated coefficient divided by its standard error. When the true coefficient is 0, this test statistic follows a standard normal distribution for sufficiently large $n$. The estimated coefficient associated with the linear term in age is ${\hat{\beta}_1}=-0.0047$ with standard error $SE(\hat{\beta}_1)=0.00094$.  The value for the Wald test statistic is then $Z=\hat{\beta}_1/SE(\hat{\beta}_1)=-5.026$, where $Z$ follows a standard normal distribution if $\beta_1=0$.  In this case, the two-sided p-value based on the standard normal distribution for testing $H_0:\beta_1=0$ is almost 0 ($p=0.000000501$).  Therefore, we have statistically significant evidence (Z = -5.026, p < .001) that average household size decreases as age of the head of household increases.  

### Using Deviances to Compare Models {#sec-Devtocompare}

There is another way in which to assess how useful age is in our model. A __deviance__ \index{deviance} is a way in which to measure how the observed data deviates from the model predictions; it will be defined more precisely in Section \@ref(sec-PoisResid), but it is similar to sum of squared errors (unexplained variability in the response) in LLSR regression.  Because we want models that minimize deviance, we calculate the __drop-in-deviance__ \index{drop-in-deviance test} when adding age to the model with no covariates (the **null model**). \index{null (reduced) model} The deviances for the null model and the model with age can be found in the model output. A residual deviance for the model with age is reported as 2337.1 with 1498 df. The output also includes the deviance and degrees of freedom for the null model (2362.5 with 1499 df). The drop-in-deviance is 25.4 (2362.5 - 2337.1) with a difference of only 1 df, so that the addition of one extra term (age) reduced unexplained variability by 25.4.  If the null model were true, we would expect the drop-in-deviance to follow a $\chi^2$ distribution with 1 df. Therefore, the p-value for comparing the null model to the model with age is found by determining the probability that the value for a $\chi^2$ random variable with one degree of freedom exceeds 25.4, which is essentially 0. Once again, we can conclude that we have statistically significant evidence ($\chi^2_{\text{df} =1}=25.4$, $p < .001$) that average household size decreases as age of the head of household increases.  


```r
# model0 is the null/reduced model
model0 <- glm(total ~ 1, family = poisson, data = fHH1)
drop_in_dev <- anova(model0, modela, test = "Chisq")
```


```
  ResidDF ResidDev Deviance Df            pval
1    1499 2362.488       NA NA              NA
2    1498 2337.089 25.39907  1 0.0000004661424
```

More formally, we are testing:

$$\textrm{Null (reduced) Model}: \log(\lambda) = \beta_0 \textrm{ or } \beta_1=0$$
$$\textrm{Larger (full) Model}: \log(\lambda) = \beta_0 + \beta_1\textrm{age} \textrm{ or } \beta_1 \neq 0 $$

In order to use the drop-in-deviance test, the models being compared must be **nested**; \index{nested models} e.g., all the terms in the smaller model must appear in the larger model. Here the smaller model is the null model with the single term $\beta_0$ and the larger model has $\beta_0$ and $\beta_1$, so the two models are indeed nested. For nested models, we can compare the models' residual deviances to determine whether the larger model provides a significant improvement.  

Here, then, is a summary of these two approaches to hypothesis testing about terms in Poisson regression models:

<p align="center"> __Drop-in-deviance test to compare models__ \index{drop-in-deviance test} </p>
- Compute the deviance for each model, then calculate: drop-in-deviance = residual deviance for reduced model -- residual deviance for the larger model.
- When the reduced model is true, the drop-in-deviance $\sim \chi^2_d$
where d= the difference in the degrees of freedom associated with the two models (that is, the difference in the number of terms/coefficients).
- A large drop-in-deviance favors the larger model.

<p align="center"> __Wald test for a single coefficient__ \index{Wald-type test} </p>
- Wald-type statistic = estimated coefficient / standard error
- When the true coefficient is 0, for sufficiently large $n$, the test statistic $\sim$ N(0,1).
- If the magnitude of the test statistic is large, there is evidence that the true coefficient is not 0.

The drop-in-deviance and the Wald-type tests usually provide consistent results; however, if there is a discrepancy, the drop-in-deviance is preferred.  Not only does the drop-in-deviance test perform better in more cases, but it's also more flexible.  If two models differ by one term, then the drop-in-deviance test essentially tests if a single coefficient is 0 like the Wald test does, while if two models differ by more than one term, the Wald test is no longer appropriate.  

### Using Likelihoods to Fit Models (optional) {#likelihood.sec}

Before continuing with model building, we take a short detour to see how coefficient estimates are determined in a Poisson regression model. The least squares approach requires a linear relationship between the parameter, $\lambda_i$ (the expected or mean response for observation $i$), and $x_i$ (the age for observation $i$). However, it is log$(\lambda_i)$, not $\lambda_i$, that is linearly related to X with the Poisson model. The assumptions of equal variance and normality also do not hold for Poisson regression.  Thus, the method of least squares will not be helpful for inference in Poisson Regression. Instead of least squares, we employ the likelihood \index{likelihood} principle to find estimates of our model coefficients. We look for those coefficient estimates for which the likelihood of our data is maximized; these are the __maximum likelihood estimates__. \index{maximum likelihood estimate (MLE)}

The likelihood for n *independent* \index{independent} observations is the product of the probabilities. For example, if we observe five households with household sizes of 4, 2, 8, 6, and 1 person beyond the head, the likelihood is:

\[ Likelihood = P(Y_1=4)*P(Y_2=2)*P(Y_3=8)*P(Y_4=6)*P(Y_5=1)\]
Recall that the probability of a Poisson response can be written

\[P(Y=y)=\frac{e^{-\lambda}\lambda^y}{y!}\] 
for $y = 0, 1, 2, ...$  So, the likelihood can be written as

\begin{align*}
 Likelihood&= \frac{ e^{-\lambda_1}\lambda_1^4 }{ 4! }*
 \frac{ e^{-\lambda_2}\lambda_2^2 }{ 2! } *\frac{e^{-\lambda_3}\lambda_3^8}{8!}*
 \frac{e^{-\lambda_4}\lambda_4^6}{6!}*\frac{e^{-\lambda_5}\lambda_5^1}{1!}
 \end{align*}
where each $\lambda_i$ can differ for each household depending on a particular $x_i$.  As in Chapter \@ref(ch-beyondmost), it will be easier to find a maximum if we take the log of the likelihood and ignore the constant term resulting from the sum of the factorials:

\begin{align}
 -logL& \propto \lambda_{1}-4log(\lambda_{1})+\lambda_{2}-2log(\lambda_{2}) \nonumber \\
 & +\lambda_{3}-8log(\lambda_{3})+\lambda_{4}-6log(\lambda_{4}) \nonumber \\
 & +\lambda_{5}-log(\lambda_{5})
 (\#eq:poisLoglik)
\end{align}

Now if we had the age of the head of the household for each house ($x_i$), we consider the Poisson regression model:

\[log(\lambda_i)=\beta_0+\beta_1x_i \]

This implies that $\lambda$ differs for each age and can be determined using

\[\lambda_i=e^{\beta_0+\beta_1x_i.}\]

 If the ages are $X=c(32,21,55,44,28)$ years, our loglikelihood can be written:

\begin{align}
 logL & \propto [-e^{\beta_0+\beta_132}+4({\beta_0+\beta_132})]+
[-e^{\beta_0+\beta_121}+2({\beta_0+\beta_121})]+ \nonumber \\ 
&  [-e^{\beta_0+\beta_155}+8({\beta_0+\beta_155})]+
[-e^{\beta_0+\beta_144}+6({\beta_0+\beta_144})]+ \nonumber \\
  &  [-e^{\beta_0+\beta_128}+({\beta_0+\beta_128})]
(\#eq:poisLoglik2)
\end{align}

To see this, match the terms in Equation \@ref(eq:poisLoglik) with those in Equation \@ref(eq:poisLoglik2), noting that $\lambda_i$ has been replaced with $e^{\beta_0+\beta_1x_i}$. It is Equation \@ref(eq:poisLoglik2) that will be used to estimate the coefficients $\beta_0$ and $\beta_1$. Although this looks a little more complicated than the loglikelihoods we saw in Chapter \@ref(ch-beyondmost), the fundamental ideas are the same. In theory, we try out different possible values of $\beta_0$ and $\beta_1$ until we find the two for which the loglikelihood is largest.  Most statistical software packages have automated search algorithms to find those values for $\beta_0$ and $\beta_1$ that maximize the loglikelihood.


### Second Order Model

In Section \@ref(sec-Devtocompare), the Wald-type test and drop-in-deviance test both suggest that a linear term in age is useful.  But our exploratory data analysis in Section \@ref(exploreHH) suggests that a quadratic model might be more appropriate.  A quadratic model would allow us to see if there exists an age where the number in the house is, on average, a maximum. The output for a quadratic model appears below.


```r
fHH1 <- fHH1 %>% mutate(age2 = age*age)
modela2 = glm(total ~ age + age2, family = poisson, 
              data = fHH1)
```


```
##                  Estimate    Std. Error    z value                             Pr(>|z|)
## (Intercept) -0.3325296333 0.17883570719  -1.859414 0.0629684664926050408073621156290756
## age          0.0708867627 0.00689044213  10.287694 0.0000000000000000000000008007069044
## age2        -0.0007083289 0.00006405674 -11.057834 0.0000000000000000000000000002008898
```

```
##  Residual deviance =  2200.944  on  1497 df 
##  Dispersion parameter =  1
```

We can assess the importance of the quadratic term in two ways. First, the p-value for the Wald-type statistic for age$^2$ is statistically significant (Z = -11.058, p < 0.001). Another approach is to perform a drop-in-deviance test.


```r
drop_in_dev <- anova(modela, modela2, test = "Chisq")
```


```
  ResidDF ResidDev Deviance Df                                    pval
1    1498 2337.089       NA NA                                      NA
2    1497 2200.944 136.1454  1 0.0000000000000000000000000000001854452
```

$H_0$: log($\lambda$)=$\beta_0+\beta_1 \textrm{age}$ (reduced model)

$H_A:$ log($\lambda$)=$\beta_0+\beta_1 \textrm{age} + \beta_2 \textrm{age}^2$ (larger model)

The first order model has a residual deviance of 2337.1 with 1498 df and the second order model, the quadratic model, has a residual deviance of 2200.9 with 1497 df. The drop-in-deviance by adding the quadratic term to the linear model is 2337.1 - 2200.9 = 136.2 which can be compared to a $\chi^2$ distribution with one degree of freedom. The p-value is essentially 0, so the observed drop of 136.2 again provides significant support for including the quadratic term.  

We now have an equation in age which yields the estimated log(mean number in the house). 

\[\textrm{log(mean numHouse)} =  -0.333 + 0.071 \textrm{age} - 0.00071 \textrm{age}^2\]



As shown in the following, with calculus we can determine that the maximum estimated additional number in the house is $e^{1.441} = 4.225$ when the head of the household is 50.04 years old.

\begin{align*}
\textrm{log(total)} & = -0.333 + 0.071\textrm{age} - 0.00071 \textrm{age}^2 \\
\frac{d}{d\textrm{age}}\textrm{log(total)} & = 0 + 0.071 - 0.0014 \textrm{age} = 0 \\
\textrm{age} & = 50.04 \\
\textrm{max[log(total)]} & = -0.333 + 0.071 \times 50.04 - 0.00071 \times (50.04)^2 = 1.441
\end{align*}


### Adding a Covariate

We should consider other covariates that may be related to household size. By controlling for important covariates, we can obtain more precise estimates of the relationship between age and household size. In addition, we may discover that the relationship between age and household size may differ by levels of a covariate. One important covariate to consider is location. As described earlier in the case study, there are 5 different regions that are associated with the `Location` variable: Central Luzon, Metro Manila, Visayas, Davao Region, and Ilocos Region. Assessing the utility of including the covariate `Location` is, in essence, comparing two nested models; here the quadratic model is compared to the quadratic model plus terms for `Location`. Results from the fitted model appears below; note that Central Luzon is the reference region that all other regions are compared to.


```r
modela2L = glm(total ~ age + age2 + location, 
               family = poisson, data = fHH1)
```


```
##                           Estimate    Std. Error     z value                             Pr(>|z|)
## (Intercept)          -0.3843337714 0.18209191495  -2.1106581 0.0348017117094914785191406281228410
## age                   0.0703628330 0.00690506688  10.1900292 0.0000000000000000000000021969828730
## age2                 -0.0007025856 0.00006420019 -10.9436677 0.0000000000000000000000000007125764
## locationDavaoRegion  -0.0193872310 0.05378272731  -0.3604732 0.7184933056101299175821850440115668
## locationIlocosRegion  0.0609819668 0.05265981030   1.1580362 0.2468492624893811049346936670190189
## locationMetroManila   0.0544800704 0.04720116232   1.1542104 0.2484139376984029756734173588483827
## locationVisayas       0.1121091959 0.04174960194   2.6852758 0.0072469976712165855986524931608983
```

```
##  Residual deviance =  2187.8  on  1493 df 
##  Dispersion parameter =  1
```



Our Poisson regression model now looks like:

\begin{align*}
\textrm{log(total)} & = -0.384 + 0.070 \cdot \textrm{age} - 0.00070 \cdot \textrm{age}^2 +0.061 \cdot \textrm{IlocosRegion} + \\ 
 & 0.054 \cdot\textrm{MetroManila}  +0.112 \cdot\textrm{Visayas} - 0.019 \cdot \textrm{DavaoRegion}
\end{align*}
Notice that because there are 5 different locations, we must represent the effects of different locations through 4 indicator variables.  For example, $\hat{\beta}_6=-0.0194$ indicates that, after controlling for the age of the head of household, the log mean household size is 0.0194 lower for households in the Davao Region than for households in the reference location of Central Luzon. In more interpretable terms, mean household size is $e^{-0.0194}=0.98$ times "higher" (i.e., 2\% lower) in the Davao Region than in Central Luzon, when holding age constant.  


```r
drop_in_dev <- anova(modela2, modela2L, test = "Chisq")
```


```
  ResidDF ResidDev Deviance Df       pval
1    1497 2200.944       NA NA         NA
2    1493 2187.800 13.14369  4 0.01059463
```

To test if the mean household size significantly differs by location, we must use a drop-in-deviance test, rather than a Wald-type test, because four terms (instead of just one) are added when including the `location` variable. From the Analysis of Deviance table above, adding the four terms corresponding to location to the quadratic model with age produces a statistically significant improvement $(\chi^2=13.144, df = 4, p=0.0106)$, so there is significant evidence that mean household size differs by location, after controlling for age of the head of household.  Further modeling (not shown) shows that after controlling for location and age of the head of household, mean household size did not differ between the two types of roofing material.



### Residuals for Poisson Models (optional) {#sec-PoisResid}

Residual plots may provide some insight into Poisson regression models, especially linearity and outliers, although the plots are not quite as useful here as they are for linear least squares regression. There are a few options for computing residuals and predicted values. Residuals may have the form of residuals for LLSR models or the form of deviance residuals which, when squared, sum to the total deviance for the model. Predicted values can be estimates of the counts, $e^{\beta_0+\beta_1X}$, or log counts, $\beta_0+\beta_1X$. We will typically use the deviance residuals and predicted counts. 

The residuals for linear least squares regression have the form:

 \begin{align}
 \textrm{LLSR residual}_i  &= \textrm{obs}_i - \textrm{fit}_i \nonumber \\
&={Y_i-\hat{\mu}_i} \nonumber \\
 &= Y_i-(\hat{\beta}_0 +\hat{\beta}_1 X_i)
(\#eq:OLSresid)
 \end{align}
Residual sum of squares (RSS) are formed by squaring and adding these residuals, and we generally seek to minimize RSS in model building. We have several options for creating residuals for Poisson regression models. One is to create residuals in much the same way as we do in LLSR. For Poisson residuals, the predicted values are denoted by $\hat{\lambda}_i$ (in place of $\hat{\mu}_i$ in Equation \@ref(eq:OLSresid)); they are then standardized by dividing by the standard error, $\sqrt{\hat{\lambda}_i}$.  These kinds of residuals are referred to as __Pearson residuals__. \index{Pearson residuals} 

\begin{equation*}
\textrm{Pearson residual}_i = \frac{Y_i-\hat{\lambda}_i}{\sqrt{\hat{\lambda}_i}}
\end{equation*}

Pearson residuals have the advantage that you are probably familiar with their meaning and the kinds of values you would expect. For example, after standardizing we expect most Pearson residuals to fall between -2 and 2.  However, __deviance residuals__ \index{deviance residuals} have some useful properties that make them a better choice for Poisson regression.  

First, we define a __deviance residual__ for an observation from a Poisson regression:

\begin{equation*}
\textrm{deviance residual}_i = \textrm{sign}(Y_i-\hat{\lambda}_i)
\sqrt{
2 \left[Y_i log\left(\frac{Y_i}{\hat{\lambda}_i}\right)
-(Y_i - \hat{\lambda}_i) \right]}
\end{equation*}
where $\textrm{sign}(x)$ is defined such that:

\[ \textrm{sign}(x) = \begin{cases} 1  & \textrm{if }\ x > 0 \\
                                    -1 & \textrm{if }\ x < 0  \\
                                    0  & \textrm{if }\ x = 0\end{cases}\]

As its name implies, a deviance residual describes how the observed data deviates from the fitted model. Squaring and summing the deviances for all observations produces the __residual deviance__ $=\sum (\textrm{deviance residual})^2_i$. \index{residual deviance} Relatively speaking, observations for good fitting models will have small deviances; that is, the predicted values will deviate little from the observed. However, you can see that the deviance for an observation does not easily translate to a difference in observed and predicted responses as is the case with LLSR models.

A careful inspection of the deviance formula reveals several places where the deviance compares $Y$ to $\hat{\lambda}$: the sign of the deviance is based on the difference between $Y$ and $\hat{\lambda}$, and under the radical sign we see the ratio $Y/\hat{\lambda}$ and the difference $Y -\hat{\lambda}$.  When $Y = \hat{\lambda}$, that is, when the model fits perfectly, the difference will be 0 and the ratio will be 1 (so that its log will be 0). So like the residuals in LLSR, an observation that fits perfectly will not contribute to the sum of the squared deviances. This definition of a deviance depends on the likelihood for Poisson models. Other models will have different forms for the deviance depending on their likelihood.

<div class="figure" style="text-align: center">
<img src="04-Poisson-Regression_files/figure-epub3/resid1-1.png" alt="Residual plot for the Poisson model of household size by age of the household head." width="60%" />
<p class="caption">(\#fig:resid1)Residual plot for the Poisson model of household size by age of the household head.</p>
</div>

A plot (Figure \@ref(fig:resid1)) of the deviance residuals versus predicted responses for the first order model exhibits curvature, supporting the idea that the model may improved by adding a quadratic term. Other details related to residual plots can be found in a variety of sources including @McCullagh1989.

### Goodness-of-Fit {#sec-PoisGOF}

The model residual deviance can be used to assess the degree to which the predicted values differ from the observed. When a model is true, we can expect the residual deviance to be distributed as a $\chi^2$ random variable with degrees of freedom equal to the model's residual degrees of freedom. Our model thus far, the quadratic terms for age plus the indicators for location, has a residual deviance of 2187.8 with 1493 df. The probability of observing a deviance this large if the model fits is esentially 0, saying that there is significant evidence of lack-of-fit. 


```r
1-pchisq(modela2$deviance, modela2$df.residual)  # GOF test
```

```
[1] 0
```

There are several reasons why **lack-of-fit** \index{lack-of-fit} may be observed. (1) We may be missing important covariates or interactions; a more comprehensive data set may be needed. (2) There may be extreme observations that may cause the deviance to be larger than expected; however, our residual plots did not reveal any unusual points. (3) Lastly, there may be a problem with the Poisson model.  In particular, the Poisson model has only a single parameter, $\lambda$, for each combination of the levels of the predictors which must describe both the mean and the variance.  This limitation can become manifest when the variance appears to be larger than the corresponding means.  In that case, the response is more variable than the Poisson model would imply, and the response is considered to be **overdispersed**. \index{overdispersion} 


## Linear Least Squares \index{linear least squares regression (LLSR)} vs. Poisson Regression \index{Poisson regression}

\begin{gather*}
\underline{\textrm{Response}} \\
\mathbf{LLSR:}\textrm{ Normal} \\
\mathbf{Poisson Regression:}\textrm{ Counts} \\
\textrm{ } \\
\underline{\textrm{Variance}} \\
\mathbf{LLSR:}\textrm{ Equal for each level of X} \\
\mathbf{Poisson Regression:}\textrm{ Equal to the mean for each level of X} \\
\textrm{ } \\
\underline{\textrm{Model Fitting}} \\
\mathbf{LLSR:}\ \mu=\beta_0+\beta_1x \textrm{ using Least Squares}\\
\mathbf{Poisson Regression:}\ log(\lambda)=\beta_0+\beta_1x \textrm{ using Maximum Likelihood}\\
\end{gather*}

\begin{gather*}
\underline{\textrm{EDA}} \\
\mathbf{LLSR:}\textrm{ Plot X vs. Y; add line} \\
\mathbf{Poisson Regression:}\textrm{ Find }log(\bar{y})\textrm{ for several subgroups; plot vs. X} \\
\textrm{ } \\
\underline{\textrm{Comparing Models}} \\
\mathbf{LLSR:}\textrm{ Extra sum of squares F-tests; AIC/BIC} \\
\mathbf{Poisson Regression:}\textrm{ Drop in Deviance tests; AIC/BIC} \\
\textrm{ } \\
\underline{\textrm{Interpreting Coefficients}} \\
\mathbf{LLSR:}\ \beta_1=\textrm{ change in }\mu_y\textrm{ for unit change in X} \\
\mathbf{Poisson Regression:}\ e^{\beta_1}=\textrm{ percent change in }\lambda\textrm{ for unit change in X} 
\end{gather*}


## Case Study: Campus Crime

Students want to feel safe and secure when attending a college or university. In response to legislation, the US Department of Education seeks to provide data and reassurances to students and parents alike. All postsecondary institutions that participate in federal student aid programs are required by the Jeanne Clery Disclosure of Campus Security Policy and Campus Crime Statistics Act and the Higher Education Opportunity Act to collect and report data on crime occurring on campus to the Department of Education. In turn, this data is publicly available on the website of the Office of Postsecondary Education. We are interested in looking at whether there are regional differences in violent crime on campus, controlling for differences in the type of school.

### Data Organization

Each row of `c_data.csv` contains crime information from a post secondary institution, either a college or university. The variables include:

- `Enrollment` = enrollment at the school
- `type` =  college (C) or university (U)
- `nv` = the number of violent crimes for that institution for the given year
- `nvrate` = number of violent crimes per 1000 students
- `enroll1000` = enrollment at the school, in thousands
- `region` = region of the country (C = Central, MW = Midwest, NE = Northeast, SE = Southeast, SW = Southwest, and W = West)




```
# A tibble: 10 x 6
   Enrollment type     nv nvrate enroll1000 region
        <dbl> <chr> <dbl>  <dbl>      <dbl> <chr> 
 1       5590 U        30 5.37         5.59 SE    
 2        540 C         0 0            0.54 SE    
 3      35747 U        23 0.643       35.7  W     
 4      28176 C         1 0.0355      28.2  W     
 5      10568 U         1 0.0946      10.6  SW    
 6       3127 U         0 0            3.13 SW    
 7      20675 U         7 0.339       20.7  W     
 8      12548 C         0 0           12.5  W     
 9      30063 U        19 0.632       30.1  C     
10       4429 C         4 0.903        4.43 C     
```

### Exploratory Data Analysis

<div class="figure" style="text-align: center">
<img src="04-Poisson-Regression_files/figure-epub3/nviolent-1.png" alt="Histogram of number of violent crimes by institution." width="60%" />
<p class="caption">(\#fig:nviolent)Histogram of number of violent crimes by institution.</p>
</div>

A graph of the number of violent crimes, Figure \@ref(fig:nviolent), reveals the pattern often found with distributions of counts of rare events. Many schools reported no violent crimes or very few crimes. A few schools have a large number of crimes making for a distribution that appears to be far from normal. Therefore, Poisson regression should be used to model our data; Poisson random variables are often used to represent counts (e.g., number of violent crimes) per unit of time or space (e.g., one year).

Let's take a look at two covariates of interest for these schools: type of institution and region. In our data, the majority of institutions are universities (65\% of the 81 schools) and only 35\% are colleges. Interest centers on whether the different regions tend to have different crime rates. Table \@ref(tab:regions) contains the name of each region and each column represents the percentage of schools in that region which are colleges or universities.  The proportion of colleges varies from a low of 20\% in the Southwest (SW) to a high of 50\% in the West (W).

<table>
<caption>(\#tab:regions)Proportion of colleges and universities within region in the campus crime data set.</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> C </th>
   <th style="text-align:right;"> MW </th>
   <th style="text-align:right;"> NE </th>
   <th style="text-align:right;"> SE </th>
   <th style="text-align:right;"> SW </th>
   <th style="text-align:right;"> W </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> C </td>
   <td style="text-align:right;"> 0.294 </td>
   <td style="text-align:right;"> 0.3 </td>
   <td style="text-align:right;"> 0.381 </td>
   <td style="text-align:right;"> 0.4 </td>
   <td style="text-align:right;"> 0.2 </td>
   <td style="text-align:right;"> 0.5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> U </td>
   <td style="text-align:right;"> 0.706 </td>
   <td style="text-align:right;"> 0.7 </td>
   <td style="text-align:right;"> 0.619 </td>
   <td style="text-align:right;"> 0.6 </td>
   <td style="text-align:right;"> 0.8 </td>
   <td style="text-align:right;"> 0.5 </td>
  </tr>
</tbody>
</table>

While a Poisson regression model is a good first choice because the responses are counts per year, it is important to note that the counts are not directly comparable because they come from different size schools.  This issue sometimes is referred to as the need to account for *sampling effort*; in other words, we expect schools with more students to have more reports of violent crime since there are more students who could be affected.  We cannot directly compare the 30 violent crimes from the first school in the data set to no violent crimes for the second school when their enrollments are vastly different: 5,590 for school 1 versus 540 for school 2. We can take the differences in enrollments into account by including an __offset__ in our model, which we will discuss in the next section. For the remainder of the EDA, we examine the violent crime counts in terms of the rate per 1,000 enrolled ($\frac{\textrm{number of violent crimes}}{\textrm{number enrolled}} \cdot 1000$).

Note that there is a noticeable outlier for a Southeastern school (5.4 violent crimes per 1000 students), and there is an observed rate of 0 for the Southwestern colleges which can lead to some computational issues. We therefore combined the SW and SE to form a single category of the South, and we also removed the extreme observation from the data set. 



<table>
<caption>(\#tab:table4ch4)The mean and variance of the violent crime rate by region and type of institution.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> region </th>
   <th style="text-align:left;"> type </th>
   <th style="text-align:right;"> MeanCount </th>
   <th style="text-align:right;"> VarCount </th>
   <th style="text-align:right;"> MeanRate </th>
   <th style="text-align:right;"> VarRate </th>
   <th style="text-align:right;"> n </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> C </td>
   <td style="text-align:left;"> C </td>
   <td style="text-align:right;"> 1.6000000 </td>
   <td style="text-align:right;"> 3.3000000 </td>
   <td style="text-align:right;"> 0.3979518 </td>
   <td style="text-align:right;"> 0.2780913 </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> C </td>
   <td style="text-align:left;"> U </td>
   <td style="text-align:right;"> 4.7500000 </td>
   <td style="text-align:right;"> 30.9318182 </td>
   <td style="text-align:right;"> 0.2219441 </td>
   <td style="text-align:right;"> 0.0349266 </td>
   <td style="text-align:right;"> 12 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MW </td>
   <td style="text-align:left;"> C </td>
   <td style="text-align:right;"> 0.3333333 </td>
   <td style="text-align:right;"> 0.3333333 </td>
   <td style="text-align:right;"> 0.0162633 </td>
   <td style="text-align:right;"> 0.0007935 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MW </td>
   <td style="text-align:left;"> U </td>
   <td style="text-align:right;"> 8.7142857 </td>
   <td style="text-align:right;"> 30.9047619 </td>
   <td style="text-align:right;"> 0.4019003 </td>
   <td style="text-align:right;"> 0.0620748 </td>
   <td style="text-align:right;"> 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NE </td>
   <td style="text-align:left;"> C </td>
   <td style="text-align:right;"> 6.0000000 </td>
   <td style="text-align:right;"> 32.8571429 </td>
   <td style="text-align:right;"> 1.1249885 </td>
   <td style="text-align:right;"> 1.1821000 </td>
   <td style="text-align:right;"> 8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NE </td>
   <td style="text-align:left;"> U </td>
   <td style="text-align:right;"> 5.9230769 </td>
   <td style="text-align:right;"> 79.2435897 </td>
   <td style="text-align:right;"> 0.4359273 </td>
   <td style="text-align:right;"> 0.3850333 </td>
   <td style="text-align:right;"> 13 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> S </td>
   <td style="text-align:left;"> C </td>
   <td style="text-align:right;"> 1.1250000 </td>
   <td style="text-align:right;"> 5.8392857 </td>
   <td style="text-align:right;"> 0.1865996 </td>
   <td style="text-align:right;"> 0.1047178 </td>
   <td style="text-align:right;"> 8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> S </td>
   <td style="text-align:left;"> U </td>
   <td style="text-align:right;"> 8.6250000 </td>
   <td style="text-align:right;"> 68.2500000 </td>
   <td style="text-align:right;"> 0.5713162 </td>
   <td style="text-align:right;"> 0.2778065 </td>
   <td style="text-align:right;"> 16 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> W </td>
   <td style="text-align:left;"> C </td>
   <td style="text-align:right;"> 0.5000000 </td>
   <td style="text-align:right;"> 0.3333333 </td>
   <td style="text-align:right;"> 0.0680164 </td>
   <td style="text-align:right;"> 0.0129074 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> W </td>
   <td style="text-align:left;"> U </td>
   <td style="text-align:right;"> 12.5000000 </td>
   <td style="text-align:right;"> 57.0000000 </td>
   <td style="text-align:right;"> 0.4679478 </td>
   <td style="text-align:right;"> 0.0246670 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
</tbody>
</table>

<div class="figure" style="text-align: center">
<img src="04-Poisson-Regression_files/figure-epub3/boxtyperegion-1.png" alt="Boxplot of violent crime rate by region and type of institution." width="60%" />
<p class="caption">(\#fig:boxtyperegion)Boxplot of violent crime rate by region and type of institution.</p>
</div>

Table \@ref(tab:table4ch4) and Figure \@ref(fig:boxtyperegion) display mean violent crime rates that are generally lower at the colleges within a region (with the exception of the Northeast). In addition, the regional pattern of rates at universities appears to differ from that of the colleges.

### Accounting for Enrollment

Although working with the observed rates (per 1000 students) is useful during the exploratory data analysis, we do not use these rates explicitly in the model. The counts (per year) are the Poisson responses when modeling, so we must take into account the enrollment in a different way. Our approach is to include  a term on the right side of the model called an __offset__, \index{offset} which is the log of the enrollment, in thousands. There is an intuitive heuristic for the form of the offset. If we think of $\lambda$ as the mean number of violent crimes per year, then $\lambda/\textrm{enroll1000}$ represents the number per 1000 students, so that the yearly count is adjusted to be comparable across schools of different sizes. Adjusting the yearly count by enrollment is equivalent to adding $log(\textrm{enroll1000})$ to the right-hand side of the Poisson regression equation---essentially adding a predictor with a fixed coefficient of 1:

\begin{align*} 
log(\frac{\lambda}{\textrm{enroll1000}} )= \beta_0 + \beta_1(\textrm{type}) \nonumber \\
log(\lambda)-log(\textrm{enroll1000}) = \beta_0 + \beta_1(\textrm{type}) \nonumber \\
log(\lambda) = \beta_0 + \beta_1(\textrm{type}) + log(\textrm{enroll1000})
\end{align*}

While this heuristic is helpful, it is important to note that it is *not* $\frac{\lambda}{ \textrm{enroll1000}}$ that we are modeling.  We are still modeling $log(\lambda)$, but we're adding an offset to adjust for differing enrollments, where the offset has the unusual feature that the coefficient is fixed at 1.0. As a result, no estimated coefficient for `enroll1000` or $log(\textrm{enroll1000})$ will appear in the output. As this heuristic illustrates, modeling $log(\lambda)$ and adding an offset is equivalent to modeling rates, and coefficients can be interpreted that way.

## Modeling Assumptions

In Table \@ref(tab:table4ch4), we see that the variances are greatly higher than the mean counts in almost every group.  Thus, we have reason to question the Poisson regression assumption of variability equal to the mean; we will have to return to this issue after some initial modeling.  The fact that the variance of the rate of violent crimes per 1000 students tends to be on the same scale as the mean tells us that adjusting for enrollment may provide some help, although that may not completely solve our issues with excessive variance.   

As far as other model assumptions, linearity with respect to $log(\lambda)$ is difficult to discern without continuous predictors, and it is not possible to assess independence without knowing how the schools were selected.

## Initial Models

We are interested primarily in differences in violent crime between institutional types controlling for difference in regions, so we fit a model with region, institutional type, and our offset. Note that the central region is the reference level in our model.


```r
modeltr <- glm(nv ~ type + region, family = poisson,
               offset = log(enroll1000), data = c.data)
```


```
##                Estimate Std. Error    z value                    Pr(>|z|)
## (Intercept) -1.54779781  0.1711432 -9.0438770 0.0000000000000000001512114
## typeU        0.27955741  0.1331438  2.0996654 0.0357582836729686479038115
## regionMW     0.09911529  0.1775221  0.5583264 0.5766215224391659788238940
## regionNE     0.77812815  0.1530654  5.0836313 0.0000003702864084551364871
## regionS      0.58238370  0.1489555  3.9097841 0.0000923786341589608468164
## regionW      0.26275357  0.1875285  1.4011390 0.1611724971574762244053147
```

```
##  Residual deviance =  348.6844  on  74 df 
##  Dispersion parameter =  1
```

From our model, the Northeast and the South differ significantly from the Central region (p= 0.00000037 and p=0.0000924, respectively). The estimated coefficient of 0.778 means that the violent crime rate per 1,000 in the Northeast is nearly 2.2 ($e^{0.778}$) times that of the Central region controlling for the type of school. A Wald-type confidence interval for this factor can be constructed by first calculating a CI for the coefficient (0.778 $\pm$ $1.96 \cdot 0.153$) and then exponentiating (1.61 to 2.94). 

### Tukey's Honestly Significant Differences

Comparisons to regions other than the Central region can be accomplished by changing the reference region.  If many comparisons are made, it would be best to adjust for multiple comparisons using a method such as **Tukey's Honestly Significant Differences**, \index{Tukey's honestly significant differences} which considers all pairwise comparisons among regions.  This method helps control the large number of false positives that we would see if we ran multiple t-tests comparing groups. The honestly significant difference compares a standardized mean difference between two groups to a critical value from a studentized range distribution.


```r
mult_comp <- summary(glht(modeltr, mcp(region="Tukey")))
```


```
## # A tibble: 10 x 5
##    comparison estimate    SE z_value    p_value
##    <chr>         <dbl> <dbl>   <dbl>      <dbl>
##  1 MW - C       0.0991 0.178   0.558 0.980     
##  2 NE - C       0.778  0.153   5.08  0.00000542
##  3 S - C        0.582  0.149   3.91  0.000859  
##  4 W - C        0.263  0.188   1.40  0.621     
##  5 NE - MW      0.679  0.155   4.37  0.000103  
##  6 S - MW       0.483  0.151   3.19  0.0120    
##  7 W - MW       0.164  0.189   0.864 0.908     
##  8 S - NE      -0.196  0.122  -1.61  0.486     
##  9 W - NE      -0.515  0.166  -3.11  0.0155    
## 10 W - S       -0.320  0.163  -1.96  0.279
```

In our case, Tukey's Honestly Significant Differences simultaneously evaluates all 10 mean differences between pairs of regions.  We find that the Northeast has significantly higher rates of violent crimes than the Central, Midwest, and Western regions, while the South has significantly higher rates of violent crimes than the Central and the Midwest, controlling for the type of institution. In the primary model, the University indicator is significant and, after exponentiating the coefficient, can be interpreted as an approximately ($e^{0.280}$) 32\% increase in violent crime rate over colleges after controlling for region.  

These results certainly suggest significant differences in regions and type of institution. However, the EDA  findings suggest the effect of the type of institution may vary depending upon the region, so we consider a model with an interaction between region and type.


```r
modeli <- glm(nv ~ type + region + region:type, 
              family = poisson,
              offset = log(enroll1000), data = c.data)
```


```
##                  Estimate Std. Error    z value      Pr(>|z|)
## (Intercept)    -1.4741049  0.3535533 -4.1693997 0.00003054030
## typeU           0.1959429  0.3775497  0.5189858 0.60377066217
## regionMW       -1.9765174  1.0606601 -1.8634785 0.06239497250
## regionNE        1.5528776  0.3818812  4.0663892 0.00004774717
## regionS        -0.1562461  0.4859126 -0.3215520 0.74779214307
## regionW        -1.8336605  0.7905694 -2.3194177 0.02037240065
## typeU:regionMW  2.1964565  1.0765395  2.0402936 0.04132109554
## typeU:regionNE -1.0697510  0.4199573 -2.5472854 0.01085646141
## typeU:regionS   0.8121189  0.5107849  1.5899429 0.11184767247
## typeU:regionW   2.4106278  0.8139679  2.9615759 0.00306069005
```

```
##  Residual deviance =  276.7038  on  70 df 
##  Dispersion parameter =  1
```

These results provide convincing evidence of an interaction between the effect of region and the type of institution. A drop-in-deviance test like the one we carried out in the previous case study confirms the significance of the contribution of the interaction to this model.  We have statistically significant evidence ($\chi^2=71.98, df=4, p<.001$) that the difference between colleges and universities in violent crime rate differs by region.  For example, our model estimates that violent crime rates are 13.6 ($e^{.196+2.411}$) times higher in universities in the West compared to colleges, while in the Northeast we estimate that violent crime rates are 2.4 ($\frac{1}{e^{.196-1.070}}$) times higher in colleges.  


```r
drop_in_dev <- anova(modeltr, modeli, test = "Chisq")
```


```
  ResidDF ResidDev Deviance Df                   pval
1      74 348.6844       NA NA                     NA
2      70 276.7038 71.98061  4 0.00000000000000866357
```

The residual deviance (276.70 with 70 df) suggests significant lack-of-fit in the interaction model (p < .001). One possibility is that there are other important covariates that could be used to describe the differences in the violent crime rates. Without additional covariates to consider, we look for extreme observations, but we have already eliminated the most extreme of the observations. 

In the absence of other covariates or extreme observations, we consider overdispersion as a possible explanation of the significant lack-of-fit. 

## Overdispersion {#sec-overdispPois}

### Dispersion Parameter Adjustment

**Overdispersion** \index{overdispersion} suggests that there is more variation in the response than the model implies. Under a Poisson model, we would expect the means and variances of the response to be about the same in various groups. Without adjusting for overdispersion, we use incorrect, artificially small standard errors leading to artificially small p-values for model coefficients.  We may also end up with artificially complex models.

We can take overdispersion into account in several different ways. The simplest is to use an estimated dispersion factor to inflate standard errors. Another way is to use a negative-binomial regression model.  We begin with using an estimate of the dispersion parameter. 
 
We can estimate a dispersion parameter, $\phi$, by dividing the model deviance by its corresponding degrees of freedom; i.e., $\hat\phi=\frac{\sum(\textrm{Pearson residuals})^2}{n-p}$ where $p$ is the number of model parameters. It follows from what we know about the $\chi^2$ distribution that if there is no overdispersion, this estimate should be close to one. It will be larger than one in the presence of overdispersion. We inflate the standard errors by multiplying the variance by $\phi$, so that the standard errors are larger than the likelihood approach would imply; i.e., $SE_Q(\hat\beta)=\sqrt{\hat\phi}*SE(\hat\beta)$, where $Q$ stands for "quasi-Poisson" \index{quasi-Poisson} since multiplying variances by $\phi$ is an ad-hoc solution. Our process for model building and comparison is called **quasilikelihood**---similar to likelihood but without exact underlying distributions. \index{quasilikelihood}  If we choose to use a dispersion parameter with our model, we refer to the approach as quasilikelihood. The following output illustrates a quasi-Poisson approach to the interaction model:


```r
modeliq <- glm(nv ~ type + region + region:type, 
               family = quasipoisson,
               offset = log(enroll1000), data = c.data)
```


```
##                  Estimate Std. Error    t value   Pr(>|t|)
## (Intercept)    -1.4741049  0.7455328 -1.9772503 0.05195260
## typeU           0.1959429  0.7961337  0.2461181 0.80631106
## regionMW       -1.9765174  2.2365990 -0.8837156 0.37987490
## regionNE        1.5528776  0.8052675  1.9283997 0.05786179
## regionS        -0.1562461  1.0246369 -0.1524893 0.87923974
## regionW        -1.8336605  1.6670624 -1.0999351 0.27512877
## typeU:regionMW  2.1964565  2.2700835  0.9675664 0.33659214
## typeU:regionNE -1.0697510  0.8855580 -1.2079966 0.23111397
## typeU:regionS   0.8121189  1.0770850  0.7539971 0.45337991
## typeU:regionW   2.4106278  1.7164026  1.4044652 0.16460212
```

```
##  Residual deviance =  276.7038  on  70 df 
##  Dispersion parameter =  4.446556
```

In the absence of overdispersion, we expect the dispersion parameter estimate to be 1.0. The estimated dispersion parameter here is much larger than 1.0 (4.447) indicating overdispersion (extra variance) that should be accounted for. The larger estimated standard errors in the quasi-Poisson model reflect the adjustment. For example, the standard error for the West region term from a likelihood based approach is 0.7906, whereas the quasilikelihood standard error is $\sqrt{4.47}*0.7906$ or 1.6671. This term is no longer significant under the quasi-Poisson model.  In fact, after adjusting for overdispersion (extra variation), none of the model coefficients in the quasi-Poisson model are significant at the .05 level!  This is because standard errors were all increased by a factor of 2.1 ($\sqrt{\hat\phi}=\sqrt{4.447}=2.1$), while estimated coefficients remain unchanged.

Note that tests for individual parameters are now based on the t-distribution rather than a standard normal distribution, with test statistic $t=\frac{\hat\beta}{SE_Q(\hat\beta)}$ following an (approximate) t-distribution with $n-p$ degrees of freedom if the null hypothesis is true ($H_O:\beta=0$).  Drop-in-deviance tests can be similarly adjusted for overdispersion in the quasi-Poisson model.  In this case, you can divide the test statistic (per degree of freedom) by the estimated dispersion parameter and compare the result to an F-distribution with the difference in the model degrees of freedom for the numerator and the degrees of freedom for the larger model in the denominator.  That is, $F=\frac{\textrm{drop in deviance}}{\textrm{difference in df}} / {\hat\phi}$ follows an (approximate) F-distribution when the null hypothesis is true ($H_0$: reduced model sufficient).  The output below tests for an interaction between region and type of institution after adjusting for overdispersion (extra variance):


```r
modeltrq <- glm(nv ~ type + region, family = quasipoisson,
               offset = log(enroll1000), data = c.data)
drop_in_dev <- anova(modeltrq, modeliq, test = "F")
```


```
  ResidDF ResidDev        F Df        pval
1      74 348.6844       NA NA          NA
2      70 276.7038 4.046987  4 0.005213313
```

Here, even after adjusting for overdispersion, we still have statistically significant evidence ($F=4.05, p=.0052$) that the difference between colleges and universities in violent crime rate differs by region.  

### No Dispersion vs. Overdispersion

Table \@ref(tab:compTable) summarizes the comparison between Poisson inference (tests and confidence intervals assuming no overdispersion) and quasi-Poisson inference (tests and confidence intervals after accounting for overdispersion). 



<table>
<caption>(\#tab:compTable)Comparison of Poisson and quasi-Poisson inference.</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:left;"> Poisson </th>
   <th style="text-align:left;"> quasi-Poisson </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Estimate </td>
   <td style="text-align:left;"> $\hat{\beta}$ </td>
   <td style="text-align:left;"> $\hat{\beta}$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Std error </td>
   <td style="text-align:left;"> $SE(\hat{\beta})$ </td>
   <td style="text-align:left;"> $SE_Q(\hat{\beta}) = \sqrt{\hat{\phi}} SE(\hat{\beta})$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Wald-type test stat </td>
   <td style="text-align:left;"> $Z = \hat{\beta} / SE(\hat{\beta})$ </td>
   <td style="text-align:left;"> $t = \hat{\beta} / SE_Q(\hat{\beta})$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Confidence interval </td>
   <td style="text-align:left;"> $\hat{\beta} \pm z^{'} SE(\hat{\beta})$ </td>
   <td style="text-align:left;"> $\hat{\beta} \pm t^{'} SE_Q(\hat{\beta})$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Drop in deviance test </td>
   <td style="text-align:left;"> $\chi^2 = \textrm{resid dev(reduced) - resid dev(full)}$ </td>
   <td style="text-align:left;"> $F = (\chi^2 / \textrm{difference in df}) / \hat{\phi}$ </td>
  </tr>
</tbody>
</table>

### Negative Binomial Modeling

Another approach to dealing with overdispersion is to model the response using a negative binomial instead of a Poisson distribution. An advantage of this approach is that it introduces another parameter in addition to $\lambda$, which gives the model more flexibility and, as opposed to the quasi-Poisson model, the negative binomial model assumes an explicit likelihood model. You may recall that negative binomial random variables take on non-negative integer values, which is consistent with modeling counts. This model posits selecting a $\lambda$ for each institution and then generating a count using a Poisson random variable with the selected $\lambda$. With this approach, the counts will be more dispersed than would be expected for observations based on a single Poisson variable with rate $\lambda$. (See Guided Exercises on the Gamma-Poisson mixture in Chapter \@ref(ch-distthry).)

Mathematically, you can think of the negative binomial model as a Poisson model where $\lambda$ is also random, following a gamma distribution.  Specifically, if $Y|\lambda \sim \textrm{Poisson}(\lambda)$ and $\lambda \sim \textrm{gamma}(r,\frac{1-p}{p})$, then $Y \sim \textrm{NegBinom}(r,p)$ where $E(Y)=\frac{pr}{1-p}=\mu$ and $Var(Y)=\frac{pr}{(1-p)^2}=\mu+\frac{\mu^2}{r}$.  The overdispersion in this case is given by $\frac{\mu^2}{r}$, which approaches 0 as $r$ increases (so smaller values of $r$ indicate greater overdispersion).

Here is what happens if we apply a negative binomial regression model \index{negative binomial regression} to the interaction model, which we've already established suffers from overdispersion issues under regular Poisson regression:




```r
# Account for overdispersion with negative binomial model
modelinb <- glm.nb(nv ~ type + region + region:type, 
               offset(log(enroll1000)), data = c.data2)
```


```
##                  Estimate Std. Error    z value    Pr(>|z|)
## (Intercept)     0.4903997  0.4281146  1.1454869 0.252007539
## typeU           1.2174403  0.4607695  2.6421897 0.008237192
## regionMW       -1.0952625  0.8075381 -1.3562982 0.175004299
## regionNE        1.3966486  0.5052906  2.7640504 0.005708871
## regionS         0.1460595  0.5559125  0.2627384 0.792752254
## regionW        -1.1857844  0.6870117 -1.7260034 0.084346810
## typeU:regionMW  1.6342405  0.8497990  1.9230907 0.054468660
## typeU:regionNE -1.1259013  0.5600954 -2.0101955 0.044410503
## typeU:regionS   0.4512731  0.5995489  0.7526878 0.451637530
## typeU:regionW   2.0386980  0.7526887  2.7085541 0.006757710
```

```
##  Residual deviance =  199.5711  on  69 df 
##  Dispersion parameter (theta) =  1.312886
```

These results differ from the quasi-Poisson model.  Several effects are now statistically significant at the .05 level: the effect of type of institution for the Central region ($Z=2.64, p=.008$), the difference between Northeast and Central regions for colleges ($Z=2.76, p=.006$), the difference between Northeast and Central regions in type effect ($Z=-2.01, p=.044$), and the difference between West and Central regions in type effect ($Z=2.71, p=.007$).  In this case, compared to the quasi-Poisson model, negative binomial coefficient estimates are generally in the same direction and similar in size, but negative binomial standard errors are somewhat smaller.

In summary, we explored the possibility of differences in the violent crime rate between colleges  and universities, controlling for region. Our initial efforts seemed to suggest that there are indeed differences between colleges and universities, and the pattern of those differences depends upon the region. However, this model exhibited significant lack-of-fit which remained after the removal of an extreme observation. In the absence of additional covariates, we accounted for the lack-of-fit by using a quasilikelihood approach and a negative binomial regression, which provided slightly different conclusions.  We may want to look for additional covariates and/or more data.

## Case Study: Weekend Drinking {#cs:drinking}

Sometimes when analyzing Poisson data, you may see many more zeros in your data set than you would expect for a Poisson random variable. For example, an informal survey of students in an introductory statistics course included the question, "How many alcoholic drinks did you consume last weekend?".  This survey was conducted on a dry campus where no alcohol is officially allowed, even among students of drinking age, so we expect that some portion of the respondents never drink. The non-drinkers would thus always report zero drinks. However, there will also be students who are drinkers reporting zero drinks because they just did not happen to drink during the past weekend. Our zeros, then, are a **mixture** of responses from non-drinkers and drinkers who abstained during the past weekend. Ideally, we'd like to sort out the non-drinkers and drinkers when performing our analysis.



### Research Question

The purpose of this survey is to explore factors related to drinking behavior on a dry campus. What proportion of students on this dry campus never drink? What factors, such as off-campus living and sex, are related to whether students drink? Among those who do drink, to what extent is moving off campus associated with the number of drinks in a weekend? It is commonly assumed that males' alcohol consumption is greater than females'; is this true on this campus? Answering these questions would be a simple matter if we knew who was and was not a drinker in our sample. Unfortunately, the non-drinkers did not identify themselves as such, so we will need to use the data available with a model that allows us to estimate the proportion of drinkers and non-drinkers.

### Data Organization

Each line of `weekendDrinks.csv` contains data provided by a student in an introductory statistics course. In this analysis, the response of interest is the respondent's report of the number of alcoholic `drinks` they consumed the previous weekend, whether the student lives `off.campus`, and `sex`. We will also consider whether a student is likely a `firstYear` student based on the `dorm` they live in.  Here is a sample of observations from this data set:






```r
head(zip.data[2:5])
```

```
  drinks sex off.campus firstYear
1      0   f          0      TRUE
2      5   f          0     FALSE
3     10   m          0     FALSE
4      0   f          0     FALSE
5      0   m          0     FALSE
6      3   f          0     FALSE
```

### Exploratory Data Analysis
As always we take stock of the amount of data; here there are 77 observations. Large sample sizes are preferred for the type of model we will consider, and n=77 is on the small side. We proceed with that in mind. 

A premise of this analysis is that we believe that those responding zero drinks are coming from a mixture of non-drinkers and drinkers who abstained the weekend of the survey.

- __Non-drinkers__: respondents who never drink and would always reply with zero.
- __Drinkers__: obviously this includes those responding with one or more drinks, but it also includes people who are drinkers but did not happen to imbibe the past weekend. These people reply zero but are not considered non-drinkers.

<div class="figure" style="text-align: center">
<img src="04-Poisson-Regression_files/figure-epub3/obsVmodel-1.png" alt="Observed (a) versus modeled (b) number of drinks." width="60%" />
<p class="caption">(\#fig:obsVmodel)Observed (a) versus modeled (b) number of drinks.</p>
</div>

Beginning the EDA with the response, number of drinks, we find that over 46\% of the students reported no drinks during the past weekend. Figure \@ref(fig:obsVmodel)a portrays the observed number of drinks reported by the students. The mean number of drinks reported the past weekend is 2.013. Our sample consists of 74\% females and 26\% males, only 9\% of whom live off campus.

Because our response is a count, it is natural to consider a Poisson regression model. You may recall that a Poisson distribution has only one parameter, $\lambda$, for its mean and variance. Here we will include an additional parameter, $\alpha$. We define $\alpha$ to be the true proportion of *non-drinkers* in the population.

The next step in the EDA is especially helpful if you suspect your data contains excess zeros.  Figure \@ref(fig:obsVmodel)b is what we might expect to see under a Poisson model. Bars represent the probabilities for a Poisson distribution (using the Poisson probability formula) with $\lambda$ equal to the mean observed number of drinks, 2.013 drinks per weekend. Comparing this Poisson distribution to what we observed (Figure \@ref(fig:obsVmodel)a), it is clear that many more zeros have been reported by the students than you would expect to see if the survey observations were coming from a Poisson distribution. This doesn't surprise us because we had expected a subset of the survey respondents to be non-drinkers; i.e., they would not be included in this Poisson process. This circumstance actually arises in many Poisson regression settings. We will define $\lambda$ to be the mean number of drinks *among those who drink*, and $\alpha$ to be the proportion of *non-drinkers* ("true zeros").  Then, we will attempt to model $\lambda$ and $\alpha$ (or functions of $\lambda$ and $\alpha$) simultaneously using covariates like sex, first-year status, and off-campus residence.  This type of model is referred to as a __zero-inflated Poisson model__ or __ZIP model__. \index{zero-inflated Poisson}

### Modeling
We first fit a simple Poisson model with the covariates `off.campus` and `sex`.


```r
pois.m1 <- glm(drinks ~ off.campus + sex, family = poisson,
               data = zip.data)
```


```
##              Estimate Std. Error  z value             Pr(>|z|)
## (Intercept) 0.1292692  0.1241189 1.041494 0.297646216957151599
## off.campus  0.8976455  0.2008250 4.469790 0.000007829626934807
## sexm        1.1153735  0.1610661 6.924943 0.000000000004361498
```

```
##  Residual deviance =  230.544  on  74 df 
##  Dispersion parameter =  1
```


```r
# Exponentiated coefficients
exp(coef(pois.m1))
```

```
## (Intercept)  off.campus        sexm 
##    1.137996    2.453819    3.050707
```

```r
# Goodness-of-fit test
gof.pvalue = 1 - pchisq(pois.m1$deviance, pois.m1$df.residual)
gof.pvalue
```

```
## [1] 0
```

Both covariates are statistically significant, but a goodness-of-fit test reveals that there remains significant lack-of-fit (residual deviance: 230.54 with only 74 df; p<.001 based on $\chi^2$ test with 74 df). In the absence of important missing covariates or extreme observations, this lack-of-fit may be explained by the presence of a group of non-drinkers.

A zero-inflated Poisson regression model to take non-drinkers into account consists of two parts:

- One part  models the association, among drinkers, between number of drinks and the predictors of sex and off-campus residence.
- The other part uses a predictor for first-year status to obtain an estimate of the proportion of non-drinkers based on the reported zeros.

The form for each part of the model follows. The first part looks like an ordinary Poisson regression model:

\[
log(\lambda)=\beta_0+\beta_1\textrm{off.campus}+ \beta_2\textrm{sex}
\]
where $\lambda$ is the mean number of drinks in a weekend *among those who drink*.
The second part has the form

\[
logit(\alpha)=\beta_0+\beta_1\textrm{firstYear}
\]
where $\alpha$ is the probability of being in the non-drinkers group and $logit(\alpha) = log( \alpha/(1-\alpha))$.  We'll provide more detail on the logit in Chapter \@ref(ch-logreg). There are many ways in which to structure this model; here we use different predictors in the two pieces, athough it would have been perfectly fine to use the same predictors for both pieces, or even no predictors for one of the pieces.

### Fitting a ZIP Model

How is it possible to fit such a model? We cannot observe whether a respondent is a drinker or not (which probably would've been good to ask). The ZIP model is a special case of a more general type of statistical model referred to as a __latent variable model__. More specifically, it is a type of a __mixture model__ \index{mixture model} where observations for one or more groups occur together and the group membership is unknown.  Zero-inflated models are a particularly common example of a mixture model, but the response does not need to follow a Poisson distribution.  Likelihood methods are at the core of this methodology, but fitting is an iterative process where it is necessary to start out with some guesses (or starting values).  In general, it is important to know that models like ZIP exist, although we'll only explore interpretations and fitting options for a single case study here.

Here is the general idea of how ZIP models are fit.  Imagine that the graph of the Poisson distribution in Figure \@ref(fig:obsVmodel)b is removed from the observed data distribution in Figure \@ref(fig:obsVmodel)a. Some zero responses will remain. These would correspond to non-drinkers, and the proportion of all observations these zeros constitute might make a reasonable estimate for $\alpha$, the proportion of non-drinkers. The likelihood is used and some iterating in the fitting process is involved  because the Poisson distribution in Figure \@ref(fig:obsVmodel)b is based on the mean of the observed data, which means it is the average among all students, not only among drinkers. Furthermore, the likelihood incorporates the predictors, `sex` and `off.campus`. So there is a little more to it than computing the proportion of zeros, but this heuristic should provide you a general idea of how these kinds of models are fit. We will use the R function `zeroinfl` from the package `pscl` to fit a ZIP model. 


```r
zip.m2 <- zeroinfl(drinks ~ off.campus + sex | firstYear, 
                   data = zip.data)
```


```
## $count
##              Estimate Std. Error  z value          Pr(>|z|)
## (Intercept) 0.7542752  0.1440035 5.237894 0.000000162418898
## off.campus  0.4159402  0.2058603 2.020498 0.043331786196900
## sexm        1.0208997  0.1751938 5.827261 0.000000005634448
## 
## $zero
##                 Estimate Std. Error   z value   Pr(>|z|)
## (Intercept)   -0.6036153  0.3114485 -1.938090 0.05261222
## firstYearTRUE  1.1363880  0.6095155  1.864412 0.06226386
```

```
##  Log likelihood =  -140.764
```


```r
exp(coef(zip.m2))   # exponentiated coefficients
```

```
##  count_(Intercept)   count_off.campus         count_sexm   zero_(Intercept) zero_firstYearTRUE 
##          2.1260699          1.5157953          2.7756910          0.5468311          3.1154950
```

Our model uses `firstYear` to distinguish drinkers and non-drinkers ("Zero-inflation model coefficients") and `off.campus` and `sex` to help explain the differences in the number of drinks among drinkers ("Count model coefficients").  Again, we could have used the same covariates for the two pieces of a ZIP model, but neither `off.campus` nor `sex` proved to be a useful predictor of drinkers vs. non-drinkers after we accounted for first-year status.  

We'll first consider the "Count model coefficients," which provide information on how the sex and off-campus status of a student who is a drinker are related to the number of drinks reported by that student over a weekend. As we have done with previous Poisson regression models, we exponentiate each coefficient for ease of interpretation. Thus, for those who drink, the average number of drinks for males is $e^{1.0209}$ or 2.76 times the number for females (Z = 5.827, p < 0.001) given that you are comparing people who live in comparable settings, i.e., either both on or both off campus. Among drinkers, the mean number of drinks for students living off campus is $e^{0.4159}=1.52$ times that of students living on campus for those of the same sex (Z = 2.021, p = 0.0433).

The "Zero-inflation model coefficients" refer to separating drinkers from non-drinkers. An important consideration in separating drinkers from non-drinkers may be whether this is their first year, where `firstYear` is a 0/1 indicator variable. 
  
We have
\[ 
log(\alpha/(1-\alpha)) =-0.6036+1.1364\textrm{firstYear}
 \]

However, we are interested in $\alpha$, the proportion of non-drinkers. Exponentiating the coefficient for the first-year term for this model yields 3.12. Here it is interpreted as the odds ($\frac{\alpha}{1-\alpha}$) that a first-year student is a non-drinker is 3.12 times the odds that an upper-class student is a non-drinker. Furthermore, with a little algebra (solving the equation with $log(\alpha/(1-\alpha)$) for $\alpha$),
 we have
 
 \[
 \hat{\alpha} =
 \frac{e^ {-0.6036+1.1364(\textrm{firstYear})}}
 {1+e^{
 -0.6036+1.1364(\textrm{firstYear})
 }
 }.
 \]
 
The estimated chance that a first-year student is a non-drinker is

\[
\frac{e^{0.533}}{1+e^{0.533}} = 0.630
\]
or 63.0\%, while for non-first-year students, the estimated probability of being a non-drinker is 0.354.  If you have seen logistic regression, you'll recognize that this transformation is what is used to estimate a probability. More on this in Chapter \@ref(ch-logreg).

### The Vuong Test (optional)

Moving from ordinary Poisson to zero-inflated Poisson has helped us address additional research questions: What proportion of students are non-drinkers, and what factors are associated with whether or not a student is a non-drinker?  While a ZIP model seems more faithful to the nature and structure of this data, can we quantitatively show that a zero-inflated Poisson is better than an ordinary Poisson model?

We cannot use the drop-in-deviance test we discussed earlier because these two models are not nested within one another. Vuong [-@Vuong1989] devised a test to make this comparison for the special case of comparing a zero-inflated model and ordinary regression model. Essentially, the Vuong Test \index{Vuong test} is able to compare predicted probabilities of __non-nested__ models.


```r
vuong(pois.m1, zip.m2)
```

```
Vuong Non-Nested Hypothesis Test-Statistic: 
(test-statistic is asymptotically distributed N(0,1) under the
 null that the models are indistinguishible)
-------------------------------------------------------------
              Vuong z-statistic             H_A   p-value
Raw                   -2.688692 model2 > model1 0.0035866
AIC-corrected         -2.534095 model2 > model1 0.0056369
BIC-corrected         -2.352922 model2 > model1 0.0093133
```

Here, we have structured the Vuong Test to compare Model 1: Ordinary Poisson Model to Model 2: Zero-inflation Model. If the two models do not differ, the test statistic for Vuong would be asymptotically standard Normal and the p-value would be relatively large. Here the first line of the output table indicates that the zero-inflation model is better ($Z=-2.69,p=.0036$). Note that the test depends upon sufficiently large n for the Normal approximation, so since our sample size (n=77) is somewhat small, we need to interpret this result with caution. More research is underway to address statistical issues related to these comparisons.

### Residual Plot
Fitted values ($\hat{y}$) and residuals ($y-\hat{y}$) can be computed for zero-inflation models and plotted. Figure \@ref(fig:poisRes) reveals that one observation appears to be extreme (Y=22 drinks during the past weekend). Is this a legitimate observation or was there a transcribing error? Without the original respondents, we cannot settle this question. It might be worthwhile to get a sense of how influential this extreme observation is by removing Y=22 and refitting the model.

<div class="figure" style="text-align: center">
<img src="04-Poisson-Regression_files/figure-epub3/poisRes-1.png" alt="Residuals by fitted counts for ZIP model." width="60%" />
<p class="caption">(\#fig:poisRes)Residuals by fitted counts for ZIP model.</p>
</div>

### Limitations

Given that you have progressed this far in your statistical education, the weekend drinking survey question should raise some red flags. What time period constitutes the "weekend"?  Will some students be thinking of only Saturday night, while others include Friday night or possibly Sunday evening? What constitutes a drink---a bottle of beer? How many drinks will a respondent report for a bottle of wine? Precise definitions would vastly improve the quality of this data.  There is also an issue related to confidentiality.  If the data is collected in class, will the teacher be able to identify the respondent? Will respondents worry that a particular response will affect their grade in the class or lead to repercussions on a dry campus?

In addition to these concerns, there are a number of other limitations that should be noted. Following the concern of whether this data represents a random sample of any population (it doesn't), we also must be concerned with the size of this data set. ZIP models are not appropriate for small samples and this data set is not impressively large.

At times, a mixture of zeros occurs naturally. It may not come about because of neglecting to ask a critical question on a survey, but the information about the subpopulation may simply not be ascertainable. For example, visitors from a state park were asked as they departed how many fish they caught, but those who report 0 could be either non-fishers or fishers who had bad luck. These kinds of circumstances occur often enough that ZIP models are becoming increasingly common.

Actually, applications which extend beyond ordinary Poisson regression applications---ZIPs and other Poisson modeling approaches such as hurdle models and quasi-Poisson applications---are becoming increasingly common. So it is worth taking a look at these variations of Poisson regression models. Here we have only skimmed the surface of zero-inflated models, but we want you to be aware of models of this type. ZIP models demonstrate that modeling can be flexible and creative---a theme we hope you will see throughout this book.

## Exercises

### Conceptual Exercises {#exer:concept}

Exercises 1-4 involve predicting a __response__ using one or more __explanatory variables__, where these examples have response variables that are counts per some unit of time or space. List the response (both what is being counted and over what unit of time or space) and relevant explanatory variables.  

1. Are the number of motorcycle deaths in a given year related to a state's helmet laws?
2. Does the number of employers conducting on-campus interviews during a year differ for public and private colleges? 
3. Does the daily number of asthma-related visits to an Emergency Room differ depending on air pollution indices?
4. Has the number of deformed fish in randomly selected Minnesota lakes been affected by changes in trace minerals in the water over the last decade? 
\vspace{3mm}
5. Models of the form $Y_i=\beta_0+\beta_1X_i+\epsilon_i, \epsilon_i \sim iidN(0,\sigma)$ are fit using the method of least squares. What method is used to fit Poisson regression models?
6. What should be done before adjusting for overdispersion?
7. Why are quasi-Poisson models used, and how do the results typically compare for corresponding models using regular Poisson regression?
8. Why is the log of mean counts, log($\bar{Y}$), not $\bar{Y}$, plotted against X when assessing the assumptions for Poisson regression? 
9. How can the assumption of *mean=variance* be checked for Poisson regression? What if there are not many repeated observations at each level of X?
10. Is it possible that a predictor is significant for a model fit using Poisson regression, but not for a model for the same data fit using quasi-Poisson regression? Explain.

Complete (a)-(d) in the context of the study for Exercises 11-13. 
    
a. Define the response. 
b. What are the possible values for the response?
c. What does $\lambda$ represent? 
d. Would a zero-inflated model be considered here? If so, what would be a "true zero"?

11. __Fish (or, as they say in French, poisson).__ A state wildlife biologist collected data from 250 park visitors as they left at the end of their stay. Each was asked to report the number of fish they caught during their one-week stay. On average, visitors caught 21.5 fish per week.

12. __Methadone program recidivism.__ Program facilitators keep track of the number of times their program's patients relapse within five years of initial treatment. Data on 100 patients yielded a mean number of 2.8 relapses per patient within the five years of initial treatment.

13. __Clutch size.__ Thirty nests were located and the number of eggs in each nest were counted at the start of a season. Later in the season following a particularly destructive storm, the mean clutch size of the 30 nests was only 1.7 eggs per nest.
\vspace{3mm}

14. __Credit card use.__ A survey of 1,000 consumers asked respondents how many credit cards they use. Interest centers on the relationship between credit card use and income, in \$10,000. The estimated coefficient for income is 2.1.

    - Identify the predictor and interpret the estimated coefficient for the predictor in this context.
    - Describe how the assumption of linearity can be assessed in this example.
    - Suggest a way in which to assess the equal mean and variance assumption.
    
<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:tab2chp4)Sample data for Exercise 15.</caption>
 <thead>
  <tr>
   <th style="text-align:right;"> Age </th>
   <th style="text-align:right;"> Time Online </th>
   <th style="text-align:right;"> Number of Dates Arranged Online </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 38 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 55 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
</tbody>
</table>

15. __Dating online.__ Researchers are interested in the number of dates respondents arranged online and whether the rates differ by age group. Questions which elicit responses similar to this can be found in the Pew Survey concerning dating online and relationships [@Duggan2013]. Each survey respondent was asked how many dates they have arranged online in the past 3 months as well as the typical amount of time, $t$, in hours, they spend online weekly. Some rows of data appear in Table \@ref(tab:tab2chp4).
	
    - Identify the response, predictor, and offset in this context.  Does using an offset make sense?
    - Write out a model for this data. As part of your model description, define the parameter, $\lambda$.
    - Consider a zero-inflated Poisson model for this data. Describe what the `true zeros' would be in this setting.
    
<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:tab3chp4)Data from Scotto et al. (1974) on the number of cases of non-melanoma skin cancer for women by age group in two metropolitan areas (Minneapolis-St. Paul and Dallas-Ft. Worth); the year is unknown.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Number of Cases </th>
   <th style="text-align:left;"> Population </th>
   <th style="text-align:left;"> Age Group </th>
   <th style="text-align:left;"> City </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 172675 </td>
   <td style="text-align:left;"> 15-24 </td>
   <td style="text-align:left;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 123065 </td>
   <td style="text-align:left;"> 25-34 </td>
   <td style="text-align:left;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ... </td>
   <td style="text-align:left;"> ... </td>
   <td style="text-align:left;"> ... </td>
   <td style="text-align:left;"> ... </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 226 </td>
   <td style="text-align:left;"> 29007 </td>
   <td style="text-align:left;"> 75-84 </td>
   <td style="text-align:left;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 65 </td>
   <td style="text-align:left;"> 7538 </td>
   <td style="text-align:left;"> 85+ </td>
   <td style="text-align:left;"> 2 </td>
  </tr>
</tbody>
<tfoot>
<tr>
<td style = 'padding: 0; border:0;' colspan='100%'><sup></sup> The columns contain: number of cases, population size, age group, and city (1=Minneapolis-St. Paul, 2=Dallas-Ft. Worth).</td>
</tr>
</tfoot>
</table>

16. __Poisson approximation: rare events.__ For rare diseases, the probability of a case occurring, $p$, in a very large population, $n$, is small. With a small $p$ and large $n$, the random variable $Y$= the number of cases out of $n$ people can be approximated using a Poisson random variable with $\lambda = np$. If the count of those with the disease is observed in several different populations independently of one another, the $Y_i$ represents the number of cases in the $i^{th}$ population and can be approximated using a Poisson random variable with $\lambda_i=n_ip_i$ where $p_i$ is the probability of a case for the $i^{th}$ population. Poisson regression can take into account the differences in the population sizes, $n_i$, using as an offset log($n_i$) as well as differences in a population characteristic like $x_i$. The coefficient of the offset is set at one; it is not estimated like the other coefficients.  Thus the model statement has the form: $log(\lambda_i) = \beta_0+\beta_1x_i + log(n_i)$, where $Y_i  \sim$ Poisson($\lambda_i = n_i p_i$). Note that $\lambda_i$ depends on $x_i$ which may differ for the different populations.

    @Scotto1974 wondered if skin cancer rates by age group differ by city.  Based on their data in Table \@ref(tab:tab3chp4), identify and describe the following quantities which appear in the description of the Poisson approximation for rare events:

      - A case,
      - The population size, $n_i$,
      - Probability, $p_i$,
      - Poisson parameter, $\lambda_i$,
      - Poisson random variables, $Y_i$, and
      - The predictors, $X_i$.

### Guided Exercises

1. **College burglaries.**  We wish to build a regression model to describe the number of burglaries on a college campus in a year.  Our population of interest will be U.S. liberal arts colleges.
    a. Describe why the response variable ($Y$ = # burglaries on campus in a year) could be modeled by a Poisson distribution.
    b. Describe explanatory variables which might explain differences in $\lambda_i$ = mean number of burglaries per year on campus $i$.
    c. Consider a campus with an average of 5 burglaries per year.  Use `dpois()` to sketch a plot of the distribution of $Y$ for this campus.  Use `rpois()` to verify that both the mean and variance of $Y$ are given by $\lambda=5$.
    d. Consider a campus with an average of 20 burglaries per year and repeat (c).

2. __Elephant mating.__ How does age affect male elephant mating patterns? An article by @Poole1989 investigated whether mating success in male elephants increases with age and whether there is a peak age for mating success. To address this question, the research team followed 41 elephants for one year and recorded both their ages and their number of matings. The data [@Ramsey2002] is found in `elephant.csv`, and the variables are:
    - `MATINGS` = the number of matings in a given year
    - `AGE` = the age of the elephant in years.
    a. Create a histogram of MATINGS. Is there preliminary evidence that number of matings could be modeled as a Poisson response? Explain.
    b. Plot MATINGS by AGE. Add a least squares line. Is there evidence that modeling matings using a linear regression with age might not be appropriate? Explain.  (Hints: fit a smoother; check residual plots).
    c. For each age, calculate the mean number of matings. Take the log of each mean and plot it by AGE.
        i. What assumption can be assessed with this plot?
        ii. Is there evidence of a quadratic trend on this plot?
    d. Fit a Poisson regression model with a linear term for AGE. Exponentiate and then interpret the coefficient for AGE. 
    e. Construct a 95\% confidence interval for the slope and interpret in context (you may want to exponentiate endpoints).
    f. Are the number of matings significantly related to age? Test with
        i. a Wald test and
        ii. a drop in deviance test.
    g. Add a quadratic term in AGE to determine whether there is a maximum age for the number of matings for  elephants. Is a quadratic model preferred to a linear model? To investigate this question, use
        i. a Wald test and
        ii. a drop in deviance test.
    h. What can we say about the goodness-of-fit of the model with age as the sole predictor? Compare the residual deviance for the linear model to a $\chi^2$ distribution with the residual model degrees of freedom.
    i. Fit the linear model using quasi-Poisson regression. (Why?)
        i. How do the estimated coefficients change?
        ii. How do the standard errors change?
        iii. What is the estimated dispersion parameter?
        iv. An estimated dispersion parameter greater than 1 suggests overdispersion. When adjusting for overdispersion, are you more or less likely to obtain a significant result when testing coefficients? Why?  

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:ex3chp4)A small subset of hypothetical data on Minnesota workplace rules on smoking.</caption>
 <thead>
  <tr>
   <th style="text-align:right;"> Subject </th>
   <th style="text-align:right;"> X (location) </th>
   <th style="text-align:right;"> Y (cigarettes) </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
</tbody>
<tfoot>
<tr>
<td style = 'padding: 0; border:0;' colspan='100%'><sup></sup> X is 0 for home and 1 for work.</td>
</tr>
<tr>
<td style = 'padding: 0; border:0;' colspan='100%'><sup></sup> Y is number of cigaretttes in a 2-hour period.</td>
</tr>
</tfoot>
</table>

3. __Smoking at work and home.__ An earlier study examined the effect of workplace rules in Minnesota which require smokers to smoke cigarettes outside.  The number of cigarettes smoked by smokers in a 2-hour period was recorded, along with whether the smoker was at home or at work.  A (very) small subset of the data appears in Table \@ref(tab:ex3chp4).

    - Model 1: Assume that $Y \sim \textrm{Poisson}(\lambda)$; there is no difference between home and work. 
    - Model 2: Assume that $Y \sim \textrm{Poisson}(\lambda_W)$ when the smoker is at work, and $Y \sim \textrm{Poisson}(\lambda_H)$ when the smoker is at home. 
    - Model 3: Assume that $Y \sim \textrm{Poisson}(\lambda)$ and  $log(\lambda)=\beta_0+\beta_1X$. 

    a. Write out the likelihood $L(\lambda)$ and the log-likelihood $logL(\lambda)$ in Model 1. Use the data values above, and simplify where possible.
    b. Intuitively, what would be a reasonable estimate for $\lambda$ based on this data?  Why?
    c. Find the maximum likelihood estimator for $\lambda$ in Model 1 using an optimization routine in R (but not the `glm()` function).  Use R to produce a plot of the likelihood function $L(\lambda)$.
    d. Write out the log-likelihood function $logL(\lambda_W, \lambda_H)$ in Model 2.  Use the data values above, and simplify where possible.
    e. Intuitively, what would be reasonable estimates for $\lambda_W$ and $\lambda_H$ based on this data?  Why?
    f. Find the maximum likelihood estimators for $\lambda_W$ and $\lambda_H$ in Model 2 using an optimization routine in R (but not the `glm()` function).

4.  __Smoking at work and home (continued).__ We will use the same data set in this question as we used in Question 3. 

    a. Write out the log-likelihood function $logL(\beta_0, \beta_1)$ in Model 3.  Again, use the data values above, and simplify where possible.
    b. Find the maximum likelihood estimators for $\beta_0$ and $\beta_1$ in Model 3 using an optimization routine in R (but not the `glm()` function).  Use R to produce a 3D plot of the log-likelihood function.
    c. Confirm your estimates for Model 1 and Model 3 using `glm()`.  Then show that the MLEs for Model 3 agree with the MLEs for Model 2.
  
    For the remaining questions, we will focus exclusively on Model 3.

    d. State a (one-sided) hypothesis for $\beta_1$ in the context of the problem (i.e., explain how your hypothesis relates to smoking at home and at work).  Note: we will nevertheless use two-sided tests and intervals in the following questions.
    e. Do we need to include an offset in our Poisson regression model?  Why or why not?
    f. Give estimates of $\beta_0$ and $\beta_1$, and provide interpretations for both in the context of the problem. 
    g. Provide and interpret a 95% confidence interval for $\beta_1$.
    h. Provide two *different* significance tests for $\beta_1$, in each case providing a test statistic and a p-value and a conclusion in the context of the problem.
    i. Provide a goodness-of-fit test for Model 3, again providing a test statistic, p-value, and conclusion in context.
    j. Can we generalize results of this study to all Minnesota smokers?  Why or why not?
    k. Can we claim that rules restricting smoking in the workplace have caused lower levels of smoking at work?  Explain.
    l. Give two ways in which this study might be improved (besides simply “bigger sample size”).

5. __Campus crime.__ The data set `campuscrime09.csv` contains the number of burglaries reported at a collection of 47 U.S. public universities with over 10,000 students in the year 2009.  In addition, covariates are included which may explain differences in crime rates, including total number of students, percentage of men, average SAT and ACT test scores, and tuition.
    a. Perform an exploratory data analysis. Support your analysis with plots and summary statistics.  
        i. Analyze whether number of burglaries could be reasonably modeled with a Poisson distribution.
        ii. Analyze which covariates you expect to be the best predictors of burglaries.
    b. Consider a model with 4 predictors:  `act.comp + tuition + pct.male + total`.  Try fitting a linear regression with `burg09` as the response. Are there any concerns with this linear regression model?
    c. Run a Poisson regression model with the 4 predictors from (b).  Interpret the coefficients for `tuition` and `pct.male`.
    d. Replace `tuition` with tuition in thousands in your model from (c) – i.e., `tuition.thous`=`tuition`/1000.  How does your new model compare to your model in (c)?  Interpret the coefficient for `tuition.thous`.
    e. We will consider the possibility of including the total number of students at a university as an offset.  
        i. Explain why we might consider `total` as an offset.
        ii.	Refit your model from (d) with total (actually, log(total)) as an offset rather than as a predictor.  Does this new model appear to fit better or worse than the model from (d)?
  	    iii. Refit your model from (d) with log(total) rather than total – so log(total) is a predictor and not an offset.  If total were a good candidate for an offset, what would we expect the coefficient of log(total) to be?  Does a 95% confidence interval for that coefficient contain the value you expected?
    f. Run the following model, then interpret the coefficients for `tuition.thous` and the interaction between `tuition.thous` and `act.comp`.
    

```r
crime <- mutate(crime, total.thous = total/1000)
fit3 <- glm(burg09 ~ act.comp + tuition.thous + 
            total.thous + act.comp:tuition.thous +
            act.comp:total.thous, family = poisson, 
            data = crime)
```

6. __U.S. National Medical Expenditure Survey.__ The data set `NMES1988` in the `AER` package contains a sample of individuals over 65 who are covered by Medicare in order to assess the demand for health care through physician office visits, outpatient visits, ER visits, hospital stays, etc.  The data can be accessed by installing and loading the `AER` package and then running `data(NMES1988)`.  More background information and references about the `NMES1988` data can be found in help pages for the `AER` package.  

    a. Show through graphical means that there are more respondents with 0 `visits` than might be expected under a Poisson model.
    b. Fit a ZIP model for the number of physician office `visits` using `chronic`, `health`, and `insurance` as predictors for the Poisson count, and `chronic` and `insurance` as the predictors for the binary part of the model.  Then, provide interpretations in context for the following model parameters:

    - `chronic` in the Poisson part of the model
    - poor `health` in the Poisson part of the model
    - the Intercept in the logistic part of the model
    - `insurance` in the logistic part of the model
 
    c. Is there significant evidence that the ZIP model is an improvement over a simple Poisson regression model? 


7. __Going vague: ambiguity in political issue statements.__  In the following exercise, you will use a **hurdle model** \index{hurdle model} to analyze the data. A hurdle model is similar to a zero-inflated Poisson model, but instead of assuming that "zeros" are comprised of two distinct groups---those who would always be 0 and those who happen to be 0 on this occasion (e.g., non-drinkers and drinkers who had zero drinks over the weekend in Case Study \@ref(cs:drinking))---the hurdle model assumes that "zeros" are a single entity. Therefore, in a hurdle model, cases are classified as either "zeros" or "non-zeros", where "non-zeros" *hurdle* the 0 threshold---they must always have counts of 1 or above.  We will use the `pscl` package and the `hurdle` function in it to analyze a hurdle model.  Note that coefficients in the "zero hurdle model" section of the output relate predictors to the log-odds of being a *non-zero* (i.e., having at least one issue statement), which is opposite of the ZIP model.

    In a 2018 study, @Chapp2018 scraped every issue statement from webpages of candidates for the U.S. House of Representatives, counting the number of issues candidates commented on and scoring the level of ambiguity of each statement.  We will focus on the issue counts, and determining which attributes (of both the district as a whole and the candidates themselves) are associated with candidate silence (commenting on 0 issues) and a willingness to comment on a greater number of issues.  The data set `ambiguity.csv` contains the following variables:  

    - `name` : candidate name
    - `distID` : unique identification number for Congressional district
    - `ideology` : candidate left-right orientation
    - `democrat` : 1 if Democrat, 0 if Republican
    - `mismatch` : disagreement between candidate ideology and district voter ideology
    - `incumbent` : 1 if incumbent, 0 if not
    - `demHeterogeneity` : how much voters in a district differ according to race, education, occupation, etc.
    - `attHeterogeneity` : how much voters in a district differ according to political ideology
    - `distLean` : overall ideological lean in a district
    - `totalIssuePages` : number of issues candidates commented on (response)
  
    a. Create a frequency plot of `totalIssuePages`. Why might we consider using a hurdle model compared to a Poisson model? Why can’t we use a zero-inflated Poisson model?
    b. Create a plot of the empirical log odds of having at least one issue statement by ideology.  You may want to group ideology values first.  What can you conclude from this plot?  (See Chapter \@ref(ch-logreg) for more details.)
    c. Create a scatterplot that shows the log of the mean number of issues vs. ideology group by party, among candidates with at least one issue statement. What can we conclude from this plot?
    d. Create a hurdle model with `ideology` and `democrat` as predictors in both parts. Interpret `ideology` in both parts of the model. 
    e. Repeat (d), but include an interaction in both parts. Interpret the interaction in the zero hurdle part of the model.
    f. Find the best model you can to determine `totalIssuePages`. Write a short paragraph discussing implications of your model.  

### Open-Ended Exercises

1. __Airbnb in NYC.__ @Awad2017 scraped 40628 Airbnb listings from New York City in March 2017 and put together the data set `NYCairbnb.csv`.  Key variables include:

    - `id` = unique ID number for each unit
    - `last_scraped` = date when information scraped
    - `host_since` = date when host first listed the unit on Airbnb
    - `days` = `last_scraped` - `host_since` = number of days the unit has been listed
    - `room_type` = Entire home/apt., Private room, or Shared room
    - `bathrooms` = number of bathrooms
    - `bedrooms` = number of bedrooms
    - `price` = price per night (dollars)
    - `number_of_reviews` = number of reviews for the unit on Airbnb
    - `review_scores_cleanliness` = cleanliness score from reviews (1-10)
    - `review_scores_location` = location score from reviews (1-10)
    - `review_scores_value` = value score from reviews (1-10)
    - `instant_bookable` = "t" if instantly bookable, "f" if not

    Perform an EDA, build a model, and interpret model coefficients to describe variation in the number of reviews (a proxy for the number of rentals, which is not available) as a function of the variables provided.  Don't forget to consider an offset, if needed.


2. __Crab satellites.__ @Brockmann1996 carried out a study of nesting female horseshoe crabs. Female horseshoe crabs often have male crabs attached to a female's nest known as *satellites*. One objective of the study was to determine which characteristics of the female were associated with the number of satellites. Of particular interest is the relationship between the width of the female carapace and satellites.

    The data can be found in `crab.csv`. It includes:

    - `NumSat` = number of satellites
    - `Width` = carapace width (cm)
    - `Wt` = weight (kg)
    - `Sp` = spine condition (1 = both good, 2 = one worn or broken, 3 = both worn or broken)
    - `C` = color (1 = light medium, 2 = medium, 3 = dark medium, 4 = dark)

    Use Poisson regression to investigate the research question. Be sure you work to obtain an appropriate model before considering overdispersion.  Should a hurdle model be considered here?  If so, fit a hurdle model and interpret in context.


3. __Doctor visits.__ Data was collected on doctor visits from a sample of 5,190 people in the 1977/1978 Australian Health Survey. @Cameron1986 sought to explain the variation in doctor visits using one or more explanatory variables. The data can be found in an R data set from `library(AER)` accessible with the command `data("DoctorVisits")`. Variable descriptions can be found under `help("DoctorVisits")`.

    Explore the use of a zero-inflated model for this data. Begin with a histogram of the number of visits, complete an EDA, and then fit several models. Summarize your results.

4. __More fish.__ The number of fish caught (`count`), persons in the party (`persons`), the number of children in the party (`child`), whether or not they brought a camper into the park (`camper`), and the length of stay (`LOS`) were recorded for 250 camping parties. The data can be found in `fish2.csv` (source: @idre2018).  Create and assess a model for the number of fish caught.
