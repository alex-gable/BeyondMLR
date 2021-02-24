---
title: "Chapter 9"
subtitle: "Two-Level Longitudinal Data"
output:
  pdf_document:
    number_sections: yes
  html_document: default
editor_options: 
  chunk_output_type: console
---



# Two-Level Longitudinal Data {#ch-lon}

## Learning Objectives

After finishing this chapter, you should be able to:

- Recognize longitudinal data as a special case of multilevel data, with time at Level One.
- Consider patterns of missingness and implications of that missing data on multilevel analyses.
- Apply exploratory data analysis techniques specific to longitudinal data.
- Build and understand a taxonomy of models for longitudinal data.
- Interpret model parameters in multilevel models with time at Level One.
- Compare models, both nested and not, with appropriate statistical tests and summary statistics.
- Consider different ways of modeling the variance-covariance structure in longitudinal data.
- Understand how a parametric bootstrap test of significance works and when it might be useful.


```r
# Packages required for Chapter 9
library(GGally)
library(data.table)
library(Hmisc)
library(mice)
library(lattice)
library(nlme)
library(reshape2)
library(MASS)
library(mnormt)
library(lme4)
library(gridExtra) 
library(knitr)
library(kableExtra)
library(broom)
library(tidyverse)
```

## Case Study: Charter Schools {#cs:charter}

Charter schools were first introduced in the state of Minnesota in 1991 [@CharterSchools]. Since then, charter schools have begun appearing all over the United States. While publicly funded, a unique feature of charter schools is their independence from many of the regulations that are present in the public school systems of their respective city or state. Thus, charters will often extend the school days or year and tend to offer non-traditional techniques and styles of instruction and learning.

One example of this unique schedule structure is the KIPP (Knowledge Is Power Program) Stand Academy in Minneapolis, MN. KIPP stresses longer days and better partnerships with parents, and they claim that 80\% of their students go to college from a population where 87\% qualify for free and reduced lunch and 95\% are African American or Latino [@KIPP]. However, the larger question is whether or not charter schools are out-performing non-charter public schools in general.  Because of the relative youthfulness of charter schools, data has just begun to be collected to evaluate the performance of charter versus non-charter schools and some of the factors that influence a school's performance. Along these lines, we will examine data collected by the Minnesota Department of Education for all Minnesota schools during the years 2008-2010.

Comparisons of student performance in charter schools versus public schools have produced conflicting results, potentially as a result of the strong differences in the structure and population of the student bodies that represent the two types of schools. A study by the Policy and Program Studies Service of five states found that charter schools are less likely to meet state performance standards than conventional public schools [@Finnigan2004]. However, @Witte2007 performed a statistical analysis comparing Wisconsin charter and non-charter schools and found that average achievement test scores were significantly higher in charter schools compared to non-charter schools, after controlling for demographic variables such as the percentage of white students. In addition, a study of California students who took the Stanford 9 exam from 1998 through 2002 found that charter schools, on average, were performing at the same level as conventional public schools [@Buddin2005]. Although school performance is difficult to quantify with a single measure, for illustration purposes in this chapter we will focus on that aspect of school performance measured by the math portion of the Minnesota Comprehensive Assessment (MCA-II) data for 6th grade students enrolled in 618 different Minnesota schools during the years 2008, 2009, and 2010 [@MNDepartmentOfEducation]. Similar comparisons could obviously be conducted for other grade levels or modes of assessment.

As described in @Green2003, it is very challenging to compare charter and public non-charter schools, as charter schools are often designed to target or attract specific populations of students. Without accounting for differences in student populations, comparisons lose meaning. With the assistance of multiple school-specific predictors, we will attempt to model sixth grade math MCA-II scores of Minnesota schools, focusing on the differences between charter and public non-charter school performances. In the process, we hope to answer the following research questions:

- Which factors most influence a school's performance in MCA testing?
- How do the average math MCA-II scores for 6th graders enrolled in charter schools differ from scores for students who attend non-charter public schools? Do these differences persist after accounting for differences in student populations?
- Are there differences in yearly improvement between charter and non-charter public schools?

## Initial Exploratory Analyses {#exploratoryanalysis}

### Data Organization {#data}

Key variables in `chart_wide_condense.csv` which we will examine to address the research questions above are:

- `schoolid` = includes district type, district number, and school number
- `schoolName` = name of school
- `urban` = is the school in an urban (1) or rural (0) location?
- `charter` = is the school a charter school (1) or a non-charter public school (0)?
- `schPctnonw` = proportion of non-white students in a school (based on 2010 figures)
- `schPctsped` = proportion of special education students in a school (based on 2010 figures)
- `schPctfree` = proportion of students who receive free or reduced lunches in a school (based on 2010 figures).  This serves as a measure of poverty among school families.
- `MathAvgScore.0` = average MCA-II math score for all sixth grade students in a school in 2008
- `MathAvgScore.1` = average MCA-II math score for all sixth grade students in a school in 2009
- `MathAvgScore.2` = average MCA-II math score for all sixth grade students in a school in 2010

This data is stored in WIDE format, with one row per school, as illustrated in Table \@ref(tab:table1chp9).



<table class="table" style="font-size: 9px; margin-left: auto; margin-right: auto;">
<caption style="font-size: initial !important;">(\#tab:table1chp9)The first six observations in the wide data set for the Charter Schools case study.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> schoolid </th>
   <th style="text-align:left;"> schoolName </th>
   <th style="text-align:right;"> urban </th>
   <th style="text-align:right;"> charter </th>
   <th style="text-align:right;"> schPctnonw </th>
   <th style="text-align:right;"> schPctsped </th>
   <th style="text-align:right;"> schPctfree </th>
   <th style="text-align:right;"> MathAvgScore.0 </th>
   <th style="text-align:right;"> MathAvgScore.1 </th>
   <th style="text-align:right;"> MathAvgScore.2 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Dtype 1 Dnum 1 Snum 2 </td>
   <td style="text-align:left;"> RIPPLESIDE ELEMENTARY </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.0000000 </td>
   <td style="text-align:right;"> 0.1176471 </td>
   <td style="text-align:right;"> 0.3627451 </td>
   <td style="text-align:right;"> 652.8 </td>
   <td style="text-align:right;"> 656.6 </td>
   <td style="text-align:right;"> 652.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Dtype 1 Dnum 100 Snum 1 </td>
   <td style="text-align:left;"> WRENSHALL ELEMENTARY </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.0303030 </td>
   <td style="text-align:right;"> 0.1515152 </td>
   <td style="text-align:right;"> 0.4242424 </td>
   <td style="text-align:right;"> 646.9 </td>
   <td style="text-align:right;"> 645.3 </td>
   <td style="text-align:right;"> 651.9 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Dtype 1 Dnum 108 Snum 30 </td>
   <td style="text-align:left;"> CENTRAL MIDDLE </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.0769231 </td>
   <td style="text-align:right;"> 0.1230769 </td>
   <td style="text-align:right;"> 0.2615385 </td>
   <td style="text-align:right;"> 654.7 </td>
   <td style="text-align:right;"> 658.5 </td>
   <td style="text-align:right;"> 659.7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Dtype 1 Dnum 11 Snum 121 </td>
   <td style="text-align:left;"> SANDBURG MIDDLE </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.0977444 </td>
   <td style="text-align:right;"> 0.0827068 </td>
   <td style="text-align:right;"> 0.2481203 </td>
   <td style="text-align:right;"> 656.4 </td>
   <td style="text-align:right;"> 656.8 </td>
   <td style="text-align:right;"> 659.9 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Dtype 1 Dnum 11 Snum 193 </td>
   <td style="text-align:left;"> OAK VIEW MIDDLE </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.0537897 </td>
   <td style="text-align:right;"> 0.0953545 </td>
   <td style="text-align:right;"> 0.1418093 </td>
   <td style="text-align:right;"> 657.7 </td>
   <td style="text-align:right;"> 658.2 </td>
   <td style="text-align:right;"> 659.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Dtype 1 Dnum 11 Snum 195 </td>
   <td style="text-align:left;"> ROOSEVELT MIDDLE </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.1234177 </td>
   <td style="text-align:right;"> 0.0886076 </td>
   <td style="text-align:right;"> 0.2405063 </td>
   <td style="text-align:right;"> 655.9 </td>
   <td style="text-align:right;"> 659.1 </td>
   <td style="text-align:right;"> 660.3 </td>
  </tr>
</tbody>
</table>

For most statistical analyses, it will be advantageous to convert WIDE format to LONG format, with one row per year per school.  To make this conversion, we will have to create a time variable, which under the LONG format is very flexible---each school can have a different number of and differently-spaced time points, and they can even have predictors which vary over time.  Details for making this conversion can be found in the R Markdown code for this chapter, and the form of the LONG data in this study is exhibited in the next section.

### Missing Data {#missing}

In this case, before we convert our data to LONG form, we should first address problems with missing data. \index{missing data}  Missing data is a common phenomenon in longitudinal studies. For instance, it could arise if a new school was started during the observation period, a school was shut down during the observation period, or no results were reported in a given year. Dealing with missing data in a statistical analysis is not trivial, but fortunately many multilevel packages (including the lme4 package in R) are adept at handling missing data.

First, we must understand the extent and nature of missing data in our study. Table \@ref(tab:table2chp9) is a frequency table of missing data patterns, where 1 indicates presence of a variable and 0 indicates a missing value for a particular variable, is a helpful starting point. Among our 618 schools, 540 had complete data (all covariates and math scores for all three years), 25 were missing a math score for 2008, 35 were missing math scores in both 2008 and 2009, etc.

The number of schools with a particular missing data pattern are listed in the left column; the remaining columns of 0's and 1's describe the missing data pattern, with 0 indicating a missing value.  Some covariates that are present for every school are not listed.  The bottom row gives the number of schools with missing values for specific variables; the last entry indicates that 121 total observations were missing.

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:table2chp9)A frequency table of missing data patterns.</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> charter </th>
   <th style="text-align:right;"> MathAvgScore.2 </th>
   <th style="text-align:right;"> MathAvgScore.1 </th>
   <th style="text-align:right;"> MathAvgScore.0 </th>
   <th style="text-align:right;">  </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 540 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 25 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 35 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 46 </td>
   <td style="text-align:right;"> 61 </td>
   <td style="text-align:right;"> 121 </td>
  </tr>
</tbody>
</table>

Statisticians have devised different strategies for handling missing data; a few common approaches are described briefly here:

- Include only schools with complete data. This is the cleanest approach analytically; however, ignoring data from 12.6\% of the study's schools (since 78 of the 618 schools had incomplete data) means that a large amount of potentially useful data is being thrown away.  In addition, this approach creates potential issues with informative missingness. Informative missingness occurs when a school's lack of scores is not a random phenomenon but provides information about the effectiveness of the school type (e.g., a school closes because of low test scores).
- Last observation carried forward. Each school's last math score is analyzed as a univariate response, whether the last measurement was taken in 2008, 2009, or 2010. With this approach, data from all schools can be used, and analyses can be conducted with traditional methods assuming independent responses. This approach is sometimes used in clinical trials because it tends to be conservative, setting a higher bar for showing that a new therapy is significantly better than a traditional therapy. Of course, we must assume that a school's 2008 score is representative of its 2010 score. In addition, information about trajectories over time is thrown away.
- Imputation of missing observations. Many methods have been developed for sensibly "filling in" missing observations, using imputation models which base imputed data on subjects with similar covariate profiles and on typical observed time trends. Once an imputed data set is created (or several imputed data sets), analyses can proceed with complete data methods that are easier to apply. Risks with the imputation approach include misrepresenting missing observations and overstating precision in final results.
- Apply multilevel methods, which use available data to estimate patterns over time by school and then combine those school estimates in a way that recognizes that time trends for schools with complete data are more precise than time trends for schools with fewer measurements. @Laird1988 demonstrates that multilevel models are valid under the fairly unrestrictive condition that the probability of missingness cannot depend on any unobserved predictors or the response.  This is the approach we will follow in the remainder of the text.

Now, we are ready to create our LONG data set. Fortunately, many packages (including R) have built-in functions for easing this conversion, and the functions are improving constantly. The resulting LONG data set is shown in Table \@ref(tab:table3chp9), where `year08` measures the number of years since 2008.



(ref:caplontable3chp9) \@ref(tab:table1chp9)

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:table3chp9)The first six observations in the long data set for the Charter Schools case study; these lines correspond to the first two observations from the wide data set illustrated in Table (ref:caplontable3chp9).</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> schoolName </th>
   <th style="text-align:right;"> charter </th>
   <th style="text-align:right;"> schPctsped </th>
   <th style="text-align:right;"> schPctfree </th>
   <th style="text-align:right;"> year08 </th>
   <th style="text-align:right;"> MathAvgScore </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> RIPPLESIDE ELEMENTARY </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.1176471 </td>
   <td style="text-align:right;"> 0.3627451 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 652.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RIPPLESIDE ELEMENTARY </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.1176471 </td>
   <td style="text-align:right;"> 0.3627451 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 656.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RIPPLESIDE ELEMENTARY </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.1176471 </td>
   <td style="text-align:right;"> 0.3627451 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 652.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> WRENSHALL ELEMENTARY </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.1515152 </td>
   <td style="text-align:right;"> 0.4242424 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 646.9 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> WRENSHALL ELEMENTARY </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.1515152 </td>
   <td style="text-align:right;"> 0.4242424 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 645.3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> WRENSHALL ELEMENTARY </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.1515152 </td>
   <td style="text-align:right;"> 0.4242424 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 651.9 </td>
  </tr>
</tbody>
</table>

### Exploratory Analyses for General Multilevel Models {#generalanalyses}

Notice the **longitudinal** \index{longitudinal} structure of our data---we have up to three measurements of test scores at different time points for each of our 618 schools. With this structure, we can address questions at two levels:

- Within school---changes over time
- Between schools---effects of school-specific covariates (charter or non-charter, urban or rural, percent free and reduced lunch, percent special education, and percent non-white) on 2008 math scores and rate of change between 2008 and 2010.

As with any statistical analysis, it is vitally important to begin with graphical and numerical summaries of important variables and relationships between variables. We'll begin  with initial exploratory analyses that we introduced in the previous chapter, noting that we have no Level One covariates other than time at this point (potential covariates at this level may have included measures of the number of students tested or funds available per student). We will, however, consider the Level Two variables of charter or non-charter, urban or rural, percent free and reduced lunch, percent special education, and percent non-white.  Although covariates such as percent free and reduced lunch may vary slightly from year to year within a school, the larger and more important differences tend to occur between schools, so we used percent free and reduced lunch for a school in 2010 as a Level Two variable.

As in Chapter \@ref(ch-multilevelintro), we can conduct initial investigations of relationships between Level Two covariates and test scores in two ways.  First, we can use all 1733 observations to investigate relationships of Level Two covariates with test scores.  Although these plots will contain dependent points, since each school is represented by up to three years of test score data, general patterns exhibited in these plots tend to be real.  Second, we can calculate mean scores across all years for each of the 618 schools.  While we lose some information with this approach, we can more easily consider each plotted point to be independent.  Typically, both types of exploratory plots illustrate similar relationships, and in this case, both approaches are so similar that we will only show plots using the second approach, with one observation per school.

Figure \@ref(fig:lon-hist1) shows the distribution of MCA math test scores as somewhat left-skewed.  MCA test scores for sixth graders are scaled to fall between 600 and 700, where scores above 650 for individual students indicate "meeting standards".  Thus, schools with averages below 650 will often have increased incentive to improve their scores the following year.  When we refer to the "math score" for a particular school in a particular year, we will assume that score represents the average for all sixth graders at that school.  In Figure \@ref(fig:lon-box1), we see that test scores are generally higher for both schools in rural areas and for public non-charter schools. Note that in this data set there are 237 schools in rural areas and 381 schools in urban areas, as well as 545 public non-charter schools and 73 charter schools. In addition, we can see in Figure \@ref(fig:lon-scat1) that schools tend to have lower math scores if they have higher percentages of students with free and reduced lunch, with special education needs, or who are non-white.



<div class="figure" style="text-align: center">
<img src="09-Two-Level-Longitudinal-Data_files/figure-epub3/lon-hist1-1.png" alt="Histogram of mean sixth grade MCA math test scores over the years 2008-2010 for 618 Minnesota schools." width="60%" />
<p class="caption">(\#fig:lon-hist1)Histogram of mean sixth grade MCA math test scores over the years 2008-2010 for 618 Minnesota schools.</p>
</div>



<div class="figure" style="text-align: center">
<img src="09-Two-Level-Longitudinal-Data_files/figure-epub3/lon-box1-1.png" alt="Boxplots of categorical Level Two covariates vs. average MCA math scores.  Plot (a) shows charter vs. public non-charter schools, while plot (b) shows urban vs. rural schools." width="60%" />
<p class="caption">(\#fig:lon-box1)Boxplots of categorical Level Two covariates vs. average MCA math scores.  Plot (a) shows charter vs. public non-charter schools, while plot (b) shows urban vs. rural schools.</p>
</div>



<div class="figure" style="text-align: center">
<img src="09-Two-Level-Longitudinal-Data_files/figure-epub3/lon-scat1-1.png" alt=" Scatterplots of average MCA math scores by (a) percent free and reduced lunch, (b) percent special education, and (c) percent non-white in a school." width="60%" />
<p class="caption">(\#fig:lon-scat1) Scatterplots of average MCA math scores by (a) percent free and reduced lunch, (b) percent special education, and (c) percent non-white in a school.</p>
</div>

### Exploratory Analyses for Longitudinal Data {#longitudinalanalyses}

In addition to the initial exploratory analyses above, longitudinal data---multilevel data with time at Level One---calls for further plots and summaries that describe time trends within and across individuals. For example, we can examine trends over time within individual schools. Figure \@ref(fig:lon-lat1) provides a **lattice plot** \index{lattice plot} illustrating trends over time for the first 24 schools in the data set. We note differences among schools in starting point (test scores in 2008), slope (change in test scores over the three-year period), and form of the relationship. These differences among schools are nicely illustrated in so-called **spaghetti plots** \index{spaghetti plot} such as Figure \@ref(fig:lon-spag1), which overlays the individual schools' time trends (for the math test scores) from Figure \@ref(fig:lon-lat1) on a single set of axes.  In order to illustrate the overall time trend without making global assumptions about the form of the relationship, we overlaid in bold a non-parametric fitted curve through a **loess smoother**. \index{loess smoother} LOESS comes from "locally estimated scatterplot smoother", in which a low-degree polynomial is fit to each data point using weighted regression techniques, where nearby points receive greater weight. LOESS is a computationally intensive method which performs especially well with larger sets of data, although ideally there would be a greater diversity of x-values than the three time points we have. In this case, the loess smoother follows very closely to a linear trend, indicating that assuming a linear increase in test scores over the three-year period is probably a reasonable simplifying assumption.  To further examine the hypothesis that linearity would provide a reasonable approximation to the form of the individual time trends in most cases, Figure \@ref(fig:lon-lat2) shows a lattice plot containing linear fits through ordinary least squares rather than connected time points as in Figure \@ref(fig:lon-lat1).



<div class="figure" style="text-align: center">
<img src="09-Two-Level-Longitudinal-Data_files/figure-epub3/lon-lat1-1.png" alt="Lattice plot by school of math scores over time for the first 24 schools in the data set." width="60%" />
<p class="caption">(\#fig:lon-lat1)Lattice plot by school of math scores over time for the first 24 schools in the data set.</p>
</div>

<div class="figure" style="text-align: center">
<img src="09-Two-Level-Longitudinal-Data_files/figure-epub3/lon-spag1-1.png" alt=" Spaghetti plot of math scores over time by school, for all the charter schools and a random sample of public non-charter schools, with overall fit using loess (bold)." width="60%" />
<p class="caption">(\#fig:lon-spag1) Spaghetti plot of math scores over time by school, for all the charter schools and a random sample of public non-charter schools, with overall fit using loess (bold).</p>
</div>

<div class="figure" style="text-align: center">
<img src="09-Two-Level-Longitudinal-Data_files/figure-epub3/lon-lat2-1.png" alt=" Lattice plot by school of math scores over time with linear fit for the first 24 schools in the data set." width="60%" />
<p class="caption">(\#fig:lon-lat2) Lattice plot by school of math scores over time with linear fit for the first 24 schools in the data set.</p>
</div>



<div class="figure" style="text-align: center">
<img src="09-Two-Level-Longitudinal-Data_files/figure-epub3/lon-spag3-1.png" alt="Spaghetti plots showing time trends for each school by school type, for a random sample of charter schools (left) and public non-charter schools (right), with overall fits using loess (bold)." width="60%" />
<p class="caption">(\#fig:lon-spag3)Spaghetti plots showing time trends for each school by school type, for a random sample of charter schools (left) and public non-charter schools (right), with overall fits using loess (bold).</p>
</div>



<div class="figure" style="text-align: center">
<img src="09-Two-Level-Longitudinal-Data_files/figure-epub3/lon-spagmat1-1.png" alt="Spaghetti plots showing time trends for each school by quartiles of percent free and reduced lunch, with loess fits." width="60%" />
<p class="caption">(\#fig:lon-spagmat1)Spaghetti plots showing time trends for each school by quartiles of percent free and reduced lunch, with loess fits.</p>
</div>

Just as we explored the relationship between our response (average math scores) and important covariates in Section \@ref(generalanalyses), we can now examine the relationships between time trends by school and important covariates. For instance, Figure \@ref(fig:lon-spag3) shows that charter schools had math scores that were lower on average than public non-charter schools and more variable.  This type of plot is sometimes called a **trellis graph**, \index{trellis graph} since it displays a grid of smaller charts with consistent scales, where each smaller chart represents a condition---an item in a category. Trends over time by school type are denoted by bold loess curves.  Public non-charter schools have higher scores across all years; both school types show little growth between 2008 and 2009, but greater growth between 2009 and 2010, especially charter schools. Exploratory analyses like this can be repeated for other covariates, such as percent free and reduced lunch in Figure \@ref(fig:lon-spagmat1). The trellis plot automatically divides schools into four groups based on quartiles of their percent free and reduced lunch, and we see that schools with lower percentages of free and reduced lunch students tend to have higher math scores and less variability. Across all levels of free and reduced lunch, we see greater gains between 2009 and 2010 than between 2008 and 2009.

## Preliminary Two-Stage Modeling {#twostage9}

### Linear Trends Within Schools {#lineartwostage}

Even though we know that every school's math test scores were not strictly linearly increasing or decreasing over the observation period, a linear model for individual time trends is often a simple but reasonable way to model data. One advantage of using a linear model within school is that each school's data points can be summarized with two summary statistics---an intercept and a slope (obviously, this is an even bigger advantage when there are more observations over time per school).  For instance, we see in Figure \@ref(fig:lon-lat2) that sixth graders from the school depicted in the top right slot slowly increased math scores over the three-year observation period, while students from the school depicted in the fourth column of the top row generally experienced decreasing math scores over the same period. As a whole, the linear model fits individual trends pretty well, and many schools appear to have slowly increasing math scores over time, as researchers in this study may have hypothesized.

Another advantage of assuming a linear trend at Level One (within schools) is that we can examine summary statistics across schools. Both the intercept and slope are meaningful for each school: the *intercept* conveys the school's math score in 2008, while the *slope* conveys the school's average yearly increase or decrease in math scores over the three-year period. Figure \@ref(fig:lon-cis1) shows that point estimates and uncertainty surrounding individual estimates of intercepts and slopes vary considerably. In addition, we can generate summary statistics and histograms for the 618 intercepts and slopes produced by fitting linear regression models at Level One, in addition to R-squared values which describe the strength of fit of the linear model for each school (Figure \@ref(fig:lon-histmat1)). For our 618 schools, the mean math score for 2008 was 651.4 (SD=7.28), and the mean yearly rate of change in math scores over the three-year period was 1.30 (SD=2.51). We can further examine the relationship between schools' intercepts and slopes. Figure \@ref(fig:lon-scat5) shows a general decreasing trend, suggesting that schools with lower 2008 test scores tend to have greater growth in scores between 2008 and 2010 (potentially because those schools have more room for improvement); this trend is supported with a correlation coefficient of $-0.32$ between fitted intercepts and slopes.  Note that, with only 3 or fewer observations for each school, extreme or intractable values for the slope and R-squared are possible. For example, slopes cannot be estimated for those schools with just a single test score, R-squared values cannot be calculated for those schools with no variability in test scores between 2008 and 2010, and R-squared values must be 1 for those schools with only two test scores.



(ref:lon-cis1-cap) Point estimates and 95% confidence intervals for (a) intercepts and (b) slopes by school, for the first 24 schools in the data set.

<div class="figure" style="text-align: center">
<img src="09-Two-Level-Longitudinal-Data_files/figure-epub3/lon-cis1-1.png" alt="(ref:lon-cis1-cap)" width="60%" />
<p class="caption">(\#fig:lon-cis1)(ref:lon-cis1-cap)</p>
</div>



<div class="figure" style="text-align: center">
<img src="09-Two-Level-Longitudinal-Data_files/figure-epub3/lon-histmat1-1.png" alt=" Histograms for (a) intercepts, (b) slopes, and (c) R-squared values from fitted regression lines by school." width="60%" />
<p class="caption">(\#fig:lon-histmat1) Histograms for (a) intercepts, (b) slopes, and (c) R-squared values from fitted regression lines by school.</p>
</div>

<div class="figure" style="text-align: center">
<img src="09-Two-Level-Longitudinal-Data_files/figure-epub3/lon-scat5-1.png" alt="Scatterplot showing the relationship between intercepts and slopes from fitted regression lines by school." width="60%" />
<p class="caption">(\#fig:lon-scat5)Scatterplot showing the relationship between intercepts and slopes from fitted regression lines by school.</p>
</div>

### Effects of Level Two Covariates on Linear Time Trends {#lineartwostageL2effects}

Summarizing trends over time within schools is typically only a start, however. Most of the primary research questions from this study involve comparisons among schools, such as: (a) are there significant differences between charter schools and public non-charter schools, and (b) do any differences between charter schools and public schools change with percent free and reduced lunch, percent special education, or location?  These are Level Two questions, and we can begin to explore these questions by graphically examining the effects of school-level variables on schools' linear time trends. By school-level variables, we are referring to those covariates that differ by school but are not dependent on time. For example, school type (charter or public non-charter), urban or rural location, percent non-white, percent special education, and percent free and reduced lunch are all variables which differ by school but which don't change over time, at least as they were assessed in this study. Variables which would be time-dependent include quantities such as per pupil funding and reading scores.
	
Figure \@ref(fig:lon-box2) shows differences in the average time trends by school type, using estimated intercepts and slopes to support observations from the spaghetti plots in Figure \@ref(fig:lon-spag3).  Based on intercepts, charter schools have lower math scores, on average, in 2008 than public non-charter schools. Based on slopes, however, charter schools tend to improve their math scores at a slightly faster rate than public schools, especially at the 75th percentile and above. By the end of the three-year observation period, we would nevertheless expect charter schools to have lower average math scores than public schools. For another exploratory perspective on school type comparisons, we can examine differences between school types with respect to math scores in 2008 and math scores in 2010. As expected, boxplots by school type (Figure \@ref(fig:lon-box3)) show clearly lower math scores for charter schools in 2008, but differences are slightly less dramatic in 2010.
	


<div class="figure" style="text-align: center">
<img src="09-Two-Level-Longitudinal-Data_files/figure-epub3/lon-box2-1.png" alt="Boxplots of (a) intercepts and (b) slopes by school type (charter vs. public non-charter)." width="60%" />
<p class="caption">(\#fig:lon-box2)Boxplots of (a) intercepts and (b) slopes by school type (charter vs. public non-charter).</p>
</div>



<div class="figure" style="text-align: center">
<img src="09-Two-Level-Longitudinal-Data_files/figure-epub3/lon-box3-1.png" alt="Boxplots of (a) 2008 and (b) 2010 math scores by school type (charter vs. public non-charter)." width="60%" />
<p class="caption">(\#fig:lon-box3)Boxplots of (a) 2008 and (b) 2010 math scores by school type (charter vs. public non-charter).</p>
</div>

Any initial exploratory analyses should also investigate effects of potential confounding variables such as school demographics and location.  If we discover, for instance, that those schools with higher levels of poverty (measured by the percentage of students receiving free and reduced lunch) display lower test scores in 2008 but greater improvements between 2008 and 2010, then we might be able to use percentage of free and reduced lunch in statistical modeling of intercepts and slopes, leading to more precise estimates of the charter school effects on these two outcomes.  In addition, we should also look for any interaction with school type---any evidence that the difference between charter and non-charter schools changes based on the level of a confounding variable.  For example, do charter schools perform better relative to non-charter schools when there is a large percentage of non-white students at a school?

With a confounding variable such as percentage of free and reduced lunch, we will treat this variable as continuous to produce the most powerful exploratory analyses. We can begin by examining boxplots of free and reduced lunch percentage against school type (Figure \@ref(fig:lon-boxcatmat1)).  We observe that charter schools tend to have greater percentages of free and reduced lunch students as well as greater school-to-school variability.  Next, we can use scatterplots to graphically illustrate the relationships between free and reduced lunch percentages and significant outcomes such as intercept and slope (also Figure \@ref(fig:lon-boxcatmat1)).  In this study, it appears that schools with higher levels of free and reduced lunch (i.e., greater poverty) tend to have lower math scores in 2008, but there is little evidence of a relationship between levels of free and reduced lunch and improvements in test scores between 2008 and 2010.  These observations are supported with correlation coefficients between percent free and reduced lunch and intercepts (r=-0.61) and slopes (r=-0.06).

A less powerful but occasionally informative way to look at the effect of a continuous confounder on an outcome variable is by creating a categorical variable out of the confounder.  For instance, we could classify any school with a percentage of free and reduced lunch students above the median as having a high percentage of free and reduced lunch students, and all other schools as having a low percentage of free and reduced lunch students.  Then we could examine a possible interaction between percent free and reduced lunch and school type through a series of four boxplots (Figure \@ref(fig:lon-boxmat1)).  In fact, these boxplots suggest that the gap between charter and public non-charter schools in 2008 was greater in schools with a high percentage of free and reduced lunch students, while the difference in rate of change in test scores between charter and public non-charter schools appeared similar for high and low levels of free and reduced lunch.  We will investigate these trends more thoroughly with statistical modeling.



<div class="figure" style="text-align: center">
<img src="09-Two-Level-Longitudinal-Data_files/figure-epub3/lon-boxcatmat1-1.png" alt="(a) Boxplot of percent free and reduced lunch by school type (charter vs. public non-charter), along with scatterplots of (b) intercepts and (c) slopes from fitted regression lines by school vs. percent free and reduced lunch." width="60%" />
<p class="caption">(\#fig:lon-boxcatmat1)(a) Boxplot of percent free and reduced lunch by school type (charter vs. public non-charter), along with scatterplots of (b) intercepts and (c) slopes from fitted regression lines by school vs. percent free and reduced lunch.</p>
</div>



<div class="figure" style="text-align: center">
<img src="09-Two-Level-Longitudinal-Data_files/figure-epub3/lon-boxmat1-1.png" alt="Boxplots of (a) intercepts and (b) slopes from fitted regression lines by school vs. school type (charter vs. public non-charter), separated by high and low levels of percent free and reduced lunch." width="60%" />
<p class="caption">(\#fig:lon-boxmat1)Boxplots of (a) intercepts and (b) slopes from fitted regression lines by school vs. school type (charter vs. public non-charter), separated by high and low levels of percent free and reduced lunch.</p>
</div>

The effect of other confounding variables (e.g., percent non-white, percent special education, urban or rural location) can be investigated in a similar fashion to free and reduced lunch percentage, both in terms of main effect (variability in outcomes such as slope and intercept which can be explained by the confounding variable) and interaction with school type (ability of the confounding variable to explain differences between charter and public non-charter schools).  We leave these explorations as an exercise.

### Error Structure Within Schools {#lineartwostageerror2}

Finally, with longitudinal data it is important to investigate the error variance-covariance structure of data collected within a school (the Level Two observational unit). In multilevel data, as in the examples we introduced in Chapter \@ref(ch-corrdata), we suspect observations within group (like a school) to be correlated, and we strive to model that correlation. When the data within group is collected over time, we often see distinct patterns in the residuals that can be modeled---correlations which decrease systematically as the time interval increases, variances that change over time, correlation structure that depends on a covariate, etc. A first step in modeling the error variance-covariance structure is the production of an exploratory plot such as Figure \@ref(fig:lon-cor1). To generate this plot, we begin by modeling MCA math score as a linear function of time using all 1733 observations and ignoring the school variable.  This population (marginal) trend is illustrated in Figure \@ref(fig:lon-spag1) and is given by:

\begin{equation*}
\hat{Y}_{ij}=651.69+1.20\textrm{Time}_{ij},
\end{equation*}
where $\hat{Y}_{ij}$ is the predicted math score of the $i^{th}$ school at time $j$, where time $j$ is the number of years since 2008.  In this model, the predicted math score will be identical for all schools at a given time point $j$. Residuals $Y_{ij}-\hat{Y}_{ij}$ are then calculated for each observation, measuring the difference between actual math score and the average overall time trend.  Figure \@ref(fig:lon-cor1) then combines three pieces of information: the upper right triangle contains correlation coefficients for residuals between pairs of years, the diagonal contains histograms of residuals at each time point, and the lower left triangle contains scatterplots of residuals from two different years.  In our case, we see that correlation between residuals from adjacent years is strongly positive (0.81-0.83) and does not drop off greatly as the time interval between years increases.



<div class="figure" style="text-align: center">
<img src="09-Two-Level-Longitudinal-Data_files/figure-epub3/lon-cor1-1.png" alt="Correlation structure within school.  The upper right contains correlation coefficients between residuals at pairs of time points, the lower left contains scatterplots of the residuals at time point pairs, and the diagonal contains histograms of residuals at each of the three time points." width="60%" />
<p class="caption">(\#fig:lon-cor1)Correlation structure within school.  The upper right contains correlation coefficients between residuals at pairs of time points, the lower left contains scatterplots of the residuals at time point pairs, and the diagonal contains histograms of residuals at each of the three time points.</p>
</div>

## Initial Models {#lineartwostageerror}

Throughout the exploratory analysis phase, our original research questions have guided our work, and now with modeling we return to familiar questions such as:

- are differences between charter and public non-charter schools (in intercept, in slope, in 2010 math score) statistically significant?
- are differences between school types statistically significant, even after accounting for school demographics and location?
- do charter schools offer any measurable benefit over non-charter public schools, either overall or within certain subgroups of schools based on demographics or location?

As you might expect, answers to these questions will arise from proper consideration of variability and properly identified statistical models.
As in Chapter \@ref(ch-multilevelintro), we will begin model fitting with some simple, preliminary models, in part to establish a baseline for evaluating larger models. Then, we can build toward a final model for inference by attempting to add important covariates, centering certain variables, and checking assumptions. 

### Unconditional Means Model {#modela}

In the multilevel context, we almost always begin with the **unconditional means model**, \index{unconditional means model} in which there are no predictors at any level. The purpose of the unconditional means model is to assess the amount of variation at each level, and to compare variability within school to variability between schools. Define $Y_{ij}$ as the MCA-II math score from school $i$ and year $j$. Using the composite model specification from Chapter \@ref(ch-multilevelintro):

\begin{equation*}
Y _{ij} = \alpha_{0} + u_{i} + \epsilon_{ij} \textrm{ with } u_{i} \sim N(0, \sigma^2_u) \textrm{ and } \epsilon_{ij} \sim N(0, \sigma^2)
\end{equation*}
the unconditional means model can be fit to the MCA-II data:


```r
#Model A (Unconditional means model)
model.a <- lmer(MathAvgScore~ 1 + (1|schoolid), 
                REML=T, data=chart.long)
```


```
##  Groups   Name        Variance Std.Dev.
##  schoolid (Intercept) 41.869   6.4706  
##  Residual             10.571   3.2514
```

```
##  Number of Level Two groups =  618
```

```
##             Estimate Std. Error  t value
## (Intercept)  652.746  0.2725853 2394.648
```

From this output, we obtain estimates of our three model parameters:

- $\hat{\alpha}_{0}$ = 652.7 = the mean math score across all schools and all years

- $\hat{\sigma}^2$= 10.6 = the variance in within-school deviations between individual scores and the school mean across all years

- $\hat{\sigma}^2_u$= 41.9 = the variance in between-school deviations between school means and the overall mean across all schools and all years

Based on the intraclass correlation coefficient:

\begin{equation*}
\hat{\rho}=\frac{\hat{\sigma}^2_u}{\hat{\sigma}^2_u + \hat{\sigma}^2} = \frac{41.869}{41.869+10.571}= 0.798
\end{equation*}
79.8\% of the total variation in math scores is attributable to differences among schools rather than changes over time within schools.  We can also say that the average correlation for any pair of responses from the same school is 0.798.

### Unconditional Growth Model {#modelb9}

The second model in most multilevel contexts introduces a covariate at Level One (see Model B in Chapter \@ref(ch-multilevelintro)). With longitudinal data, this second model introduces time as a predictor at Level One, but there are still no predictors at Level Two. This model is then called the **unconditional growth model**. \index{unconditional growth model} The unconditional growth model allows us to assess how much of the within-school variability can be attributed to systematic changes over time.

At the lowest level, we can consider building individual growth models over time for each of the 618 schools in our study. First, we must decide upon a form for each of our 618 growth curves. Based on our initial exploratory analyses, assuming that an individual school's MCA-II math scores follow a linear trend seems like a reasonable starting point. Under the assumption of linearity, we must estimate an intercept and a slope for each school, based on their 1-3 test scores over a period of three years.  Compared to time series analyses of economic data, most longitudinal data analyses have relatively few time periods for each subject (or school), and the basic patterns within subject are often reasonably described by simpler functional forms. 

Let $Y_{ij}$ be the math score of the $i^{th}$ school in year $j$. Then we can model the linear change in math test scores over time for School $i$ according to Model B:

\begin{equation*}
Y_{ij} = a_{i} + b_{i}\textrm{Year08}_{ij} + \epsilon_{ij} \textrm{ where } \epsilon_{ij} \sim N(0, \sigma^2)
\end{equation*}

The parameters in this model $(a_{i}, b_{i},$ and $\sigma^2)$ can be estimated through LLSR methods. $a_{i}$ represents the true intercept for School $i$---i.e., the expected test score level for School $i$ when time is zero (2008)---while $b_{i}$ represents the true slope for School $i$---i.e., the expected yearly rate of change in math score for School $i$ over the three-year observation period. Here we use Roman letters rather than Greek for model parameters since models by school will eventually be a conceptual first step in a multilevel model.  The $\epsilon_{ij}$ terms represent the deviation of School $i$'s actual test scores from the expected results under linear growth---the part of school $i$'s test score at time $j$ that is not explained by linear changes over time. The variability in these deviations from the linear model is given by $\sigma^2$.  In Figure \@ref(fig:lon-scat3), which illustrates a linear growth model for Norwood Central Middle School, $a_{i}$ is estimated by the $y$-intercept of the fitted regression line, $b_{i}$ is estimated by the slope of the fitted regression line, and $\sigma^2$ is estimated by the variability in the vertical distances between each point (the actual math score in year $j$) and the line (the predicted math score in year $j$).  

<div class="figure" style="text-align: center">
<img src="09-Two-Level-Longitudinal-Data_files/figure-epub3/lon-scat3-1.png" alt="Linear growth model for Norwood Central Middle School." width="60%" />
<p class="caption">(\#fig:lon-scat3)Linear growth model for Norwood Central Middle School.</p>
</div>

In a multilevel model, we let intercepts ($a_{i}$) and slopes ($b_{i}$) vary by school and build models for these intercepts and slopes using school-level variables at Level Two.  An unconditional growth model features no predictors at Level Two and can be specified either using formulations at both levels:

- Level One:
\begin{equation*}
Y_{ij}=a_{i}+b_{i}\textrm{Year08}_{ij} + \epsilon_{ij}
\end{equation*}

- Level Two:
\begin{align*}
a_{i}&=\alpha_{0} + u_{i}\\
b_{i}&=\beta_{0} + v_{i}
\end{align*}

or as a composite model:

\begin{equation*}
Y_{ij}=\alpha_{0} + \beta_{0}\textrm{Year08}_{ij}+u_{i}+v_{i}\textrm{Year08}_{ij} + \epsilon_{ij}
\end{equation*}
where $\epsilon_{ij}\sim N(0,\sigma^2)$ and

\[ \left[ \begin{array}{c}
            u_{i} \\ v_{i}
          \end{array}  \right] \sim N \left( \left[
          \begin{array}{c}
            0 \\ 0
          \end{array} \right], \left[
          \begin{array}{cc}
            \sigma_{u}^{2} & \\
            \sigma_{uv} & \sigma_{v}^{2}
          \end{array} \right] \right) . \]

As before, $\sigma^2$ quantifies the within-school variability (the scatter of points around schools' linear growth trajectories), while now the between-school variability is partitioned into variability in initial status $(\sigma^2_u)$ and variability in rates of change $(\sigma^2_v)$.

Using the composite model specification, the unconditional growth model can be fit to the MCA-II test data:


```r
#Model B (Unconditional growth)
model.b <- lmer(MathAvgScore~ year08 + (year08|schoolid), 
  REML=T, data=chart.long)
```


```
##  Groups   Name        Variance Std.Dev. Corr 
##  schoolid (Intercept) 39.44100 6.28021       
##           year08       0.11053 0.33246  0.723
##  Residual              8.82002 2.96985
```

```
##  Number of Level Two groups =  618
```

```
##               Estimate Std. Error    t value
## (Intercept) 651.407658 0.27933862 2331.96419
## year08        1.264953 0.08996996   14.05973
```

```
##  AIC =  10351.51 ;  BIC =  10384.26
```

From this output, we obtain estimates of our six model parameters:

- $\hat{\alpha}_{0}$ = 651.4 = the mean math score for the population of schools in 2008.
- $\hat{\beta}_{0}$ = 1.26 = the mean yearly change in math test scores for the population during the three-year observation period.
- $\hat{\sigma}^2$ = 8.82 = the variance in within-school deviations.
- $\hat{\sigma}^2_u$ = 39.4 = the variance between schools in 2008 scores.
- $\hat{\sigma}^2_v$ = 0.11 = the variance between schools in rates of change in math test scores during the three-year observation period.
- $\hat{\rho}_{uv}$ = 0.72 = the correlation in schools' 2008 math score and their rate of change in scores between 2008 and 2010.

We see that schools had a mean math test score of 651.4 in 2008 and their mean test scores tended to increase by 1.26 points per year over the three-year observation period, producing a mean test score at the end of three years of 653.9. According to the t-value (14.1), the increase in mean test scores noted during the three-year observation period is statistically significant.

The estimated within-school variance $\hat{\sigma}^2$ decreased by about 17\% from the unconditional means model, implying that 17\% of within-school variability in test scores can be explained by a linear increase over time:

\begin{align*}
\textrm{Pseudo }R^2_{L1} & = \frac{\hat{\sigma}^2(\textrm{uncond means}) - \hat{\sigma}^2(\textrm{uncond growth})}{\hat{\sigma^2}(\textrm{uncond means})} \\
 & = \frac{10.571-8.820}{10.571}= 0.17
\end{align*}

### Modeling Other Trends over Time {#othertimetrends}

While modeling linear trends over time is often a good approximation of reality, it is by no means the only way to model the effect of time. One alternative is to model the quadratic effect of time, which implies adding terms for both time and the square of time. Typically, to reduce the correlation between the linear and quadratic components of the time effect, the time variable is often centered first; we have already "centered" on 2008. Modifying Model B to produce an **unconditional quadratic growth model** would take the following form:

- Level One:
\begin{equation*}
Y_{ij}=a_{i}+b_{i}\textrm{Year08}_{ij}+c_{i}\textrm{Year08}^{2}_{ij} + \epsilon_{ij}
\end{equation*}

- Level Two:
\begin{align*}
a_{i} & = \alpha_{0} + u_{i}\\
b_{i} & = \beta_{0} + v_{i}\\
c_{i} & = \gamma_{0} + w_{i}
\end{align*}
where $\epsilon_{ij}\sim N(0,\sigma^2)$ and

\[ \left[ \begin{array}{c}
            u_{i} \\ v_{i} \\ w_{i}
          \end{array}  \right] \sim N \left( \left[
          \begin{array}{c}
            0 \\ 0 \\ 0
          \end{array} \right], \left[
          \begin{array}{ccc}
            \sigma_{u}^{2} & & \\
            \sigma_{uv} & \sigma_{v}^{2} & \\
            \sigma_{uw} & \sigma_{vw} & \sigma_{w}^{2}
          \end{array} \right] \right) . \]

With the extra term at Level One for the quadratic effect, we now have 3 equations at Level Two, and 6 variance components at Level Two (3 variance terms and 3 covariance terms).  However, with only a maximum of 3 observations per school, we lack the data for fitting 3 equations with error terms at Level Two. Instead, we could model the quadratic time effect with fewer variance components---for instance, by only using an error term on the intercept at Level Two:  
\begin{align*}
a_{i} & = \alpha_{0} + u_{i}\\
b_{i} & = \beta_{0}\\ 
c_{i} & = \gamma_{0}
\end{align*}
where $u_{i}\sim N(0,\sigma^2_u)$.  Models like this are frequently used in practice---they allow for a separate overall effect on test scores for each school, while minimizing parameters that must be estimated.  The tradeoff is that this model does not allow linear and quadratic effects to differ by school, but we have little choice here without more observations per school.  Thus, using the composite model specification, the unconditional quadratic growth model with random intercept for each school can be fit to the MCA-II test data:




```r
# Modeling quadratic time trend
model.b2 <- lmer(MathAvgScore~ yearc + yearc2 + (1|schoolid), 
  REML=T, data=chart.long)
```


```
##  Groups   Name        Variance Std.Dev.
##  schoolid (Intercept) 43.0504  6.5613  
##  Residual              8.5227  2.9194
```

```
##  Number of Level Two groups =  618
```

```
##               Estimate Std. Error     t value
## (Intercept) 651.942353 0.29229212 2230.447942
## yearc         1.269933 0.08757833   14.500542
## yearc2        1.068414 0.15046352    7.100817
```

```
##  AIC =  10308.16 ;  BIC =  10335.45
```

From this output, we see that the quadratic effect is positive and significant (t=7.1), in this case indicating that increases in test scores are greater between 2009 and 2010 than between 2008 and 2009.  Based on AIC and BIC values, the quadratic growth model outperforms the linear growth model with random intercepts only at level Two (AIC: 10308 vs. 10354; BIC: 10335 vs. 10375).  

Another frequently used approach to modeling time effects is the **piecewise linear model**.  In this model, the complete time span of the study is divided into two or more segments, with a separate slope relating time to the response in each segment.  In our case study there is only one piecewise option---fitting separate slopes in 2008-09 and 2009-10.  With only 3 time points, creating a piecewise linear model is a bit simplified, but this idea can be generalized to segments with more than two years each.  



The performance of this model is very similar to the quadratic growth model by AIC and BIC measures, and the story told by fixed effects estimates is also very similar.  While the mean yearly increase in math scores was 0.2 points between 2008 and 2009, it was 2.3 points between 2009 and 2010.  

Despite the good performances of the quadratic growth and piecewise linear models on our three-year window of data, we will continue to use linear growth assumptions in the remainder of this chapter.  Not only is a linear model easier to interpret and explain, but it's probably a more reasonable assumption in years beyond 2010.  Predicting future performance is more risky by assuming a steep one-year rise or a non-linear rise will continue, rather than by using the average increase over two years.

## Building to a Final Model {#finalmodel}

### Uncontrolled Effects of School Type {#sec:modelc9}

Initially, we can consider whether or not there are significant differences in individual school growth parameters (intercepts and slopes) based on school type. From a modeling perspective, we would build a system of two Level Two models:

\begin{align*}
a_{i} & = \alpha_{0} + \alpha_{1}\textrm{Charter}_i + u_{i} \\
b_{i} & = \beta_{0} + \beta_{1}\textrm{Charter}_i + v_{i}
\end{align*}
where $\textrm{Charter}_i=1$ if School $i$ is a charter school, and $\textrm{Charter}_i=0$ if School $i$ is a non-charter public school. In addition, the error terms at Level Two are assumed to follow a multivariate normal distribution:

\[ \left[ \begin{array}{c}
            u_{i} \\ v_{i}
          \end{array}  \right] \sim N \left( \left[
          \begin{array}{c}
            0 \\ 0
          \end{array} \right], \left[
          \begin{array}{cc}
            \sigma_{u}^{2} & \\
            \sigma_{uv} & \sigma_{v}^{2}
          \end{array} \right] \right) . \]

With a binary predictor at Level Two such as school type, we can write out what our Level Two model looks like for public non-charter schools and charter schools.

- Public schools

\begin{align*}
a_{i} & = \alpha_{0} + u_{i}\\
b_{i} & = \beta_{0} + v_{i},
\end{align*}

- Charter schools

\begin{align*}
a_{i} & = (\alpha_{0} + \alpha_{1}) + u_{i}\\
b_{i} & = (\beta_{0}+ \beta_{1}) + v_{i}
\end{align*}

Writing the Level Two model in this manner helps us interpret the model parameters from our two-level model. We can use statistical software (such as the `lmer()` function from the `lme4` package in R) to obtain parameter estimates using our $1733$ observations, after first converting our Level One and Level Two models into a composite model (Model C) with fixed effects and variance components separated:

\begin{align*}
Y_{ij} & = a_{i} + b_{i}\textrm{Year08}_{ij}+ \epsilon_{ij} \\
       & = (\alpha_{0} + \alpha_{1}\textrm{Charter}_i +u_{i}) + (\beta_{0} + \beta_{1}\textrm{Charter}_i + v_{i})\textrm{Year08}_{ij} + \epsilon_{ij} \\
       & = [\alpha_{0} + \beta_{0}\textrm{Year08}_{ij} +\alpha_{1}\textrm{Charter}_i+ \beta_{1}\textrm{Charter}_i\textrm{Year08}_{ij}] + \\
       & \quad [u_{i} + v_{i}\textrm{Year08}_{ij} + \epsilon_{ij}]
\end{align*}


```r
#Model C (uncontrolled effects of school type on 
#   intercept and slope)
model.c <- lmer(MathAvgScore~ charter + year08 + 
  charter:year08 + (year08|schoolid), 
  REML=T, data=chart.long)
```


```
##  Groups   Name        Variance Std.Dev. Corr 
##  schoolid (Intercept) 35.83192 5.98598       
##           year08       0.13115 0.36215  0.880
##  Residual              8.78445 2.96386
```

```
##  Number of Level Two groups =  618
```

```
##                   Estimate Std. Error     t value
## (Intercept)    652.0584390 0.28449366 2291.996410
## charter         -6.0184317 0.86561887   -6.952750
## year08           1.1970882 0.09427115   12.698351
## charter:year08   0.8557047 0.31429522    2.722614
```

```
##  AIC =  10307.6 ;  BIC =  10351.26
```

Armed with our parameter estimates, we can offer concrete interpretations:

- Fixed effects:

    - $\hat{\alpha}_{0} = 652.1.$ The estimated mean test score for 2008 for non-charter public schools is 652.1.
    - $\hat{\alpha}_{1}= -6.02.$ Charter schools have an estimated test score in 2008 which is 6.02 points lower than public non-charter schools.
    - $\hat{\beta}_{0}= 1.20.$ Public non-charter schools have an estimated mean increase in test scores of 1.20 points per year.
    - $\hat{\beta}_{1}= 0.86.$ Charter schools have an estimated mean increase in test scores of 2.06 points per year over the three-year observation period, 0.86 points higher than the mean yearly increase among public non-charter schools.

- Variance components:

    - $\hat{\sigma}_u= 5.99.$ The estimated standard deviation of 2008 test scores is 5.99 points, after controlling for school type.
    - $\hat{\sigma}_v= 0.36.$ The estimated standard deviation of yearly changes in test scores during the three-year observation period is 0.36 points, after controlling for school type.
    - $\hat{\rho}_{uv}= 0.88.$ The estimated correlation between 2008 test scores and yearly changes in test scores is 0.88, after controlling for school type.
    - $\hat{\sigma}= 2.96.$ The estimated standard deviation in residuals for the individual growth curves is 2.96 points.

Based on t-values reported by R, the effects of `year08` and `charter` both appear to be statistically significant, and there is also significant evidence of an interaction between `year08` and `charter`. Public schools had a significantly higher mean math score in 2008, while charter schools had significantly greater improvement in scores between 2008 and 2010 (although the mean score of charter schools still lagged behind that of public schools in 2010, as indicated in the graphical comparison of Models B and C in Figure \@ref(fig:lon-scat4)). Based on pseudo R-squared values, the addition of a charter school indicator to the unconditional growth model has decreased unexplained school-to-school variability in 2008 math scores by 4.7\%, while unexplained variability in yearly improvement actually increased slightly.  Obviously, it makes little sense that introducing an additional predictor would *reduce* the amount of variability in test scores explained, but this is an example of the limitations in the pseudo R-squared values discussed in Section \@ref(pseudoR2).



<div class="figure" style="text-align: center">
<img src="09-Two-Level-Longitudinal-Data_files/figure-epub3/lon-scat4-1.png" alt=" Fitted growth curves for Models B and C." width="60%" />
<p class="caption">(\#fig:lon-scat4) Fitted growth curves for Models B and C.</p>
</div>



### Add Percent Free and Reduced Lunch as a Covariate {#modeld}

Although we will still be primarily interested in the effect of school type on both 2008 test scores and rate of change in test scores (as we observed in Model C), we can try to improve our estimates of school type effects through the introduction of meaningful covariates. In this study, we are particularly interested in Level Two covariates---those variables which differ by school but which remain basically constant for a given school over time---such as urban or rural location, percentage of special education students, and percentage of students with free and reduced lunch. In Section \@ref(twostage9), we investigated the relationship between percent free and reduced lunch and a school's test score in 2008 and their rate of change from 2008 to 2010.

Based on these analyses, we will begin by adding percent free and reduced lunch as a Level Two predictor for both intercept and slope (Model D):

- Level One:
\begin{equation*}
Y_{ij}=a_{i} + b_{i}\textrm{Year08}_{ij} + \epsilon_{ij}
\end{equation*}

- Level Two:
\begin{align*}
a_{i} & = \alpha_{0} + \alpha_{1}\textrm{Charter}_i + \alpha_{2}\textrm{schpctfree}_i + u_{i}\\
b_{i} & = \beta_{0} + \beta_{1}\textrm{Charter}_i + \beta_{2}\textrm{schpctfree}_i + v_{i}
\end{align*}

The composite model is then:

\begin{align*}
Y_{ij}= [\alpha_{0}&+\alpha_{1}\textrm{Charter}_i +\alpha_{2}\textrm{schpctfree}_i + \beta_{0}\textrm{Year08}_{ij} \nonumber \\
 &+ \beta_{1}\textrm{Charter}_i\textrm{Year08}_{ij}  + \beta_{2}\textrm{schpctfree}_i\textrm{Year08}_{ij}] \nonumber \\ 
 &+ [u_{i} + v_{i}\textrm{Year08}_{ij} + \epsilon_{ij}]
\end{align*}
where error terms are defined as in Model C.


```r
#Model D2 (Introduce SchPctFree at level 2)
model.d2 <- lmer(MathAvgScore~ charter + SchPctFree + year08 + 
  charter:year08 + SchPctFree:year08 + (year08|schoolid),
  REML=T, data=chart.long)
```


```
##  Groups   Name        Variance Std.Dev. Corr 
##  schoolid (Intercept) 19.13217 4.37403       
##           year08       0.16036 0.40044  0.514
##  Residual              8.79802 2.96615
```

```
##  Number of Level Two groups =  618
```

```
##                      Estimate  Std. Error     t value
## (Intercept)       659.2784830 0.444689776 1482.558220
## charter            -3.4399398 0.712835646   -4.825712
## SchPctFree         -0.1665390 0.008907096  -18.697342
## year08              1.6413693 0.189499111    8.661620
## charter:year08      0.9807566 0.318583378    3.078493
## SchPctFree:year08  -0.0104086 0.003839183   -2.711149
```

```
##  AIC =  9988.308 ;  BIC =  10042.88
```


```r
drop_in_dev <- anova(model.d2, model.c, test = "Chisq")
```


```
         npar       AIC      BIC    logLik       dev    Chisq Df
model.c     8 10304.757 10348.42 -5144.378 10288.757       NA NA
model.d2   10  9967.305 10021.88 -4973.653  9947.305 341.4513  2
                                                                                        pval
model.c                                                                                   NA
model.d2 0.000000000000000000000000000000000000000000000000000000000000000000000000007158167
```

Compared to Model C, the introduction of school-level poverty based on percentage of students receiving free and reduced lunch in Model D leads to similar conclusions about the significance of the charter school effect on both the intercept and the slope, although the magnitude of these estimates changes after controlling for poverty levels.  The estimated gap in test scores between charter and non-charter schools in 2008 is smaller in Model D, while estimates of improvement between 2008 and 2010 increase for both types of schools.  Inclusion of free and reduced lunch reduces the unexplained variability between schools in 2008 math scores by 27\%, while unexplained variability in rates of change between schools again increases slightly based on pseudo R-squared values. A **likelihood ratio test** using maximum likelihood estimates illustrates that adding free and reduced lunch as a Level Two covariate significantly improves our model ($\chi^2 = 341.5, df=2, p<.001$).  Specific fixed effect parameter estimates are given below:

- $\hat{\alpha}_{0}= 659.3.$ The estimated mean math test score for 2008 is 659.3 for non-charter public schools with no students receiving free and reduced lunch.

- $\hat{\alpha}_{1}= -3.44.$ Charter schools have an estimated mean math test score in 2008 which is 3.44 points lower than non-charter public schools, controlling for effects of school-level poverty.

- $\hat{\alpha}_{2}= -0.17.$ Each 10\% increase in the percentage of students at a school receiving free and reduced lunch is associated with a 1.7 point decrease in mean math test scores for 2008, after controlling for school type.

- $\hat{\beta}_{0}= 1.64.$ Public non-charter schools with no students receiving free and reduced lunch have an estimated mean increase in math test score of 1.64 points per year during the three years of observation.

- $\hat{\beta}_{1}= 0.98.$ Charter schools have an estimated mean yearly increase in math test scores over the three-year observation period of 2.62, which is 0.98 points higher than the annual increase for public non-charter schools, after controlling for school-level poverty.

- $\hat{\beta}_{2}= -0.010.$ Each 10\% increase in the percentage of students at a school receiving free and reduced lunch is associated with a 0.10 point decrease in rate of change over the three years of observation, after controlling for school type.

### A Final Model with Three Level Two Covariates {#modelf9}

We now begin iterating toward a "final model" for these data, on which we will base conclusions. Being cognizant of typical features of a "final model" as outlined in Chapter \@ref(ch-multilevelintro), we offer one possible final model for this data---Model F:

- Level One:

\begin{equation*}
Y_{ij}= a_{i} + b_{i}\textrm{Year08}_{ij} + \epsilon_{ij}
\end{equation*}

- Level Two:

\begin{align*}
a_{i} & = \alpha_{0} + \alpha_{1}\textrm{Charter}_i + \alpha_{2}\textrm{urban}_i + \alpha_{3}\textrm{schpctsped}_i + \alpha_{4}\textrm{schpctfree}_i + u_{i} \\
b_{i} & = \beta_{0} + \beta_{1}\textrm{Charter}_i + \beta_{2}\textrm{urban}_i + \beta_{3}\textrm{schpctsped}_i + v_{i}
\end{align*}

where we find the effect of charter schools on 2008 test scores after adjusting for urban or rural location, percentage of special education students, and percentage of students that receive free or reduced lunch, and the effect of charter schools on yearly change between 2008 and 2010 after adjusting for urban or rural location and percentage of special education students.  We can use AIC and BIC criteria to compare Model F with Model D, since the two models are not nested.  By both criteria, Model F is significantly better than Model D: AIC of 9885 vs. 9988, and BIC of 9956 vs. 10043.  Based on the R output below, we offer interpretations for estimates of model fixed effects:


```r
model.f2 <- lmer(MathAvgScore ~ charter + urban + SchPctFree + 
  SchPctSped + charter:year08 + urban:year08 + 
  SchPctSped:year08 + year08 +
  (year08|schoolid), REML=T, data=chart.long)
```


```
##  Groups   Name        Variance   Std.Dev. Corr 
##  schoolid (Intercept) 16.9475588 4.116741      
##           year08       0.0047529 0.068941 0.845
##  Residual              8.8219701 2.970180
```

```
##  Number of Level Two groups =  618
```

```
##                       Estimate  Std. Error     t value
## (Intercept)       661.01041989 0.512888445 1288.799595
## charter            -3.22286480 0.698547425   -4.613666
## urban              -1.11382743 0.427566467   -2.605039
## SchPctFree         -0.15281371 0.008096391  -18.874300
## SchPctSped         -0.11770002 0.020612190   -5.710214
## year08              2.14429501 0.200866567   10.675221
## charter:year08      1.03087317 0.315158973    3.270962
## urban:year08       -0.52749174 0.186480338   -2.828672
## SchPctSped:year08  -0.04674212 0.010165862   -4.597949
```

```
##  AIC =  9884.646 ;  BIC =  9955.595
```

- $\hat{\alpha}_{0}= 661.0.$ The estimated mean math test score for 2008 is 661.0 for public schools in rural areas with no students qualifying for special education or free and reduced lunch.
- $\hat{\alpha}_{1}= -3.22.$ Charter schools have an estimated mean math test score in 2008 which is 3.22 points lower than non-charter public schools, after controlling for urban or rural location, percent special education, and percent free and reduced lunch.
- $\hat{\alpha}_{2}= -1.11.$ Schools in urban areas have an estimated mean math score in 2008 which is 1.11 points lower than schools in rural areas, after controlling for school type, percent special education, and percent free and reduced lunch.
- $\hat{\alpha}_{3}= -0.118.$ A 10\% increase in special education students at a school is associated with a 1.18 point decrease in estimated mean math score for 2008, after controlling for school type, urban or rural location, and percent free and reduced lunch.
- $\hat{\alpha}_{4}= -0.153.$ A 10\% increase in free and reduced lunch students at a school is associated with a 1.53 point decrease in estimated mean math score for 2008, after controlling for school type, urban or rural location, and percent special education.
- $\hat{\beta}_{0}= 2.14.$ Public non-charter schools in rural areas with no students qualifying for special education have an estimated increase in mean math test score of 2.14 points per year over the three-year observation period, after controlling for percent of students receiving free and reduced lunch.
- $\hat{\beta}_{1}= 1.03.$ Charter schools have an estimated mean annual increase in math score that is 1.03 points higher than public non-charter schools over the three-year observation period, after controlling for urban or rural location, percent special education, and percent free and reduced lunch.
- $\hat{\beta}_{2}= -0.53.$ Schools in urban areas have an estimated mean annual increase in math score that is 0.53 points lower than schools from rural areas over the three-year observation period, after controlling for school type, percent special education, and percent free and reduced lunch.
- $\hat{\beta}_{3}= -0.047.$ A 10\% increase in special education students at a school is associated with an estimated mean annual increase in math score that is 0.47 points lower over the three-year observation period, after controlling for school type, urban or rural location, and percent free and reduced lunch.

From this model, we again see that 2008 sixth grade math test scores from charter schools were significantly lower than similar scores from public non-charter schools, after controlling for school location and demographics.  However, charter schools showed significantly greater improvement between 2008 and 2010 compared to public non-charter schools, although charter school test scores were still lower than public school scores in 2010, on average.  We also tested several interactions between Level Two covariates and charter schools and found none to be significant, indicating that the 2008 gap between charter schools and public non-charter schools was consistent across demographic subgroups.  The faster improvement between 2008 and 2010 for charter schools was also consistent across demographic subgroups (found by testing three-way interactions). Controlling for school location and demographic variables provided more reliable and nuanced estimates of the effects of charter schools, while also providing interesting insights.  For example, schools in rural areas not only had higher test scores than schools in urban areas in 2008, but the gap grew larger over the study period given fixed levels of percent special education, percent free and reduced lunch, and school type.  In addition, schools with higher levels of poverty lagged behind other schools and showed no signs of closing the gap, and schools with higher levels of special education students had both lower test scores in 2008 and slower rates of improvement during the study period, again given fixed levels of other covariates.

As we demonstrated in this case study, applying multilevel methods to two-level longitudinal data yields valuable insights about our original research questions while properly accounting for the structure of the data.

### Parametric Bootstrap Testing {#longitudinal-paraboot}

We could further examine whether or not a simplified version of Model F, with fewer random effects, might be preferable.  For instance, consider testing whether we could remove $v_i$ in Model F in Section \@ref(modelf9), to create Model F0.  Removing $v_i$ is equivalent to setting $\sigma_{v}^{2} = 0$ and $\rho_{uv} = 0$.  We begin by comparing Model F (full model) to Model F0 (reduced model) using a likelihood ratio test:


```r
#Model F0 (remove 2 variance components from Model F)
model.f0 <- lmer(MathAvgScore ~ charter + urban + SchPctFree + 
  SchPctSped + charter:year08 + urban:year08 + 
  SchPctSped:year08 + year08 +
  (1|schoolid), REML=T, data=chart.long)
```


```
##  Groups   Name        Variance Std.Dev.
##  schoolid (Intercept) 17.4598  4.1785  
##  Residual              8.8254  2.9708
```

```
##  Number of Level Two groups =  618
```

```
##                      Estimate  Std. Error     t value
## (Intercept)       661.0259880 0.516905987 1278.812791
## charter            -3.2191845 0.705433198   -4.563415
## urban              -1.1185211 0.432039642   -2.588932
## SchPctFree         -0.1530190 0.008099538  -18.892315
## SchPctSped         -0.1181299 0.020798860   -5.679634
## year08              2.1392435 0.200974349   10.644361
## charter:year08      1.0315694 0.315510172    3.269528
## urban:year08       -0.5232986 0.186506710   -2.805790
## SchPctSped:year08  -0.0464524 0.010169161   -4.567968
```

```
##  AIC =  9880.977 ;  BIC =  9941.011
```


```r
drop_in_dev <- anova(model.f2, model.f0, test = "Chisq")
```


```
         npar      AIC      BIC    logLik      dev     Chisq Df      pval
model.f0   11 9853.571 9913.605 -4915.786 9831.571        NA NA        NA
model.f2   13 9857.234 9928.183 -4915.617 9831.234 0.3375994  2 0.8446781
```

When testing random effects at the boundary (such as $\sigma_{v}^{2} = 0$) or those with restricted ranges (such as $\rho_{uv} = 0$), using a chi-square distribution to conduct a likelihood ratio test is not appropriate. In fact, this will produce a conservative test, with p-values that are too large and not rejected enough (@Bryk2002, @Singer2003, @Faraway2005). For example, we should suspect that the p-value (.8447) produced by the likelihood ratio test comparing Models F and F0 is too large, that the real probability of getting a likelihood ratio test statistic of 0.3376 or greater when Model F0 is true is smaller than .8447. 

Researchers often use methods like the __parametric bootstrap__ \index{parametric bootstrap} to better approximate the distribution of the likelihood test statistic and produce more accurate p-values by simulating data under the null hypothesis.  Here are the basic steps for running a parametric bootstrap procedure to compare Model F0 with Model F (see associated diagram in Figure \@ref(fig:parabootdiagram)):

- Fit Model F0 (the null model) to obtain estimated fixed effects and variance components (this is the "parametric" part.)
- Use the estimated fixed effects and variance components from the null model to regenerate a new set of math test scores with the same sample size ($n=1733$) and associated covariates for each observation as the original data (this is the "bootstrap" part.)
- Fit both Model F0 (the reduced model) and Model F (the full model) to the new data
- Compute a likelihood ratio statistic comparing Models F0 and F
- Repeat the previous 3 steps many times (e.g., 1000)
- Produce a histogram of likelihood ratio statistics to illustrate its behavior when the null hypothesis is true
- Calculate a p-value by finding the proportion of times the bootstrapped test statistic is greater than our observed test statistic

<div class="figure">
<img src="data/ParametricBootstrapDiagram.png" alt="The steps in conducting a parametric bootstrap test comparing Models F and F0." width="100%" height="100%" />
<p class="caption">(\#fig:parabootdiagram)The steps in conducting a parametric bootstrap test comparing Models F and F0.</p>
</div>

Let's see how new test scores are generated under the parametric bootstrap. Consider, for instance, $i=1$ and $j=1,2,3$; that is, consider test scores for School #1 (Rippleside Elementary) across all three years (2008, 2009, and 2010).  Table \@ref(tab:rippleside) shows the original data for Rippleside Elementary.

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:rippleside)Original data for Rippleside Elementary (School 1).</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> schoolName </th>
   <th style="text-align:right;"> urban </th>
   <th style="text-align:right;"> charter </th>
   <th style="text-align:right;"> schPctsped </th>
   <th style="text-align:right;"> schPctfree </th>
   <th style="text-align:right;"> year08 </th>
   <th style="text-align:right;"> MathAvgScore </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> RIPPLESIDE ELEMENTARY </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.1176471 </td>
   <td style="text-align:right;"> 0.3627451 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 652.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RIPPLESIDE ELEMENTARY </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.1176471 </td>
   <td style="text-align:right;"> 0.3627451 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 656.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RIPPLESIDE ELEMENTARY </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.1176471 </td>
   <td style="text-align:right;"> 0.3627451 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 652.6 </td>
  </tr>
</tbody>
</table>

__Level Two__

One way to see the data generation process under the null model (Model F0) is to start with Level Two and work backwards to Level One. Recall that our Level Two models for $a_{i}$ and $b_{i}$, the true intercept and slope for school $i$, in Model F0 are:

\begin{align*}
a_{i} & = \alpha_{0} + \alpha_{1}\textrm{Charter}_i + \alpha_{2}\textrm{urban}_i + \alpha_{3}\textrm{schpctsped}_i + \alpha_{4}\textrm{schpctfree}_i + u_{i} \\
b_{i} & = \beta_{0} + \beta_{1}\textrm{Charter}_i + \beta_{2}\textrm{urban}_i + \beta_{3}\textrm{schpctsped}_i
\end{align*}

All the $\alpha$ and $\beta$ terms will be fixed at their estimated values, so the one term that will change for each bootstrapped data set is $u_{i}$. As we obtain a numeric value for $u_{i}$ for each school, we will fix the subscript. For example, if $u_{i}$ is set to -5.92 for School \#1, then we would denote this by $u_{1}=-5.92$. Similarly, in the context of Model F0, $a_{1}$ represents the 2008 math test score for School \#1, where $u_{1}$ quantifies how School \#1's 2008 score differs from the average 2008 score across all schools with the same attributes: charter status, urban or rural location, percent of special education students, and percent of free and reduced lunch students.

According to Model F0, each $u_{i}$ is sampled from a normal distribution with mean 0 and standard deviation 4.18. That is, a random component to the intercept for School \#1 ($u_{1}$) would be sampled from a normal distribution with mean 0 and SD 4.18; say, for instance, $u_{1}=-5.92$. We would sample $u_{2},...,u_{618}$ in a similar manner for all 618 schools.  Then we can produce a model-based intercept and slope for School \#1:

\begin{align*}
a_{1} & = 661.03-3.22(0)-1.12(0)-0.12(11.8)-0.15(36.3)-5.92 = 648.2 \\
b_{1} & = 2.14+1.03(0)-0.52(0)-.046(11.8) = 1.60
\end{align*}

Notice a couple of features of the above derivations. First, all of the coefficients from the above equations ($\alpha_{0}=661.03$, $\alpha_{1}=-3.22$, etc.) come from the estimated fixed effects from Model F0. Second, "public non-charter" is the reference level for `charter` and "rural" is the reference level for `urban`, so both of those predictors are 0 for Rippleside Elementary. Third, the mean intercept (2008 test scores) for schools like Rippleside that are rural and public non-charter, with 11.8\% special education students and 36.3\% free and reduced lunch students, is 661.03 - 0.12(11.8) - 0.15(36.3) = 654.2.  The mean yearly improvement in test scores for rural, public non-charter schools with 11.8\% special education students is then 1.60 points per year (2.14 - .046*11.8).  School \#1 (Rippleside) therefore has a 2008 test score that is 5.92 points below the mean for all similar schools, but every such school is assumed to have the same improvement rate in test scores of 1.60 points per year because of our assumption that there is no school-to-school variability in yearly rate of change (i.e., $v_{i}=0$).

__Level One__

We next proceed to Level One, where the scores from Rippleside are modeled as a linear function of year ($654.2 + 1.60\textrm{Year08}_{ij}$) with a normally distributed residual $\epsilon_{1k}$ at each time point $k$. Three residuals (one for each year) are sampled independently from a normal distribution with mean 0 and standard deviation 2.97 -- the standard deviation again coming from parameter estimates from fitting Model F0 to the actual data. Suppose we obtain residuals of $\epsilon_{11}=-3.11$, $\epsilon_{12}=1.19$, and $\epsilon_{13}=2.41$.  In that case, our parametrically generated data for Rippleside Elementary (School \#1) would look like:

\[ \begin{array}{rcccl}
   Y_{11} & = & 654.2+1.60(0)-3.11 & = & 651.1 \\
   Y_{12} & = & 654.2+1.60(1)+1.19 & = & 657.0 \\
   Y_{13} & = & 654.2+1.60(2)+2.41 & = & 659.8 \\
   \end{array} \]

We would next turn to School \#2 ($i=2$)---Wrenshall Elementary. Fixed effects would remain the same but covariates would change, as Wrenshall has 15.2\% special education students and 42.4\% free and reduced lunch students. We would, however, sample a new residual $u_{2}$ at Level Two, producing a different intercept $a_{2}$ than observed for School \#1. Three new independent residuals $\epsilon_{2k}$ would also be selected at Level One, from the same normal distribution as before with mean 0 and standard deviation 2.97.

Once an entire set of simulated scores for every school and year have been generated based on Model F0, two models are fit to this data:

- Model F0 -- the correct (null) model that was actually used to generate the responses
- Model F -- the incorrect (full) model that contains two extra variance components -- $\sigma_{v}^{2}$ and $\sigma_{uv}$ -- that were not actually used when generating the responses




```r
# Generate 1 set of bootstrapped data and run chi-square test
#  (will also work if use REML models, but may take longer)
set.seed(3333)
d <- drop(simulate(model.f0ml))
m2 <-refit(model.f2ml, newresp=d)
m1 <-refit(model.f0ml, newresp=d)
drop_in_dev <- anova(m2, m1, test = "Chisq")
```


```
   npar      AIC      BIC    logLik      dev    Chisq Df      pval
m1   11 9891.186 9951.220 -4934.593 9869.186       NA NA        NA
m2   13 9890.605 9961.554 -4932.302 9864.605 4.580989  2 0.1012164
```

A likelihood ratio test statistic is calculated comparing Model F0 to Model F. For example, after continuing as above to generate new $Y_{ij}$ values corresponding to all 1733 score observations, we fit both models to the "bootstrapped" data.  Since the data was generated using Model F0, we would expect the two extra terms in Model F ($\sigma^2_{v}$ and $\sigma_{uv}$) to contribute very little to the quality of the fit; Model F will have a slightly larger likelihood and loglikelihood since it contains every parameter from Model F0 plus two more, but the difference in the likelihoods should be due to chance. In fact, that is what the output above shows. Model F does have a larger loglikelihood than Model F0 (-4932 vs. -4935), but this small difference is not statistically significant based on a chi-square test with 2 degrees of freedom (p=.1012).

However, we are really only interested in saving the likelihood ratio test statistic from this bootstrapped sample ($2*(-4932) - (-4935) = 4.581$). By generating ("bootstrapping") many sets of responses based on estimated parameters from Model F0 and calculating many likelihood ratio test statistics, we can observe how this test statistic behaves under the null hypothesis of $\sigma_{v}^{2} = \sigma_{uv} = 0$, rather than making the (dubious) assumption that its behavior is described by a chi-square distribution with 2 degrees of freedom.  Figure \@ref(fig:paraboot9) illustrates the null distribution of the likelihood ratio test statistic derived by the parametric bootstrap procedure as compared to a chi-square distribution. A p-value for comparing our full and reduced models can be approximated by finding the proportion of likelihood ratio test statistics generated under the null model which exceed our observed likelihood ratio test (0.3376). The parametric bootstrap provides a more reliable p-value in this case (.578 from table below); a chi-square distribution puts too much mass in the tail and not enough near 0, leading to overestimation of the p-value. Based on this test, we would still choose our simpler Model F0.


```r
bootstrapAnova(mA=model.f2ml, m0=model.f0ml, B=1000)
```


```
   npar    logLik      dev     Chisq Df pval_boot
m0   11 -4915.786 9831.571        NA NA        NA
mA   13 -4915.617 9831.234 0.3375992  2     0.578
```

<div class="figure" style="text-align: center">
<img src="09-Two-Level-Longitudinal-Data_files/figure-epub3/paraboot9-1.png" alt="Null distribution of likelihood ratio test statistic derived using parametric bootstrap (histogram) compared to a chi-square distribution with 2 degrees of freedom (smooth curve).  The vertical line represents the observed likelihood ratio test statistic." width="60%" />
<p class="caption">(\#fig:paraboot9)Null distribution of likelihood ratio test statistic derived using parametric bootstrap (histogram) compared to a chi-square distribution with 2 degrees of freedom (smooth curve).  The vertical line represents the observed likelihood ratio test statistic.</p>
</div>

Another way of examining whether or not we should stick with the reduced model or reject it in favor of the larger model is by generating parametric bootstrap samples, and then using those samples to produce 95\% confidence intervals for both $\rho_{uv}$ and $\sigma_{v}$. 


```r
bootciF = confint(model.f2, method="boot", oldNames=F)
bootciF
```

```
##                                        2.5 %       97.5 %
## sd_(Intercept)|schoolid           3.80184379   4.50865387
## cor_year08.(Intercept)|schoolid  -1.00000000   1.00000000
## sd_year08|schoolid                0.00920325   0.91063778
## sigma                             2.77939260   3.07775590
## (Intercept)                     660.07199594 662.04727509
## charter                          -4.58861102  -2.07372480
## urban                            -2.03160030  -0.29151544
## SchPctFree                       -0.16942601  -0.13737688
## SchPctSped                       -0.15606471  -0.07548349
## year08                            1.72245669   2.56105679
## charter:year08                    0.44913855   1.65928305
## urban:year08                     -0.90594030  -0.17156435
## SchPctSped:year08                -0.06698487  -0.02616520
```

From the output above, the 95\% bootstrapped confidence interval for $\rho_{uv}$ (-1, 1) contains 0, and the interval for $\sigma_{v}$ (0.0092032, 0.9106378) nearly contains 0, providing further evidence that the larger model is not needed.

In this section, we have offered the parametric bootstrap as a noticeable improvement over the likelihood ratio test with an approximate chi-square distribution for testing random effects, especially those near a boundary. Typically when we conduct hypothesis tests involving variance terms we are testing at the boundary, since we are asking if the variance term is really necessary (i.e., $H_0: \sigma^2=0$ vs. $H_A: \sigma^2 > 0$). However, what if we are conducting a hypothesis test about a fixed effect? For the typical test of whether or not a fixed effect is significant -- e.g., $H_0: \alpha_i=0$ vs. $H_A: \alpha_i \neq 0$ -- we are *not* testing at the boundary, since most fixed effects have no bounds on allowable values. We have often used a likelihood ratio test with an approximate chi-square distribution in these settings.  Does that provide accurate p-values? Although some research (e.g., @Faraway2005) shows that p-values of fixed effects from likelihood ratio tests can tend to be anti-conservative (too low), in general the approximation is not bad. We will continue to use the likelihood ratio test with a chi-square distribution for fixed effects, but you could always check your p-values using a parametric bootstrap approach.

## Covariance Structure among Observations {#errorcovariance}

Part of our motivation for framing our model for multilevel data was to account for the correlation among observations made on the same school (the Level Two observational unit). Our two-level model, through error terms on both Level One and Level Two variables, actually implies a specific within-school covariance structure \index{covariance structure} among observations, yet we have not (until now) focused on this imposed structure.  For example:

- What does our two-level model say about the relative variability of 2008 and 2010 scores from the same school?
- What does it say about the correlation between 2008 and 2009 scores from the same school?

In this section, we will describe the within-school covariance structure imposed by our two-level model and offer alternative covariance structures that we might consider, especially in the context of longitudinal data.  In short, we will discuss how we might decide if our implicit covariance structure in our two-level model is satisfactory for the data at hand.  Then, in the succeeding optional section, we provide derivations of the imposed within-school covariance structure for our standard two-level model using results from probability theory.

### Standard Covariance Structure {#standarderror}

We will use Model C (uncontrolled effects of school type) to illustrate covariance structure within subjects. Recall that, in composite form, Model C is:

\begin{align*}
Y_{ij} & = a_{i}+b_{i}\textrm{Year08}_{ij}+ \epsilon_{ij} \\
       & = (\alpha_{0}+ \alpha_{1}\textrm{Charter}_i + u_{i}) + (\beta_{0}+\beta_{1}\textrm{Charter}_i +v_{i}) \textrm{Year08}_{ij} + \epsilon_{ij} \\
       & = [\alpha_{0}+\alpha_{1}\textrm{Charter}_i + \beta_{0}\textrm{Year08}_{ij} + \beta_{1}\textrm{Charter}_i\textrm{Year08}_{ij}] + [u_{i} \\
       & \quad + v_{i}\textrm{Year08}_{ij} + \epsilon_{ij}]
\end{align*}
\noindent where $\epsilon_{ij}\sim N(0,\sigma^2)$ and

\[ \left[ \begin{array}{c}
            u_{i} \\ v_{i}
          \end{array}  \right] \sim N \left( \left[
          \begin{array}{c}
            0 \\ 0
          \end{array} \right], \left[
          \begin{array}{cc}
            \sigma_{u}^{2} & \\
            \sigma_{uv} & \sigma_{v}^{2}
          \end{array} \right] \right) . \]

For School $i$, the covariance structure for the three time points has general form:

\[ Cov(\mathbf{Y}_i) =  \left[
          \begin{array}{cccc}
            Var(Y_{i1}) & Cov(Y_{i1},Y_{i2}) & Cov(Y_{i1},Y_{i3}) \\
            Cov(Y_{i1},Y_{i2}) & Var(Y_{i2}) & Cov(Y_{i2},Y_{i3}) \\
            Cov(Y_{i1},Y_{i3}) & Cov(Y_{i2},Y_{i3}) & Var(Y_{i3})
          \end{array} \right] \]
where, for instance, $Var(Y_{i1})$ is the variability in 2008 test scores (time $j=1$), $Cov(Y_{i1},Y_{i2})$ is the covariance between 2008 and 2009 test scores (times $j=1$ and $j=2$), etc.  Since covariance measures the tendency of two variables to move together, we expect positive values for all three covariance terms in $Cov(\mathbf{Y}_i)$, since schools with relatively high test scores in 2008 are likely to also have relatively high test scores in 2009 or 2010.  The correlation between two variables then scales covariance terms to values between -1 and 1, so by the same rationale, we expect correlation coefficients between two years to be near 1.  If observations within school were independent---that is, knowing a school had relatively high scores in 2008 tells nothing about whether that school will have relatively high scores in 2009 or 2010---then we would expect covariance and correlation values near 0.

It is important to notice that the error structure at Level Two is *not* the same as the within-school covariance structure among observations.  That is, the relationship between $u_{i}$ and $v_{i}$ from the Level Two equations is not the same as the relationship between test scores from different years at the same school (e.g., the relationship between $Y_{i1}$ and $Y_{i2}$).  In other words,

\[ Cov(\mathbf{Y}_i) \neq \left[ \begin{array}{c}
            u_{i} \\ v_{i}
          \end{array}  \right] \sim N \left( \left[
          \begin{array}{c}
            0 \\ 0
          \end{array} \right], \left[
          \begin{array}{cc}
            \sigma_{u}^{2} & \\
            \sigma_{uv} & \sigma_{v}^{2}
          \end{array} \right] \right) . \]
Yet, the error structure and the covariance structure *are* connected to each other, as we will now explore.

Using results from probability theory (see Section \@ref(optionalcov)), we can show that:

\begin{align*}
Var(Y_{ij}) & = \sigma_{u}^{2} + t^{2}_{ij} \sigma_{v}^{2} + \sigma^{2} + 2t_{ij}\sigma_{uv}, \\
Cov(Y_{ij},Y_{ik}) & = \sigma_{u}^{2} + t_{ij}t_{ik} \sigma_{v}^{2} + (t_{ij}+t_{ik})\sigma_{uv}
\end{align*}
for all $i$, where our time variable (`year08`) has values $t_{i1}=0$, $t_{i2}=1$, and $t_{i3}=2$ for every School $i$.  Intuitively, these formulas are sensible.  For instance, $Var(Y_{i1})$, the uncertainty (variability) around a school's score in 2008, increases as the uncertainty in intercepts and slopes increases, as the uncertainty around that school's linear time trend increases, and as the covariance between intercept and slope residuals increases (since if one is off, the other one is likely off as well).  Also, $Cov(Y_{i1},Y_{i2})$, the covariance between 2008 and 2009 scores, does not depend on Level One error.  Thus, in the 3-by-3 within-school covariance structure of the charter schools case study, our standard two-level model determines all 6 covariance matrix elements through the estimation of four parameters ($\sigma_{u}^{2}, \sigma_{uv}, \sigma_{v}^{2}, \sigma^2$) and the imposition of a specific structure related to time.



To obtain estimated variances for individual observations and covariances between two time points from the same school, we can simply plug estimated variance components from our two-level model along with time points from our data collection into the equations above.  For instance, in Section \@ref(sec:modelc9), we obtained the following estimates of variance components:  $\hat{\sigma}^{2}=8.784$, $\hat{\sigma}^{2}_{u}=35.832$, $\hat{\sigma}^{2}_{v}=0.131$, and $\hat{\sigma}_{uv}=\hat{\rho}\hat{\sigma_{u}}\hat{\sigma_{v}}=1.907$.  Therefore, our estimated within-school variances for the three time points would be:

\begin{align*}
\hat{Var}(Y_{i1}) & = 35.832 + 0^{2} 0.131 + 8.784 + 2(0)1.907 = 44.62 \\
\hat{Var}(Y_{i2}) & = 35.832 + 1^{2} 0.131 + 8.784 + 2(1)1.907 = 48.56 \\
\hat{Var}(Y_{i3}) & = 35.832 + 2^{2} 0.131 + 8.784 + 2(2)1.907 = 52.77
\end{align*}
\noindent and our estimated within-school covariances between different time points would be:

\begin{align*}
\hat{Cov}(Y_{i1},Y_{i2}) & = 35.832 + (0)(1)0.131 + (0+1)1.907 = 37.74 \\
\hat{Cov}(Y_{i1},Y_{i3}) & = 35.832 + (0)(2)0.131 + (0+2)1.907 = 39.65 \\
\hat{Cov}(Y_{i2},Y_{i3}) & = 35.832 + (1)(2)0.131 + (1+2)1.907 = 41.81
\end{align*}
In fact, these values will be identical for every School $i$, since scores were assessed at the same three time points.  Thus, we will drop the subscript $i$ moving forward.  

Written in matrix form, our two-level model implicitly imposes this estimated covariance structure on within-school observations for any specific School $i$:

\[ \hat{Cov}(\mathbf{Y}) =  \left[
          \begin{array}{cccc}
            44.62 & &   \\
            37.74 & 48.56 &  \\
            39.65 & 41.81 & 52.77
          \end{array} \right] \]
and this estimated covariance matrix can be converted into an estimated within-school correlation matrix using the identity $Corr(Y_{1},Y_{2})=\frac{Cov(Y_{1},Y_{2})}{\sqrt{Var(Y_{1}) Var(Y_{2})}}$:

\[ \hat{Corr}(\mathbf{Y}) =  \left[
          \begin{array}{cccc}
            1 & &   \\
            .811 & 1 &  \\
            .817 & .826 & 1
          \end{array} \right] \]

A couple of features of these two matrices can be highlighted that offer insights into implications of our standard two-level model on the covariance structure among observations at Level One from the same school:

- Many longitudinal data sets show higher correlation for observations that are closer in time.  In this case, we see that correlation is very consistent between all pairs of observations from the same school; the correlation between test scores separated by two years (.817) is approximately the same as the correlation between test scores separated by a single year (.811 for 2008 and 2009 scores; .826 for 2009 and 2010 scores).
- Many longitudinal data sets show similar variability at all time points.  In this case, the variability in 2010 (52.77) is about 18\% greater than the variability in 2008 (44.62), while the variability in 2009 is in between (48.56).
- Our two-level model actually imposes a quadratic structure on the relationship between variance and time; note that the equation for $Var(Y_{j})$ contains both $t^{2}_{j}$ and $t_{j}$.  The variance is therefore minimized at $t=\frac{-\sigma_{uv}}{\sigma^{2}_{v}}$.  With the charter school data, the variance in test scores is minimized when $t=\frac{-\sigma_{uv}}{\sigma^{2}_{v}}=\frac{-1.907}{0.131}=-14.6$; that is, the smallest within-school variance in test scores is expected 14.6 years prior to 2008 (i.e., about 1994), and the variance increases parabolically from there.  In general, cases in which $\sigma^{2}_{v}$ and $\sigma_{uv}$ are relatively small have little curvature and fairly consistent variability over time.
- There is no requirement that time points within school need to be evenly spaced or even that each school has an equal number of measurements over time, which makes the two-level model structure nicely flexible.

### Alternative Covariance Structures {#alternateerror}

The standard covariance structure that's implied by our multilevel modeling structure provides a useful model in a wide variety of situations---it provides a reasonable model for Level One variability with a relatively small number of parameters, and it has sufficient flexibility to accommodate irregular time intervals as well as subjects with a different number of observations over time.  However, there may be cases in which a better fitting model requires additional parameters, or when a simpler model with fewer parameters still provides a good fit to the data.  Here is an outline of a few alternative error structures:

- _Unstructured_ - Every variance and covariance term for observations within a school is a separate parameter and is therefore estimated uniquely; no patterns among variances or correlations are assumed.  This structure offers maximum flexibility but is most costly in terms of parameters estimated.
- _Compound symmetry_ - Assume variance is constant across all time points and correlation is constant across all pairs of time points.  This structure is highly restrictive but least costly in terms of parameters estimated.
- _Autoregressive_ - Assume variance is constant across all time points, but correlation drops off in a systematic fashion as the gap in time increases.  Autoregressive models expand compound symmetry by allowing for a common structure where points closest in time are most highly correlated.
- _Toeplitz_ - Toeplitz is similar to the autoregressive model, except that it does not impose any structure on the decline in correlation as time gaps increase.  Thus, it requires more parameters to be estimated than the autoregressive model while providing additional flexibility.
- _Heterogeneous variances_ - The assumption that variances are equal across time points found in the compound symmetry, autoregressive, and Toeplitz models can be relaxed by introducing additional parameters to allow unequal (heterogeneous) variances.

When the focus of an analysis is on stochastic parameters (variance components) rather than fixed effects, parameter estimates are typically based on restricted maximum likelihood (REML) methods; model performance statistics then reflect only the stochastic portion of the model. Models with the same fixed effects but different covariance structures can be compared as usual---with AIC and BIC measures when models are not nested and with likelihood ratio tests when models are nested.  However, using a chi-square distribution to conduct a likelihood ratio test in these cases can often produce a conservative test, with p-values that are too large and not rejected enough (@Bryk2002; @Singer2003; @Faraway2005). In Section \@ref(longitudinal-paraboot), we introduced the parametric bootstrap as a potentially better way of testing models nested in their random effects.

### Non-longitudinal Multilevel Models

Careful modeling and estimation of the Level One covariance matrix is especially important and valuable for longitudinal data (with time at Level One) and as we've seen, our standard two-level model has several nice properties for this purpose.  The standard model is also often appropriate for non-longitudinal multilevel models as discussed in Chapter \@ref(ch-multilevelintro), although we must remain aware of the covariance structure implicitly imposed.  In other words, the ideas in this section generalize even if time isn't a Level One covariate.

As an example, in Case Study \@ref(cs:music) where Level One observational units are musical performances rather than time points, the standard model implies the following covariance structure for Musician $i$ in Model C, which uses an indicator for large ensembles as a Level One predictor:

\begin{align*}
Var(Y_{ij}) & = \sigma_{u}^{2} + \textrm{Large}^{2}_{ij} \sigma_{v}^{2} + \sigma^{2} + 2\textrm{Large}_{ij}\sigma_{uv} \\
 & = \left\{ \begin{array}{ll}
                 \sigma^{2} + \sigma_{u}^{2} & \mbox{if $\textrm{Large}_{ij}=0$} \\
                 \sigma^{2} + \sigma_{u}^{2} + \sigma_{v}^{2} + 2\sigma_{uv} & \mbox{if $\textrm{Large}_{ij}=1$}
               \end{array}
       \right.
\end{align*}
\noindent and

\begin{align*}
Cov(Y_{ij},Y_{ik}) & = \sigma_{u}^{2} + \textrm{Large}_{ij}\textrm{Large}_{ik} \sigma_{v}^{2} + (\textrm{Large}_{ij} + 
  \textrm{Large}_{ik}) \sigma_{uv} \\
 & = \left\{ \begin{array}{ll}
                 \sigma_{u}^{2} & \mbox{if $\textrm{Large}_{ij}=\textrm{Large}_{ik}=0$} \\
                 \sigma_{u}^{2} + \sigma_{uv} & \mbox{if $\textrm{Large}_{ij}=0$, $\textrm{Large}_{ik}=1$ or vice versa} \\
                 \sigma_{u}^{2} + \sigma_{v}^{2} + 2\sigma_{uv} & \mbox{if $\textrm{Large}_{ij}=\textrm{Large}_{ik}=1$}
               \end{array}
       \right.
\end{align*}
Note that, in the Music Performance Anxiety case study, each subject will have a unique Level One variance-covariance structure, since each subject has a different number of performances and a different mix of large ensemble and small ensemble or solo performances.

### Final Thoughts Regarding Covariance Structures

In the charter school example, as is often true in multilevel models, the choice of covariance matrix does not greatly affect estimates of fixed effects.  The choice of covariance structure could potentially impact the standard errors of fixed effects, and thus the associated test statistics, but the impact appears minimal in this particular case study.  In fact, the standard model typically works very well.  So is it worth the time and effort to accurately model the covariance structure?  If primary interest is in inference regarding fixed effects, and if the standard errors for the fixed effects appear robust to choice of covariance structure, then extensive time spent modeling the covariance structure is not advised.  However, if researchers are interested in predicted random effects and estimated variance components in addition to estimated fixed effects, then choice of covariance structure can make a big difference.  For instance, if researchers are interested in drawing conclusions about particular schools rather than charter schools in general, they may more carefully model the covariance structure in this study.

### Details of Covariance Structures (optional) {#optionalcov}

Using Model C as specified in Section \@ref(standarderror), we specified the general covariance structure for School $i$ as:

\[ Cov(\mathbf{Y}_i) =  \left[
          \begin{array}{cccc}
            Var(Y_{i1}) & Cov(Y_{i1},Y_{i2}) & Cov(Y_{i1},Y_{i3}) \\
            Cov(Y_{i1},Y_{i2}) & Var(Y_{i2}) & Cov(Y_{i2},Y_{i3}) \\
            Cov(Y_{i1},Y_{i3}) & Cov(Y_{i2},Y_{i3}) & Var(Y_{i3})
          \end{array} \right] \]
If $Y_1 = a_1 X_1 + a_2 X_2 + a_3$ and $Y_2 = b_1 X_1 + b_2 X_2 + b_3$ where $X_1$ and $X_2$ are random variables and $a_i$ and $b_i$ are constants for $i=1,2,3$, then we know from probability theory that:

\begin{align*}
Var(Y_1) & = a^{2}_{1} Var(X_1) + a^{2}_{2} Var(X_2) + 2 a_1 a_2 Cov(X_1,X_2) \\
Cov(Y_1,Y_2) & = a_1 b_1 Var(X_1) + a_2 b_2 Var(X_2) + (a_1 b_2 + a_2 b_1) Cov(X_1,X_2)
\end{align*}
\noindent Applying these identities to Model C, we first see that we can ignore all fixed effects, since they do not contribute to the variability.  Thus,

\begin{align*}
Var(Y_{ij}) & = Var(u_{i}+v_{i}\textrm{Year08}_{ij}+\epsilon_{ij}) \\
 & = Var(u_{i}) + \textrm{Year08}^{2}_{ij} Var(v_{i}) + Var(\epsilon_{ij}) + 2\textrm{Year08}_{ij} Cov(u_{i},v_{i}) \\
 & = \sigma_{u}^{2} + \textrm{Year08}^{2}_{ij} \sigma_{v}^{2} + \sigma^{2} + 2\textrm{Year08}_{ij}\sigma_{uv} \\
 & = \sigma_{u}^{2} + t^{2}_{j} \sigma_{v}^{2} + \sigma^{2} + 2t_{j}\sigma_{uv}
\end{align*}
\noindent where the last line reflects the fact that observations were taken at the same time points for all schools.  We can derive the covariance terms in a similar fashion:

\begin{align*}
Cov(Y_{ij},Y_{ik}) & = Cov(u_{i}+ v_{i}\textrm{Year08}_{ij}+\epsilon_{ij}, u_{i}+v_{i}\textrm{Year08}_{ik}+\epsilon_{ik}) \\
 & = Var(u_{i}) + \textrm{Year08}_{ij}\textrm{Year08}_{ik} Var(v_{i}) + \\
 & \qquad (\textrm{Year08}_{ij} + \textrm{Year08}_{ik}) Cov(u_{i},v_{i}) \\
 & = \sigma_{u}^{2} + t_{j}t_{k} \sigma_{v}^{2} + (t_{j}+t_{k})\sigma_{uv}
\end{align*}

In Model C, we obtained the following estimates of variance components:  $\hat{\sigma}^{2}=8.784$, $\hat{\sigma}^{2}_{u}=35.832$, $\hat{\sigma}^{2}_{v}=0.131$, and $\hat{\sigma}_{uv}=\hat{\rho}\hat{\sigma_{u}}\hat{\sigma_{v}}=1.907$.  Therefore, our two-level model implicitly imposes this covariance structure on within-subject observations:

\[ Cov(\mathbf{Y}_i) =  \left[
          \begin{array}{cccc}
            44.62 & &   \\
            37.74 & 48.56 &  \\
            39.65 & 41.81 & 52.77
          \end{array} \right] \]
and this covariance matrix can be converted into a within-subject correlation matrix:

\[ Corr(\mathbf{Y}_i) =  \left[
          \begin{array}{cccc}
            1 & &   \\
            .811 & 1 &  \\
            .817 & .826 & 1
          \end{array} \right] \]

## Notes on Using R (optional) {#notesr9}

The model below is our final model with $\sigma_{uv}$ set to 0---i.e., we have added the restriction that Level Two error terms are uncorrelated.  Motivation for this restriction came from repeated estimates of correlation in different versions of the final model near 1, when empirically a slightly negative correlation might be expected.  As we will describe in Chapter \@ref(ch-3level), inclusion of the Level Two correlation as a model parameter appears to lead to boundary constraints---maximum likelihood parameter estimates near the maximum or minimum allowable value for a parameter. A likelihood ratio test using full maximum likelihood estimates confirms that the inclusion of a correlation term does not lead to an improved model (LRT test statistic = .223 on 1 df, $p=.637$); a parametric bootstrap test provides a similar result and is more trustworthy when testing a hypothesis about a variance component.  Estimates of fixed effects and their standard errors are extremely consistent with the full model in Section \@ref(modelf9); only the estimate of the variability in $\sigma_{1}$ is noticeably higher.


```r
# Modified final model
model.f2a <- lmer(MathAvgScore ~ charter + urban + SchPctFree +
  SchPctSped + charter:year08 + urban:year08 +
  SchPctSped:year08 + year08 +
  (1|schoolid) + (0+year08|schoolid), REML=T, data=chart.long)
```


```
##  Groups     Name        Variance Std.Dev.
##  schoolid   (Intercept) 17.35510 4.16595 
##  schoolid.1 year08       0.11383 0.33738 
##  Residual                8.71637 2.95235
```

```
##  Number of Level Two groups =  618
```

```
##                       Estimate  Std. Error     t value
## (Intercept)       661.01769691 0.515461147 1282.381224
## charter            -3.22468385 0.703173543   -4.585900
## urban              -1.11662897 0.430421958   -2.594266
## SchPctFree         -0.15294540 0.008096443  -18.890443
## SchPctSped         -0.11777062 0.020739096   -5.678677
## year08              2.14270674 0.202089529   10.602760
## charter:year08      1.03341248 0.317173866    3.258189
## urban:year08       -0.52441964 0.187678333   -2.794247
## SchPctSped:year08  -0.04672235 0.010218935   -4.572135
```

```
##  AIC =  9882.823 ;  BIC =  9948.314
```


```r
# LRT comparing final model in chapter (model.f2ml) with maximum
#  likelihood estimates to modified final model (model.f2aml)
#  with uncorrelated Level Two errors.
drop_in_dev <- anova(model.f2ml, model.f2aml, test = "Chisq")
```


```
            npar      AIC      BIC    logLik      dev     Chisq Df      pval
model.f2aml   12 9855.457 9920.948 -4915.728 9831.457        NA NA        NA
model.f2ml    13 9857.234 9928.183 -4915.617 9831.234 0.2231332  1 0.6366629
```


## Exercises

### Conceptual Exercises

1. __Parenting and gang activity.__  @Walker-Barnes2001 describe, "Ethnic differences in the effect of parenting on gang involvement and gang delinquency: a longitudinal, hierarchical linear modeling perspective".  In this study, 300 ninth graders from one high school in an urban southeastern city were assessed at the beginning of the school year about their gang activity, the gang activity of their peers, behavior of their parents, and their ethnic and cultural heritage.  Then, information about their gang activity was collected at 7 additional occasions during the school year. For this study: (a) give the observational units at Level One and Level Two, and (b) list potential explanatory variables at both Level One and Level Two.

2. Describe the difference between the wide and long formats for longitudinal data in this study.

3. Describe scenarios or research questions in which a lattice plot would be more informative than a spaghetti plot, and other scenarios or research questions in which a spaghetti plot would be preferable to a lattice plot.

4. Walker-Barnes and Mason summarize their analytic approach in the following way, where HLM = hierarchical linear models, a synonym for multilevel models:

    *The first series [of analyses] tested whether there was overall change and/or significant individual variability in gang [activity] over time, regardless of parenting behavior, peer behavior, or ethnic and cultural heritage.  Second, given the well documented relation between peer and adolescent behavior . . . HLM analyses were conducted examining the effect of peer gang [activity] on [initial gang activity and] changes in gang [activity] over time.  Finally, four pairs of analyses were conducted examining the role of each of the four parenting variables on [initial gang activity and] changes in gang [activity].*

    The last series of analyses controlled for peer gang activity and ethnic and cultural heritage, in addition to examining interactions between parenting and ethnic and cultural heritage.

    Although the authors examined four parenting behaviors---behavioral control, lax control, psychological control, and parental warmth---they did so one at a time, using four separate multilevel models.  Based on their description, write out a sample model from each of the three steps in the series.  For each model, (a) write out the two-level model for predicting gang activity, (b) write out the corresponding composite model, and (c) determine how many model parameters (fixed effects and variance components) must be estimated.

5. Table \@ref(tab:table4chp9) shows a portion of Table 2: Results of Hierarchical Linear Modeling Analyses Modeling Gang Involvement from @Walker-Barnes2001.  Provide interpretations of significant coefficients in context.



<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;border-bottom: 0;">
<caption>(\#tab:table4chp9)A portion of Table 2: Results of Hierarchical Linear Modeling Analyses Modeling Gang Involvement from Walker-Barnes and Mason (2001).</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Predictor </th>
   <th style="text-align:left;"> Coefficient </th>
   <th style="text-align:left;"> SE </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;font-weight: bold;"> Intercept (initial status) </td>
   <td style="text-align:left;font-weight: bold;">  </td>
   <td style="text-align:left;font-weight: bold;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Base (intercept for predicting int term) </td>
   <td style="text-align:left;"> -.219 </td>
   <td style="text-align:left;"> .160 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Peer behavior </td>
   <td style="text-align:left;"> .252** </td>
   <td style="text-align:left;"> .026 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Black ethnicity </td>
   <td style="text-align:left;"> .671* </td>
   <td style="text-align:left;"> .289 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> White/Other ethnicity </td>
   <td style="text-align:left;"> .149 </td>
   <td style="text-align:left;"> .252 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Parenting </td>
   <td style="text-align:left;"> .076 </td>
   <td style="text-align:left;"> .050 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Black ethnicity X parenting </td>
   <td style="text-align:left;"> -.161+ </td>
   <td style="text-align:left;"> .088 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> White/Other ethnicity X parenting </td>
   <td style="text-align:left;"> -.026 </td>
   <td style="text-align:left;"> .082 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> Slope (change) </td>
   <td style="text-align:left;font-weight: bold;">  </td>
   <td style="text-align:left;font-weight: bold;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Base (intercept for predicting slope term) </td>
   <td style="text-align:left;"> .028 </td>
   <td style="text-align:left;"> .030 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Peer behavior </td>
   <td style="text-align:left;"> -.011* </td>
   <td style="text-align:left;"> .005 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Black ethnicity </td>
   <td style="text-align:left;"> -.132* </td>
   <td style="text-align:left;"> .054 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> White/Other ethnicity </td>
   <td style="text-align:left;"> -.059 </td>
   <td style="text-align:left;"> .046 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Parenting </td>
   <td style="text-align:left;"> -.015+ </td>
   <td style="text-align:left;"> .009 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Black ethnicity X parenting </td>
   <td style="text-align:left;"> -.048** </td>
   <td style="text-align:left;"> .017 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> White/Other ethnicity X parenting </td>
   <td style="text-align:left;"> .016 </td>
   <td style="text-align:left;"> .015 </td>
  </tr>
</tbody>
<tfoot>
<tr><td style="padding: 0; " colspan="100%">
<sup></sup> These columns focus on the parenting behavior of psychological control.</td></tr>
<tr><td style="padding: 0; " colspan="100%">
<sup></sup> Table reports values for coefficients in the final model with all</td></tr>
<tr><td style="padding: 0; " colspan="100%">
<sup></sup> variables entered.  * p&lt;.05; ** p&lt;.01; + p&lt;.10</td></tr>
</tfoot>
</table>

6. __Charter schools.__ Differences exist in both sets of boxplots in Figure \@ref(fig:lon-box2).  What do these differences imply for multilevel modeling?

7. What implications do the scatterplots in Figures \@ref(fig:lon-boxcatmat1) (b) and (c) have for multilevel modeling?  What implications does the boxplot in Figure \@ref(fig:lon-boxcatmat1) (a) have?

8. What are the implications of Figure \@ref(fig:lon-boxmat1) for multilevel modeling?

9. Sketch a set of boxplots to indicate an obvious interaction between percent special education and percent non-white in modeling 2008 math scores. Where would this interaction appear in the multilevel model?

10. In Model A, $\sigma^2$ is defined as the variance in within-school deviations and $\sigma^2_u$ is defined as the variance in between-school deviations.  Give potential sources of within-school and between-school deviations.

11. In Chapter \@ref(ch-multilevelintro) Model B is called the "random slopes and intercepts model", while in this chapter Model B is called the "unconditional growth model".  Are these models essentially the same or systematically different?  Explain.

12. In Section \@ref(modelb9), why don't we examine the pseudo R-squared value for Level Two?

13. If we have test score data from 2001-2010, explain how we'd create new variables to fit a piecewise model.

14. In Section \@ref(modeld), could we have used percent free and reduced lunch as a Level One covariate rather than 2010 percent free and reduced lunch as a Level Two covariate?  If so, explain how interpretations would have changed.  What if we had used average percent free and reduced lunch over all three years or 2008 percent free and reduced lunch instead of 2010 percent free and reduced lunch.  How would this have changed the interpretation of this term?

15. In Section \@ref(modeld), why do we look at a 10\% increase in the percentage of students receiving free and reduced lunch when interpreting $\hat{\alpha}_{2}$?

16. In Section \@ref(modelf9), if the gap in 2008 math scores between charter and non-charter schools differed for schools of different poverty levels (as measured by percent free and reduced lunch), how would the final model have differed?

17. Explain in your own words why "the error structure at Level Two is _not_ the same as the within-school covariance structure among observations".

18. Here is the estimated unstructured covariance matrix for Model C:

    \[ Cov(\mathbf{Y}_i) =  \left[
          \begin{array}{cccc}
            41.87 & &   \\
            36.46 & 48.18 &  \\
            35.20 & 39.84 & 45.77
          \end{array} \right] \]
    Explain why this matrix cannot represent an estimated covariance matrix with a compound symmetry, autoregressive, or Toeplitz structure.  Also explain why it cannot represent our standard two-level model.

### Guided Exercises

1. __Teen alcohol use.__ @Curran1997 collected data on 82 adolescents at three time points starting at age 14 to assess factors that affect teen drinking behavior.  Key variables in the data set `alcohol.csv` (accessed via @Singer2003) are as follows:
    - `id` = numerical identifier for subject
    - `age` = 14, 15, or 16
    - `coa` = 1 if the teen is a child of an alcoholic parent; 0 otherwise
    - `male` = 1 if male; 0 if female
    - `peer` = a measure of peer alcohol use, taken when each subject was 14.  This is the square root of the
      sum of two 6-point items about the proportion of friends who drink occasionally or regularly.
    - `alcuse` = the primary response.  Four items---(a) drank beer or wine, (b) drank hard liquor, (c) 5 or
      more drinks in a row, and (d) got drunk---were each scored on an 8-point scale, from 0="not at all" to
      7="every day".  Then `alcuse` is the square root of the sum of these four items. </ul>

    Primary research questions included:  Do trajectories of alcohol use differ by parental alcoholism?  Do trajectories of alcohol use differ by peer alcohol use?

    a. Identify Level One and Level Two predictors.
    
    b. Perform a quick EDA.  What can you say about the shape of `alcuse`, and the relationship between `alcuse` and `coa`, `male`, and `peer`?  Appeal to plots and summary statistics in making your statements.
    
    c. Generate a plot as in Figure \@ref(fig:lon-lat1) with alcohol use over time for all 82 subjects.  Comment.
    
    d. Generate three spaghetti plots with loess fits similar to Figure \@ref(fig:lon-spag3) (one for `coa`, one for `male`, and one after creating a binary variable from `peer`).  Comment on what you can conclude from each plot.
    
    e. Fit a linear trend to the data from each of the 82 subjects using `age` as the time variable.  Generate histograms as in Figure \@ref(fig:lon-histmat1) showing the results of these 82 linear regression lines, and generate pairs of boxplots as in Figure \@ref(fig:lon-box2) for `coa` and `male`.  No commentary necessary.  [Hint: to produce Figure \@ref(fig:lon-box2), you will need a data frame with one observation per subject.]
    
    f. Repeat 5 using centered age (`age14 = age - 14`) as the time variable.  Also generate a pair of scatterplots as in Figure \@ref(fig:lon-boxcatmat1) for peer alcohol use.  Comment on trends you observe in these plots.  [Hint: after forming `age14`, append it to your current data frame.]
    
    g. Discuss similarities and differences between (5) and (6).  Why does using `age14` as the time variable make more sense in this example?
    
    h. (Model A) Run an unconditional means model.  Report and interpret the intraclass correlation coefficient.
    
    i. (Model B) Run an unconditional growth model with `age14` as the time variable at Level One.  Report and interpret estimated fixed effects, using proper notation.  Also report and interpret a pseudo R-squared value.
    
    j. (Model C) Build upon the unconditional growth model by adding the effects of having an alcoholic parent and peer alcohol use in both Level Two equations.  Report and interpret all estimated fixed effects, using proper notation.
    
    k. (Model D) Remove the child of an alcoholic indicator variable as a predictor of slope in Model C (it will still be a predictor of intercept).  Write out Model D as both a two-level and a composite model using proper notation (including error distributions); how many parameters (fixed effects and variance components) must be estimated?  Compare Model D to Model C using an appropriate method and state a conclusion.

2. __Ambulance diversions__.  One response to emergency department overcrowding is “ambulance diversion”---closing its doors and forcing ambulances to bring patients to alternative hospitals.  The California Office of Statewide Health Planning and Development collected data on how often hospitals enacted “diversion status”, enabling researchers to investigate factors associated with increasing amounts of ambulance diversions.  An [award-winning](https://www.causeweb.org/usproc/usclap/2019/spring/winners) student project [@Radtke2019] examined a data set (`ambulance3.csv`) which contains the following variables from 184 California hospitals over a 3-year period (2013-2015):

    - `diverthours` = number of hours of diversion status over the year (response)
    - `year2013` = year (centered at 2013)
    - `totalvisits1` = total number of patient visits to the emergency department over the year (in 1000s)
    - `ems_basic` = 1 if the emergency department can only handle a basic level of severity; 0 if the emergency department can handle higher levels of severity
    - `stations` = number of emergency department stations available for patients (fixed over 3 years)
    
    a. State the observational units at Level One and Level Two in this study, then state the explanatory variables at each level from the list above.

    b. Create latticed spaghetti plots that illustrate the relationship between diversion hours and (i) EMS level, and (ii) number of stations (divided into "high" and "low").  Describe terms that might be worth testing in your final model based on these plots.

    c. Write out an unconditional growth model, where $Y_{ij}$ is the number of diversion hours for the $i^{th}$ hospital in the $j^{th}$ year.  Interpret both $a_i$ and $v_i$ in the context of this problem (using words – no numbers necessary).

    d. In Model E (see R code at the end of these problems), focus on the `ems_basic:year2013` interaction term.
      - provide a careful interpretation in context.
      - why are there no p-values for testing significance in the `lmer()` output?
      - confidence intervals can be formed for parameters in Model E using two different methods.  What can we conclude about the significance of the interaction from the CIs?  Be sure to make a statement about significance in context; no need to interpret the CI itself.

    e. Write out Model G in terms of its Level 1 and Level 2 equations (see R code at the end of these problems).  Be sure to use proper subscripts everywhere, and be sure to also provide expressions for any assumptions made about error terms.  How many total parameters must be estimated?

    f. In Model G, provide careful interpretations in context for the coefficient estimates of `year2013` and `stations`.

    g. We wish to compare Models D and D0. 
      - Write out null and alternative hypotheses in terms of model parameters.
      - State a conclusion based on a likelihood ratio test.
      - State a conclusion based on a parametric bootstrap.
      - Generate a plot that compares the null distributions and p-values for the likelihood ratio test and parametric bootstrap.
      - Why might we consider using a parametric bootstrap p-value rather than a likelihood ratio test p-value?
      - Show how you would produce a bootstrapped value for $Y_{11}$, the first row of `ambulance3.csv`.  Show all calculations with as many specific values filled in as possible.  If you need to select a random value from a normal distribution, identify the mean and SD for the normal distribution you’d like to sample from.


```r
modelD <- lmer(diverthours ~ year2013 + ems_basic + 
  (year2013 | id), data = ambulance3)

modelD0 <- lmer(diverthours ~ year2013 + ems_basic + 
  (1 | id), data = ambulance3)

modelE <- lmer(diverthours ~ year2013 + ems_basic +
  ems_basic:year2013 + (year2013 | id), data = ambulance3)

modelG <- lmer(diverthours ~ year2013 + totalvisits1 + 
  ems_basic + stations + ems_basic:year2013 + 
  stations:year2013 + (year2013 | id), data = ambulance3)
```

### Open-Ended Exercises

1. __UCLA nurse blood pressure study.__ A study by @Goldstein2000 collected information from 203 registered nurses in the Los Angeles area between 24 and 50 years of age on blood pressure (BP) and potential factors that contribute to hypertension.  This information includes family history, and whether the subject had one or two hypertensive parents, as well as a wide range of measures of the physical and emotional condition of each nurse throughout the day.  Researchers sought to study the links between BP and family history, personality, mood changes, working status, and menstrual phase.  

    Data from this study provided by @Weiss2005 includes observations (40-60 per nurse) repeatedly taken on the 203 nurses over the course of a single day.  The first BP measurement was taken half an hour before the subject's normal start of work, and BP was then measured approximately every 20 minutes for the rest of the day.  At each BP reading, the nurses also rate their mood on several dimensions, including how stressed they feel at the moment the BP is taken.  In addition, the activity of each subject during the 10 minutes before each reading was measured using an actigraph worn on the waist.  Each of the variables in `nursebp.csv` is described below:  
    - `SNUM`: subject identification number
    - `SYS`: systolic blood pressure (mmHg)
    - `DIA`: diastolic blood pressure (mmHg)
    - `HRT`: heart rate (beats per minute)
    - `MNACT5`: activity level (frequency of movements in 1-minute intervals, over a 10-minute period )
    - `PHASE`: menstrual phase (follicular---beginning with the end of menstruation and ending with ovulation, or luteal---beginning with ovulation and ending with pregnancy or menstruation)
    - `DAY`: workday or non-workday
    - `POSTURE`: position during BP measurement---either sitting, standing, or reclining
    - `STR`, `HAP`, `TIR`: self-ratings by each nurse of their level of stress, happiness and tiredness at the time of each BP measurement on a 5-point scale, with 5 being the strongest sensation of that feeling and 1 the weakest
    - `AGE`: age in years
    - `FH123`: coded as either NO (no family history of hypertension), YES (1 hypertensive parent), or YESYES (both parents hypertensive)
    - `time`: in minutes from midnight
    - `timept`: number of the measurement that day (approximately 50 for each subject)
    - `timepass`: time in minutes beginning with 0 at time point 1 </ul>

    Using systolic blood pressure as the primary response, write a short report detailing factors that are significantly associated with higher systolic blood pressure.  Be sure to support your conclusions with appropriate exploratory plots and multilevel models.  In particular, how are work conditions---activity level, mood, and work status---related to trends in BP levels?  As an appendix to your report, describe your modeling process---how did you arrive at your final model, which covariates are Level One or Level Two, what did you learn from exploratory plots, etc.?  

    Potential alternative directions: consider diastolic blood pressure or heart rate as the primary response variable, or even try modeling emotion rating using a multilevel model.

2. __Completion rates at U.S. colleges.__ Education researchers wonder which factors most affect the completion rates at U.S. colleges.  Using the IPEDS database containing data from 1310 institutions over the years 2002-2009 [@IPEDS], the following variables were assembled in `colleges.csv`:  
    - `id` = unique identification number for each college or university </ul>
    
    Response:  
    - `rate` = completion rate (number of degrees awarded per 100 students enrolled) </ul>
    
    Level 1 predictors:  
    - `year`
    - `instpct` = percentage of students who receive an institutional grant
    - `instamt` = typical amount of an institutional grant among recipients (in \$1000s) </ul>
    
    Level 2 predictors:  
    - `faculty` = mean number of full-time faculty per 100 students during 2002-2009
    - `tuition` = mean yearly tuition during 2002-2009 (in \$1000s) </ul>

    Perform exploratory analyses and run multilevel models to determine significant predictors of baseline (2002) completion rates and changes in completion rates between 2002 and 2009.  In particular, is the percentage of grant recipients or the average institutional grant awarded related to completion rate?

3. __Beating the Blues.__ Depression is a common mental disorder affecting approximately 121 million people worldwide, making it one of the leading causes of disability. Evidence has shown that cognitive behavioral therapy (CBT) can be an effective treatment, but delivery of the usual face-to-face treatment is expensive and dependent on the availability of trained therapists. As a result, Proudfoot et al. [-@Proudfoot2003] developed and studied an interactive multimedia program of CBT called Beating the Blues (BtheB). In their study, 167 participants suffering from anxiety and/or depression were randomly allocated to receive BtheB therapy or treatment as usual (TAU). BtheB consisted of 8, 50-minute computerized weekly sessions with “homework” projects between sessions, while treatment as usual consisted of whatever treatment the patient’s general practitioner (GP) prescribed, including drug treatment or referral to a counselor. Subjects in the BtheB group could also receive pharmacotherapy if prescribed by their GP (who reviewed a computer-generated progress report after each subject’s session), but they could not receive face-to-face counseling. The primary response was the Beck Depression Inventory (BDI), measured prior to treatment, at the end of treatment (2 months later), and at 2, 4, and 6 months post-treatment follow-up. Researchers wished to examine the effect of treatment on depression levels, controlling for potential explanatory variables such as baseline BDI, if the patient took anti-depressant drugs, and the length of the current episode of depression (more or less than 6 months).  Was treatment effective in both the active treatment phase and the post-treatment follow-up?

    Data from the BtheB study can be found in `BtheB.csv`; it is also part of the `HSAUR` package [@Everitt2006] in R. Examination of the data reveals the following variables:

    - `drug` = Was the subject prescribed concomitant drug therapy?
    - `length` = Was the current episode of depression (at study entry) longer or shorter than 6 months?
    - `treatment` = TAU or BtheB
    - `bdi.pre` = Baseline BDI at time of study entry (before treatment began)
    - `bdi.2m` = BDI level after 2 months (at the end of treatment phase)
    - `bdi.4m` = BDI level after 4 months (or 2 months after treatment ended)
    - `bdi.6m` = BDI level after 6 months (or 4 months after treatment ended)
    - `bdi.8m` = BDI level after 8 months (or 6 months after treatment ended)
    
    Things to consider when analyzing data from this case study:

    - Examine patterns of missing data.
    - Convert to LONG form (and eliminate subjects with no post-baseline data).
    - Exploratory data analyses, including lattice, spaghetti, and correlation plots. 
    - Set time 0 to be 2 months into the study (then the intercept represents BDI level at the end of active treatment, while the slope represents change in BDI level over the posttreatment follow-up). 
    - Note that treatment is the variable of primary interest, while baseline BDI, concomitant drug use, and length of previous episode are confounding variables.
