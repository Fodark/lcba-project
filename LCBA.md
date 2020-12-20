# LCBA - Data Science 2020/21

Dall'Asen Nicola - 211662 - nicola.dallasen@studenti.unitn.it
Debeni Andrea - 211664 - andrea.debeni@studenti.unitn.it
Zorzoni Riccardo - ?????? - riccardo.zorzoni@studenti.unitn.it

## Abstract

The aim of this study is to provide an insight on the most important features that customer take into consideration when they choose a new smartphone, in order to provide market segments and analyze customers' tastes.
The study has been conducted through the use of a choice-based conjoint analysis on data collected through an online survey thanks to the statistical tool of Multinomial Logistic Regression and Bootstrap prediction.

## Data collection

### Method used

In order to collect the data needed for our analysis we designed a survey where respondents were asked to choose the smartphone profile that best suited their real-life choices. How the profiles were generated is explained further in this section. Each person answering the survey was asked to answer to 13 questions and each question was composed by 3 different smartphone profiles.

Every profile is composed by several features that commonly describe such product, i.e. price, RAM and storage, quality of the display, daily usage (if the product is able to carry out daily tasks in an optimal way), quality of the camera and battery. Given the extreme heterogeneity of evaluation of such features, a known and established source was taken as ground truth for these values [2], this source provides reviews for different smartphones and a numeric summary of the features taken into account. To avoid having respondents deal with many numerical values which provide little to none meaning to the average user, the features, except price and RAM/storage, were grouped into three categories, i.e. "low", "medium" and "high" and the conversion tables are provided below. Prices were rounded to the closest hundredth, while RAM/storage was grouped into the three most common configuration for recent smartphones, "4/64 GB" for low-end devices, "8/128" for medium-end and "12/256" for high-end.    

---

[1] Taherdoost, Hamed. (2016). Sampling Methods in Research Methodology; How to Choose a Sampling Technique for Research. International Journal of Academic Research in Management. 5. 18-27. 10.2139/ssrn.3205035. 
[2] Galeazzi Andrea, https://andreagaleazzi.com/

