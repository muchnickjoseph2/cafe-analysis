# Hypotheses: Ratings topic

Written before cleaning or looking at the data. alpha = .05.

## A1 One sample
H0: The mean Google rating of Prague cafes equals 4.5.
H1: The mean Google rating differs from 4.5. (two-tailed)

## A2 Two groups
H0: Mean Google rating is the same for cafes with an above-median and a below-median number of ratings.
H1: Cafes with an above-median number of ratings have a lower mean rating (above-median minus below-median < 0). (one-tailed)

## A3 Paired
H0: The mean difference between Adjusted Rating and raw rating is 0.
H1: The mean difference is not 0. (two-tailed)

## A4 More groups
H0: Mean Google rating is equal in the few (<100), medium (100-1,000) and many (>1,000) ratings groups.
H1: At least one group mean differs. (two-tailed)

## A5 Two categorical
H0: The share of top-rated cafes (rating >= 4.7) is the same for below-median and above-median numbers of ratings.
H1: The share of top-rated cafes is higher among cafes with a below-median number of ratings. (one-tailed)

## A6 Correlation
H0: The correlation between Google rating and number of ratings is 0.
H1: The correlation is not 0. (two-tailed)

## A7 Simple regression
H0: The slope of Google rating on number of ratings (per 1,000 ratings) is 0.
H1: The slope is not 0. (two-tailed)

## A8 Multiple regression
H0: Controlling for number of ratings, Cafe Type has no association with rating (both type coefficients = 0).
H1: At least one Cafe Type coefficient differs from 0. (two-tailed)

## A9 Interaction
H0: The slope of Adjusted Rating on raw rating is the same for cafes with <100 and >=100 ratings (interaction = 0).
H1: The slopes differ (interaction not 0). (two-tailed)

## A10 Logistic regression
H0: The number of ratings (per 1,000) has no effect on the log-odds of being top-rated (coefficient = 0).
H1: The coefficient is not 0. (two-tailed)