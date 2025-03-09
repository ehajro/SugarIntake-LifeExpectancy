## How to run
**In order to look at the code with the output, comments, and some notes about the project, I recommend looking at the SugarIntake-LifeExpectancy.md file here.**

I have also included an R Script with the code and comments only. I recommend running these files using the latest version of RStudio. 

## Datasets
I have included here the life expectancy and the sugar intake datasets as csv files. You need to download these datasets and save them in the same folder as the markdown file and R Script
in order for the code to run properly. 

These datasets can also be found on the Gapminder website: https://www.gapminder.org/data/

## Project overview
This project aims to explore the relationship between sugar intake and life expectancy accross different countries using data from the year 2018.

The sugar intake variable has been converted into a categorical variable where Category 1 represents the lowest sugar intake and Category 5 represents the highest sugar intake.

After taking a look at the density plots, means, and standard deviations of the life expectancy for each sugar intake category, I decided to use the Bayesian Hierarchical Model outlined above.

I then ran the JAGS scripts for the model and obtained the posterior summaries of all parameters in this model. The results can be seen in the markdown file above.
