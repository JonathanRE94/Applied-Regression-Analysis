Applied different data analysis methods in R to create a Reduced regression model that influenced the salaries among programmers and engineers in Silicon Valley in the year 2000 of the Census, ultimately a better fit than the original database model.

The Reduced model is much better than the initial Base model based on fit, explaining variance, and predictive capability.

The following are the main data predictors in significance order: -Wage income (wageinc) -Education level (educ) -Age (age) -Number of weeks worked (wkswrkd) -Occupation type (occ) -Sex type (sex) -English Proficiency (engl) -Citizenship (cit) -Place of Birth (powspuma) -Year of Entry (yrentry) -Birth Year (birth)

The below model is the Base model before regression analysis: Initial Base model <- wageinc ~age + cit + educ + engl + occ + birth + sex + wkswrkd + yrentry + powspuma

Following is the Reduced model after regression analysis with interaction variables including non-linear polynomial terms: Reduced model <- logwageinc ~ educ + age + wkswrkd + occ + sex + engl + cit + I(age^2) + I(wkswrkd^2) + educ:occ + wkswrkd:occ + educ:age + age:engl + educ:sex + educ:wkswrkd + sex:engl + wkswrkd:engl + wkswrkd:sex
