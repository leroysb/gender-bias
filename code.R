# UC-Berkeley admission data
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")
if(!require(broom)) install.packages("broom", repos = "http://cran.us.r-project.org")

if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(rmarkdown)) install.packages("rmarkdown", repos = "http://cran.us.r-project.org")


data(admissions)
str(admissions)
admissions
?admissions
head(admissions)

# majors
unique((admissions$major))

# Total number of admittance and applicants
totals <- admissions %>% group_by(gender) %>% summarize(admitted=sum(admitted), applicants=sum(applicants))
totals %>% summarize(total_admitted=sum(admitted), total_applicants=sum(applicants))

# percent men and women accepted
admissions %>% group_by(gender) %>% 
  summarize(percentage = round(sum(admitted*applicants)/sum(applicants),1))


# test whether gender and admission are independent
admissions %>% group_by(gender) %>% 
  summarize(total_admitted = round(sum(admitted / 100 * applicants)), 
            not_admitted = sum(applicants) - sum(total_admitted)) %>%
  select(-gender) %>% 
  do(tidy(chisq.test(.)))
# A statistical test, the chi-squared test, clearly rejects the hypothesis that gender and admissions are independent as the p value is very small.

# percent admissions by major
admissions %>% select(major, gender, admitted) %>%
  spread(gender, admitted) %>%
  mutate(women_minus_men = women - men)
# 4 majors favor women over men

## The paradox is that analyzing the totals suggest a dependence between admissions and gender.
## But when the data is grouped by major, this dependence seems to disappear.
## Clearly, Z (major) is associated with Y (females), because the more selective a major, the lower the probability that someone enters that major.

## is major selectivity associated with gender?
# plot total percent admitted to major versus percent women applicants
admissions %>% 
  group_by(major) %>% 
  summarize(major_selectivity = sum(admitted * applicants) / sum(applicants),
            percent_women_applicants = sum(applicants * (gender=="women")) /
              sum(applicants) * 100) %>%
  ggplot(aes(major_selectivity, percent_women_applicants, label = major)) +
  geom_text()
ggsave("figs/MajorSelectivity-vs-PercentWomenApplicants.png")
## The plot suggests that women were much more likely to apply to the two hard majors. Gender and major selectivity are confounded.
## Major E is much harder to enter than major B. And over 60% of applicants to major E were women, while less than 30% of the applicants of major B were women.


# plot number of applicants admitted and not
admissions %>%
  mutate(yes = round(admitted/100*applicants), no = applicants - yes) %>%
  select(-applicants, -admitted) %>%
  gather(admission, number_of_students, -c("major", "gender")) %>%
  ggplot(aes(gender, number_of_students, fill = admission)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(. ~ major) + ggtitle("Number of Applicants Admitted and Not")
ggsave("figs/StratifiedMajorGroupedGender-vs-Applicants.png")


admissions %>% 
  mutate(percent_admitted = admitted * applicants/sum(applicants)) %>%
  ggplot(aes(gender, y = percent_admitted, fill = major)) +
  geom_bar(stat = "identity", position = "stack")
ggsave("figs/Gender-vs-Admitted.png")
## This breakdown lets us see that the majority of accepted men came from two majors, A and B. It also lets us see that few women apply to these two easy majors.

# condition on major and then look at differences
admissions %>% 
  ggplot(aes(major, admitted, col = gender, size = applicants)) + 
  geom_point()
ggsave("figs/Spread-Major-vs-Admitted.png")
## The size of the dot represents the number of applicants, and explains the paradox. We see large red dots and small blue dots for the easiest majors, A and B.

# average difference by major
admissions %>%  group_by(gender) %>% summarize(average = mean(admitted))
## If we first stratify by major, compute the difference, and then average, we find that the percent difference is actually quite small.