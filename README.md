# COVID19-factors
Based on a project for a linear regression class in spring 2021

## Project title
Factors in COVID-19 Case Counts in the United States

## YouTube link
https://youtu.be/4ADDa6QuJRg

## Project background

Numerous studies have found that wearing masks (Centers for Disease Control and Prevention, 2020; Wang et al., 2020) and reduced mobility (Nouvellet et al., 2021; Badr et al., 2020) decrease the probability of COVID-19 infection. Therefore, we should expect to find that at the aggregate level, U.S. states whose populations wear masks more frequently and use public transit less frequently will have lower increase in confirmed COVID-19 cases. As such, the hypotheses that will be tested are as follows.

H0: The reported rate of not wearing masks is positively correlated with the number of confirmed COVID-19 cases.

*β1>0*.

H2: Visits to public transportation stations is positively correlated with the number of confirmed COVID-19 cases.

*β2>0*.

## Result highlight

After transformation, there was significant multicollinearity between mask-wearing and mobility and between mask-wearing and population density. To address this, interaction terms were added. Therefore, two additional hypotheses were tested.

H3: The interaction of the reported rate of not wearing masks and visits to public transportation stations is positively correlated with the number of confirmed COVID-19 cases.

*β3>0*.

H4: The interaction of the reported rate of not wearing masks and population density is positively correlated with the number of confirmed COVID-19 cases.

*β4>0*.

The hypothesis tests, based on ridge regression and bootstrapping, fail to support H1 (p = 0.25) and H4 (p = 0.985), but the data do support H2 (p = 0) and H3 (p = 0). That is, there is not significant evidence that the rate of mask-wearing, on its own, is negatively correlated with COVID-19 cases. Additionally, the data also do not have significant evidence that the rate of not wearing masks and population density are positively correlated with COVID-19 cases. However, there is strong evidence that mobility is positively correlated with COVID-19 cases, and that the interaction of the rate of not wearing masks and mobility is positively correlated with COVID-19 cases.

From a practical perspective, this seems to indicate that masks are not enough on their own, and that reduced public mobility is more effective in reducing COVID-19 cases. High mask-wearing rates in combination with low public mobility is also associated with low COVID-19 case totals.

## Project references

Centers for Disease Control and Prevention. (2020, November 20). Science brief: Community use of cloth masks to control the spread of SARS-CoV-2. https://www.cdc.gov/coronavirus/2019-ncov/science/science-briefs/masking-science-sars-cov2.html?CDC_AA_refVal=https%3A%2F%2Fwww.cdc.gov%2Fcoronavirus%2F2019-ncov%2Fmore%2Fmasking-science-sars-cov2.html

Wang, Y., Tan, H., Zhang, L. Zhang, M., Guo, D., Wu, W., Zhang, X., Kan, G. L., Jia, L., Huo, D., Liu, B., Wang, X., Sun, Y., Wang, Q., Yang, P., & MacIntyre, C. R. (2020). Reduction of secondary transmission of SARS-CoV-2 in households by face mask use, disinfection and social distancing: a cohort study in Beijing, China. BMJ Global Health, 5(5): e002794. doi: 10.1136/bmjgh-2020-002794

Nouvellet, P., Sangeeta, B., Cori, A., Ainslie, K. E. C., Baguelin, M., Bhatt, S., Boonyasiri, A., Brazeau, N. F., Cattarino, L., Cooper, L. V., Coupland, H., Cocunuba, Z. M., Cuomo-Dannenburg, G., Dighe, A., Djaafara, B. A., Dorigatti, I., Eales, O. D., Van Elsland, S. L., Nascimento, F. F., … Donnelly, C. A. (2021). Reduction in mobility and COVID 19 transmission. Nature Communications, 12(1090). doi: 10.1038/s41467-021-21358-2

Badr, H. S., Du, H., Marshall, M., Dong, E., Squire, M. M., Gardner, L. M. (2020). Association between mobility patterns and COVID-19 transmission in the USA: A mathematical modelling study. The Lancet Infectious Diseases, 20(11), 1247-1254. doi: 10.1016/S1473-3099(20)30553-3
