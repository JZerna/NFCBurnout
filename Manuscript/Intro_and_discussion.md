# Introduction

Need for Cognition (NFC) is a stable intrinsic motivation to seek out and especially to enjoy effortful cognitive activities [@Cacioppo1982].
As it bridges the gap between cognition and motivation, NFC is considered to be an investment trait [@Stumm2013], and has come to the fore of psychological research in the last years.
NFC can easily be assessed using the Need for Cognition Scale (NCS), a self-report questionnaire with 18 to 34 items [@Cacioppo1982; @Cacioppo1984].
While many studies have found positive associations of NFC with academic performance [@Cazan2014; @Elias2002; @Grass2017; @Lavrijsen2021; @Zheng2020], recent investigations have also looked at NFC as a personal resource in academic and work contexts.
Individuals high in NFC have more positive emotions at the end of the work day [@Rosen2020], higher work motivation, perceive their roles as less ambiguous [@Nowlin2017], are less likely to drop out of college [@Grass2017; @Klaczynski1996], and have less anxiety regarding their course work [@Karagiannopoulou2020].
These findings suggest that individuals high in NFC might be less prone to experience adverse effects of work stress, which range from physical [@Dragano2017; @Steptoe2013] to psychological [@Madsen2017; @Maslach2016; @Wiesner2005].

One of these psychological consequences is burnout, a state of exhaustion and cynicism caused by long-term overstimulation in the workplace, which results in employees being dissatisfied, being sick more often, and performing poorly [@Schaufeli2014].
Burnout is especially prevalent in social jobs such as healthcare or teaching because the worker is always in conflict between advocating for their client and meeting the goals set by the employer [@GrayStanley2011; @Lloyd2002].
Lackritz [-@Lackritz2004] found that university teachers' burnout scores were higher the more students they had, the higher their teaching load was, and the more time they spent grading students' work.
Burnout is most often assessed using the Maslach Burnout Inventory (MBI) [@Maslach1997], a self-report questionnaire with three subscales: Emotional exhaustion, depersonalisation, and reduced personal efficacy.

How personality traits influence a person's susceptibility to burnout is of interest for scientists and employers alike, as it can indicate how to create a healthier work environment that is beneficial for all employees.
Individuals with high burnout scores are often passive copers, high in neuroticism, low in self-esteem, and have an external locus of control [@Schaufeli2014].
NFC on the other hand is negatively associated with those variables [@Double2016; @Fleischhauer2019; @Ghorbani2004; @Grass2018; @Osberg1987], suggesting that people high in NFC are less prone to experience burnout.
This is supported by the findings that NFC is negatively associated with burnout scores in adults [@Fleischhauer2019], students [@Fleischhauer2019; @Naderi2018], and teacher trainees [@Grass2018].
However, the associations of NFC with the sum score and the subscales of the MBI are not always consistent between these studies.
This is likely not caused by inaccurate measurement, since the validity of both NCS [@Bless1994; @Osberg1987; @Tolentino1990] and MBI [@Brady2021; @Kantas1997; @Schaufeli2001; @ValdiviaVazquez2021] has been demonstrated in multiple studies.
What is more likely is the influence of one or more other variables, moderating or mediating the association of NFC and burnout.
Grass et al. [-@Grass2018] investigated such a mediation and found that the relation of NFC and the MBI subscale reduced personal-efficacy was fully mediated by habitual reappraisal, and active and passive coping, but not by habitual suppression or self-control.
Reappraisal and suppression are two emotion regulation strategies, which refer to the cognitive reassessment of a stressor and the inhibition of emotional reactions, respectively [@Gross1998a].
The findings by Grass et al. [-@Grass2018] suggest that individuals high in NFC experience a weaker decline in personal efficacy in response to long-term stress because they actively reassess the situation in a way that reinforces their sense of self-efficacy and don't avoid dealing with the stressor.
One goal of this paper was to replicate the findings of Grass et al. [-@Grass2018] using a multiple mediation model on cross-sectional self-report data of teachers.
We expected NFC to be negatively associated with reduced personal efficacy via higher reappraisal scores, but not via suppression, via self-control, or directly.

Furthermore, we extended the analysis to other possible mediators.
These mediators were motivated by our own recent survey of the literature on NFC and wellbeing, which suggested that individuals high in NFC might not only have a high level of personal resources but also overestimate their own resources to a certain degree [@Zerna2021].
Only a balance of resources and demands results in personal wellbeing, while an imbalance threatens wellbeing, regardless of whether this imbalance is in favour of resources or demands [@Dodge2012].
Following the framework of Hobfoll [-@Hobfoll1989], resources can be objects with practical or status purpose, conditions like marriage or tenure, personality aspects like coping style, and energies such as time, money, or knowledge.
In the case of NFC, resources are from the categories personality and energies: Personality, because NFC is a trait, encompassing a curious, analytic, and passionate approach to challenges, and energies, because individuals high in NFC have been coping actively all their life, which enriches their level of experience and knowledge in approaching challenges [@Cacioppo1996].
These personal resources matter with regards to stress assessment (how the situation is appraised) and with regards to both coping and recovery [@Salanova2006].
We therefore investigated whether the association of NFC and burnout was mediated by different ratios of demands and resources; demands that are too high to be dealt with using one’s personal resources (DTH), demands that are too low for one’s personal resources (DTL), and a balanced fit of demands and resources (DRF).
Using the same data as for the replication, we computed a structural equation model (SEM) to assess the influence of these mediators.
Since individuals high in NFC are confident in their abilities [@Bye2009; @Ghorbani2004; @Heppner1983; @Klaczynski1996], we expected NFC to be negatively associated with DTH, positively associated with DTL, and positively associated with DRF.
And since burnout results from constant unpleasant activation by high demands, we expected it to be positively associated with DTH and negatively associated with DRF.
However, we had no hypothesis regarding the association of DTL and burnout, because even though DTL is akin to the concept of boredom and the consequences of boredom and burnout are very similar, burnout is a state with even lower activation and even more negative affect than boredom [@Schaufeli2014].
It has already been shown that the Covid-19 pandemic has exacerbated the rising prevalence of burnout [@Froebe2021], so we incorporated the degree of feeling burdened by the pandemic in an exploratory approach.

# Methods

We report how we determined our sample size, all data exclusions (if any), all manipulations, and all measures in the study [@Simmons2012].
Our preregistration, the data, and the R Markdown document used to analyze the data and write this manuscript are available at [https://osf.io/36ep9/](https://osf.io/36ep9/).

## Participants
Teachers were recruited via social media, emails to colleagues of N.E. and to Saxon schools with the request to pass on the information.
All teachers were eligible, no payment was issued.
Of the $N=$ 278 participants, who started filling out the online survey, $N =$ `r nrow(data)` (`r round((table(data$gender)[1]/nrow(data))*100, digits = 1)`% female, aged 20 to 67 years) data sets were complete and those participants indicated to have answered truthfully.
All of them were currently teaching at a primary, secondary, comprehensive, or vocational school.
Data was collected between the 12th of June and the 24th of July 2020.
At this point, schools had been switching between digital and hybrid forms of teaching for at least three months due to the Covid-19 pandemic, causing additional stress for many teachers.

## Material
All questionnaires were used in their German form.
Burnout was assessed using the 21-item Maslach Burnout Inventory (MBI) [@Buessing1992], NFC using the 16-item Need for Cognition Scale (NCS) [@Bless1994], self-control using the 13-item Self-Control Scale (SCS) [@Bertrams2009a], reappraisal and suppression using the 10-item Emotion Regulation Questionnaire (ERQ) [@Abler2009], and work satisfaction using the Allgemeine Arbeitszufriedenheit questionnaire (AAZ) [@Fischer2014].
Eleven items were created to assess each participant's current burden by the Covid-19 pandemic, such as whether they belonged to a risk group or whether they currently had a higher workload.
The Covid-19 items can be found in the **Supplementary Material**.
The survey also included the Subjective Wellbeing Index of the World Health Organization [@Braehler2007], which we will not analyze.
Due to a technical error during survey setup, the coping style data of the Erfurter Belastungsinventar [@BoehmKasper2001] cannot be used, so we cannot replicate the mediation of NFC and burnout by active and passive coping.

## Procedure
The questionnaires were provided online using SoSci Survey [@Leiner2019].
Participants were informed about aims and duration of the study and data security, then they provided demographic information, answered the questionnaires, and could optionally enter their email address to be informed about the results of the analysis of N.E.'s thesis.

## Data analysis
We used *R Studio* [@RCT2020; @RStudioTeam2020] with the main packages *lavaan* [@Rosseel2012] and *psych* [@Revelle2021] for all our analyses.
Data were checked for multivariate normality using Mardia's coefficient.
To account for non-linear relationships, correlations were computed using Spearman's rank coefficient rather than Pearson's product moment correlation.
Internal consistencies were assessed with Cronbach's Alpha and MacDonald's Omega.
Since Cronbach's Alpha has been criticized for being insensitive to violations of internal consistency [@Dunn2014; @Taber2018], the additional computation of MacDonald's Omega has the purpose of ensuring a more reliable estimation.

### Multiple mediation model
Items were reverse coded according to the scale manuals.
NFC and self-control were computed as the sum scores of the NCS and the SCS, respectively.
Reduced personal efficacy was computed using the sum of the MBI subscale, and reappraisal and suppression were computed using the sum of each ERQ subscale.
NFC was entered as the independent variable, having a direct and multiple indirect effects on MBI via self-control, reappraisal, and suppression as mediators.
Following Grass et al. [-@Grass2018], the results of the model were appraised by using $N=2,000$ bootstrap samples for confidence intervals.
Multiple indices were used to evaluate model fit as recommended by Hu and Bentler [-@Hu1999]: the Chi-square test statistic, which measures the fit compared to a saturated model, the Comparative Fit Index (CFI), which compares the fit to the baseline model, the Standardized Root Mean Square Residual (SRMR), which compares the residuals of the observed and predicted covariance matrix, and the Root Mean Square Error of Approximation (RMSEA), which does the same as the latter but takes degrees of freedom and model complexity into account.

### Structural equation model
All items, apart from those making up the demand-resource-ratios, were reverse coded according to the scale manuals.
The latent factor NFC was computed by subjecting the NCS items to a parcelling procedure following Grass et al. [-@Grass2019], a method that is used in SEM when only relations between but not within constructs are of interest.
Principal component analysis was used to determine the factor loadings of each NCS item onto the first component.
Then, the items were randomly divided into four parcels and the average item loading per parcel was computed.
This was repeated 10,000 times to find the parcelling choice with the smallest difference in average item loadings between parcels.
The latent factor MBI was computed using the three subscales as indicators.
For the demand-resource-ratios, we used three items from the work satisfaction scale each.
The latent factor DTH was indicated by items 4, 8, and 9, DTL by the recoded items 12, 26, and 27, and DRF by items 17, 22, and 24.
The items can be translated as follows: 4) "There is too much pressure on me.", 8) "There is often too much being demanded of us at work.", 9) "I often feel tired and weary because of my work.", 12) "I can realize my ideas here.", 17) "I take pleasure in my work.", 22) "Does your place of work give you the opportunity to do what you do best?", 24) "Does your place of work give you enough opportunities to use your skills?", 26) "Are you happy with your promotion prospects?", and 27) "Are you happy with your position when comparing it to your skills?".
Model parameters were estimated using the maximum likelihood method with robust standard errors.
Model fit was evaluated by looking at the Chi-square test statistic, CFI, SRMR, and RMSEA.

### Exploratory analyses
We preregistered two exploratory analyses.
Firstly, we repeated the SEM with the subscale reduced personal efficacy in place of the MBI score, since this subscale has shown higher correlations with NFC than the other subscales [@Grass2018; @Naderi2018].
And secondly, we included a Covid-19 burden score into the SEM, computed as the sum of the Covid-19 items.

# Discussion

The present study aimed to replicate findings of mediators between Need for Cognition and burnout in teachers, as well as to extend the analysis to the role of different ratios of demands and resources in burnout using latent variable models.
In an exploratory approach, we investigated the influence of the burden that the Covid-19 pandemic has placed on teachers.
Previous studies have indicated a protective effect of NFC against burnout, but the associations with the burnout subscales were inconsistent, suggesting that there are more variables influencing this relationship.

## Replication of Grass et al. (2018)

While the mediation model had good fit, not all patterns were similar to the original study: NFC and self-control were positively associated, and reappraisal and reduced personal efficacy were negatively related, but there was no association between NFC and reappraisal.
There was, however, a positive association between self-control and reduced personal efficacy, and a negative one between NFC and suppression.

NFC had a direct and negative effect on reduced personal efficacy, but this relationship was not mediated by any other variable.
Only when the amount of teaching experience was included as a predictor of self-control next to NFC, an indirect effect via self-control reached significance, indicating that teachers with high NFC and more years of teaching experience have higher self-control and, consequently, lower reduced personal efficacy.
The higher self-control that comes with more teaching experience is in line with findings of fluctuations in self-control in young adults, reaching a low point between the age of 15 and 19 [@Olivia2019].
The participants in the study by Grass et al. [-@Grass2018] were teacher trainees with a mean age of 25.5 years, while the majority of the current sample was between 40 and 59 years old.
Therefore, it is likely that not only the teaching experience itself but also higher age might be associated with higher self-control.
However, one could argue that more experience provides the teacher with a bigger repertoire of coping strategies to enable an efficient exertion of self-control, especially for teachers high in NFC who are intrinsically motivated to find and apply such strategies.

We could replicate the relation between the two emotion regulation strategies reappraisal and suppression with reduced personal efficacy, but not their association with NFC.
There is ample evidence that reappraisal is associated with positive outcomes for students [@Levine2012;@Schmidt2010;@Haga2007] and teachers alike [@Moe2020;@Tsouloupas2010;@Jiang2016], so it is suprising that reappraisal did not mediate between NFC and reduced personal efficacy.
Reappraisal did correlate with NFC, as it should appease the preference for cognitive effort in individuals with high NFC, but it was not a mediator in this model.
One possible explanation could be that the ways by which reappraisal can be achieved, such as taking the role of an uninvolved observer, are less feasible for teachers in retaining their sense of efficacy in the classroom than the self-control needed to structurally manage students and situations.
Hence, the mediation of NFC and reduced personal efficacy by self-control when taking the years spent teaching into account.

## Demand-resource-ratio model

Despite not having good fit indices, the model suggested a complete mediation of NFC and burnout via DTH and DRF but not DTL.
Specifically, individuals with higher NFC had lower burnout scores through perceiving demands as fitting to and not exceeding their own resources.
Interestingly, the correlation between NFC and burnout, which can be classified as medium according to Gignac and Szodorai [-@Gignac2016], disappeared in the context of the demand-resource-ratios as mediators.
The mediator that did not reach significance was the perception of own resources exceeding the job demands.
As this latent variable was conceptualized as boredom at work, we could not confirm the positive association of boredom and burnout found by Reijseger et al. [-@Reijseger2013.].



* demands have ambiguous impact on psychological well-being -> challenge vs hindrance demands as proposed by [@Lepine2005;@Podsakoff2007;@Lazarus1984]
* challenge demands are being positively valued due to their potential to increase personal growth, positive affect, and problem-focused coping [@Lepine2005], e.g. time pressure, responsibility, workload, and mental overload [@Podsakoff2007]
* hindrance demands are perceived as negative because they harm personal growth, trigger negative emotions, and increase passive coping [@Lepine2005], e.g. inadequate resources, role conflict, role ambiguity, organisational politics, and uncertain job security [@Podsakoff2007]
* a reduction in self-efficacy is considered to be a precurser of burnout, not necessarily a symptom [@Cherniss1993;@Vera2012]
* hindrance but not challenge demands were positively related to burnout in teachers, and challenge demands were positively and hindrance demands negatively related to engagement [@Ventura2014]


## Exploratory analyses
* DRR model with rPE had bad fit and the indirect effect via DTH disappeared

* Covid model had good fit because I could do it all
* indirect effect of NFC and self-control on rPE via DRF
* indirect effect of NFC and self-control on emEx via DTH
* indirect effect of Covid on emEX via DTH