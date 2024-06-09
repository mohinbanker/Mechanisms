---
title: "Mechanisms Analysis"
author: "Mohin Banker"
date: "2024-03-10"
output:
  html_document:
    df_print: paged
    toc: true
    toc_depth: 3
    keep_md: yes
editor_options:
  chunk_output_type: console
---

# Methodology

## Stimuli
600 participants were randomly assigned across two conditions: (experience vs. observation). Participants were presented with two competing recommendations for two rock albums in the following scenarios:

*You’re visiting a vinyl records shop that carries hundreds of albums across various music genres.*

*Consider two albums that seem worthwhile to purchase. You’re vaguely familiar with both artists but can’t recall any specific song of theirs that you’ve heard.*

  * **Electronic Echoes** by Retrograde. Electronic Echoes is a 1982 alternative rock album with 11 tracks.
  * **Midnight Rhapsody** by Starlight Boulevard. Midnight Rhapsody is a 1986 classic rock album with 11 tracks.
  
*At the store register, you meet two employees, Person X and Person Y. You ask for their recommendations on which album to purchase.*
 
*Consider each employee's recommendation carefully before making a decision.*

  * *Person X loves rock music and has listened to all the tracks in both albums. But, Person X has not spoken with other people about which album they prefer. When you asked for Person X's recommendation, Person X suggested buying [Electronic Echoes/Midnight Rhapsody].*

  * *Person Y loves rock music and has not listened to either album. But, Person Y has spoken with a couple of people, who also love rock music and have listened to all the tracks in both albums, about which album they prefer. When you asked for Person Y’s recommendation, Person Y suggested buying [Midnight Rhapsody/Electronic Echoes].*


So, Person X  had experience, while Person Y had observations. We also counterbalanced which person (Person X or Person Y) had information from experience or observation.

## Depdendent Variables

Participants responded to a series of items measuring their preferences for the albums and their underlying beliefs that could drive their preferences.

### Preferences

  * **Choice**. Which album would you be more likely to purchase? (Electronic Echoes or Midnight Rhapsody)
  * **Strength of Preference**. How much do you prefer [choice] over [alternative]? (1 = I don't have a preference at all; 9 = I have an extremely strong preference)
  * We combine these two measures to make a single "preference" measure (1 = Strongly prefer Electronic Echoes, 18 = Strongly prefer Midnight Rhapsody)

### Block 1 of Mechanisms
These items were counterbalanced. We prioritized these items because we believed these were more likely to explain preferences for experience.

  * **Accuracy**. Whose recommendation do you think is more accurate?
  * **Credibility**. Whose recommendation do you think is more credible?
  * **Justifiability**. Who do you think can better defend the recommendation they provided?
  * **Effort**. Who do you think has put more effort into learning about these albums for their recommendation?

### Block 2 of Mechanisms
These items were also counterabalanced. We believed these items would be less predictive of preferences.

  * **Richness**. Whose recommendation do you think was based on more comprehensive knowledge about the albums?
  * **Representativeness**.
    * *Self*: Whose knowledge about the albums is more likely to represent **your** taste?
    * *Others*: Whose knowledge about the albums is more likely to represent the taste of **another person** selected at random?
  * **Expertise**. In general, who do you think has more expertise in rock music?
  * **Passion**. Who do you think is more motivated to continue learning about rock music?
  * **Uniqueness**. Who do you think has knowledge that is more unique and unlikely to be encountered elsewhere?

All mechanism items used an identical scale: (1 = Definitely Person X; 5 = They are equal; 9 = Definitely Person Y).






# Primary DVs

## Choice/Preference of Albums

High values for choice share and preference reflect more participants choosing the focal album (Midnight Rhapsody). The means show that more participants chose the album when it was recommended by someone who had listened to the albums, compared to someone who had spoken to people who listened to the albums.



```r
dt %>% 
  group_by(condition) %>% 
  summarise(choice_share = mean(choice == "MR"),
            strength_of_preference = mean(preference),
            preference = mean(full_preference),
            n = n()) %>% 
  pander
```


------------------------------------------------------------------------
  condition    choice_share   strength_of_preference   preference    n  
------------- -------------- ------------------------ ------------ -----
 Experience       0.625                5.13               10.8      301 

 Observation      0.482                5.1                9.56      299 
------------------------------------------------------------------------

## Histogram of Preferences

```r
dt %>% 
  group_by(condition, full_preference) %>% 
  summarise(n = n()) %>% 
  group_by(condition) %>% 
  mutate(perc = n/sum(n)) %>% 
  ggplot(data = ., aes(x = full_preference,
                       y = perc)) +
  geom_col() +
  scale_x_continuous(breaks = c(1, 18),
                     labels = c("Strongly prefers\nother album",
                                "Strongly prefers\nfocal album")) +
  facet_grid(condition ~ .) +
  labs(x = "",
       y = "Response Share") +
  theme_bw()
```

![](mechanisms_analysis_files/figure-html/unnamed-chunk-2-1.png)<!-- -->


### Means across presentation orders

It seems that people are more likely to choose an album if it's presented on the left side of the screen (and recommended by "Person X" instead of "Person Y"). This suggests primacy effects are present.


```r
dt %>% 
  group_by(condition, position) %>% 
  summarise(choice_share = mean(choice == "MR"),
            strength_of_preference = mean(preference),
            preference = mean(full_preference),
            n = n()) %>% 
  pander
```


-----------------------------------------------------------------------------------
  condition    position   choice_share   strength_of_preference   preference    n  
------------- ---------- -------------- ------------------------ ------------ -----
 Experience      Left        0.682                5.18               11.5      148 

 Experience     Right        0.569                5.08               10.2      153 

 Observation     Left        0.517                5.06               9.77      149 

 Observation    Right        0.447                5.13               9.35      150 
-----------------------------------------------------------------------------------

### Strength of preference by choice
We look at whether the strength of preference depended on which album was chosen. We don't expect any differences *ex ante*.

However, people reported a stronger preference conditional on selecting Midnight Rhapsody, regardless of condition or presentation order. It's not clear to me why this occurs. There may be an overall preference for Midnight Rhapsody, which means people who chose the alternative are more likely to feel ambivalent.

```r
dt %>% 
  group_by(condition, position, choice) %>% 
  summarise(strength_of_preference = mean(preference),
            n = n()) %>% 
  pander
```


----------------------------------------------------------------
  condition    position   choice   strength_of_preference    n  
------------- ---------- -------- ------------------------ -----
 Experience      Left       EE              4.79            47  

 Experience      Left       MR              5.37            101 

 Experience     Right       EE              4.97            66  

 Experience     Right       MR              5.17            87  

 Observation     Left       EE              4.94            72  

 Observation     Left       MR              5.17            77  

 Observation    Right       EE              4.82            83  

 Observation    Right       MR              5.52            67  
----------------------------------------------------------------

# Mechanisms
For each mechanism question, we compare how much responses changed across conditions. This indicates how much people believe differences between experience vs. observation are reflected in that particular measure.

## Means

```r
dt %>% 
  group_by(condition) %>% 
  summarise(credibility = mean(credibility),
            accuracy = mean(accuracy),
            justifiability = mean(justifiability),
            effort = mean(effort),
            richness = mean(richness),
            representativeness_self = mean(rep_self),
            representativeness_others = mean(rep_others),
            representativeness_diff = mean(rep_diff),
            expertise = mean(expertise),
            passion = mean(passion),
            unique = mean(unique)) %>% 
  pivot_longer(cols = credibility:unique) %>% 
  pivot_wider(names_from = "condition",
              values_from = "value") %>% 
  mutate(Difference = Experience - Observation) %>% 
  pander
```


-------------------------------------------------------------------
           name              Experience   Observation   Difference 
--------------------------- ------------ ------------- ------------
        credibility             6.16         4.19          1.98    

         accuracy               5.99         4.47          1.52    

      justifiability            6.89          3.4          3.49    

          effort                6.46           4           2.46    

         richness               6.85          3.6          3.26    

  representativeness_self       5.58         4.64         0.946    

 representativeness_others      3.79         6.21         -2.42    

  representativeness_diff       1.79         -1.58         3.37    

         expertise              6.13         4.21          1.92    

          passion               6.16         4.32          1.84    

          unique                6.09         4.23          1.86    
-------------------------------------------------------------------

## Standard deviations


```r
dt %>% 
  summarise(credibility = sd(credibility),
            accuracy = sd(accuracy),
            justifiability = sd(justifiability),
            effort = sd(effort),
            richness = sd(richness),
            representativeness_self = sd(rep_self),
            representativeness_others = sd(rep_others),
            expertise = sd(expertise),
            passion = sd(passion),
            unique = sd(unique)) %>% 
  pivot_longer(everything()) %>% 
  pander
```


-----------------------------------
           name              value 
--------------------------- -------
        credibility          2.45  

         accuracy             2.3  

      justifiability         2.94  

          effort             2.85  

         richness             2.8  

  representativeness_self    1.95  

 representativeness_others   2.44  

         expertise           2.05  

          passion            2.27  

          unique             2.15  
-----------------------------------

## Distributions

I think the most interesting part here is what proportion choose the midpoint. For measures like expertise, passion, representativeness, and uniqueness, many people are indifferent despite there being a difference between conditions. On the the other hand, richness, justifiability, and effort have fewer responses at the midpoint.


```r
dt_long %>% 
  group_by(condition, name, value) %>% 
  summarise(n = n()) %>% 
  group_by(condition, name) %>% 
  mutate(perc = n/sum(n)) %>% 
  ggplot(data = ., aes(x = value, y = perc,
                       group = condition,
                       fill = condition)) +
  geom_col(position = position_dodge(0.5),
           color = "black") +
  facet_wrap(. ~ name, nrow = 3) +
  theme_bw() +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(breaks = 1:9,
                     labels = c("Definitely\nPerson X", 2:8,
                                "Definitely\nPerson Y")) +
  labs(x = "",
       y = "Response Share")
```

![](mechanisms_analysis_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

## Correlations between mechanisms
We crafted the mechanisms to be distinct, but participants may interpret them to be similar. We can look at correlations between measures to see how similarly participants respond to them.


```r
dt %>% 
  dplyr::select(credibility,
                accuracy,
                justifiability,
                richness,
                effort,
                rep_self,
                rep_others,
                rep_diff,
                expertise,
                passion,
                unique) %>% 
  cor %>% 
  ggcorrplot(hc.order = T, type = "upper", lab = T)
```

![](mechanisms_analysis_files/figure-html/unnamed-chunk-8-1.png)<!-- -->


```r
dt %>% 
  dplyr::select(credibility,
                accuracy,
                justifiability,
                richness,
                effort,
                rep_diff,
                expertise,
                passion,
                unique) %>% 
  cor %>% 
  as.data.frame %>% 
  tibble::rownames_to_column() %>% 
  tidyr::pivot_longer(-rowname) %>% 
  filter(rowname != name) %>% 
  filter(!duplicated(paste0(pmax(rowname, name), pmin(rowname, name)))) %>% 
  mutate(magnitude = abs(value)) %>% 
  arrange(-magnitude) %>% 
  dplyr::select(-magnitude) %>% 
  mutate(value = round(value, 2)) %>% 
  rename(correlation = value,
         mechanism1 = rowname,
         mechanism2 = name) %>% 
  datatable()
```

```{=html}
<div id="htmlwidget-65f2b32aa19e23462ce3" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-65f2b32aa19e23462ce3">{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36"],["credibility","credibility","justifiability","justifiability","accuracy","expertise","credibility","richness","accuracy","credibility","accuracy","richness","richness","justifiability","effort","justifiability","justifiability","richness","credibility","effort","richness","justifiability","credibility","expertise","accuracy","effort","passion","credibility","credibility","rep_diff","accuracy","accuracy","rep_diff","accuracy","effort","rep_diff"],["accuracy","justifiability","effort","richness","justifiability","passion","effort","effort","effort","richness","richness","expertise","passion","expertise","passion","unique","passion","unique","passion","expertise","rep_diff","rep_diff","expertise","unique","passion","unique","unique","unique","rep_diff","expertise","expertise","unique","passion","rep_diff","rep_diff","unique"],[0.87,0.75,0.75,0.74,0.71,0.71,0.6899999999999999,0.6899999999999999,0.6899999999999999,0.67,0.62,0.61,0.6,0.59,0.59,0.57,0.57,0.55,0.52,0.52,0.51,0.5,0.5,0.49,0.49,0.48,0.47,0.47,0.46,0.45,0.45,0.45,0.44,0.4,0.39,0.38]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>mechanism1<\/th>\n      <th>mechanism2<\/th>\n      <th>correlation<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":3},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```


### Effect sizes across conditions
We can standardize differences by dividing differences by pooled standard deviations. We accomplish this by running individual regressions for each mechanism and inspecting effect sizes (i.e., size of test statistics). This tells us which measure was most affected by manipulating experience vs. observation.

We find the largest effects for justifiability and richness. The smallest effect sizes are in accuracy and representativeness (for one's own tastes).


```r
dt %>% 
  pivot_longer(cols = c(credibility,
                        accuracy,
                        justifiability,
                        richness,
                        effort,
                        rep_self,
                        rep_others,
                        rep_diff,
                        expertise,
                        passion,
                        unique)) %>% 
  dplyr::select(condition, name, value) %>% 
  nest(data = c(condition, value)) %>% 
  mutate(fit = map(data, ~lm(value ~ condition, data = .)),
         results = map(fit, tidy)) %>% 
  unnest(results) %>% 
  filter(term == "conditionObservation") %>% 
  arrange(p.value) %>% 
  dplyr::select(-data, -fit, -term) %>%   
  pander
```


--------------------------------------------------------------
      name        estimate   std.error   statistic   p.value  
---------------- ---------- ----------- ----------- ----------
 justifiability    -3.49       0.193       -18.1     1.65e-58 

    richness       -3.26       0.186       -17.5     7.76e-56 

    rep_diff       -3.37       0.223       -15.1     6.25e-44 

   rep_others       2.42       0.173       13.9      1.71e-38 

   expertise       -1.92       0.148        -13      4.84e-34 

     unique        -1.86       0.158       -11.8     7.92e-29 

     effort        -2.46       0.21        -11.7     1.24e-28 

    passion        -1.84       0.17        -10.8     4.85e-25 

  credibility      -1.98       0.183       -10.8     5.39e-25 

    accuracy       -1.52       0.178       -8.54     1.11e-16 

    rep_self       -0.946      0.154       -6.12     1.65e-09 
--------------------------------------------------------------


## Predicting album preference
For each mechanism, we can also fit individual models predicting album preference, and sort by effect size. This tells us which variable is most correlated with reported preferences.

Results suggest that representativeness of one's own tastes, credibility, and accuracy are the most predictive measures of reported preferences. Representativeness of others' tastes is not predictive of preferences.


```r
dt %>% 
  pivot_longer(cols = c(credibility,
                        accuracy,
                        justifiability,
                        richness,
                        effort,
                        rep_self,
                        rep_others,
                        rep_diff,
                        expertise,
                        passion,
                        unique)) %>% 
  dplyr::select(condition, name, value, full_preference) %>% 
  nest(data = c(condition, full_preference, value)) %>% 
  mutate(fit = map(data, ~lm(full_preference ~ value, data = .)),
         results = map(fit, tidy)) %>% 
  unnest(results) %>% 
  filter(term == "value") %>% 
  arrange(p.value) %>% 
  dplyr::select(-data, -fit, -term) %>%   
  pander
```


--------------------------------------------------------------
      name        estimate   std.error   statistic   p.value  
---------------- ---------- ----------- ----------- ----------
    rep_self        1.54      0.0845       18.2      4.07e-59 

  credibility       1.13      0.0701       16.1      1.36e-48 

    accuracy        1.17       0.075       15.6      2.08e-46 

     effort        0.681      0.0664       10.3      7.71e-23 

    rep_diff       0.605      0.0591       10.2      8.26e-23 

 justifiability    0.627      0.0649       9.66      1.28e-20 

    richness       0.636      0.0686       9.27      3.27e-19 

    passion         0.67       0.086        7.8      2.79e-14 

   expertise       0.714      0.0955       7.48      2.68e-13 

     unique        0.538      0.0928        5.8      1.06e-08 

   rep_others     -0.0637     0.0839      -0.759      0.448   
--------------------------------------------------------------


### Full Model
We can also input all of our mechanisms into a single linear model predicting album preference to compare the predictive power of each measure. Again, we find accuracy, credibility, and representativeness of one's own preferences to be most predictive.


```r
dt %>% 
  lm(full_preference ~ credibility + accuracy +
       justifiability + richness + rep_diff +
       expertise + passion + unique,
     data = .) %>% 
  stargazer(type = "html", report = "vctp*")
```


<table style="text-align:center"><tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="1" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td>full_preference</td></tr>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">credibility</td><td>0.758</td></tr>
<tr><td style="text-align:left"></td><td>t = 5.019</td></tr>
<tr><td style="text-align:left"></td><td>p = 0.00000<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">accuracy</td><td>0.649</td></tr>
<tr><td style="text-align:left"></td><td>t = 4.404</td></tr>
<tr><td style="text-align:left"></td><td>p = 0.00002<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">justifiability</td><td>-0.339</td></tr>
<tr><td style="text-align:left"></td><td>t = -3.291</td></tr>
<tr><td style="text-align:left"></td><td>p = 0.002<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">richness</td><td>-0.075</td></tr>
<tr><td style="text-align:left"></td><td>t = -0.759</td></tr>
<tr><td style="text-align:left"></td><td>p = 0.449</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">rep_diff</td><td>0.341</td></tr>
<tr><td style="text-align:left"></td><td>t = 5.453</td></tr>
<tr><td style="text-align:left"></td><td>p = 0.00000<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">expertise</td><td>0.112</td></tr>
<tr><td style="text-align:left"></td><td>t = 0.908</td></tr>
<tr><td style="text-align:left"></td><td>p = 0.365</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">passion</td><td>-0.017</td></tr>
<tr><td style="text-align:left"></td><td>t = -0.159</td></tr>
<tr><td style="text-align:left"></td><td>p = 0.874</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">unique</td><td>-0.097</td></tr>
<tr><td style="text-align:left"></td><td>t = -0.994</td></tr>
<tr><td style="text-align:left"></td><td>p = 0.321</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>4.991</td></tr>
<tr><td style="text-align:left"></td><td>t = 8.182</td></tr>
<tr><td style="text-align:left"></td><td>p = 0.000<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>600</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.361</td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.353</td></tr>
<tr><td style="text-align:left">Residual Std. Error</td><td>4.037 (df = 591)</td></tr>
<tr><td style="text-align:left">F Statistic</td><td>41.765<sup>***</sup> (df = 8; 591)</td></tr>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>

## Principal Components Analysis (PCA)
It seems there are 1-2 principal components. The main variable making up the second principal component is representativeness of others' tastes, while all the other variables are making up the first component.


```r
pc <- prcomp(dt %>% 
  dplyr::select(credibility,
                accuracy,
                justifiability,
                effort,
                richness,
                rep_self,
                rep_others,
                expertise,
                passion,
                unique),
             center = TRUE,
            scale. = TRUE)
print(pc)
```

```
## Standard deviations (1, .., p=10):
##  [1] 2.3810858 1.0531244 0.8747883 0.8039741 0.7502079 0.5907311 0.5623274
##  [8] 0.5149650 0.4393032 0.3515205
## 
## Rotation (n x k) = (10 x 10):
##                       PC1         PC2        PC3         PC4         PC5
## credibility    -0.3602535  0.26784455 -0.2727913  0.07181568 -0.02138570
## accuracy       -0.3461594  0.33793761 -0.2865587  0.04275562 -0.03096014
## justifiability -0.3682321 -0.05220002 -0.2526137 -0.14378038 -0.06214798
## effort         -0.3480216  0.08591169 -0.1721443 -0.23418412 -0.28943801
## richness       -0.3581212 -0.08834115 -0.0413883 -0.07706668 -0.04539213
## rep_self       -0.2642170  0.32271062  0.2584391  0.71223546  0.35609502
## rep_others      0.1285873  0.75850101  0.4025446 -0.45431013 -0.03736608
## expertise      -0.3137302 -0.19850158  0.5501187  0.02303773 -0.17029534
## passion        -0.3176384 -0.19703789  0.4508592 -0.02253137 -0.35003690
## unique         -0.2824188 -0.19609924  0.1164953 -0.44367189  0.79319652
##                        PC6         PC7         PC8         PC9         PC10
## credibility     0.31193616 -0.23584027  0.13322773  0.04627440 -0.738668413
## accuracy        0.39200608 -0.14298357  0.08546438  0.28530876  0.646723336
## justifiability -0.16069982 -0.22952928 -0.27882140 -0.77136598  0.148581598
## effort         -0.25867053  0.56595812 -0.45500238  0.31392034 -0.102263357
## richness       -0.62367653 -0.09810442  0.65894816  0.13854358  0.047206717
## rep_self       -0.20500613  0.23430129 -0.11060186 -0.12492939  0.020216119
## rep_others     -0.11404201 -0.05296963  0.05524779 -0.13911499 -0.009422629
## expertise      -0.04962207 -0.55397176 -0.36323243  0.29780707 -0.005900202
## passion         0.43339355  0.40232864  0.32889889 -0.27502092  0.021565627
## unique          0.14830495  0.12191141 -0.02382490  0.06652762 -0.018914979
```

```r
summary(pc)
```

```
## Importance of components:
##                          PC1    PC2     PC3     PC4     PC5    PC6     PC7
## Standard deviation     2.381 1.0531 0.87479 0.80397 0.75021 0.5907 0.56233
## Proportion of Variance 0.567 0.1109 0.07653 0.06464 0.05628 0.0349 0.03162
## Cumulative Proportion  0.567 0.6779 0.75439 0.81903 0.87531 0.9102 0.94183
##                            PC8    PC9    PC10
## Standard deviation     0.51496 0.4393 0.35152
## Proportion of Variance 0.02652 0.0193 0.01236
## Cumulative Proportion  0.96834 0.9876 1.00000
```


## Order Effects
We counterbalanced the order that we presented mechanism measures. There doesn't seem to be any main effects for what order the item is presented.


```r
dt_long %>% 
  group_by(name, order, condition) %>% 
  summarise(value = mean(value)) %>% 
  group_by(name, order) %>% 
  summarise(value = value[2]-value[1]) %>% 
  ggplot(data = ., aes(x = order, y = value,
                       color = name)) +
  geom_point() +
  geom_line()
```

![](mechanisms_analysis_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

# Mediation Analyses
For each mechanism, we'll conduct a mediation analysis assessing the effect of experience on album preference. We'll also conduct reverse mediation analyses to account for participants providing consistent responses with the DV.

## Credibility
#### Mediation Model

```r
estimates <- data.frame(mediator = character(),
                        acme = numeric(),
                        acme_se = numeric(),
                        ade = numeric(),
                        ade_se = numeric())

cor(dt %>% mutate(condition_num = ifelse(condition == "Observation", 0, 1)) %>% pull(condition_num), dt$credibility)
```

```
## [1] 0.4042615
```

```r
cor(dt %>% mutate(condition_num = ifelse(condition == "Observation", 0, 1)) %>% pull(condition_num), dt$full_preference)
```

```
## [1] 0.1272074
```

```r
cor(dt$credibility, dt$full_preference)
```

```
## [1] 0.5493054
```

```r
sd(dt %>% mutate(condition_num = ifelse(condition == "Observation", 0, 1)) %>% pull(condition_num))
```

```
## [1] 0.5004144
```

```r
sd(dt$credibility)
```

```
## [1] 2.445369
```

```r
sd(dt$full_preference)
```

```
## [1] 5.017066
```

```r
model.m <- lm(credibility ~ condition, data = dt)
model.y <- lm(full_preference ~ condition + credibility, data = dt)

med.model <- mediation::mediate(model.m, model.y, 
                     treat = "condition",
                     mediator = "credibility",
                     boot = T,
                     sims = 400)
summary(med.model)
```

```
## 
## Causal Mediation Analysis 
## 
## Nonparametric Bootstrap Confidence Intervals with the Percentile Method
## 
##                Estimate 95% CI Lower 95% CI Upper p-value    
## ACME             -2.412       -2.871        -1.99  <2e-16 ***
## ADE               1.137        0.365         1.84    0.01 ** 
## Total Effect     -1.275       -2.071        -0.56  <2e-16 ***
## Prop. Mediated    1.891        1.188         3.98  <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Sample Size Used: 600 
## 
## 
## Simulations: 400
```

```r
coefs <- med.model %>% 
  tidy

estimates <- rbind(estimates,
                   data.frame(mediator = "credibility",
                              acme = coefs[1,2],
                              acme_se = coefs[1,3],
                              ade = coefs[3,2],
                              ade_se = coefs[3,3]))
```


#### Reverse Mediation

```r
model.m <- lm(full_preference ~ condition, data = dt)
model.y <- lm(credibility ~ condition + full_preference, data = dt)

rev.med.model <- mediation::mediate(model.m, model.y, 
                     treat = "condition",
                     mediator = "full_preference",
                     boot = T,
                     sims = 400)
summary(rev.med.model)
```

```
## 
## Causal Mediation Analysis 
## 
## Nonparametric Bootstrap Confidence Intervals with the Percentile Method
## 
##                Estimate 95% CI Lower 95% CI Upper p-value    
## ACME            -0.3146      -0.5176        -0.13  <2e-16 ***
## ADE             -1.6609      -1.9694        -1.32  <2e-16 ***
## Total Effect    -1.9755      -2.3309        -1.62  <2e-16 ***
## Prop. Mediated   0.1592       0.0703         0.26  <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Sample Size Used: 600 
## 
## 
## Simulations: 400
```

## Accuracy
#### Mediation Model

```r
model.m <- lm(accuracy ~ condition, data = dt)
model.y <- lm(full_preference ~ condition + accuracy, data = dt)

med.model <- mediation::mediate(model.m, model.y, 
                     treat = "condition",
                     mediator = "accuracy",
                     boot = T,
                     sims = 400)
summary(med.model)
```

```
## 
## Causal Mediation Analysis 
## 
## Nonparametric Bootstrap Confidence Intervals with the Percentile Method
## 
##                Estimate 95% CI Lower 95% CI Upper p-value    
## ACME             -1.842       -2.302        -1.38  <2e-16 ***
## ADE               0.566       -0.173         1.22    0.13    
## Total Effect     -1.275       -2.061        -0.60  <2e-16 ***
## Prop. Mediated    1.444        0.910         2.87  <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Sample Size Used: 600 
## 
## 
## Simulations: 400
```

```r
coefs <- med.model %>% 
  tidy

estimates <- rbind(estimates,
                   data.frame(mediator = "accuracy",
                              acme = coefs[1,2],
                              acme_se = coefs[1,3],
                              ade = coefs[3,2],
                              ade_se = coefs[3,3]))
```

#### Reverse Mediation

```r
model.m <- lm(full_preference ~ condition, data = dt)
model.y <- lm(accuracy ~ condition + full_preference, data = dt)

rev.med.model <- mediation::mediate(model.m, model.y, 
                     treat = "condition",
                     mediator = "full_preference",
                     boot = T,
                     sims = 400)
summary(rev.med.model)
```

```
## 
## Causal Mediation Analysis 
## 
## Nonparametric Bootstrap Confidence Intervals with the Percentile Method
## 
##                Estimate 95% CI Lower 95% CI Upper p-value    
## ACME            -0.2957      -0.4769        -0.09   0.005 ** 
## ADE             -1.2228      -1.5431        -0.94  <2e-16 ***
## Total Effect    -1.5185      -1.8465        -1.22  <2e-16 ***
## Prop. Mediated   0.1947       0.0642         0.30   0.005 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Sample Size Used: 600 
## 
## 
## Simulations: 400
```

## Justifiability
#### Mediation Model

```r
model.m <- lm(accuracy ~ condition, data = dt)
model.y <- lm(full_preference ~ condition + accuracy, data = dt)

med.model <- mediation::mediate(model.m, model.y, 
                     treat = "condition",
                     mediator = "accuracy",
                     boot = T,
                     sims = 400)
summary(med.model)
```

```
## 
## Causal Mediation Analysis 
## 
## Nonparametric Bootstrap Confidence Intervals with the Percentile Method
## 
##                Estimate 95% CI Lower 95% CI Upper p-value    
## ACME             -1.842       -2.318        -1.36  <2e-16 ***
## ADE               0.566       -0.155         1.33    0.14    
## Total Effect     -1.275       -2.068        -0.46    0.01 ** 
## Prop. Mediated    1.444        0.900         3.31    0.01 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Sample Size Used: 600 
## 
## 
## Simulations: 400
```

```r
coefs <- med.model %>% 
  tidy

estimates <- rbind(estimates,
                   data.frame(mediator = "justifiability",
                              acme = coefs[1,2],
                              acme_se = coefs[1,3],
                              ade = coefs[3,2],
                              ade_se = coefs[3,3]))
```

#### Reverse Mediation

```r
model.m <- lm(full_preference ~ condition, data = dt)
model.y <- lm(accuracy ~ condition + full_preference, data = dt)

rev.med.model <- mediation::mediate(model.m, model.y, 
                     treat = "condition",
                     mediator = "full_preference",
                     boot = T,
                     sims = 400)
summary(rev.med.model)
```

```
## 
## Causal Mediation Analysis 
## 
## Nonparametric Bootstrap Confidence Intervals with the Percentile Method
## 
##                Estimate 95% CI Lower 95% CI Upper p-value    
## ACME            -0.2957      -0.4977        -0.11  <2e-16 ***
## ADE             -1.2228      -1.4856        -0.95  <2e-16 ***
## Total Effect    -1.5185      -1.8419        -1.20  <2e-16 ***
## Prop. Mediated   0.1947       0.0783         0.31  <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Sample Size Used: 600 
## 
## 
## Simulations: 400
```

## Richness
#### Mediation Model

```r
model.m <- lm(richness ~ condition, data = dt)
model.y <- lm(full_preference ~ condition + richness, data = dt)

med.model <- mediation::mediate(model.m, model.y, 
                     treat = "condition",
                     mediator = "richness",
                     boot = T,
                     sims = 400)
summary(med.model)
```

```
## 
## Causal Mediation Analysis 
## 
## Nonparametric Bootstrap Confidence Intervals with the Percentile Method
## 
##                Estimate 95% CI Lower 95% CI Upper p-value    
## ACME             -2.480       -3.123        -1.87  <2e-16 ***
## ADE               1.205        0.318         2.11  <2e-16 ***
## Total Effect     -1.275       -2.095        -0.46  <2e-16 ***
## Prop. Mediated    1.945        1.171         5.04  <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Sample Size Used: 600 
## 
## 
## Simulations: 400
```

```r
coefs <- med.model %>% 
  tidy

estimates <- rbind(estimates,
                   data.frame(mediator = "richness",
                              acme = coefs[1,2],
                              acme_se = coefs[1,3],
                              ade = coefs[3,2],
                              ade_se = coefs[3,3]))
```

#### Reverse Mediation

```r
model.m <- lm(full_preference ~ condition, data = dt)
model.y <- lm(richness ~ condition + full_preference, data = dt)

rev.med.model <- mediation::mediate(model.m, model.y, 
                     treat = "condition",
                     mediator = "full_preference",
                     boot = T,
                     sims = 400)
summary(rev.med.model)
```

```
## 
## Causal Mediation Analysis 
## 
## Nonparametric Bootstrap Confidence Intervals with the Percentile Method
## 
##                Estimate 95% CI Lower 95% CI Upper p-value    
## ACME            -0.2026      -0.3390        -0.08  <2e-16 ***
## ADE             -3.0526      -3.3786        -2.69  <2e-16 ***
## Total Effect    -3.2552      -3.5845        -2.89  <2e-16 ***
## Prop. Mediated   0.0622       0.0257         0.10  <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Sample Size Used: 600 
## 
## 
## Simulations: 400
```

## Effort
#### Mediation Model

```r
model.m <- lm(effort ~ condition, data = dt)
model.y <- lm(full_preference ~ condition + effort, data = dt)

med.model <- mediation::mediate(model.m, model.y, 
                     treat = "condition",
                     mediator = "effort",
                     boot = T,                      sims = 400)
summary(med.model)
```

```
## 
## Causal Mediation Analysis 
## 
## Nonparametric Bootstrap Confidence Intervals with the Percentile Method
## 
##                Estimate 95% CI Lower 95% CI Upper p-value    
## ACME             -1.765       -2.204        -1.35  <2e-16 ***
## ADE               0.490       -0.346         1.30    0.23    
## Total Effect     -1.275       -2.119        -0.51  <2e-16 ***
## Prop. Mediated    1.384        0.828         3.42  <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Sample Size Used: 600 
## 
## 
## Simulations: 400
```

```r
coefs <- med.model %>% 
  tidy

estimates <- rbind(estimates,
                   data.frame(mediator = "effort",
                              acme = coefs[1,2],
                              acme_se = coefs[1,3],
                              ade = coefs[3,2],
                              ade_se = coefs[3,3]))
```

#### Reverse Mediation

```r
model.m <- lm(full_preference ~ condition, data = dt)
model.y <- lm(effort ~ condition + full_preference, data = dt)

rev.med.model <- mediation::mediate(model.m, model.y, 
                     treat = "condition",
                     mediator = "full_preference",
                     boot = T,                      sims = 400)
summary(rev.med.model)
```

```
## 
## Causal Mediation Analysis 
## 
## Nonparametric Bootstrap Confidence Intervals with the Percentile Method
## 
##                Estimate 95% CI Lower 95% CI Upper p-value    
## ACME            -0.2443      -0.4251        -0.10   0.005 ** 
## ADE             -2.2141      -2.6157        -1.83  <2e-16 ***
## Total Effect    -2.4584      -2.8240        -2.07  <2e-16 ***
## Prop. Mediated   0.0994       0.0414         0.16   0.005 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Sample Size Used: 600 
## 
## 
## Simulations: 400
```

## Representativeness
### Self Representativeness
#### Mediation Model

```r
model.m <- lm(rep_self ~ condition, data = dt)
model.y <- lm(full_preference ~ condition + rep_self, data = dt)

med.model <- mediation::mediate(model.m, model.y, 
                     treat = "condition",
                     mediator = "rep_self",
                     boot = T,                      sims = 400)
summary(med.model)
```

```
## 
## Causal Mediation Analysis 
## 
## Nonparametric Bootstrap Confidence Intervals with the Percentile Method
## 
##                Estimate 95% CI Lower 95% CI Upper p-value    
## ACME             -1.464       -1.923        -1.02  <2e-16 ***
## ADE               0.189       -0.444         0.81   0.610    
## Total Effect     -1.275       -2.071        -0.55   0.005 ** 
## Prop. Mediated    1.148        0.750         2.25   0.005 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Sample Size Used: 600 
## 
## 
## Simulations: 400
```

```r
coefs <- med.model %>% 
  tidy

estimates <- rbind(estimates,
                   data.frame(mediator = "self_representativeness",
                              acme = coefs[1,2],
                              acme_se = coefs[1,3],
                              ade = coefs[3,2],
                              ade_se = coefs[3,3]))
```

#### Reverse Mediation

```r
model.m <- lm(full_preference ~ condition, data = dt)
model.y <- lm(rep_self ~ condition + full_preference, data = dt)

rev.med.model <- mediation::mediate(model.m, model.y, 
                     treat = "condition",
                     mediator = "full_preference",
                     boot = T,                      sims = 400)
summary(rev.med.model)
```

```
## 
## Causal Mediation Analysis 
## 
## Nonparametric Bootstrap Confidence Intervals with the Percentile Method
## 
##                Estimate 95% CI Lower 95% CI Upper p-value    
## ACME             -0.285       -0.477        -0.12  <2e-16 ***
## ADE              -0.661       -0.903        -0.40  <2e-16 ***
## Total Effect     -0.946       -1.237        -0.67  <2e-16 ***
## Prop. Mediated    0.301        0.158         0.48  <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Sample Size Used: 600 
## 
## 
## Simulations: 400
```

### Other Representativeness
#### Mediation Model

```r
model.m <- lm(rep_others ~ condition, data = dt)
model.y <- lm(full_preference ~ condition + rep_others, data = dt)

med.model <- mediation::mediate(model.m, model.y, 
                     treat = "condition",
                     mediator = "rep_others",
                     boot = T,                      sims = 400)
summary(med.model)
```

```
## 
## Causal Mediation Analysis 
## 
## Nonparametric Bootstrap Confidence Intervals with the Percentile Method
## 
##                Estimate 95% CI Lower 95% CI Upper p-value    
## ACME              0.211       -0.201         0.61    0.38    
## ADE              -1.486       -2.434        -0.66  <2e-16 ***
## Total Effect     -1.275       -2.083        -0.55  <2e-16 ***
## Prop. Mediated   -0.165       -0.721         0.18    0.38    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Sample Size Used: 600 
## 
## 
## Simulations: 400
```

```r
coefs <- med.model %>% 
  tidy

estimates <- rbind(estimates,
                   data.frame(mediator = "other_representativeness",
                              acme = coefs[1,2],
                              acme_se = coefs[1,3],
                              ade = coefs[3,2],
                              ade_se = coefs[3,3]))
```

#### Reverse Mediation

```r
model.m <- lm(full_preference ~ condition, data = dt)
model.y <- lm(rep_others ~ condition + full_preference, data = dt)

rev.med.model <- mediation::mediate(model.m, model.y, 
                     treat = "condition",
                     mediator = "full_preference",
                     boot = T,                      sims = 400)
summary(rev.med.model)
```

```
## 
## Causal Mediation Analysis 
## 
## Nonparametric Bootstrap Confidence Intervals with the Percentile Method
## 
##                Estimate 95% CI Lower 95% CI Upper p-value    
## ACME           -0.02020     -0.07507         0.02    0.38    
## ADE             2.44023      2.10523         2.78  <2e-16 ***
## Total Effect    2.42003      2.05832         2.73  <2e-16 ***
## Prop. Mediated -0.00835     -0.03210         0.01    0.38    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Sample Size Used: 600 
## 
## 
## Simulations: 400
```

### Difference in Representativeness
#### Mediation Model

```r
model.m <- lm(rep_diff ~ condition, data = dt)
model.y <- lm(full_preference ~ condition + rep_diff, data = dt)

med.model <- mediation::mediate(model.m, model.y, 
                     treat = "condition",
                     mediator = "rep_diff",
                     boot = T,                      sims = 400)
summary(med.model)
```

```
## 
## Causal Mediation Analysis 
## 
## Nonparametric Bootstrap Confidence Intervals with the Percentile Method
## 
##                Estimate 95% CI Lower 95% CI Upper p-value    
## ACME             -2.327       -2.862        -1.90  <2e-16 ***
## ADE               1.052        0.254         2.01   0.015 *  
## Total Effect     -1.275       -2.092        -0.43  <2e-16 ***
## Prop. Mediated    1.825        1.139         5.53  <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Sample Size Used: 600 
## 
## 
## Simulations: 400
```

```r
coefs <- med.model %>% 
  tidy

estimates <- rbind(estimates,
                   data.frame(mediator = "representativeness_diff",
                              acme = coefs[1,2],
                              acme_se = coefs[1,3],
                              ade = coefs[3,2],
                              ade_se = coefs[3,3]))
```

#### Reverse Mediation

```r
model.m <- lm(full_preference ~ condition, data = dt)
model.y <- lm(rep_diff ~ condition + full_preference, data = dt)

rev.med.model <- mediation::mediate(model.m, model.y, 
                     treat = "condition",
                     mediator = "full_preference",
                     boot = T,                      sims = 400)
summary(rev.med.model)
```

```
## 
## Causal Mediation Analysis 
## 
## Nonparametric Bootstrap Confidence Intervals with the Percentile Method
## 
##                Estimate 95% CI Lower 95% CI Upper p-value    
## ACME            -0.2646      -0.4576        -0.12  <2e-16 ***
## ADE             -3.1013      -3.5238        -2.71  <2e-16 ***
## Total Effect    -3.3659      -3.8018        -2.97  <2e-16 ***
## Prop. Mediated   0.0786       0.0379         0.13  <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Sample Size Used: 600 
## 
## 
## Simulations: 400
```

## Expertise
#### Mediation Model

```r
model.m <- lm(expertise ~ condition, data = dt)
model.y <- lm(full_preference ~ condition + expertise, data = dt)

med.model <- mediation::mediate(model.m, model.y, 
                     treat = "condition",
                     mediator = "expertise",
                     boot = T,                      sims = 400)
summary(med.model)
```

```
## 
## Causal Mediation Analysis 
## 
## Nonparametric Bootstrap Confidence Intervals with the Percentile Method
## 
##                Estimate 95% CI Lower 95% CI Upper p-value    
## ACME             -1.401       -1.873        -0.98  <2e-16 ***
## ADE               0.125       -0.827         0.98    0.78    
## Total Effect     -1.275       -2.123        -0.46  <2e-16 ***
## Prop. Mediated    1.098        0.600         2.83  <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Sample Size Used: 600 
## 
## 
## Simulations: 400
```

```r
coefs <- med.model %>% 
  tidy

estimates <- rbind(estimates,
                   data.frame(mediator = "expertise",
                              acme = coefs[1,2],
                              acme_se = coefs[1,3],
                              ade = coefs[3,2],
                              ade_se = coefs[3,3]))
```

#### Reverse Mediation

```r
model.m <- lm(full_preference ~ condition, data = dt)
model.y <- lm(expertise ~ condition + full_preference, data = dt)

rev.med.model <- mediation::mediate(model.m, model.y, 
                     treat = "condition",
                     mediator = "full_preference",
                     boot = T,                      sims = 400)
summary(rev.med.model)
```

```
## 
## Causal Mediation Analysis 
## 
## Nonparametric Bootstrap Confidence Intervals with the Percentile Method
## 
##                Estimate 95% CI Lower 95% CI Upper p-value    
## ACME            -0.1236      -0.2170        -0.05  <2e-16 ***
## ADE             -1.7986      -2.0323        -1.51  <2e-16 ***
## Total Effect    -1.9222      -2.1652        -1.64  <2e-16 ***
## Prop. Mediated   0.0643       0.0266         0.11  <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Sample Size Used: 600 
## 
## 
## Simulations: 400
```

## Passion
#### Mediation Model

```r
model.m <- lm(passion ~ condition, data = dt)
model.y <- lm(full_preference ~ condition + passion, data = dt)

med.model <- mediation::mediate(model.m, model.y, 
                     treat = "condition",
                     mediator = "passion",
                     boot = T,                      sims = 400)
summary(med.model)
```

```
## 
## Causal Mediation Analysis 
## 
## Nonparametric Bootstrap Confidence Intervals with the Percentile Method
## 
##                Estimate 95% CI Lower 95% CI Upper p-value    
## ACME            -1.2242      -1.6792        -0.80  <2e-16 ***
## ADE             -0.0512      -0.8750         0.77    0.94    
## Total Effect    -1.2754      -2.0576        -0.43    0.02 *  
## Prop. Mediated   0.9599       0.5319         2.39    0.02 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Sample Size Used: 600 
## 
## 
## Simulations: 400
```

```r
coefs <- med.model %>% 
  tidy

estimates <- rbind(estimates,
                   data.frame(mediator = "passion",
                              acme = coefs[1,2],
                              acme_se = coefs[1,3],
                              ade = coefs[3,2],
                              ade_se = coefs[3,3]))
```

#### Reverse Mediation

```r
model.m <- lm(full_preference ~ condition, data = dt)
model.y <- lm(passion ~ condition + full_preference, data = dt)

rev.med.model <- mediation::mediate(model.m, model.y, 
                     treat = "condition",
                     mediator = "full_preference",
                     boot = T,                      sims = 400)
summary(rev.med.model)
```

```
## 
## Causal Mediation Analysis 
## 
## Nonparametric Bootstrap Confidence Intervals with the Percentile Method
## 
##                Estimate 95% CI Lower 95% CI Upper p-value    
## ACME            -0.1483      -0.2780        -0.04    0.01 ** 
## ADE             -1.6901      -2.0042        -1.37  <2e-16 ***
## Total Effect    -1.8384      -2.1756        -1.51  <2e-16 ***
## Prop. Mediated   0.0806       0.0242         0.15    0.01 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Sample Size Used: 600 
## 
## 
## Simulations: 400
```

## Uniqueness
#### Mediation Model

```r
model.m <- lm(unique ~ condition, data = dt)
model.y <- lm(full_preference ~ condition + unique, data = dt)

med.model <- mediation::mediate(model.m, model.y, 
                     treat = "condition",
                     mediator = "unique",
                     boot = T,                      sims = 400)
summary(med.model)
```

```
## 
## Causal Mediation Analysis 
## 
## Nonparametric Bootstrap Confidence Intervals with the Percentile Method
## 
##                Estimate 95% CI Lower 95% CI Upper p-value    
## ACME             -0.939       -1.328        -0.51  <2e-16 ***
## ADE              -0.336       -1.307         0.59    0.53    
## Total Effect     -1.275       -2.079        -0.36  <2e-16 ***
## Prop. Mediated    0.737        0.333         2.58  <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Sample Size Used: 600 
## 
## 
## Simulations: 400
```

```r
coefs <- med.model %>% 
  tidy

estimates <- rbind(estimates,
                   data.frame(mediator = "uniqueness",
                              acme = coefs[1,2],
                              acme_se = coefs[1,3],
                              ade = coefs[3,2],
                              ade_se = coefs[3,3]))
```

#### Reverse Mediation

```r
model.m <- lm(full_preference ~ condition, data = dt)
model.y <- lm(unique ~ condition + full_preference, data = dt)

rev.med.model <- mediation::mediate(model.m, model.y, 
                     treat = "condition",
                     mediator = "full_preference",
                     boot = T,                      sims = 400)
summary(rev.med.model)
```

```
## 
## Causal Mediation Analysis 
## 
## Nonparametric Bootstrap Confidence Intervals with the Percentile Method
## 
##                Estimate 95% CI Lower 95% CI Upper p-value    
## ACME            -0.0977      -0.1870        -0.03  <2e-16 ***
## ADE             -1.7646      -2.0982        -1.48  <2e-16 ***
## Total Effect    -1.8623      -2.1889        -1.59  <2e-16 ***
## Prop. Mediated   0.0525       0.0172         0.10  <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Sample Size Used: 600 
## 
## 
## Simulations: 400
```

# Comparison of Estimates
## Average Causal Mediation Effects

```r
estimates <- estimates %>%
  rename(acme = estimate,
         acme_se = std.error,
         ade = estimate.1,
         ade_se = std.error.1) %>%
  mutate(acme = -acme,
         effect_size = abs(acme),
         mediator = as.factor(mediator))

ggplot(estimates,
       aes(x = reorder(mediator, -effect_size),
           y = acme,
           ymin = acme - 1.96*acme_se,
           ymax = acme + 1.96*acme_se)) +
  geom_point() +
  geom_errorbar() +
  labs(x = "Mediator",
       y = "Average Causal Mediation Effects") +
  theme_bw()
```

![](mechanisms_analysis_files/figure-html/unnamed-chunk-37-1.png)<!-- -->

## Average Direct Effect

```r
ggplot(estimates %>% mutate(direct_effect_size = abs(ade)),
       aes(x = reorder(mediator, -direct_effect_size),
           y = ade,
           ymin = ade - 1.96*ade_se,
           ymax = ade + 1.96*ade_se)) +
  geom_point() +
  geom_errorbar() +
  labs(x = "Mediator",
       y = "Average Direct Effect") +
  theme_bw()
```

![](mechanisms_analysis_files/figure-html/unnamed-chunk-38-1.png)<!-- -->



