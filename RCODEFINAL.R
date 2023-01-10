packages <- c("lavaan", "dplyr", "semTools", "semPower", "semTable", "psych", "naniar", "knitr", "this.path")
install.packages(setdiff(packages, rownames(installed.packages())))
library(lavaan)
library(knitr)
library(semTools)
library(semPower)
library(semTable)
library(dplyr)
library(naniar)
library(psych)
library(this.path)

#nastaveni slozky, ve ktere se nachazi skript, jako pracovni
setwd(this.dir())

#prvni studie - analyza

#power analyza
powerhigh <- semPower.aPriori(effect = .05, effect.measure = 'RMSEA',
                              alpha = .05, power = .80, df = 59)
powermedium <- semPower.aPriori(effect = .065, effect.measure = 'RMSEA',
                                alpha = .05, power = .80, df = 59)
powerlow <- semPower.aPriori(effect = .08, effect.measure = 'RMSEA',
                             alpha = .05, power = .80, df = 59)
summary(powerhigh)
summary(powermedium)
summary(powerlow)
#nacteni datasetu prvni studie
mydata <- read.csv2('./STUDY1-29-08-2022.csv')



#testovani unifaktoroveho modelu
unifactor <- '
ecoanxiety =~ Q12_1 + Q12_2 + Q12_3 + Q12_4 + Q12_5 + Q12_6 + Q12_7 + Q12_8 + Q12_9 + Q12_10 + Q12_11 + Q12_12 + Q12_13
'
fituni <- cfa(unifactor, mydata, estimator = "WLSMV", ordered = TRUE)
summary(fituni, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

#testovani puvodniho ctyrfaktoroveho modelu
lavmodel <- '
f1 =~ Q12_1 + Q12_2 + Q12_3 + Q12_4
f2 =~ Q12_5 + Q12_6 + Q12_7
f3 =~ Q12_8 + Q12_9 + Q12_10
f4 =~ Q12_11 + Q12_12 + Q12_13'
fit <- cfa(lavmodel, mydata, estimator="WLSMV", ordered = TRUE)
summary(fit, fit.measures = TRUE, standardized = TRUE)

# testovani modelu s jednim faktorem druheho radu 
# tento model uvadim pouze kvuli shode s textem BP, pouzivani techto modelu zrejme neni ve vetsine pripadu vhodne, 
# nebot se vysledky nedaji jasne interpretovat a predpokladem je zamenitelnost jednotlivych subskal, 
# respektive ze vsechny ovlivnuji faktor druheho radu stejne, viz Eid et al. (2017) nebo Heinrich et al. (2020)
# Eid, M., Geiser, C., Koch, T., & Heene, M. (2017). Anomalous results in G-factor models: Explanations and alternatives. Psychological Methods, 22, 541–562. https://doi.org/10.1037/met0000083
# Heinrich, M., Zagorscak, P., Eid, M., & Knaevelsrud, C. (2020). Giving G a Meaning: An Application of the Bifactor-(S-1) Approach to Realize a More Symptom-Oriented Modeling of the Beck Depression Inventory–II. Assessment, 27(7), 1429–1447. https://doi.org/10.1177/1073191118803738

secondorder <- '
f1 =~ Q12_1 + Q12_2 + Q12_3 + Q12_4
f2 =~ Q12_5 + Q12_6 + Q12_7
f3 =~ Q12_8 + Q12_9 + Q12_10
f4 =~ Q12_11 + Q12_12 + Q12_13
envanxiety =~ f1 + f2 + f3 + f4'
fitsecondorder <- cfa(secondorder, mydata, estimator="WLSMV", ordered = TRUE)
summary(fitsecondorder, fit.measures = TRUE, standardized = TRUE)

# reliabilita zvoleneho modelu (puvodni ctyrfaktorovy model)
compRelSEM(fit, ord.scale = TRUE)
# testovani bifaktor(s-1) modelu
# Timto modelem se ve sve praci v prvni studii nezabyvam z duvodu casove tisne pri jejim dokoncovani.
# Pro uplnost a koherenci s analyzou druhe studie zde nicmene uvadim vysledky i pro tento model
bifactors1 <- '
ecoanxiety =~ Q12_1 + Q12_2 + Q12_3 + Q12_4 + Q12_5 + Q12_6 + Q12_7 + Q12_8 + Q12_9 + Q12_10 + Q12_11 + Q12_12 + Q12_13
f2 =~ Q12_5 + Q12_6 + Q12_7
f3 =~ Q12_8 + Q12_9 + Q12_10
f4 =~ Q12_11 + Q12_12 + Q12_13
ecoanxiety ~~ 0*f2
ecoanxiety ~~ 0*f3
ecoanxiety ~~ 0*f4
'
fitbi <- cfa(bifactors1, mydata, estimator = "WLSMV", ordered = TRUE)
summary(fitbi, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# analyza druheho sberu dat

mydata <- read.csv2('./STUDY2-10-12-2022.csv')

#rekodovani promennych Hoggovy skaly jako ordinalnich a spocitani souctovych skoru skaly a jejich subskal
mydata <- mydata %>%
  mutate(
    EA_total = select(., starts_with("Q12")) %>% rowSums(),
    EA_affective = select(., c(Q12_1, Q12_2, Q12_3, Q12_4)) %>% rowSums(),
    EA_rumination = select(., c(Q12_5, Q12_6, Q12_7)) %>% rowSums(),
    EA_behavioral = select(., c(Q12_8, Q12_9, Q12_10)) %>% rowSums(),
    EA_personal = select(., c(Q12_11, Q12_12, Q12_13)) %>% rowSums()) %>%
  mutate_at(vars(starts_with("Q12")), funs(as.ordered(.)))
#overovani puvodniho ctyrfaktoroveho modelu
lavmodel <- '
f1 =~ Q12_1 + Q12_2 + Q12_3 + Q12_4
f2 =~ Q12_5 + Q12_6 + Q12_7
f3 =~ Q12_8 + Q12_9 + Q12_10
f4 =~ Q12_11 + Q12_12 + Q12_13'
fit <- cfa(lavmodel, mydata, estimator="WLSMV")
compRelSEM(fit, ord.scale = TRUE)
summary(fit, fit.measures = TRUE, standardized = TRUE, ci = TRUE)
residuals(fit, type="cor")$cov

parameterestimates(fit, ci = TRUE, level = 0.95)
# celkovy vysvetleny rozptyl
summary(mean(1-mean(rowSums(inspect(fit,what="std")$theta))))


lavunifactor <- '
f1 =~ Q12_1 + Q12_2 + Q12_3 + Q12_4 + Q12_5 + Q12_6 + Q12_7 + Q12_8 + Q12_9 + Q12_10 + Q12_11 + Q12_12 + Q12_13
'
fituni <- cfa(lavunifactor, mydata, estimator = "WLSMV", ordered = TRUE)
summary(fituni, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


#bifaktor(s-1) model
bifactor <- '
f1 =~ Q12_1 + Q12_2 + Q12_3 + Q12_4 + Q12_5 + Q12_6 + Q12_7 + Q12_8 + Q12_9 + Q12_10 + Q12_11 + Q12_12 + Q12_13
f2 =~ Q12_5 + Q12_6 + Q12_7
f3 =~ Q12_8 + Q12_9 + Q12_10
f4 =~ Q12_11 + Q12_12 + Q12_13
f1 ~~ 0*f2
f1 ~~ 0*f3
f1 ~~ 0*f4
'
fitbi <- cfa(bifactor, mydata, estimator = "WLSMV", ordered = TRUE)
summary(fitbi, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
#celkovy vysvetleny rozptyl
summary(mean(1-mean(rowSums(inspect(fitbi,what="std")$theta))))
summary(compareFit(fituni, fit, fitbi, argsLRT = list(method = "satorra.bentler.2001")))

# analyzy invariance - nejprve invariance prvniho a druheho datasetu
# nacteni dat a transformace promennych HEAS jako ordinalnich
longitudinal <-read.csv2('./longitudinal1.csv')
longitudinal <- longitudinal %>%
  mutate_at(vars(starts_with("Q12")), funs(as.ordered(.)))
# invariance ctyrfaktoroveho modelu
syntax.config <- measEq.syntax(configural.model = lavmodel,
                               data = longitudinal,
                               estimator="WLSMV",
                               ID.fac = "std.lv", ID.cat = "Wu.Estabrook.2016", ordered = TRUE, group = "DATASET", return.fit = TRUE)
summary(syntax.config, fit.measures=TRUE)
syntax.thresh <- measEq.syntax(configural.model = lavmodel,
                               data = longitudinal,
                               estimator="WLSMV",
                               ID.fac = "std.lv", ID.cat = "Wu.Estabrook.2016", ordered = TRUE, group = "DATASET", return.fit = TRUE, group.equal = c("thresholds"))
syntax.metric <- measEq.syntax(configural.model = lavmodel,
                               data = longitudinal,
                               estimator="WLSMV",
                               ID.fac = "std.lv", ID.cat = "Wu.Estabrook.2016",
                               ordered = TRUE, group = "DATASET", return.fit = TRUE, group.equal  = c("thresholds","loadings"))
fitcomparision <- compareFit(syntax.config, syntax.thresh, syntax.metric, argsLRT = list(method = "satorra.bentler.2001"))
summary(fitcomparision)

# invariance bifaktor(s-1) modelu
syntax.config <- measEq.syntax(configural.model = bifactor,
                               data = longitudinal,
                               estimator="WLSMV",
                               ID.fac = "std.lv", ID.cat = "Wu.Estabrook.2016", ordered = TRUE, group = "DATASET", return.fit = TRUE)
syntax.thresh <- measEq.syntax(configural.model = bifactor,
                               data = longitudinal,
                               estimator="WLSMV",
                               ID.fac = "std.lv", ID.cat = "Wu.Estabrook.2016", ordered = TRUE, group = "DATASET", return.fit = TRUE, group.equal = c("thresholds"))
syntax.metric <- measEq.syntax(configural.model = bifactor,
                               data = longitudinal,
                               estimator="WLSMV",
                               ID.fac = "std.lv", ID.cat = "Wu.Estabrook.2016",
                               ordered = TRUE, group = "DATASET", return.fit = TRUE, group.equal  = c("thresholds","loadings"))
fitcomparision <- compareFit(syntax.config, syntax.thresh, syntax.metric, argsLRT = list(method = "satorra.bentler.2001"))
summary(fitcomparision)

#konfirmacni faktorova analyza na spojenem datasetu
#CFA korelovaneho modelu
fit <- cfa(lavmodel, longitudinal, estimator="WLSMV", ordered = TRUE)
summary(fit, fit.measures = TRUE, standardized = TRUE)
residuals(fit, type="cor")$cov
#CFA bifaktor(s-1) modelu
fitbi <- cfa(bifactor, longitudinal, estimator = "WLSMV", ordered = TRUE)
summary(fitbi, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
residuals(fitbi, type="cor")$cov
#porovnani indikatoru fitu
summary(compareFit(fit, fitbi, argsLRT = list(method = "satorra.bentler.2001")))

table(longitudinal$Q5)
longitudinal <- longitudinal %>% 
  mutate(Q5 = na_if(Q5, "3")) %>%
  mutate(Q5 = na_if(Q5, "5")) %>%
  mutate(Q5 = na_if(Q5, "4"))
#invariance mereni pro gender - korelovany model
measeqconfig <- measEq.syntax(lavmodel, longitudinal, ID.fac = "std.lv", ID.cat = "Wu", group = "Q5", return.fit = TRUE)
measeqthresh <- measEq.syntax(lavmodel, longitudinal, ID.fac = "std.lv", ID.cat = "Wu", group = "Q5", group.equal = c("thresholds"), return.fit = TRUE)
measeqmetric <- measEq.syntax(lavmodel, longitudinal, ID.fac = "std.lv", ID.cat = "Wu", group = "Q5", group.equal = c("thresholds", "loadings"), return.fit = TRUE)
summary(compareFit(measeqconfig, measeqthresh, measeqmetric, argsLRT = list(method = "satorra.bentler.2001")))
#invariance mereni pro gender - bifaktorovy(s-1) model
measeqconfig <- measEq.syntax(bifactor, longitudinal, ID.fac = "std.lv", ID.cat = "Wu", group = "Q5", return.fit = TRUE)
measeqthresh <- measEq.syntax(bifactor, longitudinal, ID.fac = "std.lv", ID.cat = "Wu", group = "Q5", group.equal = c("thresholds"), return.fit = TRUE)
measeqmetric <- measEq.syntax(bifactor, longitudinal, ID.fac = "std.lv", ID.cat = "Wu", group = "Q5", group.equal = c("thresholds", "loadings"), return.fit = TRUE)
summary(compareFit(measeqconfig, measeqthresh, measeqmetric, argsLRT = list(method = "satorra.bentler.2001")))
# testing invariance for vek a vzdelani
# tvorba dummy promennych, ktera rozdeli dataset na dve priblizne stejne poloviny 
# u veku podle medianu, u dosazeneho vzdelani na vyssi odborne a vyssi a stredoskolske nebo nizsi
median(longitudinal$Q3, na.rm = TRUE)
table(longitudinal$Q8)
longitudinal <- longitudinal %>%
  mutate(agedummy = if_else(Q3>29, 1, 0)) %>%
  mutate(educdummy = if_else(Q8<5,0,1))
#kontrola velikosti vyslednych skupin
table(longitudinal$educdummy)
table(longitudinal$agedummy)
#testovani invariance pro vek - korelovany model
measeqconfig <- measEq.syntax(lavmodel, longitudinal, estimator = "WLSMV", ID.fac = "std.lv", ID.cat = "Wu", group = "agedummy", return.fit = TRUE)
measeqthresh <- measEq.syntax(lavmodel, longitudinal, estimator = "WLSMV", ID.fac = "std.lv", ID.cat = "Wu", group = "agedummy", group.equal = c("thresholds"), return.fit = TRUE)
measeqmetric <- measEq.syntax(lavmodel, longitudinal, estimator = "WLSMV", ID.fac = "std.lv", ID.cat = "Wu", group = "agedummy", group.equal = c("thresholds", "loadings"), return.fit = TRUE)
summary(compareFit(measeqconfig, measeqthresh, measeqmetric, argsLRT = list(method = "satorra.bentler.2001")))
#testovani invariance pro vek - bifaktor model
measeqconfig <- measEq.syntax(bifactor, longitudinal, estimator = "WLSMV", ID.fac = "std.lv", ID.cat = "Wu", group = "agedummy", return.fit = TRUE)
measeqthresh <- measEq.syntax(bifactor, longitudinal, estimator = "WLSMV", ID.fac = "std.lv", ID.cat = "Wu", group = "agedummy", group.equal = c("thresholds"), return.fit = TRUE)
measeqmetric <- measEq.syntax(bifactor, longitudinal, estimator = "WLSMV", ID.fac = "std.lv", ID.cat = "Wu", group = "agedummy", group.equal = c("thresholds", "loadings"), return.fit = TRUE)
summary(compareFit(measeqconfig, measeqthresh, measeqmetric, argsLRT = list(method = "satorra.bentler.2001")))

#testovani invariance pro vzdelani - korelovany model
measeqconfig <- measEq.syntax(lavmodel, longitudinal, estimator = "WLSMV", ID.fac = "std.lv", ID.cat = "Wu", group = "educdummy", return.fit = TRUE)
measeqthresh <- measEq.syntax(lavmodel, longitudinal, estimator = "WLSMV", ID.fac = "std.lv", ID.cat = "Wu", group = "educdummy", group.equal = c("thresholds"), return.fit = TRUE)
measeqmetric <- measEq.syntax(lavmodel, longitudinal, estimator = "WLSMV", ID.fac = "std.lv", ID.cat = "Wu", group = "educdummy", group.equal = c("thresholds", "loadings"), return.fit = TRUE)
summary(compareFit(measeqconfig, measeqthresh, measeqmetric, argsLRT = list(method = "satorra.bentler.2001")))
#testovani invariance pro vzdelani - bifaktor model
measeqconfig <- measEq.syntax(bifactor, longitudinal, estimator = "WLSMV", ID.fac = "std.lv", ID.cat = "Wu", group = "educdummy", return.fit = TRUE)
measeqthresh <- measEq.syntax(bifactor, longitudinal, estimator = "WLSMV", ID.fac = "std.lv", ID.cat = "Wu", group = "educdummy", group.equal = c("thresholds"), return.fit = TRUE)
measeqmetric <- measEq.syntax(bifactor, longitudinal, estimator = "WLSMV", ID.fac = "std.lv", ID.cat = "Wu", group = "educdummy", group.equal = c("thresholds", "loadings"), return.fit = TRUE)
summary(compareFit(measeqconfig, measeqthresh, measeqmetric, argsLRT = list(method = "satorra.bentler.2001")))

#Analyza dotazniku proenvironmentalniho chovani
proenvuni_model <- '
proenv =~ PROENV_1 + PROENV_2 + PROENV_3 + PROENV_4 + PROENV_5 + PROENV_6 + PROENV_7 + PROENV_8 + PROENV_9 + PROENV_10 + PROENV_11 + PROENV_12'
fit_proenvuni <- cfa(proenvuni_model, mydata, estimator = "WLSMV")
summary(fit_proenvuni, fit.measures = TRUE, standardized = TRUE)
#model dvou korelujicich faktoru
proenvmodel_basic <- '
personaldiff =~ PROENV_1 + PROENV_2 + PROENV_3 + PROENV_4 + PROENV_5 + PROENV_6
social =~ PROENV_7 + PROENV_8 + PROENV_9 + PROENV_10 + PROENV_11 + PROENV_12'
fit_proenvmodel_basic <- cfa(proenvmodel_basic, mydata, estimator="WLSMV")
residuals(fit_proenvmodel_basic, type="cor")$cov
summary(fit_proenvmodel_basic, fit.measures = TRUE, standardized = TRUE, rsquare=TRUE)
compRelSEM(fit_proenvmodel_basic, return.total = TRUE)
#revidovany model, bez ekonomicky motivovanych otazek
proenvmodel_revised <- '
social =~ PROENV_9 + PROENV_10 + PROENV_11 + PROENV_12
personaldiff =~ PROENV_2 + PROENV_3 + PROENV_7 + PROENV_8
'
fit_proenvmodel_revised <- cfa(proenvmodel_revised, mydata, estimator="WLSMV")
summary(fit_proenvmodel_revised, fit.measures = TRUE, standardized = TRUE, rsquare=TRUE)
residuals(fit_proenvmodel_revised, type="cor")$cov
compRelSEM(fit_proenvmodel_revised, return.total = TRUE, ord.scale = TRUE)


#model - proenvironmentalni politiky
proenvpolmodelbasic <- '
f2 =~ Q25_10 + Q25_11 + Q25_12 + Q25_16 + Q25_17 + Q25_18 + Q25_19 + Q25_20
f1 =~ Q25_5 + Q25_6 + Q25_7 + Q25_9 + Q25_14 + Q25_8 + Q25_13 + Q25_15
'
proenvpolmodelbasic_cfa <- cfa(proenvpolmodelbasic, mydata, estimator = "WLSMV", ordered = TRUE)
summary(proenvpolmodelbasic_cfa, fit.measures = TRUE, standardized = TRUE, rsquare=TRUE)
residuals(proenvpolmodelbasic_cfa, type="cor")$cov
compRelSEM(proenvpolmodelbasic_cfa, return.total = TRUE)
modificationindices(proenvpolmodelbasic_cfa, sort=TRUE)
proenvpolmodel <- '
f2 =~ Q25_10 + Q25_11 + Q25_12 + Q25_16 + Q25_17 + Q25_18 + Q25_19 + Q25_20
f1 =~ Q25_5 + Q25_6 + Q25_7 + Q25_9 + Q25_14
'
proenvpol_cfa <- cfa(proenvpolmodel, mydata, estimator = "WLSMV", ordered = TRUE)
summary(proenvpol_cfa, fit.measures = TRUE, standardized = TRUE, rsquare=TRUE)

compRelSEM(proenvpol_cfa, ord.scale = TRUE)
residuals(proenvpol_cfa, type="cor")$cov
modificationindices(proenvpol_cfa, sort=TRUE)
#vyvoreni souctovych skoru jednotlivych skal a subskal
mydata <- mydata %>%
  mutate(
    GAD7 = select(., starts_with("Q20")) %>% rowSums(),
    PHQ9 = select(., starts_with("Q19")) %>% rowSums(),
    PROENV_personal = select(., PROENV_2, PROENV_3, PROENV_7, PROENV_8) %>% rowSums(),
    PROENV_social = select(., PROENV_9:PROENV_12) %>% rowSums(),
    PROENVPOL_Reg = select(., c(Q25_10:Q25_12,Q25_16:Q25_20)) %>% rowSums(),
    PROENVPOL_Sup = select(., c(Q25_5:Q25_7,Q25_9,Q25_14)) %>% rowSums(),
    SWLS = select(., c(Q21_1:Q21_5)) %>% rowSums()
  )
#split dataset by total EA size
splitbyeasize <- mydata[order(mydata$EA_total),]
medianea <- median(mydata$EA_total, na.rm = TRUE)
lowerhalf <- mydata[1:round(nrow(mydata)/2),] 
higherhalf <- mydata[round((nrow(mydata)/2)+1):nrow(mydata),]
#test signifikance rozdilu korelaci environmentalni uzkosti s proenvironmentalnim chovanim ve skupinach
# s nizkou a vysokou hladinou teto uzkosti
lowercorrproenvpersonal <- corr.test(lowerhalf$EA_total, lowerhalf$PROENV_personal) %>% .$r
highercorrproenvpersonal <- corr.test(higherhalf$EA_total, higherhalf$PROENV_personal) %>% .$r  
r.test(104, highercorrproenvpersonal, lowercorrproenvpersonal)

lowercorrproenvsocial <- corr.test(lowerhalf$EA_total, lowerhalf$PROENV_social) %>% .$r
highercorrproenvsocial <- corr.test(higherhalf$EA_total, higherhalf$PROENV_social) %>% .$r
r.test(104, highercorrproenvsocial, lowercorrproenvsocial)

# posledni analyza - tedy pruzkum korelaci mezi jednotlivymi promennymi byl proveden v programu JASP
# v tomto programu byly take zkoumany vsechny deskriptivni statistiky, rozlozeni promennych a jejich normalita
# v programu JASP jsem take provadel polozkovou analyzu a analyzu korelacni matice pri redefinovani modelu nekterych dotazniku










