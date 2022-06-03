
Sdat <- Pre_Post_25to75 %>% 
                select(subject,
                       measurementPoint,
                       CJ_diff,
                       RIAS_points,
                       TROG_Block_score,
                       age,
                       condition)

Sdat = reshape2::dcast(Sdat,subject+RIAS_points+TROG_Block_score+age+condition ~ measurementPoint, value.var="CJ_diff")
Sdat = Sdat[complete.cases(Sdat),]
Sdat = reshape2::melt(Sdat, id.vars = c("subject","RIAS_points","TROG_Block_score","age","condition"),
           variable.name = "measurementPoint",value.name = "CJ_diff")


results=as.data.frame(ezANOVA(data=Sdat, dv="CJ_diff", wid=.("subject"),
                              between = c("condition"), within = c("measurementPoint"),within_covariates = c("age"),
                              type=3,detailed=T)$ANOVA)

results$pareta=results$SSn/(results$SSn+results$SSd)
is.num=sapply(results, is.numeric)
results[is.num] =lapply(results[is.num], round, 3)
results

#_______________________________________________________________________________
#Marginal estimated means
#_______________________________________________________________________________


# code
# Load the stats package library
library(rstatix)

# Run the test
get_anova_table(anova_test(CJ_diff ~ condition + age, data= Pre_Post_25to75))

# Pairwise comparisons
library(emmeans)
# install.packages("emmeans")
my_pairwise_test <- Pre_Post_25to75 %>% 
  emmeans_test(CJ_diff ~ condition, covariate = age,
               p.adjust.method = "sidak")



Sdat <- Pre_Post_25to75 %>% 
  select(subject,
         measurementPoint,
         CJ_diff,
         RIAS_points,
         TROG_Block_score,
         age,
         condition)
Sdat$conditions = paste(Sdat$measurementPoint,Sdat$condition,sep = "_")

# Display the adjusted means of each group
# Also called as the estimated marginal means (emmeans)
gDat = get_emmeans(emmeans_test(CJ_diff ~ conditions, covariate = c("age","RIAS_points","TROG_Block_score"), data = Sdat, p.adjust.method = "sidak"))

gDat = as.data.frame(gDat)

gDat$timepoint = case_when(grepl("T1",gDat$conditions)~"T1",
                           grepl("T2",gDat$conditions)~"T2")
gDat$condition = case_when(grepl("AC",gDat$conditions)~"AC",
                           grepl("MF",gDat$conditions)~"MF",
                           grepl("PF",gDat$conditions)~"PF")
gDat$timepoint = factor(gDat$timepoint,levels = c("T1","T2"))
gDat$condition = factor(gDat$condition,levels = c("MF","PF","AC"))
                           
ggplot(gDat)+
  geom_bar(aes(timepoint, emmean,fill = condition),stat = "identity",position = "dodge")+
  # geom_point(aes(conditions, emmean,color = conditions),size=2)+
  geom_errorbar(aes(x=timepoint,fill = condition, ymin=emmean-se, ymax=emmean+se),position = "dodge")