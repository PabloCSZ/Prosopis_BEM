

# libraries 

library(readxl) # To read excel files
library(tidyverse) # to process data and make the graphics
library(ggthemes) # to polish some figure details
library(MuMIn)
library(r2glmm) # to calculate the R2 of glm models
library(qpcR)
library(openxlsx) # to save results in excel format
library(ggpubr) # to put two figures together
library(flexmix)
library(AICcmodavg) # to to calculate AIC and VIC from GLM models
library(car) # To calculate VIF

# Data
setwd("E:/OneDrive/OneDrive - UDEP/Semilla/Ejecucion/Biomasa")
setwd("D:/OneDrive - UDEP/Semilla/Ejecucion/Biomasa")

# Loading the BEM database ---------------------

#trees <- read.csv("Borrador/Datos/trees.csv")
allo <- read_xlsx("Borrador/Datos/Allometric_data.xlsx")
db <- read_xlsx("Datos/BEM_database.xlsx")

# renaming some colummns for data analysis
colnames(db)[c(5,6,9,16:18)] <- c("Thick_branch","Thin_branch","AGB","BD", "Height", "Canopy")

# Basic statistics

db %>% group_by(Source) %>% 
  summarise(N = n(), 
            min_BD= min(BD, na.rm= T), 
            mean_BD = mean(BD,na.rm= T), 
            max_BD = max(BD,na.rm= T),
            min_AGB = min(AGB,na.rm= T), 
            mean_AGB = mean(AGB,na.rm= T), 
            max_AGB = max(AGB,na.rm= T))

# Filtering only Salazar data 

allo <- db %>%
  filter(Source == "Salazar,2019") %>%
  filter(!is.na(AGB))

allo <- allo %>%  mutate(AGB_ch = 0.0559*Height*0.98*BD^2)

# modeling our field data to build univarant and multivariant models for Prosopis Pallida -------------------

glm_model         <- glm(AGB ~ BD,
                         data = allo,
                         family = gaussian(link =  "log"))
log_log_model     <- lm(log(AGB) ~ log(BD), data = allo)

glm_model2         <- glm(AGB ~ BD + Height,
                         data = allo,
                         family = gaussian(link =  "log"))

glm_model3         <- glm(AGB ~ BD + Height + Canopy,
                          data = allo,
                          family = gaussian(link =  "log"))

glm_model4         <- glm(AGB ~ BD + Canopy,
                          data = allo,
                          family = gaussian(link =  "log"))

glm_model5         <- glm(AGB ~ BD * Height,
                          data = allo,
                          family = gaussian(link =  "log"))

glm_model6         <- glm(AGB ~ BD * Canopy,
                          data = allo,
                          family = gaussian(link =  "log"))

glm_model7         <- glm(AGB ~ BD * Height * Canopy,
                          data = allo,
                          family = gaussian(link =  "log"))

# calcultating VIF to see if all variables are important in the multivariant models
# not really an objective of our paper but one reviewer suggested

vif(glm_model2)
vif(glm_model3)
vif(glm_model4)
vif(glm_model5)
vif(glm_model6)
vif(glm_model7)

# Calculating R2 and RMSE for all models

# manually calculating RMSE for the log_log model and comparing it with the glm model and Chave's model

pred_log_log    <- predict(log_log_model)
pred_glm        <- predict(glm_model)

sqrt(sum((allo$AGB - exp(pred_log_log))^2)/30)
sqrt(sum((allo$AGB - exp(pred_glm))^2)/30)
sqrt(sum((allo$AGB - allo$AGB_ch)^2)/30)


# r2 for all the GLM

a<- r2beta(glm_model, partial = TRUE, data = allo)
b<- r2beta(glm_model2, partial = TRUE, data = allo)
c<- r2beta(glm_model3, partial = TRUE)
d<- r2beta(glm_model4, partial = TRUE)
e<- r2beta(glm_model5, partial = TRUE)
f<- r2beta(glm_model6, partial = TRUE)
g<- r2beta(glm_model7, partial = TRUE)
h<- r2beta(log_log_model, partial = T)


a<- cbind(model = "AGB ~ BD",a, RMSE = RMSE(glm_model), AIC = glm_model$aic, BIC = useBIC(glm_model))
b<- cbind(model = "AGB ~ BD + Height", b, RMSE = RMSE(glm_model2), AIC = glm_model2$aic, BIC = useBIC(glm_model2))
c<- cbind(model = "AGB ~ BD + Height + Canopy",c, RMSE = RMSE(glm_model3), AIC = glm_model3$aic, BIC = useBIC(glm_model3))
d<- cbind(model = "AGB ~ BD + Canopy", d, RMSE = RMSE(glm_model4), AIC = glm_model4$aic, BIC = useBIC(glm_model4))
e<- cbind(model = "AGB ~ BD * Height", e, RMSE = RMSE(glm_model5), AIC = glm_model5$aic, BIC = useBIC(glm_model5))
f<- cbind(model = "AGB ~ BD * Canopy", f, RMSE = RMSE(glm_model6), AIC = glm_model6$aic, BIC = useBIC(glm_model6))
g<- cbind(model = "AGB ~ BD * Height * Canopy", g, RMSE = RMSE(glm_model7), AIC = glm_model7$aic, BIC = useBIC(glm_model7))
h<- cbind(model = "log(AGB) ~ log(BD)", h, RMSE = 344.3277, AIC = NA, BIC = NA) # this never comes from the manual calculation made earlier

table1 <- rbind(h,a,b,c,d,e,f,g)
table1 <- table1 %>% 
  dplyr::select(c(-v1,-F, -v2,-ncp))  %>% 
  mutate(across(Rsq: BIC, round, 2))

openxlsx::write.xlsx(table1, "R2andRMSE2.xlsx")

#The relationship between Y and X can b expressed in the following equation Y = exp(bX + a), which is also expressed as Y = exp(bX)*exp(a)

# Data frame
pred_glm        <- predict(glm_model)
pred_log_log    <- predict(log_log_model)
pred_glm2        <- predict(glm_model2)
pred_glm3        <- predict(glm_model3)
pred_glm4        <- predict(glm_model4)

df <- data.frame(
  BD = allo$BD,
  y_obs = allo$AGB,
  y_log_log = pred_log_log,
  y_glm = pred_glm,
  y_glm2 = pred_glm2,
  y_glm3 = pred_glm3,
  y_glm4 = pred_glm4,
  AGB_ch = allo$AGB_ch)

# Theme---------------
custom_theme <- theme(panel.grid.major = element_blank(), # blank background 
                      panel.grid.minor = element_blank(), # blank background 
                      panel.background = element_blank(), # blank background
                      axis.line = element_line(colour = "black"), # black axis
                      panel.border = element_rect(colour = "black", size = 1,
                                                  linetype = "solid", fill = NA), # solid line border
                      axis.text = element_text(size = 18, colour = "black"), # axis color and size
                      axis.title = element_text(size = 20), # axis font size
                      legend.text = element_text(size = 24), # legend font size
                      legend.title = element_text(size = 24),# legend title font size
                      plot.title = element_text(hjust = 0.5, size = 20))

# plots --------------------

a <- ggplot(data = df, aes(x = BD)) +
  geom_point(aes(y = y_obs), size = 5) +
  geom_line(aes(y = exp(y_glm)), color = "blue", size = 1) +
  geom_line(aes(y = exp(y_log_log)), color = "red", size = 1) +
  geom_text(label= expression(paste(AGB~"="~"exp("~3.88~"+"~0.066~Basal~diameter~")")), x=35, y=3500, colour = "blue", size = 5) +
  geom_text(label= expression(paste(ln(AGB)~"="~"-"~2.6~"+"~2.5~"ln("~Basal~diameter~")")), x=35, y=2800, colour = "red",size = 5) +
  labs(x = "Basal diameter (cm)", y = "Aboveground biomass (kg)") +
  custom_theme
a

ggsave("Figuras/glm_udep.tiff", width = 6, height = 4.5, units = 'in', dpi = 300, compression = 'lzw')


b<- ggplot(data = df, aes(x = BD)) + # To see the log-log model with log transformed axes
  geom_point(aes(y = y_obs), size = 5) +
  geom_line(aes(y = exp(y_log_log)), color = "blue", size = 1) +
  coord_trans(x = "log", y = "log") +
  geom_text(label= expression(paste(exp(AGB)~"="~"-"~2.6~"+"~2.5~"exp("~Basal~diameter~")")), x=30, y=3500, size = 5) +
  labs(x = "Basal diameter (cm)", y = "Aboveground biomass (kg)") +
  custom_theme

tiff("Figuras/glm_udep_log.tiff", width = 12, height = 4.5, units = "in", res = 300, compression = "lzw")
ggarrange(a,b,  ncol = 2)
dev.off()

ggplot(data = df, aes(x = BD)) + # To visually compare all the models
  geom_point(aes(y = y_obs), size = 5) +
  geom_line(aes(y = exp(y_glm)), color = "blue", size = 1) +
  geom_line(aes(y = exp(y_glm2)), color = "red", size = 1) +
  geom_line(aes(y = exp(y_glm3)), color = "yellow", size = 1) +
  geom_line(aes(y = exp(y_glm4)), color = "green", size = 1) +
  geom_text(label= expression(paste(AGB~"="~3.88~Basal~diameter^0.066)), x=30, y=3500, size = 5) +
  labs(x = "Basal diameter (cm)", y = "Aboveground biomass (kg)") +
  custom_theme


ggplot(data = df, aes(x = y_obs)) + # To compare Chave's model and our best model
  geom_point(aes(y = AGB_ch), color = "red", size = 5) +
  geom_point(aes(y = exp(y_glm)), color = "blue", size = 5) +
  geom_text(label= "Chave RMSE = 357.7", x=1000, y=3500, size = 5, colour = "red") +
  geom_text(label= "Salazar RMSE = 242.14", x=1000, y=4000, size = 5, colour = "blue") +
  geom_abline(intercept = 0, slope = 1)+
    labs(x = "Observerd AGB", y = "Estimated AGB") +
  custom_theme

ggsave("Figuras/glm_Chave.tiff", width = 6, height = 4.5, units = 'in', dpi = 300, compression = 'lzw')

# Biomass % in the stems to estimate the benefits of prunning -----------

allo %>% mutate(branch_percent = (Thick_branch + (Thin_branch/1000))/AGB*100) %>%
  mutate(branch_extract_50 = Thick_branch + (Thin_branch/1000))
mean(allo$branch_percent, na.rm = T)
mean(allo$branch_extract_50, na.rm = T)


## Analyzing the whole Prosopis database for the second objective of the paper --------------------------
# Data cleaning ------------------------------------------------------------------------------------

summary(as.factor(db$Source))
summary(as.factor(global$fuente))

db <- db %>%
  filter(!Source %in% c("Laban, 2012", "Leon, 2005","Padron, 2004")) %>% # excluding databases that have weird values
  filter(!Source == "El Fadl, 1988" | !AGB %in% c(1950,2540)) # excluding two outliers


# Modelos -----------------------------------------------------------------

# Global
glm_global1 <- glm(AGB ~ BD, 
                   data = filter(db, Source %in% c("Salazar","Maghembe","Muturi")),
                   family = gaussian(link = "log"))

glm_global2 <- glm(AGB ~ BD,
                   data = filter(db, Source %in% c("El Fadl", "Llanos")),
                   family = gaussian(link = "log"))

glm_global3 <- glm(AGB ~ BD,
                   data = filter(db, Source %in% c("Mendez")),
                   family = gaussian(link = "log"))

# independent models 

glm_global4 <- glm(AGB ~ BD, 
                   data = filter(db, Source == "Salazar"),
                   family = gaussian(link = "log"))
glm_global5 <- glm(AGB ~ BD, 
                   data = filter(db, Source == "Maghembe"),
                   family = gaussian(link = "log"))
glm_global6 <- glm(AGB ~ BD, 
                   data = filter(db, Source == "Muturi"),
                   family = gaussian(link = "log"))

glm_global7 <- glm(AGB ~ BD, 
                   data = filter(db, Source == "El Fadl"),
                   family = gaussian(link = "log"))
glm_global8 <- glm(AGB ~ BD, 
                   data = filter(db, Source == "Llanos"),
                   family = gaussian(link = "log"))

# r2 for all the GLM
a<- r2beta(glm_global1, partial = TRUE)
b<- r2beta(glm_global2, partial = TRUE)
c<- r2beta(glm_global3, partial = TRUE)
d<- r2beta(glm_global4, partial = TRUE)
e<- r2beta(glm_global5, partial = TRUE)
f<- r2beta(glm_global6, partial = TRUE)
g<- r2beta(glm_global7, partial = TRUE)
h<- r2beta(glm_global8, partial = T)


a<- cbind(model = "Sal,Mag and Mut",a, RMSE = RMSE(glm_global1), AIC = glm_global1$aic, BIC = useBIC(glm_global1), N = 99)
b<- cbind(model = "Llanos and El", b, RMSE = RMSE(glm_global2), AIC = glm_global2$aic, BIC = useBIC(glm_global2), N = 40)
c<- cbind(model = "Mendez",c, RMSE = RMSE(glm_global3), AIC = glm_global3$aic, BIC = useBIC(glm_global3), N = 148)
d<- cbind(model = "Salazar", d, RMSE = RMSE(glm_global4), AIC = glm_global4$aic, BIC = useBIC(glm_global4), N = 30)
e<- cbind(model = "Maghembe", e, RMSE = RMSE(glm_global5), AIC = glm_global5$aic, BIC = useBIC(glm_global5), N = 11)
f<- cbind(model = "Muturi", f, RMSE = RMSE(glm_global6), AIC = glm_global6$aic, BIC = useBIC(glm_global6), N = 58)
g<- cbind(model = "El Fald", g, RMSE = RMSE(glm_global7), AIC = glm_global7$aic, BIC = useBIC(glm_global7), N = 8)
h<- cbind(model = "Llanos", h, RMSE = RMSE(glm_global8), AIC = glm_global8$aic, BIC = useBIC(glm_global8), N = 32)


table2 <- rbind(a,b,c,d,e,f,g,h)
table2 <- table2 %>% 
  dplyr::select(c(-v1,-F, -v2,-ncp))  %>% 
  mutate(across(Rsq: BIC, round, 2))

table2 <- table2[!duplicated(table2[,7]),]

openxlsx::write.xlsx(table2, "globalmodels.xlsx")


pred_glm1 <- exp(predict(glm_global1, newdata = data.frame(BD = db$BD)))
pred_glm2 <- exp(predict(glm_global2, newdata = data.frame(BD = db$BD)))
pred_glm3 <- exp(predict(glm_global3, newdata = data.frame(BD = db$BD)))

# Plots -------------------------------------------------------------------

# Theme
custom_theme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  
                      panel.background = element_blank(), axis.line = element_line(colour = "black"),
                      panel.border = element_rect(colour = "black", size = 1, linetype = "solid", fill = NA),
                      axis.text = element_text(size = 16),
                      axis.title = element_text(size = 20),
                      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
                      # axis.text.x= element_text(face ="bold"),
                      legend.text = element_text(size = 16),
                      legend.title = element_text(size = 16),
                      legend.position = "top")

# Local

# Grid of all Sources (Sources) in one figure
ggplot(data = db, aes(x = BD, y = AGB)) +
  geom_point(aes(color = Source), size = 4, alpha = .75) +
  scale_color_manual(values = c("Salazar" = "#000000",
                                "El Fadl" = "#6BDF2E", 
                                "Llanos" = "#009900", 
                                "Maghembe" = "#ff4242", 
                                "Mendez" = "#0080FF",
                                "Muturi" = "#ff9c9c")) +
  #facet_wrap(Source~.) +
  labs(x = "Basal diameter (cm)", y = "AGB (kg)") +
  custom_theme

# Global


# Test with all Sources
model0 <- lm(log(AGB) ~ log(BD) * Source, data = db)
anova(model0)
Anova(aov(model0), type = 3)

# Multiple comparsisions test
model0.aov <- aov(log(AGB) ~ log(BD) * Source, data = db)
tukey.test <- TukeyHSD(model0.aov, which = "Source")
tukey.test <- data.frame(tukey.test$Source)
tukey.test$sign <- ifelse(tukey.test$p.adj > .05, "*", "")

names(tukey.test)
tukey.test <- tukey.test %>% 
  as.data.frame() %>%
  rownames_to_column("dataset") %>% 
  mutate(across(diff:p.adj, round, 3))


openxlsx::write.xlsx(tukey.test, "tukey.xlsx")


pred_df <- data.frame(db[,c(2:4)],pred_glm1,pred_glm2,pred_glm3)

g_glob <- ggplot(data = pred_df, aes(x = BD, y = AGB)) +
  geom_point(aes(color = Source), size = 4, alpha = .75) +
  scale_color_manual(values = c("Salazar" = "#fc0f0f",
                                "El Fadl" = "#6BDF2E", 
                                "Llanos" = "#009900", 
                                "Maghembe" = "#ff4242", 
                                "Mendez" = "#0080FF",
                                "Muturi" = "#ff9c9c")) +
  geom_line(aes(y = pred_glm1), size = 1.5, linetype = "longdash", color = "#fc0f0f", alpha = .75)+
  geom_line(aes(y = pred_glm2), size = 1.5, linetype = "longdash", color = "#009900", alpha = .75)+
  geom_line(aes(y = pred_glm3), size = 1.5, linetype = "longdash", color = "#0080FF", alpha = .75)+
  geom_text(label= expression(paste(Sal~","~Mag~and~Mut[AGB]~"="~"exp("~3.71~"+"~0.068~Basal~diameter~")")), x=30, y=3500, size = 4) +
  geom_text(label= expression(paste(Lla~and~El~F[AGB]~"="~"exp("~4.64~"+"~0.054~Basal~diameter~")")), x=30, y=3000, size = 4) +
  geom_text(label= expression(paste(Mendez[AGB]~"="~"exp("~2.54~"+"~0.087~Basal~diameter~")")), x=30, y=2500, size = 4) +
  labs(color = "", x = "Basal diameter (cm)", y = "AGB (kg)")+
  custom_theme +
  theme(legend.position = "top",
        legend.title = element_text(size = 6))

g_glob


ggsave("glm_global.tiff", width = 6, height = 4.5, units = 'in', dpi = 300, compression = 'lzw')
