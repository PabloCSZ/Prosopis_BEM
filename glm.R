

# libraries 

library(readxl)
library(tidyverse)
library(MuMIn)
library(r2glmm)
library(qpcR)
library(openxlsx)

# Data
setwd("E:/OneDrive/OneDrive - UDEP/Semilla/Ejecucion/Biomasa")

#trees <- read.csv("Borrador/Datos/trees.csv")
allo <- read_xlsx("Borrador/Datos/Allometric_data.xlsx")

colnames(allo)[c(5,6,9,16:18)] <- c("Thick_branch","Thin_branch","Biomasa_total","Dm", "Height", "Canopy")

names(trees)
names(allo)
summary(as.factor(allo$Fuente))
summary(as.factor(allo$Especie))

summary(allo$Biomasa_total)
summary(trees$pesos)

palli <- allo %>%
  filter(Especie == "Pallida") %>%
  filter(!is.na(Biomasa_total))


allo <- allo %>%
  filter(Fuente == "Udep,2019") %>%
  filter(!is.na(Biomasa_total))

# modeling -------------------


summary()
glm_model         <- glm(Biomasa_total ~ Dm,
                         data = allo,
                         family = gaussian(link =  "log"))


glm_model2         <- glm(Biomasa_total ~ Dm + Height,
                         data = allo,
                         family = gaussian(link =  "log"))

glm_model3         <- glm(Biomasa_total ~ Dm + Height + Canopy,
                          data = allo,
                          family = gaussian(link =  "log"))

glm_model4         <- glm(Biomasa_total ~ Dm + Canopy,
                          data = allo,
                          family = gaussian(link =  "log"))

# Calculating R2 and RMSE for all models

summary(glm_model)
r.squaredGLMM(glm_model)
r.squaredGLMM(glm_model2)
r.squaredGLMM(glm_model3)
r.squaredGLMM(glm_model4)

a<- r2beta(glm_model, partial = TRUE)
b<- r2beta(glm_model2, partial = TRUE)
c<- r2beta(glm_model3, partial = TRUE)
d<- r2beta(glm_model4, partial = TRUE)

a <- t(a[order(rownames(a)),])
b <- t(b[order(rownames(b)),])
c <- t(c[order(rownames(c)),])
d <- t(d[order(rownames(d)),])

table1 <- rbind(round(as.numeric(a[6,1]),2),round(as.numeric(b[6,1]),2),round(as.numeric(c[6,1]),2),round(as.numeric(d[6,1]),2)) # table of partial r2 *100
table1 <- cbind(c("AGB ~ DBH","AGB ~ DBH + Height","AGB ~ DBH + Height + Canopy", "AGB ~ DBH + Height"),table1,
                c(round(RMSE(glm_model),1),round(RMSE(glm_model2),2),round(RMSE(glm_model3),2),round(RMSE(glm_model4),2)))

openxlsx::write.xlsx(table1, "Analisis/R2andRMSE.xlsx")
#The relationship between Y and X can b expressed in the following equation Y = exp(bX + a), which is also expressed as Y = exp(bX)*exp(a)

# Data frame
pred_glm        <- predict(glm_model)
pred_glm2        <- predict(glm_model2)
pred_glm3        <- predict(glm_model3)
pred_glm4        <- predict(glm_model4)

df <- data.frame(
  Dm = allo$Dm,
  y_obs = allo$Biomasa_total,
  #y_log_log = pred_log_log,
  #y_log_linear = pred_log_linear,
  y_glm = pred_glm,
  y_glm2 = pred_glm2,
  y_glm3 = pred_glm3,
  y_glm4 = pred_glm4
  
)

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
library(ggplot2)
library(ggthemes)

ggplot(data = df, aes(x = Dm)) +
  geom_point(aes(y = y_obs), size = 5) +
  geom_line(aes(y = exp(y_glm)), color = "blue", size = 1) +
  geom_text(label= expression(paste(AGB~"="~3.88~Basal~diameter^0.066)), x=30, y=3500, size = 5) +
  labs(x = "Basal diameter (cm)", y = "Aboveground biomass (kg)") +
  custom_theme

ggsave("Figuras/glm_udep.tiff", width = 6, height = 4.5, units = 'in', dpi = 300, compression = 'lzw')



ggplot(data = df, aes(x = y_obs)) +
  geom_point(aes(y = exp(y_glm)), size = 5,color = "blue") +
  geom_point(aes(y = exp(y_glm2)), size = 5,color = "red") +
  geom_point(aes(y = exp(y_glm3)), size = 5,color = "green") +
  geom_point(aes(y = exp(y_glm4)), size = 5,color = "black") +
  geom_text(label= expression(paste(AGB~"="~3.88~Basal~diameter^0.066)), x=30, y=3500, size = 5) +
  labs(x = "Basal diameter (cm)", y = "Aboveground biomass (kg)") +
  custom_theme

# Biomass % in the stems

summary(allo$Thick_branch)
summary(allo$Thin_branch)
summary(allo$Biomasa_total)

allo <- allo %>% mutate(branch_percent = (Thick_branch + (Thin_branch/1000))/Biomasa_total*100) %>%
  mutate(branch_extract_50 = Thick_branch + (Thin_branch/1000))
mean(allo$branch_percent, na.rm = T)
mean(allo$branch_extract_50, na.rm = T)
