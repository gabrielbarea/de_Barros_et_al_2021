## Script author: Gabriel E. B. de Barros
## Git: https://github.com/gabrielbarea

## Scripts in 'Ichnological aspects of the Aquidauana Formation (Upper Carboniferous, 
##	   Itararé Group, Brazil): an arthropod-colonized glacial setting'

## Scripts and data used for statistical analysis and graphic production of the paper.

## Script.R: Script in R with the functions that were used in the paper

## data_ichnos_v2.xlsx: Data with modified header for reading in R on six different
##    sheets:

##    Data = data used to create the graphs;
##    Aggregators = mean, standard deviation, maximum and minimum values;
##    Statistics Part 1 = Shapiro-Wilks, ACR and D'Agostino analysis;
##    Statistics Part 2 = Mann-Whitney-Wilcoxon;
##    Legend = legend of the other sheets;
##    R = data used in the statistical analysis;

#### Header ####
setwd() ## set directory
getwd() ## get the directory
Sys.setenv(LANG = "en") ## set the Rstudio to English

if(!require(ggplot2)) install.packages("ggplot2")
if(!require(readxl)) install.packages("readxl")
if(!require(multimode)) install.packages("multimode")
if(!require(moments)) install.packages("moments")

library(ggplot2)  ## Graphics production
library(readxl)   ## Read .xlsx files
library(multimode)## Multimodal analysis 
library(moments)  ## Identify data skewness

#### Reading the Data ####
data_ichnos <- read_excel("data_ichnos_v2.xlsx", trim_ws = FALSE, 
                          sheet = "Data") ## Read data sheet

stat_ichnos <- read_excel("data_ichnos_v2.xlsx", trim_ws = FALSE, 
                          sheet = "R") ## Read data sheet


#### Figure 2a - External width ####
EW_box <- ggplot(data=data_ichnos, mapping = 
                       aes(group = ISP, y = EW, x = reorder(ISP, EW, na.rm = TRUE))) +
      geom_boxplot(fill='#A4A4A4', color="black") + 
      theme_bw() + stat_summary(fun=mean, colour="darkred", geom="point", 
                                shape=18, size=2, show.legend=FALSE) +
      scale_x_discrete()
EW_box

#### Figure 2b - External/Internal width ratio ####
EIR_box <- ggplot(data=data_ichnos, mapping = 
                     aes(group = ISP, y = EIR, x = reorder(ISP, EIR, na.rm = TRUE))) +
   geom_boxplot(fill='#A4A4A4', color="black") + 
   theme_bw() + stat_summary(fun=mean, colour="darkred", geom="point", 
                             shape=18, size=2, show.legend=FALSE) +
   scale_x_discrete()
EIR_box

#### Figure 2c - Track row width ####
TW_box <- ggplot(data=data_ichnos, mapping = 
                    aes(group = ISP, y = TW, 
                        x = reorder(ISP, TW, na.rm = TRUE))) +
   geom_boxplot(fill='#A4A4A4', color="black") + 
   theme_bw() + stat_summary(fun=mean, colour="darkred", geom="point", 
                             shape=18, size=2, show.legend=FALSE) +
   scale_x_discrete()
TW_box

#### Figure 2d - External width all species ####
EWA_box <- ggplot(data=data_ichnos, aes(y = EW, x=1.0)) +
   geom_boxplot(fill='#A4A4A4', color="black") + 
   theme_bw() + stat_summary(fun=mean, colour="darkred", geom="point", 
                             shape=18, size=2, show.legend=FALSE)
EWA_box

#### Figure 2e - Medial imprint single width ####
MICW_box <- ggplot(data=data_ichnos, mapping = 
                      aes(group = ISP, y = MICW, 
                          x = reorder(ISP, MICW, na.rm = TRUE))) +
   geom_boxplot(fill='#A4A4A4', color="black") + 
   theme_bw() + stat_summary(fun=mean, colour="darkred", geom="point", 
                             shape=18, size=2, show.legend=FALSE) +
   scale_x_discrete()
MICW_box

#### Figure 2f - Internal track external/internal width ratio ####
ITEIR_box <- ggplot(data=data_ichnos, mapping = 
                       aes(group = ISP, y = ITEIR, 
                           x = reorder(ISP, ITEIR, na.rm = TRUE))) +
   geom_boxplot(fill='#A4A4A4', color="black") + 
   theme_bw() + stat_summary(fun=mean, colour="darkred", geom="point", 
                             shape=18, size=2, show.legend=FALSE) +
   scale_x_discrete()
ITEIR_box

#### Figure 2g - Internal track external width ####
ITEW_box <- ggplot(data=data_ichnos, mapping = 
                        aes(group = ISP, y = ITEW, 
                            x = reorder(ISP, ITEW, na.rm = TRUE))) +
   geom_boxplot(fill='#A4A4A4', color="black") + 
   theme_bw() + stat_summary(fun=mean, colour="darkred", geom="point", 
                             shape=18, size=2, show.legend=FALSE) +
   scale_x_discrete()
ITEW_box

#### Supplementary Material Statistics - Cruziana problematica ####
## Shapiro-Wilks
shapiro.test(stat_ichnos$Cp_EW)
shapiro.test(stat_ichnos$Cp_IW) 
shapiro.test(stat_ichnos$Cp_TW) 
shapiro.test(stat_ichnos$Cp_AM) 
shapiro.test(stat_ichnos$Cp_EIR)

## ACR test
modetest(stat_ichnos$Cp_EW)   
modetest(stat_ichnos$Cp_IW) 
modetest(stat_ichnos$Cp_TW) 
modetest(stat_ichnos$Cp_AM) 
modetest(stat_ichnos$Cp_EIR)

## D'Agostino
agostino.test(stat_ichnos$Cp_EW)   
agostino.test(stat_ichnos$Cp_IW) 
agostino.test(stat_ichnos$Cp_TW)
agostino.test(stat_ichnos$Cp_AM) 
agostino.test(stat_ichnos$Cp_EIR)

#### Supplementary Material Statistics - Diplopodichnus biformis ####
## Shapiro-Wilks
shapiro.test(stat_ichnos$Db_EW)
shapiro.test(stat_ichnos$Db_IW) 
shapiro.test(stat_ichnos$Db_TW) 
shapiro.test(stat_ichnos$Db_EIR)

## ACR test
modetest(stat_ichnos$Db_EW)   
modetest(stat_ichnos$Db_IW) 
modetest(stat_ichnos$Db_TW) 
modetest(stat_ichnos$Db_EIR)

## D'Agostino
agostino.test(stat_ichnos$Db_EW)   
agostino.test(stat_ichnos$Db_IW) 
agostino.test(stat_ichnos$Db_TW)
agostino.test(stat_ichnos$Db_EIR)

#### Supplementary Material Statistics - Diplichnites gouldi ####
## Shapiro-Wilks
shapiro.test(stat_ichnos$Dg_EW)
shapiro.test(stat_ichnos$Dg_IW) 
shapiro.test(stat_ichnos$Dg_TW)
shapiro.test(stat_ichnos$Dg_AM)
shapiro.test(stat_ichnos$Dg_EIR)

## ACR test
modetest(stat_ichnos$Dg_EW)   
modetest(stat_ichnos$Dg_IW) 
modetest(stat_ichnos$Dg_TW) 
modetest(stat_ichnos$Dg_AM)
modetest(stat_ichnos$Dg_EIR)

## D'Agostino
agostino.test(stat_ichnos$Dg_EW)   
agostino.test(stat_ichnos$Dg_IW) 
agostino.test(stat_ichnos$Dg_TW)
agostino.test(stat_ichnos$Dg_AM)
agostino.test(stat_ichnos$Dg_EIR)

#### Supplementary Material Statistics - Umfolozia sinuosa ####
## Shapiro-Wilks
shapiro.test(stat_ichnos$Us_EW)
shapiro.test(stat_ichnos$Us_IW) 
shapiro.test(stat_ichnos$Us_TW)
shapiro.test(stat_ichnos$Us_ITEW)
shapiro.test(stat_ichnos$Us_ITIW)
shapiro.test(stat_ichnos$Us_MI)
shapiro.test(stat_ichnos$Us_AM)
shapiro.test(stat_ichnos$Us_MICW)
shapiro.test(stat_ichnos$Us_EIR)
shapiro.test(stat_ichnos$Us_ITEIR)

## ACR test
modetest(stat_ichnos$Us_EW)   
modetest(stat_ichnos$Us_IW) 
modetest(stat_ichnos$Us_TW) 
modetest(stat_ichnos$Us_ITEW)
modetest(stat_ichnos$Us_ITIW)
modetest(stat_ichnos$Us_MI)
modetest(stat_ichnos$Us_AM)
modetest(stat_ichnos$Us_MICW) 
modetest(stat_ichnos$Us_EIR)
modetest(stat_ichnos$Us_ITEIR)

## D'Agostino
agostino.test(stat_ichnos$Us_EW)   
agostino.test(stat_ichnos$Us_IW) 
agostino.test(stat_ichnos$Us_TW)
agostino.test(stat_ichnos$Us_ITEW)
agostino.test(stat_ichnos$Us_ITIW)
agostino.test(stat_ichnos$Us_MI) 
agostino.test(stat_ichnos$Us_AM) 
agostino.test(stat_ichnos$Us_MICW)
agostino.test(stat_ichnos$Us_EIR)
agostino.test(stat_ichnos$Us_ITEIR)

#### Supplementary Material Statistics - Warvichnium ulbrichi morphotype A ####
## Shapiro-Wilks
shapiro.test(stat_ichnos$WmA_EW)
shapiro.test(stat_ichnos$WmA_AM)

## ACR test
modetest(stat_ichnos$WmA_EW)   
modetest(stat_ichnos$WmA_AM)

## D'Agostino
agostino.test(stat_ichnos$WmA_EW)   
agostino.test(stat_ichnos$WmA_AM)

#### Supplementary Material Statistics - Warvichnium ulbrichi morphotype B ####
## Shapiro-Wilks
shapiro.test(stat_ichnos$WmB_EW)
shapiro.test(stat_ichnos$WmB_SL)

## ACR test
modetest(stat_ichnos$WmB_EW)   
modetest(stat_ichnos$WmB_SL)

## D'Agostino
agostino.test(stat_ichnos$WmB_EW)   
agostino.test(stat_ichnos$WmB_SL)


#### Supplementary Material Statistics - Mann Whitney Wilcoxon ####
## External width
pairwise.wilcox.test(stat_ichnos$W_EW, stat_ichnos$W_ISP_1, p.adjust.method = "BH")

## Track width
pairwise.wilcox.test(stat_ichnos$W_TW, stat_ichnos$W_ISP_2, p.adjust.method = "BH")

#### Paper Information ####
## Ichnological aspects of the Aquidauana Formation (Upper Carboniferous, 
##	   Itararé Group, Brazil): an arthropod-colonized glacial setting

## Gabriel E.B. de Barros 1 2; Bruno Becker-Kerber 3; Daniel Sedorko 4; João H.D. Lima 5; 
##    Mírian L.A.F. Pacheco 2 6;

## 1 Programa de Poss-Graduacao em Biologia Comparada, Departamento de Biologia, Faculdade 
##    de Filosofia, Ciencias e Letras de Ribeirao Preto (FFCLRP), Universidade de 
##    Sao Paulo (USP), Ribeirao Preto, Sao Paulo, Brazil. 
##    [gbareabarros@usp.br]

## 2 Laboratorio de Estudos Paleobiologicos (LEPBio), Departamento de Biologia (DBio), 
##    Universidade Federal de Sao Carlos (UFSCar), campus Sorocaba, Rod. Joao Leme dos 
##    Santos, km 110, CEP 18052-780, CCHB-1112, Sorocaba, Brazil. 
##    [forancelli.ufscar@gmail.com - Corresponding Author]

## 3 Programa de Pos-Graduacao em Ecologia e Recursos Naturais (PPGERN), Universidade 
##    Federal de Sao Carlos (UFSCar), Washington Luiz 325 km, CEP 13565-905, Sao Carlos, 
##    Brazil. [beckerkerber@gmail.com]

## 4 Universidade Federal de Uberlandia (UFU), campus Monte Carmelo, Laboratorio de 
##    Paleontologia Estratigrafica - LAPE, Av. XV de Novembro, 501, Monte Carmelo, MG, 
##    38500-000, Brazil. [dsedorko@gmail.com]

## 5 Grupo de Pesquisa ICHNOS, Universidade do Vale do Rio dos Sinos (UNISINOS), Av. 
##    UNISINOS, 950, Sao Leopoldo, RS, Brazil. [joaodoblerlima@gmail.com]

## 6 Instituto de Fisica (IF), Universidade de Sao Paulo (USP), Cidade Universitaria, 
##    Butanta, Sao Paulo, SP, Brazil.
