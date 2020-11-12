library(tidyverse)
library(dplyr)
library(WDI)
library(ggplot2)
library(gridExtra)
library(xlsx)
library(ggrepel)


Importação de dados direto da base de dados do BM.
bens_servico_comparativo <- WDI(country = c ('BRA', 'USA', 'ARG', 'CAN','FRA','BOL','ZAF', 'SWE', 'NOR', 'CHL'), 
                                indicator = 'GC.TAX.GSRV.RV.ZS', start = 1980, end = 2017)
  names(bens_servico_comparativo)[3]<- paste('TGS')
imposto_renda_comparativo <- WDI(country = c ('BRA', 'USA', 'ARG', 'CAN','FRA','BOL','ZAF', 'SWE', 'NOR', 'CHL'), 
                                 indicator = 'GC.TAX.YPKG.RV.ZS', start = 1980, end = 2017)
  names(imposto_renda_comparativo)[3]<- paste('TIPCG')
indice_gini_comparatico <- WDI(country = c ('BRA', 'USA', 'ARG', 'CAN','FRA','BOL','ZAF', 'SWE', 'NOR', 'CHL'), 
                               indicator = 'SI.POV.GINI', start = 1980, end = 2017)
  names(indice_gini_comparatico)[3]<- paste('GINI')
concentracao_alto_comparativo <- WDI(country = c ('BRA', 'USA', 'ARG', 'CAN','FRA','BOL','ZAF', 'SWE', 'NOR', 'CHL'), 
                                     indicator = 'SI.DST.10TH.10', start = 1980, end = 2017)
 names(concentracao_alto_comparativo)[3]<-paste('IH10')
concentracao_baixo_comparativo<- WDI(country = c ('BRA', 'USA', 'ARG', 'CAN','FRA','BOL','ZAF', 'SWE', 'NOR', 'CHL'), 
                                     indicator = 'SI.DST.FRST.10', start = 1980, end = 2017)
  names(concentracao_baixo_comparativo)[3]<-paste('IL10')
preco_cesta <- preco_cesta
alltgs = filter(taxes_on_income_vs_taxes_on_goods_and_services, Entity %in% c('Brazil','Canada','Chile', 'United States', 'Argentina','United Kingdom','South Africa', 'Sweden', 'Norway', 'Germany', 'Namibia','Finland','Turkey','Bolivia','Mexico', 'France'),
                Year %in% c(2006:2017))
  names(alltgs)[1]<-paste('País')
  names(alltgs)[3]<-paste('Ano')
  
gastos_por_aluno_secundaria_1 <- WDI(country = c ('AR','BR','CL','CO','EC','PE','MX','UY','CH','DK','FR','GB','IS','NO','SE','US','OE' ), 
                                     indicator = 'SE.XPD.SECO.PC.ZS', start = 2000, end = 2017)
gastos_por_aluno_primaria_1 <- WDI(country = c ('AR','BR','CL','CO','EC','PE','MX','UY','CH','DK','FR','GB','IS','NO','SE','US','OE'), 
                                 indicator = 'SE.XPD.PRIM.PC.ZS', start = 2000, end = 2017)

#Dataset com dados de concentração de renda, GINI e impostos
dataset_padrao2 <- data.frame(bens_servico_comparativo, indice_gini_comparatico, imposto_renda_comparativo, concentracao_alto_comparativo, concentracao_baixo_comparativo)
dataset_padrao2 <- select(dataset_padrao2, -c(country.1, iso2c.1, year.1,country.2, iso2c.2, year.2,country.3, iso2c.3, year.3,country.4, iso2c.4, year.4))

#Dataset com gastos em educação por aluno em países selecionados
gastos_e <- data.frame(gastos_por_aluno_primaria_1, gastos_educacao_secundaria_1)

#Função regressão em ggplot com coeficientes

ggplotRegression <- function(fit){
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(subtitle =  paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                           "Intercept =",signif(fit$coef[[1]],5 ),
                           " Slope =",signif(fit$coef[[2]], 5),
                           " P =",signif(summary(fit)$coef[2,4], 5)))
}

