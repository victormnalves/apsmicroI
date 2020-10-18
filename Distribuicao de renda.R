library(tidyverse)
library(dplyr)
library(WDI)
library(ggplot2)
library(gridExtra)
library(xlsx)
library(ggrepel)


#Importação de dados direto da base de dados do BM.
#bens_servico_comparativo <- WDI(country = c ('BRA', 'USA', 'ARG', 'CAN','FRA','BOL','ZAF', 'SWE', 'NOR', 'CHL'), 
   #                             indicator = 'GC.TAX.GSRV.RV.ZS', start = 1980, end = 2017)
#  names(bens_servico_comparativo)[3]<- paste('TGS')
#imposto_renda_comparativo <- WDI(country = c ('BRA', 'USA', 'ARG', 'CAN','FRA','BOL','ZAF', 'SWE', 'NOR', 'CHL'), 
  #                               indicator = 'GC.TAX.YPKG.RV.ZS', start = 1980, end = 2017)
#  names(imposto_renda_comparativo)[3]<- paste('TIPCG')
#indice_gini_comparatico <- WDI(country = c ('BRA', 'USA', 'ARG', 'CAN','FRA','BOL','ZAF', 'SWE', 'NOR', 'CHL'), 
 #                              indicator = 'SI.POV.GINI', start = 1980, end = 2017)
#  names(indice_gini_comparatico)[3]<- paste('GINI')
#concentracao_alto_comparativo <- WDI(country = c ('BRA', 'USA', 'ARG', 'CAN','FRA','BOL','ZAF', 'SWE', 'NOR', 'CHL'), 
    #                                 indicator = 'SI.DST.10TH.10', start = 1980, end = 2017)
#  names(concentracao_alto_comparativo)[3]<-paste('IH10')
#concentracao_baixo_comparativo<- WDI(country = c ('BRA', 'USA', 'ARG', 'CAN','FRA','BOL','ZAF', 'SWE', 'NOR', 'CHL'), 
     #                                indicator = 'SI.DST.FRST.10', start = 1980, end = 2017)
#  names(concentracao_baixo_comparativo)[3]<-paste('IL10')
#preco_cesta <- preco_cesta
#alltgs = filter(taxes_on_income_vs_taxes_on_goods_and_services, Entity %in% c('Brazil','Canada','Chile', 'United States', 'Argentina','United Kingdom','South Africa', 'Sweden', 'Norway', 'Germany', 'Namibia','Finland','Turkey','Bolivia','Mexico', 'France'),
#                Year %in% c(2006:2017))
#  names(alltgs)[1]<-paste('País')
#  names(alltgs)[3]<-paste('Ano')

#Dataset com dados de concentração de renda, GINI e impostos
#dataset_padrao2 <- data.frame(bens_servico_comparativo, indice_gini_comparatico, imposto_renda_comparativo, concentracao_alto_comparativo, concentracao_baixo_comparativo)
#dataset_padrao2 <- select(dataset_padrao2, -c(country.1, iso2c.1, year.1,country.2, iso2c.2, year.2,country.3, iso2c.3, year.3,country.4, iso2c.4, year.4))

#Dataset com preço da cesta básica e imposto sobre bens e serviços

#Função regressão em ggplot

ggplotRegression <- function(fit){
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(subtitle =  paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                           "Intercept =",signif(fit$coef[[1]],5 ),
                           " Slope =",signif(fit$coef[[2]], 5),
                           " P =",signif(summary(fit)$coef[2,4], 5)))
}

#correlação gini ~ impostos de consumo no Brasil
#regressao_gini_impostos_consumo_br <- ggplotRegression(lm(TGS~GINI, data = subset(dataset_padrao1, País == "Brazil"))) + ggtitle("Brazil")
#regressao_gini_impostos_consumo_br
##correlação gini ~ impostos de renda no Brasil
#regressao_gini_impostos_renda_br <- ggplotRegression(lm(TIPCG~GINI, data = subset(dataset_padrao1, País == "Brazil"))) + ggtitle("Brazil")
#regressao_gini_impostos_renda_br
##correlação gini ~ impostos de renda no Africa do Sul
#regressao_gini_impostos_renda_zaf <- ggplotRegression(lm(TIPCG~GINI, data = subset(dataset_padrao1, País == "South Africa"))) + ggtitle("South Africa")
#regressao_gini_impostos_renda_zaf
##correlação gini ~ impostos de consumo no Africa do Sul
#regressao_gini_impostos_consumo_zaf <- ggplotRegression(lm(TGS~GINI, data = subset(dataset_padrao1, País == "South Africa"))) + ggtitle("South Africa")


#ggplot(subset(dataset_padrao, country %in% c('Brazil', 'United States', 'Argentina','United Kingdom','South Africa', 'Sweden', 'Norway', 'Germany')))+
#  geom_line(aes(x=year, y=GINI, group= country, colour = country)) + ggtitle('Indice GINI através do tempo') + xlab('Ano') + ylab('Indice GINI')
#ggplot(subset(dataset_padrao, country %in% c('Brazil', 'United States', 'Argentina','United Kingdom','South Africa', 'Sweden', 'Norway', 'Germany')))+
#  geom_line(aes(x=year, y=TGS, group= country, colour = country))+ ggtitle('Impostos sobre consumo e serviços (% da receita)') + xlab('Ano') + ylab('Porcentagem de impostos')
#ggplot(subset(dataset_padrao, country %in% c('Brazil', 'United States', 'Argentina','United Kingdom','South Africa', 'Sweden', 'Norway', 'Germany')))+
#  geom_line(aes(x=year, y=TIPCG, group= country, colour = country)) + ggtitle('Impostos sobre renda, lucro e ganhos de capital (% da receita)') + xlab('Ano') + ylab('Porcentagem de impostos')
#ggplot(subset(dataset_padrao, country %in% c('Brazil', 'United States', 'Argentina','United Kingdom','South Africa', 'Sweden', 'Norway', 'Germany')))+
#  geom_line(aes(x=year, y=IL10, group= country, colour = country)) + ggtitle('Concentração da renda total nos 10% mais pobres') + xlab('Ano') + ylab('Concentração da renda total')
#ggplot(subset(dataset_padrao, country %in% c('Brazil', 'United States', 'Argentina','United Kingdom','South Africa', 'Sweden', 'Norway', 'Germany')))+
#  geom_line(aes(x=year, y=IH10, group= country, colour = country)) + ggtitle('Concentração da renda total nos 10% mais ricos') + xlab('Ano') + ylab('Concentração da renda total')
#ggplot(subset(dataset_padrao, country %in% c('Brazil')))+
#  geom_line(aes(x=year, y=TGS, group= country, colour = country))+ ggtitle('Impostos sobre consumo e serviços (% da receita)') + xlab('Ano') + ylab('Porcentagem de impostos')
#ggplot(subset(dataset_padrao, country %in% c('Brazil')))+
#  geom_line(aes(x=year, y=TIPCG, group= country, colour = country)) + ggtitleitle('Impostos sobre renda, lucro e ganhos de capital (% da receita)') + xlab('Ano') + ylab('Porcentagem de impostos')
