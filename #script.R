#script 18 e 22 - modelos 1 e 2
rm(list=ls())

### pacotes inicialização #####

library(tidyverse)
options(scipen = 1000)
library(sjPlot)
library(coefplot)
library(car)
library(ltm)
library(psych)
library(memisc)
library(labelled)
#2018 first - modelo 1

library(haven)

ESEB2018_ <- read_sav("q2018_.sav")

table(ESEB2018_$P18) #1 + ESEB2018_
table(ESEB2018_$Q502) # 1+ ESEB2018_
table(ESEB2018_$Q405)# 1+ ESEB2018_

table(ESEB2018_$D1A_FAIXAID, useNA = "always") #faixa de idade 7 mais velho
table(ESEB2018_$D2_SEXO,useNA = "always") # sexo 1- mASC e 2 FEM
table(ESEB2018_$D3_ESCOLA,useNA = "always") # Escolaridade 9 mais escolarizado
table(ESEB2018_$Q12P2_B,useNA = "always") # voto
table(ESEB2018_$D9B_FAIXA_RENDAF,useNA = "always") # renda em reais d9b

table(ESEB2018_$D12A) # raça


#recodificações

ESEB2018_$raca_branca <- ESEB2018_$D12A==2

summary(ESEB2018_$P18)
table(ESEB2018_$P18,useNA = "always")
ESEB2018_$P18[ESEB2018_$P18 == 9] <- NA # gerar missing
ESEB2018_$P18[ESEB2018_$P18 == 8] <- NA # gerar missing
summary(ESEB2018_$P18)
table(ESEB2018_$P18,useNA = "always") # PRA VERIFICAR

summary(ESEB2018_$Q502)
table(ESEB2018_$Q502,useNA = "always")
ESEB2018_$Q502[ESEB2018_$Q502 == 7] <- NA # gerar missing
ESEB2018_$Q502[ESEB2018_$Q502 == 8] <- NA # gerar missing
summary(ESEB2018_$Q502)
table(ESEB2018_$Q502,useNA = "always") # para verificar 

summary(ESEB2018_$Q405)
table(ESEB2018_$Q405,useNA = "always")
ESEB2018_$Q405[ESEB2018_$Q405 == 7] <- NA # gerar missing
ESEB2018_$Q405[ESEB2018_$Q405 == 8] <- NA # gerar missing
summary(ESEB2018_$Q405)
table(ESEB2018_$Q405,useNA = "always") # para verificar 


ESEB2018_$D9B_FAIXA_RENDAF[ESEB2018_$D9B_FAIXA_RENDAF == 98] <- NA # gerar missing
ESEB2018_$D9B_FAIXA_RENDAF[ESEB2018_$D9B_FAIXA_RENDAF == 99] <- NA # gerar missing
summary(ESEB2018_$D9B_FAIXA_RENDAF)
table(ESEB2018_$D9B_FAIXA_RENDAF,useNA = "always") # para verificar 

#
#
#descritivas para tabela 1



prop.table(table(ESEB2018_$P18,useNA = "always"))*100
prop.table(table(ESEB2018_$Q502,useNA = "always"))*100
prop.table(table(ESEB2018_$Q405,useNA = "always"))*100




#

-1*ESEB2018_$Q502 -> ESEB2018_$minoria#inverter
-1*ESEB2018_$P18 -> ESEB2018_$justica#inverter
-1*ESEB2018_$Q405 -> ESEB2018_$lider_forte#inverter
ESEB2018_$faixa_renda <- ESEB2018_$D9B_FAIXA_RENDAF
ESEB2018_$idade <- ESEB2018_$D1A_FAIXAID
ESEB2018_$Mulher <- ESEB2018_$D2_SEXO
ESEB2018_$escolaridade <- ESEB2018_$D3_ESCOLA

table(ESEB2018_$Q12P2_B,useNA = "always")

ESEB2018_$turno2<- memisc::recode(as.factor(ESEB2018_$Q12P2_B), 1 <- c(2), 2 <- c(1), 3 <- c(50,60,97,98,99))
table(ESEB2018_$turno2)
table(ESEB2018_$Q12P2_B) #para comparar


ESEB2018_ <- within(ESEB2018_, {
  turno2_<- Recode(turno2, '1 = "Bolsonaro2018"; 2 = "Haddad2018"; 3 = "Branco/Nulo/NaoSabe/NaoVotou"', as.factor=TRUE)
})
table(ESEB2018_$turno2) #para conferir
table(ESEB2018_$turno2_) # para conferir


#
ESEB2018_ <- subset(ESEB2018_, select=c(minoria,justica,lider_forte,faixa_renda,idade,Mulher,
                                      escolaridade,turno2_,raca_branca))
ESEB2018_ <- na.omit(ESEB2018_)
ESEB2018_ <- remove_labels(ESEB2018_)

# alpha de cronbach
ltm::cronbach.alpha(ESEB2018_[,1:3], CI=F)
#0.393 alpha

scree(ESEB2018_[,1:3])

pca2018 <- psych::principal(ESEB2018_[,1:3])
pca2018#appendix
factor.scores2018 <- pca2018$scores
hist(factor.scores2018, breaks=50)
ESEB2018_$dep <- factor.scores2018
ESEB2018_$dep <- as.numeric(ESEB2018_$dep)
ESEB2018_$dep <- scales::rescale(ESEB2018_$dep, to = c(0, 1))
summary(ESEB2018_$dep)#appendix
hist(ESEB2018_$dep, breaks=50)#appendix
ESEB2018_$depN <- ntile(ESEB2018_$dep, 4)
ESEB2018_$y <- ESEB2018_$depN==4

ESEB2018_$turno2_ <- relevel(ESEB2018_$turno2_, "Bolsonaro2018") #
modelo1 <- glm(y ~ turno2_+faixa_renda+idade+Mulher+raca_branca+
                 escolaridade, data = ESEB2018_, family=binomial(link=logit))
tab_model(modelo1, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars",p.threshold = c(0.1, 0.05, 0.01))#ok


#
#eseb 2022
X04810 <- read_sav("04810.sav")
table(X04810$D03)#um NA
print(X04810$D03)
X04810$D03[X04810$D03==99]<- NA
table(X04810$D03, useNA="always")#ok
X04810$escolaridade <-X04810$D03 
table(X04810$D02)#ok
X04810$Mulher <- X04810$D02
X04810$D01A_FX_ID -> X04810$idade#ok
X04810$D09a_FX_RENDAF
table(X04810$D09a_FX_RENDAF)
X04810$D09a_FX_RENDAF[X04810$D09a_FX_RENDAF>10] <- NA
table(X04810$D09a_FX_RENDAF, useNA = "always")
X04810$D09a_FX_RENDAF -> X04810$faixa_renda#OK
table(X04810$D12_COR)
X04810$D12_COR == 2 -> X04810$raca_branca
#Q10P2‐b 
X04810$Q10P2b
table(X04810$Q10P2b,useNA = "always")
X04810$Q10P2b <- ifelse(is.na(X04810$Q10P2b), -66, X04810$Q10P2b)
table(X04810$Q10P2b,useNA = "always")
# Recodificar a variável como um fator com as categorias desejadas
X04810$Q10P2b <- factor(ifelse(X04810$Q10P2b == 1, "Bolsonaro2022",
                               ifelse(X04810$Q10P2b == 2, "Lula2022",
                                      "Branco/Nulo/NaoSabe/NaoVotou")))
table(X04810$Q10P2b,useNA = "always")
X04810$Q10P2b -> X04810$turno2_
X04810$turno2_ <- relevel(X04810$turno2_, "Bolsonaro2022") #
#
X04810$Q08
table(X04810$Q08)
X04810$Q08[X04810$Q08>7]<- NA
table(X04810$Q08, useNA="always")
X04810$aprovacao_bolsonaro22 <- X04810$Q08
-1*X04810$aprovacao_bolsonaro22->X04810$aprovacao_bolsonaro22#inverti para otimo ficar 5 e ruim ficar 1
table(X04810$aprovacao_bolsonaro22)#colocar entre 1 e 5
X04810$aprovacao_bolsonaro22_1 <- scales::rescale(X04810$aprovacao_bolsonaro22, to = c(1, 3))
X04810$aprovacao_bolsonaro22 <- scales::rescale(X04810$aprovacao_bolsonaro22, to = c(1, 5))
table(X04810$aprovacao_bolsonaro22,useNA = "always")#agora recodificar
# Recodificar os valores da variável
X04810 <- X04810 %>%
  mutate(aprovacao_bolsonaro22_recod = case_when(
    aprovacao_bolsonaro22 %in% c(1, 2) ~ "Ruim/Péssimo",
    aprovacao_bolsonaro22 == 3 ~ "Regular",
    aprovacao_bolsonaro22 %in% c(4, 5) ~ "Bom/Ótimo",
    TRUE ~ as.character(aprovacao_bolsonaro22)
  ))
table(X04810$aprovacao_bolsonaro22_recod,useNA = "always")#OK

#
X04810$Q04b#SEM NA
X04810$Q04d
X04810$Q04d[X04810$Q04d>7] <- NA
X04810$Q04c
X04810$Q04c[X04810$Q04c>7] <- NA
#descritivas
prop.table(table(X04810$Q04b, useNA = "always"))*100
prop.table(table(X04810$Q04d, useNA = "always"))*100
prop.table(table(X04810$Q04c, useNA = "always"))*100
-1*X04810$Q04d -> X04810$minoria#inverter
-1*X04810$Q04b -> X04810$justica#inverter
-1*X04810$Q04c -> X04810$lider_forte#inverter


X04810 <- subset(X04810, select=c(minoria,justica,lider_forte,faixa_renda,idade,Mulher,
                                        escolaridade,turno2_,raca_branca,aprovacao_bolsonaro22_recod,
                                  aprovacao_bolsonaro22,aprovacao_bolsonaro22_1))
X04810<- na.omit(X04810)
X04810 <- remove_labels(X04810)
#
# alpha de cronbach
ltm::cronbach.alpha(X04810[,1:3], CI=F)
#0.144 alpha

scree(X04810[,1:3])

pca2022<- psych::principal(X04810[,1:3])
pca2022#appendix
factor.scores2022 <- pca2022$scores
hist(factor.scores2022, breaks=50)
X04810$dep <- factor.scores2022
X04810$dep <- as.numeric(X04810$dep)
X04810$dep <- scales::rescale(X04810$dep, to = c(0, 1))
summary(X04810$dep)#appendix
hist(X04810$dep, breaks=50)#appendix
X04810$depN <- ntile(X04810$dep, 4)
X04810$y <- X04810$depN==4
# Criando a ordem desejada dos níveis
nova_ordem <- c("Ruim/Péssimo", "Regular", "Bom/Ótimo")

# Reordenando os níveis da variável
X04810$aprovacao_bolsonaro22_recod <- factor(
  X04810$aprovacao_bolsonaro22_recod,
  levels = nova_ordem
)


#
modelo2 <- glm(y ~ turno2_+faixa_renda+idade+Mulher+raca_branca+
                 escolaridade, data = X04810, family=binomial(link=logit))
tab_model(modelo2, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars",p.threshold = c(0.1, 0.05, 0.01))#ok

str(X04810$aprovacao_bolsonaro22_recod)
X04810$aprovacao_bolsonaro22_recod <- as.factor(X04810$aprovacao_bolsonaro22_recod)
X04810$aprovacao_bolsonaro22_recod <- relevel(X04810$aprovacao_bolsonaro22_recod, "Ruim/Péssimo") #
modelo3 <- glm(y ~ aprovacao_bolsonaro22_recod+faixa_renda+idade+Mulher+raca_branca+
                 escolaridade, data = X04810, family=binomial(link=logit))
tab_model(modelo3, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars",p.threshold = c(0.1, 0.05, 0.01))#ok

tab_model(modelo1, modelo2,modelo3, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars",p.threshold = c(0.1, 0.05, 0.01))#ok
summary(modelo2)

plot_models(modelo1,modelo2,modelo3,vline.color = "#edd840",dot.size = 3, spacing=0.7, ci.lvl=0.95, grid=F)
 
plot_models(modelo1,modelo2,modelo3,
            legend.title = "Onda",
            axis.labels = c("Avaliação Regular (comparado à Avaliação Ruim/Péssimo)",
                            "Avaliação Bom/Ótimo (comparado à Avaliação Ruim/Péssimo)",
                            "Votou em Lula 2022 (comparado à Bolsonaro 2022)",
                            "Escolaridade (níveis)", "Renda (faixas)",
                            "Votou em Haddad 2018 (comparado à Bolsonaro 2018)",
                            "Branco/Nulo/NãoSabe/NãoVotou (comparado à Bolsonaro 2018 e 2022)"),
            rm.terms = c("raca_brancaTRUE","Mulher","idade"),
            m.labels = c("modelo 1 2018", "modelo 2 2022", "modelo 3 2022"),
            auto.label=F,show.values = FALSE, 
            show.p = T, p.shape = TRUE, digits=4, std.est=TRUE, 
            p.threshold = c(0.1, 0.05, 0.01), 
            vline.color = "#edd840",dot.size = 3, spacing=0.7, ci.lvl=0.95, grid=F)+
  theme_bw()+theme( legend.title = element_text(color = "blue", size = 14),
                    legend.text = element_text(size = 12),axis.title.x = element_text(size = 14), 
                    axis.text.x = element_text(size=14), axis.text.y = element_text(size=16))+
  ggtitle("")+
  theme(plot.title = element_text(family="Georgia", colour="black", size=14))


library(marginaleffects)

# Verificando se os níveis foram reordenados corretamente
levels(X04810$aprovacao_bolsonaro22_recod)
plot_cap(modelo3, condition='aprovacao_bolsonaro22_recod', conf_level = .9)            
plot_cap(modelo3, condition='aprovacao_bolsonaro22_recod', conf_level = .9, draw=F) 

a<- plot_cap(modelo3, condition='aprovacao_bolsonaro22_recod', conf_level = .9) 
a+ labs(y="Probabilidades (0 a 1) de pertencer
        ao quartil superior de Iliberal Majoritário",
        x= "Aprovação da gestão de Bolsonaro (Novembro de 2022)") + theme_bw()    

table(ESEB2018_$dep)
table(ESEB2018_$turno2_)
figura2.1 <- ggplot(data = ESEB2018_,aes(escolaridade,dep,color = turno2_)) +
  geom_smooth(method="loess",level = 0.9,show.legend = T,se=F,size=2.4) + theme_minimal()+
  theme(legend.position = "bottom")#salvo
figura2.1 <- figura2.1 + labs(x="Escolaridade (níveis)", y= "Majoritarianismo (Índice)",
                              subtitle = '2018')
figura2.1
figura2.2 <- ggplot(data = X04810,aes(escolaridade,dep,color = turno2_)) +
  geom_smooth(method="loess",level = 0.9,show.legend = T,se=F,size=2.4) + theme_minimal()+
  theme(legend.position = "bottom")#salvo
figura2.2 <- figura2.2 + labs(x="Escolaridade (níveis)", y= "Majoritarianismo (Índice)",
                              subtitle = '2022')
figura2.2
figura2.3 <- ggplot(data = X04810,aes(escolaridade,dep,color = aprovacao_bolsonaro22_recod)) +
  geom_smooth(method="loess",level = 0.9,show.legend = T,se=F,size=2.4) + theme_minimal()+
  theme(legend.position = "bottom")#salvo
figura2.3 <- figura2.3 + labs(x="Escolaridade (níveis)", y= "Majoritarianismo (Índice)",
                              subtitle = '2022')
figura2.3

figura2.4 <- ggplot(data = X04810,aes(aprovacao_bolsonaro22_1,dep)) +
  geom_smooth(method="loess",level = 0.9,show.legend = T,se=T,size=2.4) +
  facet_wrap(~turno2_)+
  theme_bw()+
  theme(legend.position = "bottom")#salvo
figura2.4 <- figura2.4 + labs(x="Aprovação Bolsonaro", y= "Majoritarianismo (Índice)",
                              subtitle = '2022') 
figura2.4

figura2.5<- ggplot(data = X04810,aes(faixa_renda,dep)) +
  geom_smooth(method="loess",level = 0.9,show.legend = T,se=T,size=2.4) +
  facet_wrap(~aprovacao_bolsonaro22_recod)+
  theme_bw()+
  theme(legend.position = "bottom")#salvo
figura2.5 <- figura2.5 + labs(x="Faixa de Renda", y= "Majoritarianismo (Índice)",
                              subtitle = 'Faixa de Renda') 
figura2.5


ggplot(data = X04810,aes(Mulher,dep)) +
  geom_smooth(method="loess",level = 0.9,show.legend = T,se=T,size=2.4) +
  facet_wrap(~aprovacao_bolsonaro22_recod)+
  theme_bw()+
  theme(legend.position = "bottom") + labs(x="Gênero (Mulher)", y= "Majoritarianismo (Índice)",
                              subtitle = 'Gênero (Mulher = 2)') 


ggplot(data = X04810,aes(idade,dep)) +
  geom_smooth(method="loess",level = 0.9,show.legend = T,se=T,size=2.4) +
  facet_wrap(~aprovacao_bolsonaro22_recod)+
  theme_bw()+
  theme(legend.position = "bottom") + labs(x="Idade", y= "Majoritarianismo (Índice)",
                                           subtitle = 'Idade') 


ggplot(data = X04810,aes(escolaridade,dep)) +
  geom_smooth(method="loess",level = 0.9,show.legend = T,se=T,size=2.4) +
  facet_wrap(~aprovacao_bolsonaro22_recod)+
  theme_bw()+
  theme(legend.position = "bottom") + labs(x="Escolaridade", y= "Majoritarianismo (Índice)",
                                           subtitle = 'Escolaridade') 

X04810$raca_branca <- as.numeric(X04810$raca_branca)
ggplot(data = X04810,aes(raca_branca,dep)) +
  geom_smooth(method="loess",level = 0.9,show.legend = T,se=T,size=2.4) +
  facet_wrap(~aprovacao_bolsonaro22_recod)+
  theme_bw()+
  theme(legend.position = "bottom") + labs(x="Raça", y= "Majoritarianismo (Índice)",
                                           subtitle = 'Raça(branca =1)') 
