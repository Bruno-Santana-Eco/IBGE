install.packages('readxl')
install.packages('mirt')
install.packages('shiny')

library(readxl)
library(mirt)
library(shiny)

itemplot(shiny = T)
caminho <- "D:/Repositório GIT/Projetos/IBGE/Datasets/Educação/Microdado_Teste1.xlsx"
data <- read_excel(caminho)
data = na.omit(data)

mydata_irt = data[12:36]
head(mydata_irt)

mod1 = mirt(mydata_irt, 
            1,
            itemtype= 'Rasch')

for (i in 1:length(mydata_irt)) {
  ItemPlot = itemfit(mod1,
                     group.bins = 11,
                     empirical.plot = i,
                     empirical.CI = .95,
                     method = 'ML')
  print(ItemPlot)
}

coef(mod1, simplify = TRUE, IRTpars=TRUE)

mod2 = mirt(mydata_irt, 
            1,
            itemtype= 'graded')

coef(mod2, simplify = TRUE, IRTpars=TRUE)

for (i in 1:length(mydata_irt)) {
  ItemPlot = itemfit(mod2,
                     group.bins = 11,
                     empirical.plot = i,
                     empirical.CI = .95,
                     method = 'ML')
  print(ItemPlot)
}
anova(mod1, mod2)
