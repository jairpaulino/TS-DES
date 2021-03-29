# SALDANHA, Raphael de Freitas; BASTOS, Ronaldo Rocha; BARCELLOS, Christovam. 
# Microdatasus: pacote para download e pré-processamento de microdados do 
# Departamento de Informática do SUS (DATASUS). Cad. Saúde Pública, Rio de 
# Janeiro , v. 35, n. 9, e00032419, 2019 . Available from 
# http://ref.scielo.org/dhcq3y.

# Wiki: https://github.com/rfsaldanha/microdatasus/wiki

# Sistemas: https://datasus.saude.gov.br/wp-content/uploads/2019/08/Catalogo-de-Produtos-DATASUS.pdf

# Opções: https://github.com/danicat/datasus/

#install.packages("devtools")
library(devtools)
devtools::install_github("rfsaldanha/microdatasus")

library(microdatasus)
# dados = fetch_datasus(year_start = 2013, 
#                       year_end = 2014, 
#                       uf = "CE", 
#                       information_system = "SINASC")
# SIM-DO - Óbitos

dados <- fetch_datasus(year_start = 2011, month_start = 1, 
                       year_end = 2011, month_end = 2,
                       #uf = "CE", 
                       information_system = "SIH-RD")

dados <- process_sih(dados)

str(dados)
summary(dados)

length(dados$UF_ZI)
dados$

t.test()
