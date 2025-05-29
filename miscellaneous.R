
# Bibliotecas
#install.packages("ggplot2")
library(ggplot2)
#install.packages("dplyr")
library(dplyr)

# Importar o ficheiro CSV para o dataframe dado
dados = read.csv("titanic_data.csv")

# Verificar os dados
head(dados)
class(dados)

################################################################################

#   /************************/
#  / Tabelas de Frequências /
# /************************/

# Survived -> Variável principal (Alvo da análise) 
Survived.ni = table(dados$Survived)
Survived.fi = prop.table(Survived.ni)

tabela.frequencias.Survived = data.frame(
  xi = names(Survived.ni),
  ni = as.numeric(Survived.ni),
  fi = round(as.numeric(Survived.fi), 3)
)

# Sex -> Útil para comparar taxas de sobrevivência por género
Sex.ni = table(dados$Sex)
Sex.fi = prop.table(Sex.ni)

###########################################################################################################
#frequencias absolutas, relativas e suas acumuladas para a variavel qualitativa SEXO

# Criar vetor com nomes em pt
nomes_sexo <- c("male" = "Masculino", "female" = "Feminino")

# Calcular as frequências
ni_sex <- table(dados$Sex)                      # Frequência absoluta
fi_sex <- prop.table(ni_sex)                    # Frequência relativa
ni_ac_sex <- cumsum(ni_sex)                     # Frequência absoluta acumulada
fi_ac_sex <- cumsum(fi_sex)                     # Frequência relativa acumulada

# Criar a tabela de frequências
tabela_frequencias_sexo <- data.frame(
  Sexo = nomes_sexo[names(ni_sex)],
  Frequencia_absoluta = as.integer(ni_sex),
  Frequencia_relativa = round(as.numeric(fi_sex), 4),
  Frequência_absoluta_acumulada = as.integer(ni_ac_sex),
  Frequência_relativa_acumulada = round(as.numeric(fi_ac_sex), 4)
)

# Ver a tabela
print(tabela_frequencias_sexo)
###########################################################################################################

# Embarked -> Entender o perfil dos passageiros por porto de embarque
Embarked.ni = table(dados$Embarked)
Embarked.fi = prop.table(Embarked.ni)

tabela.frequencias.Embarked = data.frame(
  xi = names(Embarked.ni),
  ni = as.numeric(Embarked.ni),
  fi = round(as.numeric(Embarked.fi), 3)
)

# Pclass -> Pode haver relação entre classe social e sobrevivência
Pclass.ni = table(dados$Pclass)      # frequência absoluta
Pclass.fi = prop.table(Pclass.ni)    # frequência relativa
Pclass.Ni = cumsum(Pclass.ni)        # frequência absoluta acumulada
Pclass.Fi = cumsum(Pclass.fi)        # frequência relativa acumulada

tabela.frequencias.Pclass = data.frame(
  xi = names(Pclass.ni),
  ni = as.numeric(Pclass.ni),
  fi = round(as.numeric(Pclass.fi), 3),
  Ni = as.numeric(Pclass.Ni),
  Fi = round(as.numeric(Pclass.Fi), 3)
)
tabela.frequencias.Pclass = rbind(
  tabela.frequencias.Pclass,
  data.frame(
    xi = "Total",
    ni = sum(Pclass.ni),
    fi = round(sum(Pclass.fi), 3),
    Ni = NA,
    Fi = NA
  )
)

################################################################################

#    /************************/
#   /   Gráficos Circulares  /
#  /  Qualitativas Nominais /
# /************************/

# Gráfico circular - Survived
dados %>%
  count(Survived) %>%
  mutate(Sobreviveu = case_when(
    Survived == 0 ~ "0 - Não Sobreviveu",
    Survived == 1 ~ "1 - Sobreviveu"
  ),
  prop = round(n / sum(n), 3),
  label = paste0(Sobreviveu, " (", prop * 100, "%)")) %>%
  ggplot(aes(x = "", y = prop, fill = Sobreviveu)) +
  geom_col() +
  coord_polar("y") +
  labs(title = "Distribuição de Sobrevivência") +
  theme_void() +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5))

# Gráfico circular - Sex
dados %>%
  count(Sex) %>%
  mutate(Sexo = case_when(
    Sex == "male" ~ "Masculino",
    Sex == "female" ~ "Feminino"
  ),
  prop = round(n / sum(n), 3),
  label = paste0(Sexo, " (", prop * 100, "%)")) %>%
  ggplot(aes(x = "", y = prop, fill = Sexo)) +
  geom_col() +
  coord_polar("y") +
  labs(title = "Distribuição por Sexo") +
  theme_void() +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5))

# Gráfico circular - Port
dados %>%
  count(Embarked) %>%
  mutate(Porto = case_when(
    Embarked == "C" ~ "C - Cherbourg",
    Embarked == "Q" ~ "Q - Queenstown",
    Embarked == "S" ~ "S - Southampton",
    TRUE ~ as.character(Embarked)
  ),
  prop = round(n / sum(n), 3),
  label = paste0(Porto, " (", prop * 100, "%)")) %>%
  ggplot(aes(x = "", y = prop, fill = Porto)) +
  geom_col() +
  coord_polar("y") +
  labs(title = "Distribuição por Porto de Embarque") +
  theme_void() +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5))

################################################################################

#    /************************/
#   /   Gráficos de Barras   /
#  /  Ordinais ou Discretas /
# /************************/

# Gráfico de barras - Classe
ggplot(dados, aes(x = factor(Pclass))) +
  geom_bar(fill = "coral") +
  labs(title = "Distribuição por Classe Socioeconómica",
       x = "Classe (1 = Alta, 3 = Baixa)", y = "Frequência") +
  theme_minimal()

# Gráfico de barras - SibSp
ggplot(dados, aes(x = factor(SibSp))) +
  geom_bar(fill = "#31356e") +
  labs(title = "Irmãos/Cônjuges a Bordo",
       x = "Nº de Irmãos/Cônjuges", y = "Frequência") +
  theme_minimal()
# Gráfico de barras - SibSp com fundo personalizado
ggplot(dados, aes(x = factor(SibSp))) +
  geom_bar(fill = "#31356e") +
  labs(title = "Irmãos/Cônjuges a Bordo",
       x = "Nº de Irmãos/Cônjuges", y = "Frequência") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#dae9ff", color = NA),
    plot.background = element_rect(fill = "#dae9ff", color = NA)
  )


# Gráfico de barras - Parch
ggplot(dados, aes(x = factor(Parch))) +
  geom_bar(fill = "turquoise") +
  labs(title = "Pais/Filhos a Bordo",
       x = "Nº de Pais/Filhos", y = "Frequência") +
  theme_minimal()

################################################################################

#    /***********************************/
#   / Variáveis Quantitativas Contínuas /
#  /              Boxplots             /
# /***********************************/

# # Calcular quartis e IQR para Age
#   age_cleared <- na.omit(dados$Age)
#   Age_Q1 <- quantile(age_cleared, 0.25)
#   Age_Q3 <- quantile(age_cleared, 0.75)
#   Age_IQR <- Age_Q3 - Age_Q1
# # Limites para outliers severos
#   Age_lim_inf <- Age_Q1 - 3 * Age_IQR
#   Age_lim_sup <- Age_Q3 + 3 * Age_IQR
# # Identificar outliers severos
#   Age_outliers_severos <- dados %>%
#     filter(!is.na(Age) & (Age < Age_lim_inf | Age > Age_lim_sup))
# rm(Age_outliers_severos)      

# Boxplot - Idade
ggplot(dados, aes(y = Age)) +
  geom_boxplot(outlier.shape = 1, fill = "lightblue", color = "black") +
  labs(title = "Diagrama de Extremos e Quartis da Idade",
       y = "Idade") +
  theme_minimal()


# Boxplot - Tarifa
ggplot(dados, aes(y = Fare)) +
  geom_boxplot(outlier.shape = 1, fill = "lightgreen", color = "black") +
  labs(title = "Diagrama de Extremos e Quartis do Valor do Bilhete (£)",
       y = "Valor do Bilhete") +
  theme_minimal()

################################################################################

#    /***********************************/
#   / Variáveis Quantitativas Contínuas /
#  /        Agrupamento por Classes    /
# /***********************************/  
# Determinar número de classes - Regra de Sturges:
#   k=⌊1+log2(n)⌋=⌊1+log10(n)/log10(2)⌋
### Variável Age ###
# Remover NA
age_cleared <- na.omit(dados$Age)
n_age <- length(age_cleared)
# Calcular número de classes (Sturges)
k_age <- floor(1 + log2(n_age))
k_age
# Definir amplitude da classe (arredondar por excesso)
range_age <- range(age_cleared)
amplitude_age <- ceiling((range_age[2] - range_age[1]) / k_age)
amplitude_age
# Criar classes
breaks_age <- seq(floor(range_age[1]), ceiling(range_age[2]) + amplitude_age, by = amplitude_age)
classes_age <- cut(age_cleared, breaks = breaks_age, right = FALSE, include.lowest = TRUE)

# Tabela de frequências
Age.ni <- table(classes_age)
Age.fi <- prop.table(Age.ni)
Age.Ni <- cumsum(Age.ni)
Age.Fi <- cumsum(Age.fi)
tabela.frequencias.Age <- data.frame(
  xi = names(Age.ni),
  ni = as.numeric(Age.ni),
  fi = round(as.numeric(Age.fi), 3),
  Ni = as.numeric(Age.Ni),
  Fi = round(as.numeric(Age.Fi), 3)
)
# Adicionar linha total
tabela.frequencias.Age <- rbind(
  tabela.frequencias.Age,
  data.frame(
    xi = "Total",
    ni = sum(Age.ni),
    fi = round(sum(Age.fi), 3),
    Ni = NA,
    Fi = NA
  )
)

### Variável Fare ###
# Remover NA
fare_cleared <- na.omit(dados$Fare)
n_fare <- length(fare_cleared)
# Calcular número de classes (Sturges)
k_fare <- floor(1 + log2(n_fare))
k_fare
# Definir amplitude da classe (arredondar por excesso)
range_fare <- range(fare_cleared)
amplitude_fare <- ceiling((range_fare[2] - range_fare[1]) / k_fare)
amplitude_fare
# Criar classes
breaks_fare <- seq(floor(range_fare[1]), ceiling(range_fare[2]) + amplitude_fare, by = amplitude_fare)
classes_fare <- cut(fare_cleared, breaks = breaks_fare, right = FALSE, include.lowest = TRUE)

# Tabela de frequências
Fare.ni <- table(classes_fare)
Fare.fi <- prop.table(Fare.ni)
Fare.Ni <- cumsum(Fare.ni)
Fare.Fi <- cumsum(Fare.fi)
tabela.frequencias.Fare <- data.frame(
  xi = names(Fare.ni),
  ni = as.numeric(Fare.ni),
  fi = round(as.numeric(Fare.fi), 3),
  Ni = as.numeric(Fare.Ni),
  Fi = round(as.numeric(Fare.Fi), 3)
)
# Adicionar linha total
tabela.frequencias.Fare <- rbind(
  tabela.frequencias.Fare,
  data.frame(
    xi = "Total",
    ni = sum(Fare.ni),
    fi = round(sum(Fare.fi), 3),
    Ni = NA,
    Fi = NA
  )
)


################################################################################

#   /***********************/
#  / Análises Relacionais  /
# /***********************/

# Análise Relacional - Sobrevivência vs Classe
# Mostra se passageiros de classe alta tiveram maior probabilidade de sobreviver
ggplot(dados, aes(x = factor(Pclass), fill = factor(Survived))) +
  geom_bar(position = "fill") +
  labs(title = "Proporção de Sobrevivência por Classe",
       x = "Classe", y = "Proporção", fill = "Sobreviveu") +
  theme_minimal()

# Análise Relacional - Distribuição de Idade por sobrevivência
# Mostra se idade influenciou a sobrevivência (ex: crianças e idosos).
classes_age <- cut(age_cleared, breaks = breaks_age, right = FALSE, include.lowest = TRUE)
# Criar um dataframe com as classes de idade e sobrevivência
dados_idade_surv <- data.frame(
  Classe_Idade = classes_age,
  Sobreviveu = factor(dados$Survived[!is.na(dados$Age)], labels = c("Não Sobreviveu", "Sobreviveu"))
)
ggplot(dados_idade_surv, aes(x = Classe_Idade, fill = Sobreviveu)) +
  geom_bar(position = "stack") +
  labs(title = "Distribuição da Idade por Sobrevivência",
       x = "Classe de Idade", y = "Frequência",
       fill = "Sobreviveu") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(dados_idade_surv, aes(x = Classe_Idade, fill = Sobreviveu)) +
  geom_bar(position = "stack") +
  geom_text(stat = "count", aes(label = after_stat(count)),
            position = position_stack(vjust = 0.5), size = 3, color = "white") +
  labs(title = "Distribuição da Idade por Sobrevivência",
       x = "Classe de Idade", y = "Frequência",
       fill = "Sobreviveu") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#####################################################################################################################################
#ESPADAAAAAAAAAAAAAAAAAA
################################################################################################################################
#frequencias absolutas, relativas e suas acumuladas para a variavel quantitativa IDADE

#Há 86 pessoas sem idade 332+86 = 418

# Criar categorias de idade com intervalos 
dados$faixa_idade <- cut(
  dados$Age,
  breaks = c(-Inf, 12, 18, 30, 50, Inf),
  labels = c("Criança", "Adolescente", "Jovem", "Adulto", "Idoso"),
  right = FALSE #intervalos fechados a esquerda
)

# Calcular as frequências
ni_age <- table(dados$faixa_idade)           # Frequência absoluta
fi_age <- prop.table(ni_age)                        # Frequência relativa
ni_ac_age <- cumsum(ni_age)                         # Frequência absoluta acumulada
fi_ac_age <- cumsum(fi_age)                         # Frequência relativa acumulada

# Criar a tabela de frequências
tabela_frequencias_idade <- data.frame(
  Categoria = names(ni_age),
  Frequencia_absoluta = as.integer(ni_age),
  Frequencia_relativa = round(as.numeric(fi_age), 4),# este 4 arredonda as frequencias relativas com 4 casas decimais
  Frequência_absoluta_acumulada = as.integer(ni_ac_age),
  Frequência_relativa_acumulada = round(as.numeric(fi_ac_age), 4)# este 4 arredonda as frequencias relativas com 4 casas decimais
)

# tabela
print(tabela_frequencias_idade)
###########################################################################################################################################################
#frequencias absolutas, relativas e suas acumuladas para a variavel qualitativa EMBARKED

# Criar vetor com nomes completos
nomes_portos <- c("C" = "Cherbourg", "Q" = "Queenstown", "S" = "Southampton")

# Calcular as frequências
ni_embarked <- table(dados$Embarked)            # Frequência absoluta
fi_embarked <- prop.table(ni_embarked)            # Frequência relativa
ni_ac_embarked <- cumsum(ni_embarked)             # Frequência absoluta acumulada
fi_ac_embarked <- cumsum(fi_embarked)             # Frequência relativa acumulada

# Criar a tabela de frequências
tabela_frequencias_embarked <- data.frame(
  Categoria = names(ni_embarked),
  Lugar_embarque = nomes_portos[names(ni_embarked)],
  Frequencia_absoluta = as.integer(ni_embarked),
  Frequencia_relativa = round(as.numeric(fi_embarked), 4), # este 4 arredonda as frequencias relativas com 4 casas decimais
  Frequência_absoluta_acumulada = as.integer(ni_ac_embarked),
  Frequência_relativa_acumulada = round(as.numeric(fi_ac_embarked), 4)# este 4 arredonda as frequencias relativas com 4 casas decimais
)

# tabela
print(tabela_frequencias_embarked)
###############################################################################################################################################
#frequencias absolutas, relativas e suas acumuladas para a variavel quantitativa FARE

#TEMOS UM N/A AQUI

# Criar categorias de preços (intervalos de preço)
dados$faixa_fare <- cut(
  dados$Fare,
  breaks = c(-Inf, 10, 30, 60, 100, Inf),
  labels = c("Muito Baixa", "Baixa", "Média", "Alta", "Muito Alta"),
  right = FALSE  # Intervalos fechados à esquerda: [a, b)
)

# Calcular as frequências
ni_fare <- table(dados$faixa_fare)           # Frequência absoluta
fi_fare <- prop.table(ni_fare)                      # Frequência relativa
ni_ac_fare <- cumsum(ni_fare)                       # Frequência absoluta acumulada
fi_ac_fare <- cumsum(fi_fare)                       # Frequência relativa acumulada

# Criar a tabela de frequências
tabela_frequencias_fare <- data.frame(
  Categoria = names(ni_fare),
  Frequencia_absoluta = as.integer(ni_fare),
  Frequencia_relativa = round(as.numeric(fi_fare), 4),
  Frequência_absoluta_acumulada = as.integer(ni_ac_fare),
  Frequência_relativa_acumulada = round(as.numeric(fi_ac_fare), 4)
)

# Ver a tabela
print(tabela_frequencias_fare)

###########################################################################################################
#frequencias absolutas, relativas e suas acumuladas para a variavel qualitativa SEXO

# Criar vetor com nomes em pt
nomes_sexo <- c("male" = "Masculino", "female" = "Feminino")

# Calcular as frequências
ni_sex <- table(dados$Sex)               # Frequência absoluta
fi_sex <- prop.table(ni_sex)                    # Frequência relativa
ni_ac_sex <- cumsum(ni_sex)                     # Frequência absoluta acumulada
fi_ac_sex <- cumsum(fi_sex)                     # Frequência relativa acumulada

# Criar a tabela de frequências
tabela_frequencias_sexo <- data.frame(
  Sexo = nomes_sexo[names(ni_sex)],
  Frequencia_absoluta = as.integer(ni_sex),
  Frequencia_relativa = round(as.numeric(fi_sex), 4),
  Frequência_absoluta_acumulada = as.integer(ni_ac_sex),
  Frequência_relativa_acumulada = round(as.numeric(fi_ac_sex), 4)
)

# Ver a tabela
print(tabela_frequencias_sexo)
###########################################################################################################
#grafico circular da variavel qualitativa sexo

# Converter as frequências relativas em percentagens
percentagens <- round(fi_sex * 100, 1)

# Criar o gráfico 
pie(
  fi_sex, 
  labels = paste(nomes_sexo[names(ni_sex)], "(", percentagens, "%)", sep = ""),
  main = "Distribuição do Sexo dos Passageiros",
  col = c("lightblue", "lightcoral"),
  radius = 1
)
###########################################################################################################
#grafico de barras que compara a variavel quantitativa FARE entre sexos

# Calcular as frequências relativas por sexo para cada faixa de fare
library(dplyr)
frequencias_fare_sexo <- dados %>%
  group_by(faixa_fare, Sex) %>%
  summarise(frequencia_relativa = n() / nrow(dados)) %>%
  ungroup()

# Criar o gráfico de barras comparando as frequências relativas
library(ggplot2)
ggplot(frequencias_fare_sexo, aes(x = faixa_fare, y = frequencia_relativa, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("lightblue", "lightcoral")) +
  labs(
    title = "Comparação de preço por Sexo",
    x = "Faixa de Preço",
    y = "Frequência Relativa",
    fill = "Sexo"
  ) +
  theme_minimal()
#################################################################################################################
#grafico de barras de onde embarcaram os passageiros em frequencia absoluta

# Criar o gráfico de barras com frequências absolutas
barplot(
  ni_embarked,
  main = "Distribuição do local de embarque dos Passageiros",
  xlab = "Local",
  ylab = "Frequência Absoluta",
  col = "lightblue",
  border = "darkblue",
  names.arg = nomes_portos,
  ylim = c(0, max(ni_embarked) + 100)  # Aumentando o limite superior do eixo Y
)

##################################################################################################################
#histrograma da idade

# Definir as faixas de idade novas com limites de inicio e fim para o histograma
breaks_idade <- c(0, 12, 18, 30, 50, 100)  # Faixas da idade 
frequencias_absolutas <- table(cut(dados$Age, breaks = breaks_idade))
amplitudes <- diff(breaks_idade) # Calcular as amplitudes das classes (larguras dos intervalos)
frequencias_relativas <- frequencias_absolutas / sum(frequencias_absolutas) # Calcular a frequência relativa para cada intervalo
alturas <- frequencias_relativas / amplitudes # Calcular a altura das barras, ajustando pela amplitude da classe

# Iniciar o gráfico vazio (sem barras)
plot(0, 0, type = "n", xlab = "Idade", ylab = "Densidade", xlim = c(0, 100), ylim = c(0, max(alturas) + 0.02))

# Adicionar barras com larguras diferentes
for (i in 1:(length(breaks_idade) - 1)) {
  rect(
    xleft = breaks_idade[i],  # Posição inicial
    ybottom = 0,              # A base é zero
    xright = breaks_idade[i+1], # Posição final
    ytop = alturas[i],        # A altura da barra é a frequência relativa
    col = "lightblue",        # Cor da barra
    border = "darkblue"       # Cor da borda
  )
}

# Adicionar título
#title(main = "Distribuição da Idade dos Passageiros", col.main = "black")
##############################################################################################################################################
# Boxplot(diagrama de extremos e quartis) de Fare por Sexo
boxplot(
  Fare ~ Sex,
  data = dados,
  main = "Diagrama de extremos e quartis do preço por sexo",
  ylab = "Preço do Bilhete",
  col = c("lightblue", "lightpink"),
  outline = TRUE,       # Mostrar outliers
  yaxt = "n"             # Desliga o eixo y padrão para podermos personalizar em baixo
)

# Adicionar um eixo y mais detalhado
axis(side = 2, at = seq(0, 550, by = 20))
###################################################################################################################
################################################################################
#  Fase 3
################################################################################  

####################################
# Caracterização da variável SibSp #
####################################

# Variável quantitativa discreta, com 7 valores distintos possíveis (0, 1, 2, 3, 4, 5, 8)
# Representa uma contagem de eventos (número de familiares que acompanhavam o passageiro)
# Através do grtáfico de barras da frequência absoluta da variável (Linha 164), é possível verificar
#   um pico claro em 0 ou 1, seuido de uma queda progressiva para valores maiores. Ou seja,
#   Os valores são pequenos e assimétricos à direita.
# Esta forma decresccente assimétrica é típica da Poisson.

#   /*****************************************/
#  /   Teste de ajustamento do Qui-Quadrado  /
# /*****************************************/
# H₀:Os dados de SibSp seguem uma distribuição de Poisson.
# H₁:Os dados de SibSp não seguem uma distribuição de Poisson.
lambda_sibSp = mean(dados$SibSp, na.rm = TRUE)  # Estimativa de λ
lambda_sibSp

# Tabela de frequências observadas
tabela.frequencias.SibSp = table(dados$SibSp)
tabela.frequencias.SibSp

# Calcular probabilidades teóricas da Poisson
# Número total de observações (n)
n.sibSp = sum(tabela.frequencias.SibSp)
# Criar vetor de valores observados (0 a 8)
valores.sibSp = as.numeric(names(tabela.frequencias.SibSp))
# Calcular probabilidades teóricas da Poisson, com o λ estimado
p.sibSp = dpois(valores.sibSp, lambda_sibSp)
p.sibSp
# Frequências esperadas
freq.esperadas.sibSp = round(n.sibSp * p.sibSp, 2)
freq.esperadas.sibSp

# Tabela de ajustamento
tabela.ajustamento.sibSp = data.frame(
  xi = valores.sibSp,
  ni = as.numeric(tabela.frequencias.SibSp),
  pi = round(p.sibSp, 4),
  Ei = freq.esperadas.sibSp
)
print(tabela.ajustamento.sibSp)

# Agrupando valores para o teste
tabela.agrupada.sibs = tabela.ajustamento.sibSp
tabela.agrupada.sibs$grupo = as.character(tabela.agrupada.sibs$xi)
# Agrupar valores maiores ou iguais ao limite
# Como o teste de ajustamento do Qui-Quadrado exige que todas as frequências 
# esperadas sejam ≥ 1, e que no máximo 20% sejam inferiores a 5, tivemos 
# de agrupar os valores maiores ou iguais a 5 numa única classe "≥ 3".
# Isto permite garantir a validade estatística do teste e manter a sua fiabilidade.
limite.agrupamento = 3
tabela.agrupada.sibs$grupo[tabela.agrupada.sibs$xi >= limite.agrupamento] = paste0("≥ ", limite.agrupamento)
# Reagrupar: somar ni, Ei, recalcular pi com Ei/n
tabela.agrupada.sibs = aggregate(
  cbind(ni, Ei) ~ grupo, 
  data = tabela.agrupada.sibs, 
  FUN = sum
)
tabela.agrupada.sibs
# Calcular pi = Ei / n
tabela.agrupada.sibs$pi = round(tabela.agrupada.sibs$Ei / sum(tabela.agrupada.sibs$ni), 4)
# Reorganizar colunas
tabela.agrupada.sibs = tabela.agrupada.sibs[, c("grupo", "ni", "pi", "Ei")]
print(tabela.agrupada.sibs)

# Calcular a estatística do teste de Qui-Quadrado
q.sibs = sum((tabela.agrupada.sibs$ni - tabela.agrupada.sibs$Ei)^2 / tabela.agrupada.sibs$Ei)
q.sibs

# Graus de liberdade
# Número de grupos na tabela
k.sibs = nrow(tabela.agrupada.sibs)
# 1 parâmetro (λ) estimado.
gl.sibs = k.sibs - 1 - 1
gl.sibs

# Calcular o valor crítico do Qui-Quadrado
p.sibs = pchisq(q.sibs, df = gl.sibs, lower.tail = FALSE)
p.sibs
# Nível de significância
alpha <- 0.05
# Decisão
if (p.sibs <= alpha) {
  cat("Rejeita-se H₀: os dados NÃO seguem uma Poisson(λ)")
} else {
  cat("Não se rejeita H₀: os dados PODEM ser modelados por uma Poisson(λ)")
}



###################################
# Caracterização da variável Age #
# Teste de ajustamento do Qui-Quadrado#
###################################

# Variável quantitativa contínua (idade dos passageiros)
# Queremos testar se segue uma distribuição Normal

# Remover valores em falta
idade <- na.omit(dados$Age)

# Estimar média e desvio padrão
media_idade <- mean(idade)
sd_idade <- sd(idade)

# Número total de observações
n_idade <- length(idade)

# Criar intervalos (classes) de 10 em 10 anos
breaks <- c(seq(0, 80, by = 10), Inf)  # Último grupo inclui todos >= 80
classes <- cut(idade, breaks = breaks, right = FALSE)

# Tabela de frequências observadas
freq_obs <- table(classes)

# Calcular probabilidades teóricas da Normal para os mesmos intervalos
p_teoricos <- pnorm(breaks[-1], mean = media_idade, sd = sd_idade) -
  pnorm(breaks[-length(breaks)], mean = media_idade, sd = sd_idade)

# Frequências esperadas
freq_esp <- n_idade * p_teoricos

# Criar tabela com os dados
tabela_age <- data.frame(
  Classe = levels(classes),
  Observado = as.numeric(freq_obs),
  Esperado = round(freq_esp, 2)
)

print(tabela_age)

# Verificar condições
cond1 <- sum(tabela_age$Esperado < 5) / nrow(tabela_age) <= 0.2
cond2 <- all(tabela_age$Esperado >= 1)

if (!cond1 | !cond2) {
  cat("Algumas classes têm frequência esperada < 5. Agrupamento necessário.\n")
  
  # Exemplo: agrupar extremos (ultimas 3 classes para dar 1 pelo menos)
  tabela_age$Grupo <- tabela_age$Classe
  tabela_age$Grupo[tabela_age$Classe %in% c("[60,70)","[70,80)", "[80,Inf)")] <- "60+"
  
  # Reagrupar
  tabela_agrupada <- aggregate(cbind(Observado, Esperado) ~ Grupo, data = tabela_age, FUN = sum)
  print(tabela_agrupada)
  
} else {
  tabela_agrupada <- tabela_age
  tabela_agrupada$Grupo <- tabela_agrupada$Classe
  print(tabela_agrupada)
}

# Estatística do teste
q_age <- sum((tabela_agrupada$Observado - tabela_agrupada$Esperado)^2 / tabela_agrupada$Esperado)

# Graus de liberdade
# k = número de grupos após agrupamento
# 2 parâmetros estimados (média e desvio padrão)
gl_age <- nrow(tabela_agrupada) - 1 - 2

# p-valor
p_age <- pchisq(q_age, df = gl_age, lower.tail = FALSE)

# Resultado
cat("Estatística Qui-Quadrado:", q_age, "\n")
cat("Graus de liberdade:", gl_age, "\n")
cat("p-valor:", p_age, "\n")

# Decisão
alpha <- 0.05
if (p_age <= alpha) {
  cat("Rejeita-se H₀: os dados de 'Age' NÃO seguem uma distribuição Normal.\n")
} else {
  cat("Não se rejeita H₀: os dados de 'Age' PODEM ser modelados por uma distribuição Normal.\n")
}

###################################
# Caracterização da variável Fare #
#Teste de ajustamento de Kolmogorov-Smirnov#
#aparece o warning porque há empates(valores repetidos)#
#Não invalida o resultado, mas significa que o p.value pode não ser totalmente fiável.#
###################################

# Remover valores em falta
fare <- na.omit(dados$Fare)

# Estimar média e desvio padrão
media_fare <- mean(fare)
sd_fare <- sd(fare)

# Aplicar teste de Kolmogorov-Smirnov à Normal com os parâmetros estimados
ks_result <- ks.test(fare, "pnorm", mean = media_fare, sd = sd_fare)

# Mostrar o resultado
print(ks_result)

if (ks_result$p.value < 0.05) {
  cat("Rejeita-se H₀: os dados de 'Fare' NÃO seguem uma distribuição Normal.\n")
} else {
  cat("Não se rejeita H₀: os dados de 'Fare' PODEM ser modelados por uma distribuição Normal.\n")
}

###################################
# Teste de Normalidade de Shapiro-Wilk para 'Fare'
###################################

# Remover valores em falta
fare <- na.omit(dados$Fare)

# Aplicar o teste de Shapiro-Wilk
shapiro_result <- shapiro.test(fare)

# Mostrar o resultado
print(shapiro_result)

# Tomada de decisão
if (shapiro_result$p.value < 0.05) {
  cat("Rejeita-se H₀: os dados de 'Fare' NÃO seguem uma distribuição Normal.\n")
} else {
  cat("Não se rejeita H₀: os dados de 'Fare' PODEM ser modelados por uma distribuição Normal.\n")
}
