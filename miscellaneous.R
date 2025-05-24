# Bibliotecas
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
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
  
  tabela.frequencias.Sex = data.frame(
    xi = names(Sex.ni),
    ni = as.numeric(Sex.ni),
    fi = round(as.numeric(Sex.fi), 3)
  )

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
    geom_bar(fill = "purple") +
    labs(title = "Irmãos/Cônjuges a Bordo",
         x = "Nº de Irmãos/Cônjuges", y = "Frequência") +
    theme_minimal()
  
# Gráfico de barras - Parch
  ggplot(dados, aes(x = factor(Parch))) +
    geom_bar(fill = "turquoise") +
    labs(title = "Pais/Filhos a Bordo",
         x = "Nº de Pais/Filhos", y = "Frequência") +
    theme_minimal()
  
################################################################################
  
#    /***********************************/
#   / Variáveis Quantitativas Contínuas /
#  /              Histogramas          /
# /***********************************/
  
  fare_cleared <- na.omit(dados$Fare)
  
  # Histograma - Idade
  ggplot(dados, aes(x = Age)) +
    geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
    labs(title = "Histograma da Idade", x = "Idade", y = "Frequência Absoluta") +
    theme_minimal()
  
  # Histograma - Tarifa
  ggplot(dados, aes(x = Fare)) +
    geom_histogram(binwidth = 10, fill = "lightgreen", color = "black") +
    labs(title = "Histograma do Valor do Bilhete", x = "Valor do Bilhete (£)", y = "Frequência Absoluta") +
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
  
  # Análise Relacional - Sobrevivência vs Sexo
  # Responde à existência de diferenças nas taxas de sobrevivência entre géneros
  ggplot(dados, aes(x = Sex, fill = factor(Survived))) +
    geom_bar(position = "fill") +
    labs(title = "Proporção de Sobrevivência por Sexo",
         x = "Sexo", y = "Proporção", fill = "Sobreviveu") +
    theme_minimal()
  
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
  
  
################################################################################
#  Fase 3
################################################################################  

####################################
# Caracterização da variável SibSp #
####################################

# Variável quantitativa discreta, com 7 valores distintos possíveis (0, 1, 2, 3, 4, 5, 8)
# Representa uma contagem de eventos (número de familiares que acompanhavam o passageiro)
# Através do grtáfico de barras da frequência absoluta da variável (Linha 145), é possível verificar
#   um pico claro em 0 ou 1, seuido de uma queda progressiva para valores maiores. Ou seja,
#   Os valores são pequenos e assimétricos à direita.
# Esta forma decresccente assimétrica é típica da Poisson.

#   /*****************************************/
#  /   Teste de ajustamento do Qui-Quadrado  /
# /*****************************************/
# H₀:Os dados de SibSp seguem uma distribuição de Poisson.
# H₁:Os dados de SibSp não seguem uma distribuição de Poisson.
lambda_sibSp = mean(dados$SibSp, na.rm = TRUE)  # Estimativa de λ

# Tabela de frequências observadas
tabela.frequencias.SibSp = table(dados$SibSp)

# Calcular probabilidades teóricas da Poisson
  # Número total de observações (n)
  n.sibSp = sum(tabela.frequencias.SibSp)
  # Criar vetor de valores observados (0 a 8)
  valores.sibSp = as.numeric(names(tabela.frequencias.SibSp))
  # Calcular probabilidades teóricas da Poisson, com o λ estimado
  p.sibSp = dpois(valores.sibSp, lambda_sibSp)
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
        # de agrupar os valores maiores ou iguais a 5 numa única classe "≥ 5".
        # Isto permite garantir a validade estatística do teste e manter a sua fiabilidade.
  limite.agrupamento = 5
  tabela.agrupada.sibs$grupo[tabela.agrupada.sibs$xi >= limite.agrupamento] = paste0("≥ ", limite.agrupamento)
  # Reagrupar: somar ni, Ei, recalcular pi com Ei/n
  tabela.agrupada.sibs = aggregate(
    cbind(ni, Ei) ~ grupo, 
    data = tabela.agrupada.sibs, 
    FUN = sum
  )
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



