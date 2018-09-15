library(ggplot2);
library(GGally);

###############################################################
# Leitura dos Dados
###############################################################
Dados = read.csv("Base.csv", sep=";", dec=",");
View(Dados);

Dados = Dados[,1:14];
names(Dados) = c("ID", "Padrao", "Fase", "Unidades", "AreaUtilTotal", "UnidadesPorAndar", "AreaUtil", "Dormitorios", "Suites", "Banheiros", "Vagas", "Lancamento", "AnoMes", "Ano");

Dados$AnoMes = factor(Dados$AnoMes);
Dados$Padrao = factor(Dados$Padrao);
Dados$Fase = factor(Dados$Fase);

# Separação para Análise
Dados.Analise = Dados;
View(Dados.Analise);

# Filtro por Período
# Dados.Analise = subset(Dados, Dados$Ano >= 2016);

###############################################################
# Análise Exploratória - Distribuição dos Dados
###############################################################
ggplot(Dados.Analise, aes(x=Unidades)) + geom_histogram(col="black", fill="blue", alpha=0.6);
ggplot(Dados.Analise, aes(x=AnoMes, y=Unidades)) + geom_boxplot();
ggplot(Dados.Analise, aes(x=Ano, y=Unidades)) + geom_boxplot();
ggplot(Dados.Analise, aes(x=Padrao, y=Unidades)) + geom_boxplot();

ggplot(Dados.Analise, aes(x=AreaUtilTotal)) + geom_histogram(col="black", fill="blue", alpha=0.6);
ggplot(Dados.Analise, aes(x=AnoMes, y=AreaUtilTotal)) + geom_boxplot();
ggplot(Dados.Analise, aes(x=Ano, y=AreaUtilTotal)) + geom_boxplot();
ggplot(Dados.Analise, aes(x=Padrao, y=AreaUtilTotal)) + geom_boxplot();

ggplot(Dados.Analise, aes(x=UnidadesPorAndar)) + geom_histogram(col="black", fill="blue", alpha=0.6);
ggplot(Dados.Analise, aes(x=AnoMes, y=UnidadesPorAndar)) + geom_boxplot();
ggplot(Dados.Analise, aes(x=Ano, y=UnidadesPorAndar)) + geom_boxplot();
ggplot(Dados.Analise, aes(x=Padrao, y=UnidadesPorAndar)) + geom_boxplot();

ggplot(Dados.Analise, aes(x=AreaUtil)) + geom_histogram(col="black", fill="blue", alpha=0.6, binwidth=25);
ggplot(Dados.Analise, aes(x=AnoMes, y=AreaUtil)) + geom_boxplot();
ggplot(Dados.Analise, aes(x=Ano, y=AreaUtil)) + geom_boxplot();
ggplot(Dados.Analise, aes(x=Padrao, y=AreaUtil)) + geom_boxplot();

ggplot(Dados.Analise, aes(x=Dormitorios)) + geom_histogram(col="black", fill="blue", alpha=0.6, binwidth=1);
ggplot(Dados.Analise, aes(x=AnoMes, y=Dormitorios)) + geom_boxplot();
ggplot(Dados.Analise, aes(x=Ano, y=Dormitorios)) + geom_boxplot();
ggplot(Dados.Analise, aes(x=Padrao, y=Dormitorios)) + geom_boxplot();

ggplot(Dados.Analise, aes(x=Suites)) + geom_histogram(col="black", fill="blue", alpha=0.6, binwidth=1);
ggplot(Dados.Analise, aes(x=AnoMes, y=Suites)) + geom_boxplot();
ggplot(Dados.Analise, aes(x=Ano, y=Suites)) + geom_boxplot();
ggplot(Dados.Analise, aes(x=Padrao, y=Suites)) + geom_boxplot();

ggplot(Dados.Analise, aes(x=Banheiros)) + geom_histogram(col="black", fill="blue", alpha=0.6, binwidth=1);
ggplot(Dados.Analise, aes(x=AnoMes, y=Banheiros)) + geom_boxplot();
ggplot(Dados.Analise, aes(x=Ano, y=Banheiros)) + geom_boxplot();
ggplot(Dados.Analise, aes(x=Padrao, y=Banheiros)) + geom_boxplot();

ggplot(Dados.Analise, aes(x=Vagas)) + geom_histogram(col="black", fill="blue", alpha=0.6, binwidth=1);
ggplot(Dados.Analise, aes(x=AnoMes, y=Vagas)) + geom_boxplot();
ggplot(Dados.Analise, aes(x=Ano, y=Vagas)) + geom_boxplot();
ggplot(Dados.Analise, aes(x=Padrao, y=Vagas)) + geom_boxplot();

# Correlações
ggpairs(Dados.Analise[,4:11]);
