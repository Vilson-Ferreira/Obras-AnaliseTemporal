library(gmodels);

###############################################################
# Clusters (Alternativa para Tipos) - Hierarquico
###############################################################

#Padronização
Dados.Numericas = Dados.Analise[6:11];
#Dados.Numericas$Unidades = scale(Dados.Numericas$Unidades);
#Dados.Numericas$AreaUtilTotal = scale(Dados.Numericas$AreaUtilTotal);
Dados.Numericas$UnidadesPorAndar = scale(Dados.Numericas$UnidadesPorAndar);
Dados.Numericas$AreaUtil = scale(Dados.Numericas$AreaUtil);
Dados.Numericas$Dormitorios = scale(Dados.Numericas$Dormitorios);
Dados.Numericas$Suites = scale(Dados.Numericas$Suites);
Dados.Numericas$Banheiros = scale(Dados.Numericas$Banheiros);
Dados.Numericas$Vagas = scale(Dados.Numericas$Vagas);
View(Dados.Numericas);

# Cluster
Cluster.Distancias = dist(Dados.Numericas, method = "euclidean");
Cluster.Hierarquia = hclust(Cluster.Distancias, method="ward.D2");

# Dendograma
plot(Cluster.Hierarquia);
rect.hclust(Cluster.Hierarquia, k=5, border="red");

Dados.Analise$GruposH5 = factor(cutree(Cluster.Hierarquia, k=5));

# Análise dos Grupos
aggregate(Dados.Analise[6:11], list(Dados.Analise$GruposH5), mean);

ggplot(Dados.Analise, aes(x=AreaUtil, y=UnidadesPorAndar, col=GruposH5)) + geom_point() + geom_hline(yintercept=0) + geom_vline(xintercept=0);
ggplot(Dados.Analise, aes(x=AreaUtil, y=Dormitorios, col=GruposH5)) + geom_point() + geom_hline(yintercept=0) + geom_vline(xintercept=0);
ggplot(Dados.Analise, aes(x=AreaUtil, y=Banheiros, col=GruposH5)) + geom_point() + geom_hline(yintercept=0) + geom_vline(xintercept=0);
ggplot(Dados.Analise, aes(x=AreaUtil, y=Suites, col=GruposH5)) + geom_point() + geom_hline(yintercept=0) + geom_vline(xintercept=0);
ggplot(Dados.Analise, aes(x=AreaUtil, y=Vagas, col=GruposH5)) + geom_point() + geom_hline(yintercept=0) + geom_vline(xintercept=0);

ggplot(Dados.Analise, aes(x=GruposH5, y=AreaUtil)) + geom_boxplot();
ggplot(Dados.Analise, aes(x=GruposH5, y=UnidadesPorAndar)) + geom_boxplot();
ggplot(Dados.Analise, aes(x=GruposH5, y=Dormitorios)) + geom_boxplot();
ggplot(Dados.Analise, aes(x=GruposH5, y=Banheiros)) + geom_boxplot();
ggplot(Dados.Analise, aes(x=GruposH5, y=Suites)) + geom_boxplot();
ggplot(Dados.Analise, aes(x=GruposH5, y=Vagas)) + geom_boxplot();