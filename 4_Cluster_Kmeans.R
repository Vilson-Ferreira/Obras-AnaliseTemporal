###############################################################
# Clusters (Alternativa para Tipos) - Kmeans
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
Cluster.Kmeans = kmeans(Dados.Numericas, 5);
Dados.Analise$GruposK5 = factor(Cluster.Kmeans$cluster);

# Análises
aggregate(Dados.Analise[6:11], list(Dados.Analise$GruposK5), mean);

ggplot(Dados.Analise, aes(x=AreaUtil, y=UnidadesPorAndar, col=GruposK5)) + geom_point() + geom_hline(yintercept=0) + geom_vline(xintercept=0);
ggplot(Dados.Analise, aes(x=AreaUtil, y=Dormitorios, col=GruposK5)) + geom_point() + geom_hline(yintercept=0) + geom_vline(xintercept=0);
ggplot(Dados.Analise, aes(x=AreaUtil, y=Banheiros, col=GruposK5)) + geom_point() + geom_hline(yintercept=0) + geom_vline(xintercept=0);
ggplot(Dados.Analise, aes(x=AreaUtil, y=Suites, col=GruposK5)) + geom_point() + geom_hline(yintercept=0) + geom_vline(xintercept=0);
ggplot(Dados.Analise, aes(x=AreaUtil, y=Vagas, col=GruposK5)) + geom_point() + geom_hline(yintercept=0) + geom_vline(xintercept=0);

ggplot(Dados.Analise, aes(x=GruposK5, y=AreaUtil)) + geom_boxplot();
ggplot(Dados.Analise, aes(x=GruposK5, y=UnidadesPorAndar)) + geom_boxplot();
ggplot(Dados.Analise, aes(x=GruposK5, y=Dormitorios)) + geom_boxplot();
ggplot(Dados.Analise, aes(x=GruposK5, y=Banheiros)) + geom_boxplot();
ggplot(Dados.Analise, aes(x=GruposK5, y=Suites)) + geom_boxplot();
ggplot(Dados.Analise, aes(x=GruposK5, y=Vagas)) + geom_boxplot();

# Comparação com Hierarquico
CrossTable(Dados.Analise$GruposH5, Dados.Analise$GruposK5);
