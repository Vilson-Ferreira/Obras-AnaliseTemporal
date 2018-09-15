###############################################################
# Análise Temporal
###############################################################
library(TTR);
library(forecast);

Dados.Tempo = Dados.Analise;
# Filtro por Cluster
# Dados.Tempo = subset(Dados.Analise, Dados.Analise$GruposH5 == 5);

Dados.Tempo = aggregate(cbind(Dados.Tempo$Unidades, Dados.Tempo$AreaUtilTotal, Dados.Tempo$UnidadesPorAndar, Dados.Tempo$AreaUtil, Dados.Tempo$Dormitorios, Dados.Tempo$Suites, Dados.Tempo$Banheiros, Dados.Tempo$Vagas), list(Dados.Tempo$AnoMes), mean);
names(Dados.Tempo) = c("Lancamento", "Unidades", "AreaUtilTotal", "UnidadesPorAndar", "AreaUtil", "Dormitorios", "Suites", "Banheiros", "Vagas");
sort(Dados.Tempo$Lancamento, decreasing=FALSE);
View(Dados.Tempo);

Serie = ts(Dados.Tempo$AreaUtil, frequency=12, start=c(2008,7));
plot(Serie); grid(col='darkgrey', lwd=1);

# Análise de Estacionária
acf(Serie);
pacf(Serie);

# Decomposição
Serie.Decomposicao <- decompose(Serie);
plot(Serie.Decomposicao);

# Visão do Lag
# ts.plot(Serie, lag(Serie, 5), lag(Serie,-5), col=1:3, xlab='Periodo', main='Demonstrando Defasagens', lty=1:3, lwd=2);
# grid(col='darkgrey');
# legend('topleft', c(expression(Serie[t]), expression(Serie[t+5]), expression(Serie[t-5])), col=1:3, lty=1:3, bty='n', lwd=2);
# abline(v=2005, lty=2);

# Seleção do Arima
Serie.Auto = auto.arima(Serie);
summary(Serie.Auto);
plot(Serie); lines(Serie.Auto$fitted, col="red", lty=3);
plot(Dados.Tempo$Lancamento, scale(Serie.Auto$residuals)); grid(col='darkgrey', lwd=1);

# Predição
Serie.Pred = forecast(Serie.Auto, h=12);
plot(Serie.Pred); grid(col='darkgrey', lwd=1);

# Predição - Zoom
Serie.Pred = predict(Serie.Auto, n.ahead=12);
plot(Serie.Pred$pred); grid(col='darkgrey', lwd=1);