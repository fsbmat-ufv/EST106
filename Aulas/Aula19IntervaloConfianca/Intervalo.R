# Pacotes
library(ggplot2)

# Configurações da população
set.seed(123) # Reprodutibilidade
media_pop <- 50
desvio_pop <- 10

# Parâmetros da simulação
n_amostra <- 30        # tamanho de cada amostra
n_simulacoes <- 5000   # número de amostras simuladas

# Simulação das médias amostrais
medias_amostrais <- replicate(n_simulacoes, {
        mean(rnorm(n_amostra, mean = media_pop, sd = desvio_pop))
})

# Estatísticas
media_das_medias <- mean(medias_amostrais)
IC <- quantile(medias_amostrais, c(0.025, 0.975))  # intervalo de confiança empírico

# Criação do gráfico
ggplot(data = data.frame(media = medias_amostrais), aes(x = media)) +
        geom_histogram(aes(y = ..density..), bins = 50, fill = "skyblue", color = "black") +
        geom_density(color = "blue", linewidth = 1) +
        geom_vline(xintercept = media_das_medias, color = "red", linewidth = 1.2, linetype = "solid") +
        geom_vline(xintercept = IC, color = "darkgreen", linewidth = 1.2, linetype = "dashed") +
        labs(
                title = "Distribuição Amostral da Média",
                subtitle = paste0("n = ", n_amostra, " | ", n_simulacoes, " amostras simuladas"),
                x = "Média amostral",
                y = "Densidade"
        ) +
        annotate("text", x = media_das_medias, y = 0.09, label = "Média das médias", color = "red", angle = 90, vjust = -0.5) +
        annotate("text", x = IC[1], y = 0.15, label = "Limite inferior do IC", color = "darkgreen", angle = 90, vjust = -0.5) +
        annotate("text", x = IC[2], y = 0.15, label = "Limite superior do IC", color = "darkgreen", angle = 90, vjust = -0.5) +
        theme_minimal()
