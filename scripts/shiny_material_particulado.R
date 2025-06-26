library(lubridate)
library(tidyverse)
library(ggplot2)
library(sf)
library(readxl)
library(gridExtra)
library(tmap)
library(BlandAltmanLeh)
library(ggthemes)
library(stringr)
library(irr)
library(rmapshaper)



tmap_mode("view")

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Material Particulado no Estado de Santa Catarina"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Análise Mensal", icon = icon("bar-chart"),
               menuSubItem("Gráfico de Linha", tabName = "subitem1"),
               menuSubItem("grafico de barras", tabName = "subitem2"),
               menuSubItem("grafico de dispersão", tabName = "subitem3"),
               menuSubItem("grafico de blandaltman", tabName = "subitem4"),
               menuSubItem("grafico Boxplot", tabName = "subitem4_5")),
      menuItem("Análise Anual", icon = icon("bar-chart"),
               menuSubItem("grafico de dispersão", tabName = "subitem5"),
               menuSubItem("grafico de blandaltman", tabName = "subitem6")),
      menuItem("Mapas", icon = icon("map"),
               menuSubItem("Mapa Copernicus", tabName = "subitem9"),
               menuSubItem("Mapa Donkelar", tabName = "subitem10")),
      menuItem("ICC", icon = icon("chart-line"), tabName = "iccinfo"),
      menuItem("Referências",icon = icon("book"),tabName = "referencias")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("subitem1", plotOutput("distPlot")),
      tabItem("subitem2", plotOutput("distPlot2")),
      tabItem("subitem3", plotOutput("distPlot3")),
      tabItem("subitem4", plotOutput("distPlot4")),
      tabItem("subitem4_5", plotOutput("distPlot4_5")),
      tabItem("subitem5", plotOutput("distPlot5")),
      tabItem("subitem6", plotOutput("distPlot6")),
      tabItem("subitem9", tmapOutput("distPlot9")),
      tabItem("subitem10", tmapOutput("distPlot10")),
      tabItem("iccinfo",
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE,
                    title = strong("Informações sobre o ICC (Coeficiente de Correlação Intraclasse)"),
                    p("O ICC expressa a proporção de variabilidade total que é devida à variabilidade entre as unidades."),
                    p("Neste caso de estar avaliando a concordância entre duas medidas, o ICC pode ser interpretado como uma medida de concordância que mede o grau de afastamento das duas medidas à reta de 45 graus, onde teria concordância perfeita, pois ambas medidas seriam iguais."),
                    br(),
                    p(strong("Classificação da concordância baseada em Weir (2005):")),
                    tags$ul(
                      tags$li(strong("0,00 ≤ ICC ≤ 0,20:"), " concordância pobre"),
                      tags$li(strong("0,20 < ICC ≤ 0,40:"), " concordância razoável"),
                      tags$li(strong("0,40 < ICC ≤ 0,60:"), " concordância boa"),
                      tags$li(strong("0,60 < ICC ≤ 0,80:"), " concordância muito boa"),
                      tags$li(strong("0,80 < ICC ≤ 1,00:"), " concordância excelente")
                    )
                )
              ),
              fluidRow(
                valueBoxOutput("icc_valor", width = 6),
                valueBoxOutput("icc_ic", width = 6)
              )
      ),
      tabItem("referencias",
              fluidPage(
                h3("Referências"),
                HTML('
            <p><strong><b>Weir, J. P. (2005). Quantifying test-retest reliability using the intraclass correlation coefficient and the SEM.</b></strong> <em>Journal of Strength and Conditioning Research</em>, 19(1), 231–240. Disponível em: 
            <a href="https://journals.lww.com/nsca-jscr/Fulltext/2005/02000/Quantifying_Test_Retest_Reliability_Using_the.32.aspx" target="_blank">
            https://journals.lww.com/nsca-jscr/Fulltext/2005/02000/Quantifying_Test_Retest_Reliability_Using_the.32.aspx</a></p>

            <p><strong><b>Copernicus Atmosphere Monitoring Service (CAMS).</b></strong> Disponível em: 
            <a href="https://atmosphere.copernicus.eu/charts/packages/cams/" target="_blank">
            https://atmosphere.copernicus.eu/charts/packages/cams/</a></p>

            <p><strong><b>SatPM2.5 (Satellite-derived PM2.5).</b></strong> Disponível em: 
            <a href="https://sites.wustl.edu/acag/datasets/surface-pm2-5/#V6.GL.02.03" target="_blank">
            https://sites.wustl.edu/acag/datasets/surface-pm2-5/#V6.GL.02.03</a></p>
            
            <p><strong>Artigo: Desempenho dos dados de material particulado fino sobre a qualidade do ar em estudo epidemiológico em Salvador, Brasil.</strong> Disponível em: <a href="https://pmc.ncbi.nlm.nih.gov/articles/PMC11654642/" target="_blank">https://pmc.ncbi.nlm.nih.gov/articles/PMC11654642/</a></p>
          ')
              )
      )
    )
  )
)

# Server
server <- function(input, output) {
  output$distPlot <- renderPlot({
    ggplot(base_u2, aes(x = mes)) +
      geom_line(aes(y = PM2.5, color = "Copernicus PM2.5"), linewidth = 1) +
      geom_line(aes(y = Media_PM25, color = "Donkelar PM2.5"), linewidth = 1) +
      scale_color_manual(
        values = c("Copernicus PM2.5" = "blue4", "Donkelar PM2.5" = "green4"),
        name = "Fonte"
      ) +
      scale_x_continuous(breaks = 1:12) +
      labs(
        x = "Mês",
        y = "Concentração de PM2.5 (µg/m³)",
        title = "Gráfico dependente do tempo"
      ) +
      theme_minimal()
  })
  
  output$distPlot2 <- renderPlot({
    ggplot(base_long, aes(x = factor(mes), y = valor, fill = tipo)) +
      geom_bar(stat = "summary", fun = mean, position = "dodge") +
      labs(
        title = "Médias mensais - PM2.5",
        x = "Mês",
        y = expression(PM[2.5] ~ (mu * g/m^3)),
        fill = ""
      ) +
      scale_fill_manual(
        values = c("PM2.5" = "blue4", "Media_PM25" = "green4"),
        labels = c("PM2.5" = "PM2.5-Copernicus", "Media_PM25" = "PM2.5-Donkelar")
      ) +
      theme_minimal()
  })
  
  output$distPlot3 <- renderPlot({
    ggplot(data = base_uf_u2, mapping = aes(x = PM2.5, y = Media_PM25)) +
      geom_point() +
      labs(x = "PM2.5 - Copernicus", y = "PM2.5 - Donkelar", title = "Gráfico de Dispersão por Mês") +
      theme_bw()
  })
  
  output$distPlot4 <- renderPlot({
    bland.altman.plot(scUF$PM2.5, scUF2$Media_PM25,
                      main = "Gráfico de Bland-Altman para PM2.5 por mês",
                      xlab = "Média dos valores de PM2.5",
                      ylab = "Diferença entre os valores de PM2.5")
  })
  
  output$distPlot4_5 <- renderPlot({
    ggplot(box, aes(x = factor(mes), y = Valor, fill = Variavel)) +
      geom_boxplot() +
      labs(x = "Mês", y = "Valor", title = "Boxplot de Valor por Mês e Variável", fill = "Fonte de dados") +
      scale_fill_manual(values = c("Copernicus" = "blue4", "Donkelar" = "green4")) +
      theme_minimal()
  })
  
  output$distPlot5 <- renderPlot({
    ggplot(data = base_ano_unida, mapping = aes(x = PM2.5, y = Media_PM25)) +
      geom_point() +
      labs(x = "PM2.5 - Copernicus", y = "PM2.5 - Donkelar", title = "Gráfico de Dispersão por Ano") +
      theme_bw()
  })
  
  output$distPlot6 <- renderPlot({
    bland.altman.plot(scano$PM2.5, scano2$Media_PM25,
                      main = "Gráfico de Bland-Altman para PM2.5",
                      xlab = "Média dos valores de PM2.5",
                      ylab = "Diferença entre os valores de PM2.5")
  })
  
  output$distPlot9 <- renderTmap({
    tm_shape(base_map_uf) +
      tm_fill(col = "PM2.5",
              palette = "PuBuGn",
              id = "nome_casos",
              breaks = c(6, 10, 15, 20),
              colorNA = "transparent",
              textNA = "") +
      tm_borders() +
      tm_layout(panel.labels = "Base Copernicus")
  })
  
  output$distPlot10 <- renderTmap({
    tm_shape(base_map_uf2) +
      tm_fill(col = "Media_PM25",
              palette = "PuBuGn",
              id = "nome_casos",
              breaks = c(6, 10, 15, 20),
              colorNA = "transparent",
              textNA = "") +
      tm_borders() +
      tm_layout(panel.labels = "Base Donkelar")
  })
  
  output$icc_valor <- renderValueBox({
    valueBox(value = paste0("ICC:",ValorICC_UF," (", classificacao_icc(ValorICC_UF)," )"),
             subtitle = "Valor do ICC",
             icon = icon("ruler-combined"),
             color = "olive")
  })
  
  output$icc_ic <- renderValueBox({
    valueBox(value = paste0("IC95%: [", IC95_ICC_UF[1], " - ", IC95_ICC_UF[2],"]"),
             subtitle = "Intervalo de Confiança",
             icon = icon("chart-area"),
             color = "teal")
  })
}

shinyApp(ui, server)