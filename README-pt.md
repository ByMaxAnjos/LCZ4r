# Ferramentas para Análise de Zonas Climáticas Locais e Ilhas de Calor Urbanas em R

<img align="right" src="https://github.com/ByMaxAnjos/LCZ4r/blob/main/inst/figures/logo.png?raw=true" alt="Logotipo LCZ4r" width="140">
  
<!-- badges: start -->
[![Ciclo de vida: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

O [**pacote LCZ4r**](https://github.com/ByMaxAnjos/LCZ4r) oferece um conjunto abrangente de ferramentas para analisar e visualizar **Zonas Climáticas Locais (LCZ)** e **Ilhas de Calor Urbanas (UHI)** no R. Projetado para pesquisadores, planejadores urbanos e cientistas do clima, o LCZ4r simplifica o processo de download, processamento e interpretação de dados de LCZ.

---

# 1. Instalação

O pacote `LCZ4r` está disponível no GitHub e pode ser instalado de duas maneiras.  
Recomendamos a **Opção 1** para a maioria dos usuários.

## Opção 1: Instalar a partir do GitHub (Recomendado)

Esta é a maneira mais rápida de obter a versão mais recente do pacote.

::: callout-tip
**Pré-requisito:** Se você já possui os pacotes `remotes` ou `devtools` instalados, pode pular esta etapa.
:::

```r
if (!require("remotes")) { install.packages("remotes")}
# Instalar ou atualizar diretamente do GitHub
remotes::install_github("ByMaxAnjos/LCZ4r", upgrade = "never")
```

::: callout-warning
Atualizações: O `LCZ4r` está em desenvolvimento ativo. Para atualizar, basta executar novamente o comando acima.
O R substituirá automaticamente a versão anterior. Após a atualização, é recomendado reiniciar a sessão do R
(Sessão > Reiniciar R).
:::

## Opção 2: Instalar a partir de Arquivo Local (.zip)

Esta opção é recomendada para: conexões de internet instáveis, redes restritas (ex.: ambientes institucionais) e sessões de treinamento com múltiplos usuários.

Passos:

1. **Baixar o pacote**
👉 https://github.com/ByMaxAnjos/LCZ4r/archive/refs/heads/main.zip

2. **Extrair o arquivo**
👉 Após o download, extraia o arquivo .zip para uma pasta local (ex.: Downloads ou Área de Trabalho)

3. **Instalar localmente**

```r
# IMPORTANTE: Ajuste o caminho para onde você extraiu a pasta
remotes::install_local(
  "C:/Caminho/para/sua/pasta/LCZ4r-main", 
  upgrade = "never"
)
```

::: callout-tip
Exemplo: Se você baixou e extraiu o arquivo para a pasta Downloads:

```r
remotes::install_local(
   "/Users/maxanjos/Downloads/LCZ4r-main",
  upgrade = "never"
)
```
:::

## Carregando o pacote

Após a instalação, carregue o pacote sempre que iniciar uma nova sessão do R:

```r
library(LCZ4r)
```

---

## [Execute o LCZ4r no Posit Cloud, sem necessidade de instalar o RStudio!](https://bymaxanjos.github.io/LCZ4r/en/articles/posit_cloud.html)

<a href="https://posit.cloud/content/9921467" target="_blank">
  <p style="text-align:center;">
    <img src="https://github.com/ByMaxAnjos/LCZ4r/raw/main/inst/figures/posit_cloud.png" 
         alt="Posit Cloud" 
         width="60%" 
         style="border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2); transition: transform 0.3s ease;">
  </p>
  <p style="text-align:center;">
    (Clique na imagem para explorar o LCZ4r no Posit Cloud.)
  </p>
</a>

---

## Inspiração

O pacote **LCZ4r** é inspirado nos seguintes trabalhos:

- **Stewart, I., and T. Oke, 2012**: [Local Climate Zones for Urban Temperature Studies](https://doi.org/10.1175/BAMS-D-11-00019.1).

- **Ching, J., et al., 2018**: [WUDAPT: An Urban Weather, Climate, and Environmental Modeling Infrastructure for the Anthropocene](https://doi.org/10.1175/BAMS-D-16-0236.1).
- **Demuzere, M., et al., 2019**: [Mapping Europe into Local Climate Zones](https://doi.org/10.1371/journal.pone.0214474).
- **Demuzere, M., et al., 2020**: [Combining Expert and Crowd-Sourced Training Data to Map Urban Form and Functions for the Continental US](https://doi.org/10.1038/s41597-020-00605-z).
- **Demuzere, M., et al., 2022**: [A Global Map of Local Climate Zones to Support Earth System Modelling and Urban-Scale Environmental Science](https://doi.org/10.5194/essd-14-3835-2022).

---

## Tem Comentários ou Sugestões?

Valorizamos sua contribuição! Se você tem ideias para melhorias ou encontrou algum problema, por favor, informe-nos abrindo uma issue no GitHub.

<button type="button" class="btn" style="background-color: #008000; color: white; padding: .5rem 1rem; font-size: 1rem; border: none; border-radius: .25rem;">
  <a href='https://github.com/ByMaxAnjos/LCZ4r/issues/new' style="text-decoration: none; color: white;">
    Abrir uma Issue no GitHub
  </a>
</button>
```

