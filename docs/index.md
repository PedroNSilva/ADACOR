--- 
lang: pt-BR
title: Análise de Dados Amostrais com R
author: |
  | Djalma Galvão Carneiro Pessoa e Pedro Luis do Nascimento Silva 
  | 
  | 
  | 
  | Editores
  | Pedro Luis do N. Silva, Antonio José R. Dias, Zélia M. Bianchini e Sonia Albieri   
  
date: '23 de fevereiro de 2022, 15:42:10'
summary_depth: 3
summary_float:
   collapsed: false
site: bookdown::bookdown_site

header-includes:
    - \usepackage{float}
      \usepackage{amsmath}
      \usepackage{bm}

geometry: "left=1.5cm,right=1cm,top=1.5cm,bottom=1.5cm"
fontsize: 12pt

output:
  bookdown::gitbook: default
  bookdown::pdf_book: default  

documentclass: book          

bibliography: [book.bib, packages.bib]
#biblio-style: apalike

csl: Modified-apa.csl
suppress-bibliography: false
#link-citations: yes
github-repo: PedroNSilva/ADACOR
cover-image: "Figuras/capa.png"
description: "Análise de dados de pesquisas amostrais."
---

# Bem-vindo {-}
<div style="text-align: justify">
Uma preocupação básica de toda instituição produtora de informações estatísticas é com a utilização "correta" de seus dados. Isso pode ser interpretado de várias formas, algumas delas com reflexos até na confiança do público e na
própria sobrevivência do órgão. Do nosso ponto de vista, enfatizamos um aspecto
técnico particular, mas nem por isso menos importante para os usuários dos dados.

A revolução da informática, com a resultante facilidade de acesso ao computador, criou condições extremamente favoráveis à utilização de dados estatísticos produzidos por órgãos como o IBGE. Algumas vezes esses dados são utilizados para fins puramente descritivos. Outras vezes, porém, sua utilização é feita para fins analíticos, envolvendo a construção de modelos, quando o objetivo é extrair conclusões aplicáveis também a populações distintas daquela da qual se extraiu a amostra. Neste caso, é comum empregar, sem grandes preocupações, pacotes
computacionais padrões disponíveis para a seleção e ajuste de modelos. É neste ponto que entra a nossa preocupação com o uso adequado dos dados produzidos pelo IBGE.

O que torna tais dados especiais para quem pretende usá-los para fins
analíticos? Esta é a questão básica que é amplamente discutida ao longo deste texto. A mensagem principal que pretendemos transmitir é que certos cuidados precisam ser tomados para utilização correta dos dados de pesquisas amostrais como as que o IBGE realiza.

O que torna especiais dados como os produzidos pelo IBGE é que estes são obtidos através de pesquisas amostrais complexas de populações finitas que envolvem: **probabilidades distintas de seleção, estratificação e conglomeração das unidades, ajustes para compensar não resposta e outros ajustes**. Os sistemas
tradicionais de análise ignoram estes aspectos, podendo produzir estimativas incorretas tanto dos parâmetros como para as variâncias destas estimativas. Quando utilizamos a amostra para estudos analíticos, as opções disponíveis nos pacotes estatísticos usuais para levar em conta os pesos distintos das observações são apropriadas somente para observações independentes e identicamente distribuídas - IID. Além disso, a variabilidade dos pesos produz
impactos tanto na estimação pontual quanto na estimação das variâncias dessas estimativas, que sofre ainda influência da estratificação e conglomeração.

O objetivo deste livro é analisar o impacto das simplificações feitas ao utilizar procedimentos e pacotes usuais de análise de dados e apresentar os ajustes necessários desses procedimentos de modo a incorporar na análise, de forma apropriada, os aspectos aqui ressaltados. Para isto são apresentados exemplos de análises de dados obtidos em pesquisas amostrais complexas, usando pacotes clássicos e também pacotes estatísticos especializados. A comparação
dos resultados das análises feitas das duas formas permite avaliar o impacto de ignorar o plano amostral na análise dos dados resultantes de pesquisas amostrais complexas.

## Agradecimentos {-}

A elaboração de um texto como esse não se faz sem a colaboração de muitas pessoas. Em primeiro lugar, agradecemos à Comissão Organizadora do SINAPE por ter propiciado a oportunidade ao selecionar nossa proposta de minicurso. Agradecemos também ao IBGE por ter proporcionado as condições e os meios usados para a produção da monografia, bem como o acesso aos dados detalhados e identificados que utilizamos em vários exemplos.

No plano pessoal, agradecemos a Zélia Bianchini pela revisão do manuscrito e sugestões que o aprimoraram. Agradecemos a Marcos Paulo de Freitas e Renata Duarte pela ajuda com a computação de vários exemplos. Agradecemos a Waldecir Bianchini, Luiz Pessoa e Marinho Persiano pela colaboração na utilização do processador de textos. Aos demais colegas do Departamento de Metodologia do IBGE, agradecemos o companheirismo e solidariedade nesses meses de trabalho na preparação do manuscrito.

Finalmente, agradecemos a nossas famílias pela aceitação resignada de nossas ausências e pelo incentivo à conclusão da empreitada.
