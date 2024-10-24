# Comparação entre Linguagens de Programação

# Introdução

A proposta da minha produção personalizada foi fazer uma comparação entre as linguagens funcionais **Haskell** e **Clojure**, juntamente da linguagem imperativa que mais trabalhamos até o momento, a **linguagem C**. Nesse arquivo, proponho dez diferentes exercícios para serem realizados e comparados entre as três linguagens. Dentre eles, temos exercícios trabalhados em aula, exercícios gerados e exercícios clássicos da programação.

![linguagens](https://github.com/user-attachments/assets/a1c364a3-2096-485a-9477-d67e5d02725c)

# Contextualizando

Nessa parte do arquivo, irei abordar um resumo do que será usado de cada linguagem para resolver as questões.

# Bibliotecas

## C
Usadas no código:
- **`#include <stdio.h>`**: Nos dá as funções para entrada e saída, como `printf` e `scanf`.  
- **`#include <string.h>`**: Funções para manipular strings, como `strlen`, que conta o número de caracteres em uma string.  
- **`#include <math.h>`**: Fornece funções matemáticas.  

### Outras bibliotecas famosas:
- **`GLib`**: Uma biblioteca de utilidades para C que fornece estruturas de dados, manipulação de strings e funções de utilidade, facilitando o desenvolvimento de aplicativos complexos.
- **`libcurl`**: Uma biblioteca para transferências de dados com suporte a diversos protocolos, como HTTP, FTP e SMTP, permitindo a comunicação de rede de forma simples e eficiente.
- **`OpenSSL`**: Uma biblioteca que fornece ferramentas de criptografia e protocolos de segurança, como SSL e TLS, essencial para a criação de aplicações seguras que exigem proteção de dados.
- **`SDL (Simple DirectMedia Layer)`**: Uma biblioteca que facilita a criação de aplicações multimídia, como jogos, proporcionando acesso a gráficos, som e entrada de dispositivos de forma simples e eficiente.

## Haskell
- **`Hakyll`**: Um framework de geração de sites estáticos que permite a construção de sites de maneira declarativa, utilizando o sistema de templates do Haskell para criar páginas dinâmicas e personalizadas.
- **`Yesod`**: Um framework web poderoso que facilita a criação de aplicações web seguras e escaláveis, utilizando a tipagem forte do Haskell para garantir a segurança em tempo de compilação e a geração automática de rotas.
- **`Servant`**: Uma biblioteca para construir APIs RESTful de forma declarativa, permitindo a definição de tipos de API e gerando automaticamente o código necessário para manipulá-las, garantindo a compatibilidade entre cliente e servidor.
- **`HUnit`**: Uma biblioteca de testes unitários para Haskell, que fornece uma estrutura para a criação de testes e verificação de resultados, ajudando a garantir a qualidade do código e a detecção precoce de erros.

No entanto, para resolver as questões, será usada apenas a biblioteca padrão.

## Clojure
- **`Onyx`**: Sintaxe declarativa e um editor gráfico para definir pipelines de dados, suportando processamento de streaming e em lote, tolerância a falhas e gerenciamento de estado.
- **`Kafka`**: Usado para fornecimento de eventos, processamento de fluxo, integração de dados e análise.
- **`ZooKeeper`**: Serviço de coordenação distribuída com armazenamento hierárquico de chave-valor, útil para descoberta de serviços, gerenciamento de configuração e sincronização.
- **`Datomic`**: Armazena dados como tuplas imutáveis com suporte para transações ACID, evolução de esquema e consultas de log de dados.

No entanto, para resolver as questões também será usada somente a biblioteca padrão.

