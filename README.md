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

# Sintaxe

## 1. Declaração de Funções
**C**
```c
int soma(int a, int b) {
    return a + b;
}
```
**Haskell**
```haskell
soma :: Int -> Int -> Int
soma a b = a + b
```
**Clojure**
```clojure
(defn soma [a b]
  (+ a b))
```
Em C, as funções devem ter tipos definidos, enquanto Haskell e Clojure têm uma tipagem mais flexível, permitindo inferência de tipos.

## 2. Entrada e Saída de Dados
**C**
```c
#include <stdio.h>

int main() {
    int num;
    printf("Digite um número: ");
    scanf("%d", &num);
    printf("O dobro é: %d\n", num * 2);
    return 0;
}
```
**Haskell**
```haskell
main :: IO ()
main = do
  putStrLn "Digite um número:"
  num <- readLn
  putStrLn ("O dobro é: " ++ show (num * 2))
```
**Clojure**
```clojure
(defn main []
  (println "Digite um número:")
  (let [num (Integer/parseInt (read-line))]
    (println "O dobro é:" (* num 2))))
```
C utiliza funções específicas como `printf` e `scanf` para entrada e saída, enquanto Haskell e Clojure usam uma abordagem mais declarativa.

## 3. Manipulação de Listas
**C**
```c
#include <stdio.h>

int main() {
    int arr[] = {1, 2, 3, 4, 5};
    int soma = 0;
    for (int i = 0; i < 5; i++) {
        soma += arr[i];
    }
    printf("Soma: %d\n", soma);
    return 0;
}
```
**Haskell**
```haskell
main :: IO ()
main = do
  let arr = [1, 2, 3, 4, 5]
  putStrLn ("Soma: " ++ show (sum arr))
```
**Clojure**: 
```clojure
(defn soma-lista [arr]
  (reduce + arr))

(defn main []
  (println "Soma:" (soma-lista [1 2 3 4 5])))
```
C usa um loop explícito para somar elementos de um array, enquanto Haskell e Clojure oferecem funções de alto nível para manipulação de listas.

## 4. Condicionais
**C**
```c
#include <stdio.h>

int main() {
    int num;
    printf("Digite um número: ");
    scanf("%d", &num);
    if (num % 2 == 0) {
        printf("%d é par\n", num);
    } else {
        printf("%d é ímpar\n", num);
    }
    return 0;
}
```
**Haskell**
```haskell
main :: IO ()
main = do
  putStrLn "Digite um número:"
  num <- readLn
  if even num
    then putStrLn (show num ++ " é par")
    else putStrLn (show num ++ " é ímpar")
```
**Clojure**
```clojure
(defn par-ou-impar [num]
  (if (even? num)
    (println num "é par")
    (println num "é ímpar")))

(defn main []
  (println "Digite um número:")
  (let [num (Integer/parseInt (read-line))]
    (par-ou-impar num)))
```
A estrutura condicional é semelhante, mas Haskell e Clojure tendem a ser mais concisos, utilizando funções como `even?` para simplificar as condições.

## Laços
**C**
   ```c
   for (int i = 0; i < n; i++) {
       // Código a ser repetido
   }
   ```

2. **`while`**: Usado quando o número de iterações não é conhecido, mas uma condição deve ser verdadeira.
   ```c
   while (condicao) {
       // Código a ser repetido
   }
   ```

3. **`do while`**: Semelhante ao `while`, mas garante que o bloco de código seja executado pelo menos uma vez.
   ```c
   do {
       // Código a ser repetido
   } while (condicao);
   ```

**Haskell**
Haskell não possui estruturas de laço imperativas, pois é uma linguagem funcional. Em vez disso, pode-se usar recursão ou funções de ordem superior:

1. **Recursão**: Uma função pode chamar a si mesma.
   ```haskell
   fatorial 0 = 1
   fatorial n = n * fatorial (n - 1)
   ```

2. **`map`**: Aplica uma função a cada elemento de uma lista.
   ```haskell
   resultado = map (+1) [1, 2, 3]  -- [2, 3, 4]
   ```

3. **`fold`**: Reduz uma lista a um único valor usando uma função acumuladora.
   ```haskell
   soma = foldr (+) 0 [1, 2, 3]  -- 6
   ```

**Clojure**
Clojure utiliza uma abordagem funcional, com os seguintes exemplos:

1. **`loop/recur`**: Um laço que permite a iteração com recursão de cauda.
   ```clojure
   (defn fatorial [n]
     (loop [acc 1, i n]
       (if (zero? i)
         acc
         (recur (* acc i) (dec i)))))
   ```

2. **`map`**: Aplica uma função a cada elemento de uma coleção.
   ```clojure
   (map inc [1 2 3])  ;; (2 3 4)
   ```

3. **`reduce`**: Reduz uma coleção a um único valor usando uma função acumuladora.
   ```clojure
   (reduce + [1 2 3])  ;; 6
   ```

   Isso é, por enquanto o suficiente pra entender um pouco de cada linguagem, os termos novos ou que não estamos acostumados serão destacados nas questões.

   
