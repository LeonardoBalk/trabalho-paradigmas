
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

## 5. Laços
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
# Na prática
Então vamos lá, escolhi 10 exercícios para compararmos mais afundo as três linguagens, são eles:

**1**.  Criar uma função que conte os caracteres em uma string. 
**2.** Criar uma função que conte as vogais em uma string.  
**3.** Criar uma função que remova todas as ocorrências de um caractere em uma string.  
**4.** Criar uma função que conte quantas strings, em uma lista, não terminam com a letra 's'.  
**5.** Criar uma função que calcule a soma de uma lista de números inteiros.  
**6.** Criar uma função que calcule a média de uma lista de números.  
**7.** Criar uma função que inverta uma lista de inteiros.  
**8.** Criar uma função que calcule a área de círculos a partir de uma lista de raios.  
**9.** Criar uma função que inverta uma string.  
**10.** Criar uma função que faça uma calculadora simples.

# Sugestões de resoluções em Haskell, Clojure e C
Irei apresentar sugestões para resolver os exercícios propostos, lembrando que existem diversas formas de implementar alguns exercícios nessas determinadas linguagens.


### 1. Função para contar Caracteres em uma String

**C**
 ```c
void contarCaracteres() {
    char str[100];
    printf("Digite uma string: ");
    scanf("%s", str);
    printf("Número de caracteres: %lu\n", strlen(str));
}
```

**Clojure**

 ```clojure
(defn contarCaracteres [str]
  (count str))
  ```

**Haskell**

 ```haskell
contarCaracteres :: String -> Int
contarCaracteres str = length str
 ```

-   **Diferenças:**
    -   Em C, usamos `strlen`(função da biblioteca <string.h> que retorna o tamanho da string) para calcular o comprimento da string, que deve ser uma string terminada em nulo.
    -   Em Clojure, a função `count` retorna a contagem diretamente, pois as strings são tratadas como sequências.
    -   Haskell utiliza `length`, que também opera em listas, já que strings são listas de caracteres.

### 2. Função para contar Vogais em uma String

**C**
```c
void contarVogais() {
    char str[100];
    int quantidade = 0;
    printf("Digite uma string: ");
    scanf("%s", str);
    for (int i = 0; str[i] != '\0'; i++) {
        char c = str[i];
        if (c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u' || 
            c == 'A' || c == 'E' || c == 'I' || c == 'O' || c == 'U') {
            quantidade++;
        }
    }
    printf("Número de vogais: %d\n", quantidade);
}
```

**Clojure**

```clojure
(defn contarVogais [str]
  (count (filter #{'a 'e 'i 'o 'u 'A 'E 'I 'O 'U} str))) 
```
**Haskell**

```haskell
contarVogais :: String -> Int
contarVogais str = length (filter (`elem` "aeiouAEIOU") str)
```

-   **Diferenças:**
    -   O C utiliza um loop para contar as vogais, verificando cada caractere.
    -   Clojure aplica `filter` e um conjunto literal para contar as vogais.
    -   Haskell faz uso de `filter` e `elem`, funções já mencionadas da programação funcional.

### 3. Remover Ocorrências de um Caractere

**C**

```c
void removerCaractere() {
    char str[100], c;
    printf("Digite uma string: ");
    scanf("%s", str);
    printf("Digite o caractere a ser removido: ");
    scanf(" %c", &c);
    char resultado[100];
    int j = 0;
    for (int i = 0; str[i] != '\0'; i++) {
        if (str[i] != c) {
            resultado[j++] = str[i];
        }
    }
    resultado[j] = '\0';
    printf("String sem o caractere '%c': %s\n", c, resultado);
}
```

**Clojure**

```clojure
(defn removerOcorrencias [char str]
  (remove #{char} str)) 
```

**Haskell**

```haskell
removerOcorrencias :: Char -> String -> String
removerOcorrencias c str = filter (/= c) str
```
-   **Diferenças:**
    -   O C usa um loop e uma lógica manual para construir a nova string.
    -   Clojure utiliza `remove` para criar uma nova sequência sem o caractere indesejado.
    -   Haskell faz uso de `filter`, que é mais conciso e idiomático.

### 4. Contar Strings que Não Terminam com 's'

**C**

```c
`void contarStringsSemS() {
    int n, quantidade = 0;
    printf("Digite o número de strings: ");
    scanf("%d", &n);
    char strings[n][100];
    printf("Digite as strings:\n");
    for (int i = 0; i < n; i++) {
        scanf("%s", strings[i]);
        if (strings[i][strlen(strings[i]) - 1] != 's') {
            quantidade++;
        }
    }
    printf("Número de strings que não terminam com 's': %d\n", quantidade);
}
```
**Clojure**

```clojure
(defn contarSemS [xs]
  (count (remove #(= (last %) \s) xs)))
  ``` 

**Haskell**

```haskell
contarSemS :: [String] -> Int
contarSemS xs = length (filter (\s -> last s /= 's') xs)
```
-   **Diferenças:**
    -   C precisa lidar com arrays multidimensionais e o loop para capturar o número de strings.
    -   Clojure e Haskell têm funções embutidas para filtrar e contar.

### 5. Soma de uma Lista de Números

**C**
```c
void somaLista() {
    int arr[100], n, soma = 0;
    printf("Digite o número de elementos: ");
    scanf("%d", &n);
    printf("Digite os elementos:\n");
    for (int i = 0; i < n; i++) {
        scanf("%d", &arr[i]);
        soma += arr[i];
    }
    printf("Soma da lista: %d\n", soma);
}
```

**Clojure**

```clojure
(defn somaLista [xs]
  (apply + xs))
  ```

**Haskell**

```haskell
somaLista :: [Int] -> Int
somaLista xs = sum xs 
```

-   **Diferenças:**
    -   A soma em C requer um loop explícito.
    -   Clojure e Haskell oferecem funções nativas para a operação de soma.

### 6. Calcular Média de uma Lista de Números

**C**

```c
void calcularMediaLista() {
    int n;
    float soma = 0.0;
    printf("Digite o número de elementos: ");
    scanf("%d", &n);
    if (n <= 0) {
        printf("Número de elementos deve ser maior que zero.\n");
        return;
    }
    float arr[n];
    printf("Digite os elementos:\n");
    for (int i = 0; i < n; i++) {
        scanf("%f", &arr[i]);
        soma += arr[i];
    }
    printf("Média da lista: %.2f\n", soma / n);
}
```

**Clojure**
```clojure
(defn mediaLista [xs]
  (/ (apply + xs) (count xs)))
  ```

**Haskell**

```haskell
mediaLista :: [Float] -> Float
mediaLista xs = sum xs / fromIntegral (length xs)
```

-   **Diferenças:**
    -   O C exige checagem manual de divisão por zero.
    -   Clojure e Haskell têm formas mais idiomáticas de calcular a média, embora Haskell precise converter o tamanho da lista para um tipo compatível.

### 7. Inverter uma Lista de Inteiros

**C**
```c
void inverterLista() {
    int n;
    printf("Digite o número de elementos: ");
    scanf("%d", &n);
    int arr[n];
    printf("Digite os elementos:\n");
    for (int i = 0; i < n; i++) {
        scanf("%d", &arr[i]);
    }
    printf("Lista invertida: ");
    for (int i = n - 1; i >= 0; i--) {
        printf("%d ", arr[i]);
    }
    printf("\n");
}
```

**Clojure**
```clojure
`(defn inverterLista [xs]
  (reverse xs))
  ```

**Haskell**
```haskell
inverterLista :: [Int] -> [Int]
inverterLista xs = reverse xs
```

-   **Diferenças:**
    -   O C usa um loop manual para imprimir os elementos na ordem inversa.
    -   Clojure e Haskell utilizam `reverse`, que é uma função embutida.

### 8. Calcular Áreas de Círculos

**C**
```c
void calcAreas() {
    int n;
    printf("Digite o número de raios que serão calculados: ");
    scanf("%d", &n);
    float raios[n], areas[n];
    printf("Digite os raios:\n");
    for (int i = 0; i < n; i++) {
        scanf("%f", &raios[i]);
        areas[i] = M_PI * raios[i] * raios[i]; // Cálculo da área
    }
    printf("Áreas dos círculos:\n");
    for (int i = 0; i < n; i++) {
        printf("%.2f\n", areas[i]);
    }
}
```

**Clojure:**
```clojure
`(defn calcAreas [raios]
  (map #(Math/PI (* % %)) raios))
  ```

**Haskell:**
```haskell
calcAreas :: [Float] -> [Float]
calcAreas raios = map (\r -> pi * r * r) raios
```

-   **Diferenças:**
    -   O C requer a definição de um array para armazenar os resultados.
    -   Clojure e Haskell utilizam `map` para aplicar a função de área a cada raio de forma mais concisa.


### 9. Função que Inverte uma String

#### C
```c
void inverterString() {
    char str[100];
    printf("Digite uma string: ");
    scanf("%s", str);

    int n = strlen(str);
    for (int i = 0; i < n / 2; i++) {
        char temp = str[i];
        str[i] = str[n - i - 1];
        str[n - i - 1] = temp;
    }

    printf("String invertida: %s\n", str);
}
```



#### Haskell

```haskell
inverterString :: String -> String
inverterString str = reverse str
```

#### Clojure

```clojure
(defn inverterString [str]
  (reverse str))` 
  ```

-   **Diferenças:**
    -   O C usa um loop manual para imprimir os elementos na ordem inversa.
    -   Clojure e Haskell utilizam `reverse`, que é uma função built-in.

### 10. Função que Faz uma Calculadora Simples

#### C


```c
clojurevoid calculadora() {
    int operacao;
    float num1, num2;

    printf("1. Somar\n2. Subtrair\n3. Multiplicar\n4. Dividir\nDigite a operação: ");
    scanf("%d", &operacao);
    printf("Digite dois números: ");
    scanf("%f %f", &num1, &num2);

    switch (operacao) {
        case 1:
            printf("Resultado: %.2f\n", num1 + num2);
            break;
        case 2:
            printf("Resultado: %.2f\n", num1 - num2);
            break;
        case 3:
            printf("Resultado: %.2f\n", num1 * num2);
            break;
        case 4:
            if (num2 != 0) {
                printf("Resultado: %.2f\n", num1 / num2);
            } else {
                printf("Não pode dividir por zero\n");
            }
            break;
        default:
            printf("Operação não existe\n");
            break;
    }
}
```
#### Haskell
```haskell
calculadora :: Float -> Float -> String -> Float
calculadora x y op
  | op == "somar"       = x + y
  | op == "subtrair"    = x - y
  | op == "multiplicar"  = x * y
  | op == "dividir"     = if y /= 0 then x / y else error "Não pode dividir por zero"
  | otherwise           = error "Operação inválida"
  ```

 
#### Clojure

```clojure
(defn calculadora [x y op]
  (cond
    (= op "somar") (+ x y)
    (= op "subtrair") (- x y)
    (= op "multiplicar") (* x y)
    (= op "dividir") (if (not= y 0) (/ x y) (throw (Exception. "Não pode dividir por zero")))
    :else (throw (Exception. "Operação inválida"))))
    ```
    
