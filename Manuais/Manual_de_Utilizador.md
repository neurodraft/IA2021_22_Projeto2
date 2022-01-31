 # Manual de Utilizador 
<br>




### <p style="text-align: center;"> Inteligencia Artificial 2020/21</p>




  ## <p style="text-align: center;"> BLOKUS </p>

<br>


                                                                                                                           

<div style="text-align:center"><img src="Blokus.png" height="220" width="500"/></div>

<br>

## <p style="text-align: center;"> PROJETO 2

<p style="text-align: center;"> <b>Docente:</b> Joaquim Filipe</p>


 ### <p style="text-align: center;"> <b> Realizado por :</b> </p>
 <p style="text-align: center;">Bernardo Mota nº201900947
 <br>
Frederico Alcaria nº201701440 </p>

<div style="page-break-after: always;"></div>

## Acrónimos

* **BFS:** Algoritmo de busca em largura
* **DFS:** Algoritmo de busca em profundidade
* **A***: Algoritmo de procura informada
* **IDE***: integrated deveopment environment (ambiente de desenvolvimento integrado)

<br>

# Introdução

Este manual visa a ser um guia compreensivo para a correta utilização do programa desenvolvido, utilizando a linguagem de programação funcional LISP. O objetivo deste programa é indicar quais os passos necessários para chegar ao objetivo dos problemas dados. Esta versão do Blokus usa um tabuleiro de 14 por 14, mas os problemas alteram as condições de jogo dos tabuleiros e definem os seus objetivos.

<br>

# Instalação e utilização

Para puder executar o programa é necessário o IDE [LispWorks](http://www.lispworks.com/) ou outro que consiga interpretar a linguagem LISP.

## Abrir e Compilar os ficheiros

No LispWorks vai ser preciso compilar o ficheiro project.lisp. Ir File>Compile and Load e escolher o ficheiro project.lisp. Os restantes ficheiros vão ser compilados ao iniciar o programa, irá ser pedido o path de onde se encontram os ficheiros do projeto necessários (procura.lisp , puzzle.lisp , problemas.dat). 

## Executar o Programa

Para executar o programa é necessário abrir um listener e chamar a função iniciar escrevendo (iniciar)

## Navegar no Programa

Para navegar no programa é necessário escrever na consola o nº respetivo à opção que deseja escolher.

<div style="page-break-after: always;"></div>

# Input/Output

**Tipos de input:**

<ul>
  <li>Consola: A interação com o programa é através do listener. O programa lê o input e corre a opção associada ao nº introduzido.</li>
</ul>

**Tipos de output:**

<ul>
  <li>Ficheiros: o programa vai gerar um ficheiro com o nome log.dat que guarda toda a informação relacionada à execução do programa.</li>
  <li>Consola: A interação com o utilizador é feita através do listener. O programa mostra os menus com as várias opções possíveis, e quando necessário mostram um exemplo de input.</li>
</ul>

O ficheiro log.dat é gerado no durante a execução do jogo e regista a sequência de estados até à conclusão do jogo e as estatísticas de execução:

<br>

Ex:
```lisp

 ```

<div style="page-break-after: always;"></div>

# Exemplo de aplicação

Ao iniciar o programa irá ser pedido o file path até ao local onde se encontra o projeto.

```lisp
Escreva o path da localizacao do projeto entre aspas
Exemplo: ''C:/Users/username/Desktop/''
```

Ao inserir o path surgirá o menu com a seguinte interface, para escolher uma opção é só introduzir no listener o número correspondente à ação que quer realizar.

```lisp
 _____________________________________
|                                     |
|           JOGO DO BLOKUS            |
|                                     |
|     1 - Humano VS Computador        |
|     2 - Computador VS Computador    |
|     0 - Sair                        |
|_____________________________________|
 ```

Se escolher 1 irá passar para o modo de jogo de Humano contra Computador, se escolher 2 vai para o modo de jogo Computador contra Computador, se escolher 0 o programa fecha.

Se escolher o modo de jogo Humano VS Computador vai aparecer o menu para escolher qual o jogar que deseja ser.

```lisp
 _____________________________________
|                                     |
|           JOGO DO BLOKUS            |
|                                     |
|    Qual o jogador que deseja?       |
|                                     |
|             1 - Jogador 1           |
|             2 - Jogador 2           |
|             0 - Voltar              |
|                                     |
|_____________________________________|
 ```

Se escolher o modo de jogo Computador VS Computador o menu de escolher o jogador não irá aparecer mas irá aparecer o menu para escolher o tempo limite. O tempo limite define o tempo que o computador têm para fazer a sua jogada. O menu de tempo limite aparece nos dois modos de jogo.


```lisp
 _____________________________________
|                                     |
|           JOGO DO BLOKUS            |
|                                     |
|        Qual o tempo limite?         |
|        Entre 1000 a 20000 ms.       |
|                                     |
|       0 - Voltar                    |
|                                     |
|_____________________________________|
 ```

Ao escolher o tabuleiro, o menu dos algortimos vai aparecer com 3 escolhas possíveis:
<ul>
  <li>BFS - Breadth-First Search</li>
  <li>DFS - Depth-First Search</li>
  <li>A*
</ul>



## Conclusão do Programa

Quando o algoritmo terminar irá aparecer a sequência de estados até ao fim do problema e as estatísticas relacionadas à execução do algoritmo. Nas estatísticas irá ser possível ver:

<ul>
  <li>Factor de ramificação média</li>
  <li>Número de nós gerados</li>
  <li>Número de nós expandidos</li>
  <li>Penetrância</li>
  <li>Tempo de execução em segundos</li>
</ul>

Ex:

```lisp
- --/-/-/-/-/E S T A T I S T I C A S/-/-/-/-/-- - 
Factor de ramificação média: 21.36111 
Número de nós gerados: 769 
Número de nós expandidos: 36 
Penetrância: 0.041612484 
Tempo de execução em segundos: 0.416 
- --/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-- - 
 ```

Toda a informação que é apresentada no final da execução tambem irá ser guardada no ficheiro resultados.dat. O utilizador depois terá a opção de continuar a usar ou fechar o programa.B