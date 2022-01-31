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
 <p style="text-align: center;">Bernardo Mota n�201900947
 <br>
Frederico Alcaria n�201701440 </p>

<div style="page-break-after: always;"></div>

## Acr�nimos

* **BFS:** Algoritmo de busca em largura
* **DFS:** Algoritmo de busca em profundidade
* **A***: Algoritmo de procura informada
* **IDE***: integrated deveopment environment (ambiente de desenvolvimento integrado)

<br>

# Introdu��o

Este manual visa a ser um guia compreensivo para a correta utiliza��o do programa desenvolvido, utilizando a linguagem de programa��o funcional LISP. O objetivo deste programa � indicar quais os passos necess�rios para chegar ao objetivo dos problemas dados. Esta vers�o do Blokus usa um tabuleiro de 14 por 14, mas os problemas alteram as condi��es de jogo dos tabuleiros e definem os seus objetivos.

<br>

# Instala��o e utiliza��o

Para puder executar o programa � necess�rio o IDE [LispWorks](http://www.lispworks.com/) ou outro que consiga interpretar a linguagem LISP.

## Abrir e Compilar os ficheiros

No LispWorks vai ser preciso compilar o ficheiro project.lisp. Ir File>Compile and Load e escolher o ficheiro project.lisp. Os restantes ficheiros v�o ser compilados ao iniciar o programa, ir� ser pedido o path de onde se encontram os ficheiros do projeto necess�rios (procura.lisp , puzzle.lisp , problemas.dat). 

## Executar o Programa

Para executar o programa � necess�rio abrir um listener e chamar a fun��o iniciar escrevendo (iniciar)

## Navegar no Programa

Para navegar no programa � necess�rio escrever na consola o n� respetivo � op��o que deseja escolher.

<div style="page-break-after: always;"></div>

# Input/Output

**Tipos de input:**

<ul>
  <li>Consola: A intera��o com o programa � atrav�s do listener. O programa l� o input e corre a op��o associada ao n� introduzido.</li>
</ul>

**Tipos de output:**

<ul>
  <li>Ficheiros: o programa vai gerar um ficheiro com o nome log.dat que guarda toda a informa��o relacionada � execu��o do programa.</li>
  <li>Consola: A intera��o com o utilizador � feita atrav�s do listener. O programa mostra os menus com as v�rias op��es poss�veis, e quando necess�rio mostram um exemplo de input.</li>
</ul>

O ficheiro log.dat � gerado no durante a execu��o do jogo e regista a sequ�ncia de estados at� � conclus�o do jogo e as estat�sticas de execu��o:

<br>

Ex:
```lisp

 ```

<div style="page-break-after: always;"></div>

# Exemplo de aplica��o

Ao iniciar o programa ir� ser pedido o file path at� ao local onde se encontra o projeto.

```lisp
Escreva o path da localizacao do projeto entre aspas
Exemplo: ''C:/Users/username/Desktop/''
```

Ao inserir o path surgir� o menu com a seguinte interface, para escolher uma op��o � s� introduzir no listener o n�mero correspondente � a��o que quer realizar.

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

Se escolher 1 ir� passar para o modo de jogo de Humano contra Computador, se escolher 2 vai para o modo de jogo Computador contra Computador, se escolher 0 o programa fecha.

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

Se escolher o modo de jogo Computador VS Computador o menu de escolher o jogador n�o ir� aparecer mas ir� aparecer o menu para escolher o tempo limite. O tempo limite define o tempo que o computador t�m para fazer a sua jogada. O menu de tempo limite aparece nos dois modos de jogo.


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

Ao escolher o tabuleiro, o menu dos algortimos vai aparecer com 3 escolhas poss�veis:
<ul>
  <li>BFS - Breadth-First Search</li>
  <li>DFS - Depth-First Search</li>
  <li>A*
</ul>



## Conclus�o do Programa

Quando o algoritmo terminar ir� aparecer a sequ�ncia de estados at� ao fim do problema e as estat�sticas relacionadas � execu��o do algoritmo. Nas estat�sticas ir� ser poss�vel ver:

<ul>
  <li>Factor de ramifica��o m�dia</li>
  <li>N�mero de n�s gerados</li>
  <li>N�mero de n�s expandidos</li>
  <li>Penetr�ncia</li>
  <li>Tempo de execu��o em segundos</li>
</ul>

Ex:

```lisp
- --/-/-/-/-/E S T A T I S T I C A S/-/-/-/-/-- - 
Factor de ramifica��o m�dia: 21.36111 
N�mero de n�s gerados: 769 
N�mero de n�s expandidos: 36 
Penetr�ncia: 0.041612484 
Tempo de execu��o em segundos: 0.416 
- --/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-- - 
 ```

Toda a informa��o que � apresentada no final da execu��o tambem ir� ser guardada no ficheiro resultados.dat. O utilizador depois ter� a op��o de continuar a usar ou fechar o programa.B