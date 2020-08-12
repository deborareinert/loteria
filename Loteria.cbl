      $set sourceformat"free"
       *>Divis�o de identifica��o do programa
       Identification Division.
       Program-id. "Loteria".
       Author. "Debora Reinert".
       Installation. "PC".
       Date-written. 07/08/2020.
       Date-compiled. 07/08/2020.
       *>Divis�o para configura��o do ambiente
       Environment Division.
       Configuration Section.
       special-names. decimal-point is comma.
       *>-----Declara��o dos recursos externos
       Input-output Section.
       File-control.
       I-O-Control.

       *>Declara��o de vari�veis
       Data Division.

       *>----Variaveis de arquivos
       File Section.

       *>----Variaveis de trabalho
       working-storage section.
       01  numero-sorteio.
           05  sorteio                             pic 9(02) occurs 6.
       01  aposta.
           05  numero2                             pic 9(02).
           05  sorteio2                            pic 9(02).
           05  controle2                           pic 9(01).
       77  controle3                               pic 9(09).
       77  ind                                     pic 9(02).
       77  quantidade_numero                       pic 9(02).
       77  semente                                 pic 9(08).
       77  numero_random                           pic 9(02)V9.
       77  ind2                                    pic 9(02).
       77  ind3                                    pic 9(03).
       01  numeros-aposta occurs 10.
           05  numero-esc                          pic 9(02).
       77  controle                                pic x(1).
           88  trocou                              value "t" "T".
           88  nao_trocou                          value "n" "N".
       77  proximo                                 pic x(01) value space.

       *>----Variaveis para comunica��o entre programas
       linkage section.

       *>----Declara��o de tela
       screen section.

       *>Declara��o do corpo do programa
       procedure Division.

                  perform inicializa.
                  perform processamento.
                  perform finaliza.

      *>-----------------------------------------------------------------------------------------------------------------------------------------------------------
      *> Se��o de Inicializa��o
      *>-----------------------------------------------------------------------------------------------------------------------------------------------------------

       inicializa section.

                  move 0 to numero-esc(10)
                  move 0 to numero-esc(9)
                  move 0 to numero-esc(8)
                  move 0 to numero-esc(7)
                  move 0 to numero-esc(6)
                  move 0 to numero-esc(5)
                  move 0 to numero-esc(4)
                  move 0 to numero-esc(3)
                  move 0 to numero-esc(2)
                  move 0 to numero-esc(1)
                  move 0 to controle3
                  move 0 to quantidade_numero
                  move 1 to ind
                  move 0 to controle2
                  move 0 to sorteio2
                  .
       inicializa-exit.
                  exit.
      *>-----------------------------------------------------------------------------------------------------------------------------------------------------------
      *> Se��o de processamento
      *>-----------------------------------------------------------------------------------------------------------------------------------------------------------

       processamento section.

            perform until quantidade_numero >= 6 and quantidade_numero <= 10
              display "                   Loteria"
              display "Digite a quantidade de n�meros que deseja apostar"
              display "                   {6 - 10}  "
              accept quantidade_numero
            end-perform
            perform varying ind from 1 by 1 until ind > quantidade_numero
                 display "Digite o numero que deseja incluir na aposta:"
                 accept numero2       perform verifica-numeros
                      move numero2 to numero-esc(ind)
                  end-perform
                  perform until controle2 = 6
                      move 0 to sorteio(6)
                      move 0 to sorteio(5)
                      move 0 to sorteio(4)
                      move 0 to sorteio(3)
                      move 0 to sorteio(2)
                      move 0 to sorteio(1)
                      perform numrandom
                      perform teste

                      display "Numeros sorteados: " sorteio(6) " . " sorteio(5) " . " sorteio(4) " . " sorteio(3) " . " sorteio(2) " . "
                      sorteio(1)
                      display "Foram realizados " controle3 " sorteios"

      *>              vari�vel para chamar o pr�ximo sorteio
                      accept proximo

                  end-perform
                  if controle2 = 6 then
                      display "Voce ganhou! Foram executas" controle3 "tentativas at� o acerto"
                  end-if
              .
       processamento-exit.
       exit.

      *>-----------------------------------------------------------------------------------------------------------------------------------------------------------
      *> Repetir sorteio
      *>-----------------------------------------------------------------------------------------------------------------------------------------------------------

       sorteio-rep section.

                  set nao_trocou to true
                  perform varying ind2 from 1 by 1 until sorteio(ind2) = 0 or trocou
                      if sorteio2 = sorteio(ind2) then
                          compute ind2 = ind2 - 1
                          set trocou to true
                      end-if
                  end-perform
                  .
       sorteio-rep-exit.
           exit.
      *>-----------------------------------------------------------------------------------------------------------------------------------------------------------
      *> Se��o para verifica��o de ganhador
      *>-----------------------------------------------------------------------------------------------------------------------------------------------------------

       teste section.

            move 0 to controle2
            perform varying ind2 from 1 by 1 until ind2 > 6
                perform varying ind from 1 by 1 until ind > quantidade_numero
                    if sorteio(ind2) = numeros-aposta(ind) then
                       add 1 to controle2
                    end-if
                end-perform
            end-perform
                  move 1 to ind
                  move 1 to ind2
                  .
              teste-exit.
                  exit.
      *>-----------------------------------------------------------------------------------------------------------------------------------------------------------
      *> Verifica��o dos numeros apostados
      *>-----------------------------------------------------------------------------------------------------------------------------------------------------------

      *> Esta section verifica os numeros apostados, para que n�o estejam repetidos ou fora do  intervalo de n�meros permitidos
       verifica-numeros section.
           perform varying ind from 1 by 1 until numero-esc(ind) = 0

      *>      verifi��o para n�meros j� apostados
              if numero2 = numero-esc(ind) then
                 display "Este numero ja esta incluido na aposta"
                 display "Insira um novo numero"
                 accept numero2
              end-if

      *>      verifica��o para n�meros fora do intervalo permitido
              if numero2 > 60 or numero2 < 1 then
                 display "Numero Invalido"
                 display "Insira um numero valido"
                 accept numero2
              end-if

                  end-perform
                  .
       verifica-numeros-exit.

      *>-----------------------------------------------------------------------------------------------------------------------------------------------------------
      *> Se��o de random
      *>-----------------------------------------------------------------------------------------------------------------------------------------------------------

       numrandom section.

           perform varying ind2 from 1 by 1 until ind2 > 6
               accept semente from time
               compute semente = (semente + (ind2 * ind)) * (semente * (quantidade_numero - ind2)) / sorteio2
               compute numero_random = function random(semente)
               multiply numero_random by 60 giving sorteio2
               perform sorteio-rep
               move sorteio2 to sorteio(ind2)
           end-perform

                  add 1 to controle3

                  .
       numrandom-exit.
            exit.

      *>-----------------------------------------------------------------------------------------------------------------------------------------------------------
      *> Finaliza��o
      *>-----------------------------------------------------------------------------------------------------------------------------------------------------------

       finaliza section.

          stop run
                  .
       finaliza-exit.
           exit.
