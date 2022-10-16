# Operacoes basicas de matematica

aritmetica <- function(a, b, op){
    #op == divisao, multiplicacao, soma e subtracao
    if (op == "divisao") x <- divisao(a,b)
    if (op == "multiplicacao") x <- multiplicacao(a,b)
    if (op == "soma") x <- soma(a,b)
    if (op == "subtracao") x <- subtração(a,b)
}
