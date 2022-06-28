const operators = [';', 'U', '~', '?']
var program
var graph
var currentNode;
var currentOperator;

const getNextTriple = (indexNode) => {
    let response = undefined;
    graph.forEach(element => {
        if (element[0] === indexNode) {
            response = element;
            return element;
        }
    });
    return response;
}

const getOperations = (program) => {
    debugger
    if (operators.includes(program[0])) {
        currentOperator = program.shift()
    }

    let edge = ''
    while (program[0] === '(') {
        program.shift()
    }

    while (program[0] !== ',' && program[0] !== ')' && program.length > 0) {
        edge += program.shift()
    }

    if (program.length === 0) { //O Programa está correto!
        console.log("É VERDADE");
    }

    switch (currentOperator) {
        case ';':
            if (currentNode[2] === edge) {
                currentNode = getNextTriple(currentNode[1]);
            }
            else {
                console.log("ERROR")
                return;
            }
            program.shift() //tira vírgula ou outros operadores
            getOperations(program)
            break;

        case '?':
            program.shift() //tira vírgula ou outros operadores
            getOperations(program)

        default:
            console.log("Aloo");
            break;
    }
}


const start = () => {
    graph = [[1, 2, "a"], [2, 3, "b"], [3, -1, ""]];
    // graph = [[1, 2, "a"], [1, 3, "b"], [2, -1, ''], [3, -1, '']];
    program = ";(a,?b)"
    currentNode = graph[0]; //Início do programa
    getOperations(program.split(""));
}



start();


/*
- Como escrever programas grandes em forma de prefixado?
- Como escrever as uniões em forma de predicado?

- Como funciona a '?'
- O que devo usar na comparação do grafo? A Aresta? Ou o próximo nó? (Pra validar se está certo ou não)


- Dica: Contdor de parênteses;
- Para escolhas não determinísticas: return esquerda or direita;



*/