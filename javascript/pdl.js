const operators = [';', 'U', '*'];
var program
var graph
var currentOperator;


const getFirstSubProgram = (program, start) => {
    let opennedBracket = 0;
    let closedBracket = 0;

    while (!operators.includes(program[start]) && start < program.length) {
        start++;
    }

    let end = start;
    while (end < program.length) {
        if(program[end] === '('){
            opennedBracket++;
        }
        else if(program[end] === ')'){
            closedBracket++;
        }
        else if(program[end] === ',' && opennedBracket === closedBracket){
            return program.substring(start, end);
        }
        end++;
    }
    //Caso base: Não há mais operações internas. Devemos pegar os valores entre parênteses
    return program.split(',')[0].replace("U", "").replace(";", "").replace("(", "").replace(")", "");
}

const getSecondSubProgram = (program, start) => {
    let opennedBracket = 0;
    let closedBracket = 0;

    while (!operators.includes(program[start]) && start < program.length){
        start++;
    }

    let end = start;
    while (end < program.length) {
        if(closedBracket >= opennedBracket && opennedBracket !== 0){
            return program.substring(start, end);
        }
        if(program[end] === '('){
            opennedBracket++;
        }
        else if(program[end] === ')'){
            closedBracket++;
        }
        end++;
    }
    //Caso base: Não há mais operações internas. Devemos pegar os valores entre parênteses
    return program.split(',')[1].replace("U", "").replace(";", "").replace("(", "").replace(")", "");
}


const executeProgram = (program, cursor) => {
    let paramA = null;
    let paramB = null;


    if (operators.includes(program[cursor])) {
        currentOperator = program[cursor];
        cursor++;
        switch (currentOperator) {
            case ';':
                paramA = getFirstSubProgram(program, cursor);
                paramB = getSecondSubProgram(program, cursor + paramA.length + 1); //+1 para tirar a vírgula
                return executeProgram(paramA, 0) && executeProgram(paramB, 0); //Operador sequencial executa os dois!
            case 'U':
                paramA = getFirstSubProgram(program, cursor);
                paramB = getSecondSubProgram(program, cursor + paramA.length + 1); //+1 para tirar a vírgula
                return executeProgram (paramA, 0) || executeProgram (paramB, 0);
            case '*':
                console.log("Operador * não implementado");
                return false;

            default:
                return false;
        }
    }


    if(program.includes('?')){
        return true;
    }

    //Essa verificação está bem ruim, mas não consegui chegar em nada melhor que isso.
    //O ideal seria usar a recursão para navegar pelo grafo verificando sempre a próxima aresta e não uma aresta aleatória, como está acontecendo aqui.
    let needRemove = -1;
    for(let i = 0; i < graph.length; i++){
        if(graph[i][2] === program){
            needRemove = i;
            break;
        }
    }
    if(needRemove !== -1){
        graph.splice(needRemove, 1);
        return true;
    }

    //Se não está no grafo, é falso.
    return false;
}


const start = () => {
    //graph = [[1, 2, "w"], [1, 3, "z"], [2, 4, "y"]]; //Grafo correto
    graph = [[1, 2, "w"], [1, 3, "a"], [2, 4, "b"]]; //Grafo incorreto
    program = "U(;(x?,;(w,y)),;(~x?,z))";
    console.log(executeProgram(program, 0));
}


start();
