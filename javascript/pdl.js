const operators = [';', 'U', '*'];
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
    return program.split(',')[1].replace("U", "").replace(";", "").replace("(", "").replace(")", "");
}


const executeProgram = (program, cursor, graph) => {
    let paramA = null;
    let paramB = null;


    if (operators.includes(program[cursor])) {
        currentOperator = program[cursor];
        cursor++;
        switch (currentOperator) {
            case ';':
                paramA = getFirstSubProgram(program, cursor);
                paramB = getSecondSubProgram(program, cursor + paramA.length + 1); //+1 para tirar a vírgula
                return executeProgram(paramA, 0, graph) && executeProgram(paramB, 0, graph); //Operador sequencial executa os dois!
            case '?':
                break;

            case '~':
                break;

            case 'U':
                paramA = getFirstSubProgram(program, cursor);
                paramB = getSecondSubProgram(program, cursor + paramA.length + 1); //+1 para tirar a vírgula
                return executeProgram (paramA, 0, graph) || executeProgram (paramB, 0, graph);

            default:
                return;
                break;
        }
    }


    if(program.includes('?')){
        return true;
    }

    graph.forEach(element => {
        if(element[2] === program){
            return true;
        }
    });

    return false;
}


const start = () => {
    graph = [[0, 1, "y"], [0, 2, "z"]];
    program = "U(;(x?,y),;(~x,z))";
    currentNode = graph[0]; //Início do programa
    console.log(executeProgram(program, 0, graph));
}



start();
