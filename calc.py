import ply.yacc as yacc
import ply.lex as lex


# Project inspired in the PLY lesson from https://www.dabeaz.com/ply/ply.html

# Lexical analysis
reserved  = {
     'int' : 'INTDEC',
    'float' : 'FLOATDEC',
    'string': 'STRING',
    'print' : 'PRINT',
    'bool' : 'BOOL',
    'and' : 'AND',
    'or' : 'OR',
    'true': 'TRUE',
    'false': 'FALSE',
    'if': 'IF',
    'elif': 'ELIF',
    'else': 'ELSE',
    'do': 'DO',
    'while': 'WHILE',
    'for': 'FOR',
}

#tuple to avoid errors and values can be modified
tokens = tuple(['INUMBER', 'FNUMBER', 'NAME','STRING_VAL','ADDITION',
                'SUBTRACTION','MULTI','DIVISION','EXP','EQUALS',
                'EQ_MORE','EQ_LESS','NOT_EQUALS','MORE','LESS',
                'ASSIGN','L_PAREN','R_PAREN','L_KEY','R_KEY','FINISH'] + list(reserved.values())) 

#Regex -> Lex identifies tokens

t_ADDITION = r'\+'
t_SUBTRACTION = r'-'
t_MULTI = r'\*'
t_DIVISION = r'/'
t_EXP = r'\^'
t_ASSIGN = r'='
t_EQUALS = r'=='
t_NOT_EQUALS = r'!='
t_EQ_MORE = r'>='
t_EQ_LESS = r'<='
t_MORE = r'>'
t_LESS = r'<'
t_L_PAREN = r'\('
t_R_PAREN = r'\)'
t_L_KEY = r'{'
t_R_KEY = r'}'
t_FINISH = r';'


# Token
def t_NAME(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = reserved .get(t.value, 'NAME')
    return t

def t_FNUMBER(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

def t_INUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_STRING_VAL(t):
    r'\"[^\n]+\"'
    t.value = t.value.replace("\"", "")
    return t

t_ignore = ' \t'

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


lexer = lex.lex()

# ***************** Here starts syntactical analysis *********************
# Parsing rules

precedence = (
    ('left', 'AND', 'OR'),
    ('nonassoc', 'EQUALS', 'NOT_EQUALS', 'EQ_MORE', 'EQ_LESS', 'MORE', 'LESS'),
    ('left', 'ADDITION', 'SUBTRACTION'),
    ('left', 'MULTI', 'DIVISION'),
    ('left', 'EXP'),
    ('right', 'UMINUS'),  
)


names = ()




def p_start(p):
    '''start : statement'''
    global names
    names = p[1]

def p_statement(p):
    '''statement : statement_print FINISH statement
                | statement_register FINISH statement
                | statement_condition statement
                | statement_for statement
                | statement_while statement
                | empty'''
    if len(p) > 2:
        if p[2] == ';':
            p[2] = p[3]
        p[0] = (p[1],) + p[2]
    else:
        p[0] = ()

def p_expression_NAME(p):
    "expression : NAME"
    p[0] = p[1]

def p_expression_binop(p):
    '''expression : expression AND expression
                | expression OR expression
                | expression ADDITION expression
                | expression SUBTRACTION expression
                | expression MULTI expression
                | expression DIVISION expression
                | expression EXP expression
                | expression EQUALS expression
                | expression NOT_EQUALS expression
                | expression EQ_MORE expression
                | expression EQ_LESS expression
                | expression MORE expression
                | expression LESS expression'''

    p[0] = ('operation', p[1], p[2], p[3])

def p_expression_valuesues(p):
    '''expression : FNUMBER
                 | INUMBER
                 | STRING_VAL
                 | bool_val'''
    p[0] = p[1]

def p_bool_val(p):
    '''bool_val : TRUE
              | FALSE'''
    if p[1] == "true":
        p[0] = True
    elif p[1] == "false":
        p[0] = False

def p_expression_parenthesis(p):
    'expression : L_PAREN expression R_PAREN'
    p[0] = p[2]

def p_expression_uminus(p):
    'expression : SUBTRACTION expression %prec UMINUS'
    p[0] = -p[2]

def p_statement_print(p):
    'statement_print : PRINT expression'
    p[0] = ('print', p[2])

def p_statement_register(p):
    '''statement_register : declare_register
            | declare_assign_register
            | assign_register'''
    p[0] = p[1]

def p_declare_register(p):
    '''declare_register : type NAME'''
    p[0] = ('declare', p[1], p[2])

def p_declare_assign_register(p):
    '''declare_assign_register : type NAME ASSIGN expression'''
    p[0] = ('declare_assign', p[1], p[2], p[4])

def p_assign_register(p):
    '''assign_register : NAME ASSIGN expression'''
    p[0] = ('assign', p[1], p[3])

def p_statement_condition(p):
    '''statement_condition : if_condition elif_condition else_condition'''
    p[0] = ('condition', p[1], p[2], p[3])

def p_if_condition(p):
    '''if_condition : IF L_PAREN expression R_PAREN L_KEY statement R_KEY'''
    p[0] = ('if', p[3], p[6])


def p_elif_condition(p):
    '''elif_condition : ELIF L_PAREN expression R_PAREN L_KEY statement R_KEY elif_condition
                | empty'''
    if len(p) > 2:
        p[0] = (('elif', p[3], p[6]), ) + p[8]
    else:
        p[0] = ()

def p_else_condition(p):
    '''else_condition : ELSE L_KEY statement R_KEY
                | empty'''
    if len(p) > 2:
        p[0] = ('else', p[3])

def p_statement_for(p):
    '''statement_for : FOR L_PAREN declare_assign_register FINISH expression FINISH assign_register R_PAREN L_KEY statement R_KEY'''
    p[0] = ('for', p[3], p[5], p[7], p[10])

def p_statement_while(p):
    '''statement_while : WHILE L_PAREN expression R_PAREN L_KEY statement R_KEY
                 | DO L_KEY statement R_KEY WHILE L_PAREN expression R_PAREN FINISH'''
    if p[1] == "while":
        p[0] = ('while', p[3], p[6])
    else:
        p[0] = ('do-while', p[7], p[3])


def p_empty(p):
    'empty :'
    pass


def p_type(p):
    '''type : BOOL
           | INTDEC
           | FLOATDEC
           | STRING'''
    p[0] = p[1]

def p_error(p):
    if p:
        print("Syntax error at '%s'" % p.value)
    else:
        print("Syntax error at EOF")


yacc.yacc()
file = open("code2.txt", "r")
s = file.read()
yacc.parse(s)

#**************** Intermediate Code Generator*********************

fo = open("output.txt", "w")

def results_label():
    global label_Cnt
    label_Cnt += 1
    return "L" + str(label_Cnt)


def results_variable():
    global variable_Cnt
    variable_Cnt += 1
    return "V" + str(variable_Cnt)

def parseInst(inst):
    if type(inst) is not tuple:
        return inst

    cStmt = inst[0]

    if cStmt == "print":
        parsedStatement = parseInst(inst[1])
        fo.write(f'print {parsedStatement} \n')

    elif cStmt == "declare":
        dataType = inst[1]
        id = inst[2]
        fo.write(f'{dataType} {id} \n')

    elif cStmt == "declare_assign":
        dataType = inst[1]
        id = inst[2]
        fo.write(f'{dataType} {id} \n')
        parsedStatement = parseInst(inst[3])
        fo.write(f'{id} = {parsedStatement} \n')

    elif cStmt == "assign":
        id = inst[1]
        parsedStatement = parseInst(inst[2])
        fo.write(f'{id} = {parsedStatement} \n')

    elif cStmt == "condition":
        ifStatement = inst[1]
        elifStatements = inst[2]
        elseStatement = inst[3]

        condition = parseInst(ifStatement[1])
        currentCheckpoint = results_label()
        endingCheckpoint = results_label()
        statements = ifStatement[2]
        fo.write(f'if {condition} fails, go to {currentCheckpoint}\n')

        for statement in statements:
            parseInst(statement)
        fo.write(f'go to {endingCheckpoint}\n')
        fo.write(f'label {currentCheckpoint}\n')

        for elifStatement in elifStatements:
            condition = parseInst(elifStatement[1])
            statements = elifStatement[2]
            currentCheckpoint = results_label()

            fo.write(f'if {condition} fails, go to {currentCheckpoint}\n')

            for statement in statements:
                parseInst(statement)

            fo.write(f'go to {endingCheckpoint}\n')
            fo.write(f'label {currentCheckpoint}\n')

        if elseStatement is not None:
            statements = elseStatement[1]

            for statement in statements:
                parseInst(statement)

        fo.write(f'label {endingCheckpoint}\n')

    elif cStmt == "operation":

        leftStatement = parseInst(inst[1])
        operation = inst[2]
        rightStatement = parseInst(inst[3])
        addressCode = results_variable()

        fo.write(
            f'{addressCode} = {leftStatement} {operation} {rightStatement} \n')

        return addressCode

    elif cStmt == "do-while":
            statements = inst[2]
            startCheckpoint = results_label()

            fo.write(f'go to {startCheckpoint}\n')

            for statement in statements:
                parseInst(statement)

            condition = parseInst(inst[1])
            fo.write(f'if {condition} fails, go to {startCheckpoint}\n')

    elif cStmt == "while":
        statements = inst[2]
        startCheckpoint = results_label()
        endingCheckpoint = results_label()
        condition = parseInst(inst[1])

        fo.write(f'label {startCheckpoint}\n')
        fo.write(f'if {condition} fails, go to {endingCheckpoint}\n')

        for statement in statements:
            parseInst(statement)

        fo.write(f'go to {startCheckpoint}\n')
        fo.write(f'label {endingCheckpoint}\n')

        
    elif cStmt == "for":
        parseInst(inst[1])
        innerStatements = inst[4]

        startCheckpoint = results_label()
        endingCheckpoint = results_label()

        fo.write(f'label {startCheckpoint}\n')

        condition = parseInst(inst[2])
        fo.write(f'if {condition} fails, go to {endingCheckpoint}\n')

        for statement in innerStatements:
            parseInst(statement)

        fo.write(f'go to {startCheckpoint}\n')
        fo.write(f'label {endingCheckpoint}\n')

    

    

    else:
        fo.write(
            f'-----ERROR: Unknown statement: {cStmt}-----\n')


variable_Cnt = -1
label_Cnt = -1


for inst in names:
    parseInst(inst)
fo.close()
