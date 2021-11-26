import ply.yacc as yacc
import ply.lex as lex

# Project inspired in the PLY lesson from https://www.dabeaz.com/ply/ply.html

# Lexical analysis
literals = ['=', '+', '-', '*', '/', '(', ')']

reserved = { 
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
tokens = tuple(['INUMBER', 'FNUMBER', 'NAME','STRINGVAL','ADDITION',
                'SUBTRACTION','MULTI','DIVISION','EXP','EQUALS',
                'EQ_MORE','EQ_LESS','NOT_EQUALS','MORE','LESS',
                'ASSIGN','L_PAREN','R_PAREN','L_KEY','R_KEY','FINISH'] + list(reserved.values())) 

#Regex -> Lex identifies tokens

t_ADDITION = r'\+'
t_SUBTRACTION = r'-'
t_MULTI = r'\*'
t_DIVISION = r'/'
t_EXP = r'\^'
t_EQUALS = r'=='
t_EQ_MORE =r'>='
t_EQ_LESS =r'<='
t_NOT_EQUALS = r'!='
t_MORE = r'>'
t_LESS = r'<'
t_ASSIGN = r'=='
t_L_PAREN = r'\('
t_R_PAREN = r'\)'
t_L_KEY = r'{'
t_R_KEY = r'}'
t_FINISH = r';'


# Token
def t_NAME(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value,'NAME')    # Check for reserved words
    return t

def t_FNUMBER(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

def t_INUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_STRINGVAL(t):
    r'\"[^\n]+\"'
    t.value = t.value.replace("\"", "")
    return t

t_ignore = " \t"

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

# Build the lexer
lexer = lex.lex()

# Parsing rules

precedence = (
    ('left', '+', '-'),
    ('left', '*', '/'),
    ('right', 'UMINUS'),
)

# dictionary of names
names = {}
abstractTree = []

def p_statement_declare_int(p):
    '''statement : INTDEC NAME is_assing
    '''
    if type(p[3]) == 'float':
        print('No puedes asignar flotantes a enteros')
    else:
        names[p[2]] = { "type": "INT", "value": p[3]}

def p_is_assing(p):
    '''is_assing : "=" expression 
                | '''
    p[0] = 0
    if len(p) > 2:
        p[0] = p[2]

def p_statement_declare_float(p):
    'statement : FLOATDEC NAME'
    names[p[2]] = { "type": "FLOAT", "value":0}

def p_statement_print(p):
    '''statement : PRINT '(' expression ')' '''
    print(p[3])

def p_statement_assign(p):
    'statement : NAME "=" expression'
    if p[1] not in names:
        print ( "You must declare a variable before using it")
    names[p[1]]["value"] = p[3]


def p_statement_expr(p):
    'statement : expression'
    # print(p[1])

def p_expression_binop(p):
    '''expression : expression '+' expression
                  | expression '-' expression
                  | expression '*' expression
                  | expression '/' expression'''
    if p[2] == '+':
        p[0] = p[1] + p[3]
    elif p[2] == '-':
        p[0] = p[1] - p[3]

def p_expression_uminus(p):
    "expression : '-' expression %prec UMINUS"
    p[0] = -p[2]

def p_expression_group(p):
    "expression : '(' expression ')'"
    p[0] = p[2]

def p_expression_inumber(p):
    "expression : INUMBER"
    p[0] = p[1]

def p_expression_fnumber(p):
    "expression : FNUMBER"
    p[0] = p[1]

def p_expression_name(p):
    "expression : NAME"
    try:
        p[0] = names[p[1]]["value"]
    except LookupError:
        print("Undefined name '%s'" % p[1])
        p[0] = 0

def p_error(p):
    if p:
        print(p)
        print("Syntax error at line '%s' character '%s'" % (p.lexpos, p.lineno) )
    else:
        print("Syntax error at EOF")

parser = yacc.yacc()

# Console 
#while True:
#    try:
#        s = input('calc > ')
#    except EOFError:
#        break
#    if not s:
#        continue
#    yacc.parse(s)

#File
inputData = []
with open('./code2.txt') as file:
    inputData = file.readlines()

for data in inputData:
    yacc.parse(data)