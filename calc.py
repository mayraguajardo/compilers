import ply.yacc as yacc
import ply.lex as lex

# Project inspired in the PLY lesson from https://www.dabeaz.com/ply/ply.html

# Lexical analysis

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

def t_STRING_VAL(t):
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

# ***************** Here starts syntactical analysis *********************
# Parsing rules

precedence = (
    ('left', 'AND', 'OR'),
    ('nonassoc', 'EQUALS', 'NOT_EQUALS', 'EQ_MORE', 'EQ_LESS', 'MORE', 'LESS'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'MULT', 'DIV'),
    ('left', 'EXP'),
    ('right', 'UMINUS'),
)

# dictionary of names
names = []
abstractTree = []

#initia prod

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

def p_expression_name(p):
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
                    | expression EQ_MORE expression
                    | expression EQ_LESS expression
                    | expression NOT_EQUALS expression
                    | expression MORE expression
                    | expression LESS expression'''

    p[0] = ('operation', p[1], p[2], p[3])

def p_expression_values(p):
    '''expression : FNUMBER | INUMBER | STRING_VAL | bool_val '''
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
    "expression : SUBTRACTION expression %prec UMINUS"
    p[0] = -p[2]

def p_statement_print(p):
    '''statement_print : PRINT  expression  '''
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
    '''elif_condition : ELIF L_PAREN expression R_PAREN L_KEY statement R_KEY elif_cond
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

def p_while_stmt(p):
    '''while_stmt : WHILE L_PAREN expression R_PAREN L_KEY statement R_KEY
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
           | INT
           | FLOAT
           | STRING'''
    p[0] = p[1]

def p_error(p):
    if p:
        print(p)
        print("Syntax error at line '%s' " % (p.value) )
    else:
        print("Syntax error at EOF")

parser = yacc.yacc()
file = open("code2.txt",'r')
s = file.read()
yacc.pase(s)

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