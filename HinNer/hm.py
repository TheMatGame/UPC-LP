import streamlit as st
import re
from typing import Any
from antlr4 import *
from hmLexer import hmLexer
from hmParser import hmParser
from hmVisitor import hmVisitor
from dataclasses import dataclass, field
from graphviz import Digraph

contador = 0

if 'sys' in st.session_state:
    symbols = st.session_state['sys']
else:
    symbols: dict[str, str] = {}

vars: dict[str, str] = {}
taula: dict[str, str] = {}


def getLletra(var):
    global contador
    if var in vars:
        return vars[var]
    else:
        lletra = chr(ord('a') + contador)
        contador += 1
        vars[var] = lletra
        return lletra


@dataclass
class TypeError(Exception):
    param1: Any
    param2: Any


@dataclass
class FaltaSimbol(Exception):
    param: Any


#################################################################

@dataclass
class App:
    expr1: Any
    expr2: Any
    tipus: str


@dataclass
class Abst:
    variable: Any
    expr: Any
    tipus: str


@dataclass
class Oper:
    operador: Any
    expr: Any
    tipus: str


@dataclass
class Numero:
    valor: Any
    tipus: str


@dataclass
class Variable:
    nombre: Any
    tipus: str

#################################################################


class ToSemantic(hmVisitor):
    global symbols

    def visitRoot(self, node: hmParser.RootContext):  # no hace nada
        return self.visit(node.expr())

    def visitApp(self, node: hmParser.AppContext):
        expr1 = self.visit(node.oper())
        expr2 = self.visit(node.apl())
        return App(expr1, expr2, getLletra(id(node)))

    def visitApli(self, node: hmParser.ApliContext):  # no hace nada
        return self.visit(node.apl())

    def visitPar(self, node: hmParser.ParContext):  # no hace nada
        return self.visit(node.expr(0))

    def visitAbst(self, node: hmParser.AbstContext):
        var = self.visit(node.var())
        expr = self.visit(node.apl())
        return Abst(var, expr, getLletra(id(node)))

    def visitOper(self, node: hmParser.OperContext):
        expr = self.visit(node.apl())
        operador = (f' ({node.OPS().getText()})')
        return Oper(operador, expr, getLletra(id(node)))

    def visitNumero(self, node: hmParser.NumeroContext):  # no hace nada
        return self.visit(node.num())

    def visitVariable(self, node: hmParser.VariableContext):  # no hace nada
        return self.visit(node.var())

    def visitNum(self, node: hmParser.NumContext):
        if node.getText() in symbols:
            return Numero(node.getText(), symbols[node.getText()])
        else:
            return Numero(node.getText(), getLletra(node.getText()))

    def visitVar(self, node: hmParser.VarContext):
        return Variable(node.VAR().getText(), getLletra(node.VAR().getText()))

    # VISIT DE TIPUS #
    def visitNumTipus(self, node: hmParser.NumTipusContext):
        num = node.num().getText()  # str
        type = self.visit(node.tipus())  # str
        symbols[num] = type
        return

    def visitOperTipus(self, node: hmParser.OperTipusContext):
        op = (f' ({node.OPS().getText()})')  # str
        type = self.visit(node.tipus())  # str
        symbols[op] = type
        return

    def visitTip(self, node: hmParser.TipContext):
        return node.TIPUS().getText()

    def visitTips(self, node: hmParser.TipsContext):
        tipus1 = self.visit(node.tipus())
        tipus2 = node.TIPUS().getText()
        text = (f' ({tipus2}->{tipus1}) ')
        return text
    ##################


def generar_dot(tree):
    dot = []

    def generar(tree, node_id=None):
        if isinstance(tree, App):
            dot.append(f'  node{id(tree)} [label="@ \n {tree.tipus}"]')
            dot.append(f'  node{id(tree)} -> node{id(tree.expr1)}')
            dot.append(f'  node{id(tree)} -> node{id(tree.expr2)}')
            generar(tree.expr1)
            generar(tree.expr2)
        elif isinstance(tree, Abst):
            dot.append(f'  node{id(tree)} [label="λ \n {tree.tipus}"]')
            dot.append(f'  node{id(tree)} -> node{id(tree.variable)}')
            dot.append(f'  node{id(tree)} -> node{id(tree.expr)}')
            generar(tree.expr)
            generar(tree.variable)
        elif isinstance(tree, Oper):
            dot.append(f'  node{id(tree)} [label="@ \n {tree.tipus}"]')
            if tree.operador in symbols:
                dot.append(
                    f'  node{id(tree)} -> "{tree.operador} \n {symbols[tree.operador]}"')
            else:
                dot.append(f'  node{id(tree)} -> "{tree.operador}"')
            dot.append(f'  node{id(tree)} -> node{id(tree.expr)}')
            generar(tree.expr)
        elif isinstance(tree, Numero):
            dot.append(f'  node{id(tree)} [label="{
                       tree.valor} \n {tree.tipus}"]')
        elif isinstance(tree, Variable):
            dot.append(f'  node{id(tree)} [label="{
                       tree.nombre} \n {tree.tipus}"]')

    generar(tree)
    graph_string = '''digraph { ''' + '\n'.join(dot) + ''' }'''
    return graph_string


def create_table():
    global symbols
    if symbols:
        st.write("**Taula de símbols:**")
        data = {"Símbol": [], "Tipus": []}
        for simbol, tipus in symbols.items():
            data["Símbol"].append(simbol)
            data["Tipus"].append(tipus)
        st.table(data)


def createTableInfer():
    global taula
    if taula:
        data = {"0": [], "1": []}
        for simbol, tipus in taula.items():
            data["0"].append(simbol)
            data["1"].append(tipus)
        st.table(data)


def tratarTipo(cadena):
    return re.sub(r'[^A-Z]', '', cadena)


def restarCadenas(s1, s2):
    for char in s2:
        s1 = s1.replace(char, '', 1)
    return s1


def formatear(t1):
    if len(t1) < 2:
        return t1
    x = t1[0]
    y = formatear(t1[1:])
    return '(' + x + '->' + y + ')'


def compatible(t1, t2):
    x = tratarTipo(t1)  # ej: NNN, PPP, X, AA, AB, ABB
    y = tratarTipo(t2)
    result = restarCadenas(x, y)
    if result == x:  # si result == x quiere decir que t1 y t2 no son compatible ya que ninguna letra de t1 estaba en t2
        raise TypeError(x[0], y[0])
    else:
        return result


def compatibleA(t1, t2):
    x = tratarTipo(t1)  # ej: NNN, PPP, X, AA, AB, ABB
    y = tratarTipo(t2)
    return x + y


def noValorAsignado(cadena):
    for char in cadena:
        if char.islower():
            return True
    return False


def inferir(t):
    if len(t) < 2:
        raise TypeError(0, 0)
    primera_letra = t[0]
    resto = t[1:]
    return primera_letra, resto


def guardarInfer(var, tipus):
    global taula
    taula[var] = tipus
    return


def abstraer(t):
    if len(t) == 0:
        raise TypeError(0, 0)
    res = t[0] + t
    return res


def inferencia_tipus(tree):
    global taula

    def generar(tree, node_id=None):
        if isinstance(tree, App):
            ident = tree.tipus
            tipus1 = generar(tree.expr1)
            tipus2 = generar(tree.expr2)
            if noValorAsignado(tipus1):
                tipusHijo, tree.tipus = inferir(tipus2)
                asignar(tree.expr1, tipusHijo)
            elif noValorAsignado(tipus2):
                tipusHijo, tree.tipus = inferir(tipus1)
                asignar(tree.expr2, tipusHijo)
            else:
                tree.tipus = compatible(tipus1, tipus2)
            copiaTipus = tree.tipus
            tree.tipus = formatear(tree.tipus)
            guardarInfer(ident, tree.tipus)
            return copiaTipus
        elif isinstance(tree, Abst):
            ident = tree.tipus
            tipus1 = generar(tree.variable)
            tipus2 = generar(tree.expr)
            if noValorAsignado(tipus1):
                tree.tipus = abstraer(tipus2)
                asignar(tree.variable, tree.tipus[0])
            elif noValorAsignado(tipus2):
                tree.tipus = inferir(tipus1)
                asignar(tree.expr, tree.tipus[1])
            else:
                tree.tipus = compatibleA(tipus1, tipus2)
            copiaTipus = tree.tipus
            tree.tipus = formatear(tree.tipus)
            guardarInfer(ident, tree.tipus)
            return copiaTipus
        elif isinstance(tree, Oper):
            ident = tree.tipus
            if tree.operador not in symbols:
                raise FaltaSimbol(tree.operador)
            tipus1 = symbols[tree.operador]
            tipus2 = generar(tree.expr)
            if noValorAsignado(tipus1):
                tipusHijo, tree.tipus = inferir(tipus2)
                asignar(tree.expr1, tipusHijo)
            elif noValorAsignado(tipus2):
                tipusHijo, tree.tipus = inferir(tipus1)
                asignar(tree.expr2, tipusHijo)
            else:
                tree.tipus = compatible(tipus1, tipus2)
            copiaTipus = tree.tipus
            tree.tipus = formatear(tree.tipus)
            guardarInfer(ident, tree.tipus)
            return copiaTipus
        elif isinstance(tree, Numero):
            return tree.tipus
        elif isinstance(tree, Variable):
            if tree.tipus in taula:
                tree.tipus = vars[tree.tipus]
            return tree.tipus

    def asignar(tree, tipus):
        if isinstance(tree, App):
            guardarInfer(tree.tipus, tipus)
            tree.tipus = tipus
        elif isinstance(tree, Abst):
            guardarInfer(tree.tipus, tipus)
            tree.tipus = tipus
        elif isinstance(tree, Oper):
            guardarInfer(tree.tipus, tipus)
            tree.tipus = tipus
        elif isinstance(tree, Numero):
            guardarInfer(tree.tipus, tipus)
            tree.tipus = tipus
        elif isinstance(tree, Variable):
            guardarInfer(tree.tipus, tipus)
            tree.tipus = tipus

    generar(tree)
    return tree

#####################
# Visitor (recorrer arbol AST)
#####################


class TreeVisitor(hmVisitor):
    def __init__(self):
        self.nivell = 0

    def visitSuma(self, ctx):
        [expressio1, operador, expressio2] = list(ctx.getChildren())
        print('  ' * self.nivell + '+')
        self.nivell += 1
        self.visit(expressio1)
        self.visit(expressio2)
        self.nivell -= 1

    def visitNumero(self, ctx):
        [numero] = list(ctx.getChildren())
        print("  " * self.nivell + numero.getText())


#####################
# GUI
#####################

st.title("HinNer Analyzer")
create_table()
input_expr = st.text_input("Expression:", placeholder="\\x -> (+) 2 x")
to_run = st.button("RUN")
reset_sym = st.button("Reset symbols")

if to_run:
    ##########
    # GRAMMAR
    input_stream = InputStream(input_expr)
    lexer = hmLexer(input_stream)
    token_stream = CommonTokenStream(lexer)
    parser = hmParser(token_stream)
    tree = parser.root()
    visitor = ToSemantic()
    SemanticTree = visitor.visit(tree)
    ##########

    numErrors = parser.getNumberOfSyntaxErrors()
    st.write(str(numErrors) + " syntax errors.")
    if numErrors == 0:
        st.graphviz_chart(generar_dot(SemanticTree))
        copiaSemanticTree = SemanticTree
        try:
            SemanticTree = inferencia_tipus(SemanticTree)
            st.graphviz_chart(generar_dot(SemanticTree))
            createTableInfer()
        except TypeError as e:
            st.graphviz_chart(generar_dot(copiaSemanticTree))
            st.write(f' Type Error: {e.param1} vs {e.param2}')
        except FaltaSimbol as f:
            st.graphviz_chart(generar_dot(copiaSemanticTree))
            st.write(f' El simbol "{
                     f.param}" no esta declarat, no es pot fer inferencia')
        st.session_state['sys'] = symbols
    else:
        st.write("NOTHING TO GENERATE")

if reset_sym:
    if 'sys' in st.session_state:
        del st.session_state['sys']
