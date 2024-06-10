# HinNer analyzer

Este programa permite realizar la inferencia de tipos para expresiones tipos Haskell.

## Como utilizar

Compilar gramatica:
    antlr4 -Dlanguage=Python3 -no-listener -visitor hm.g4

Ejecuta el programa con streamlit:
    streamlit run hm.py