from Graph import Graph
from GraphFormat import *
import os
import sys

class GraphReader:

    def __init__(self, archivo_entrada):
        self.archivo = archivo_entrada
        if isinstance(archivo_entrada, str):
            filename, file_extension = os.path.splitext(archivo_entrada)
            self.name = filename
            self.ext = file_extension
            temp = dame_tipo_graph(self.ext)
            self.graph = temp.dame_graph(self.archivo)
        else:
            raise ValueError('Favor de pasar el archivo como una cadena')

    def get_graph(self):
        return self.graph
    
args = sys.argv
if len(args) != 2:
    print("Favor de ejecutar el programa como:")
    print("-:$python3 GraphReader.py <file>")
else:
    gr = GraphReader(args[1])
    grafica = gr.get_graph()
    print("Nombre del archivo: " + args[1])
    print("Digrafo: "+ str(grafica.directed()))
    print("Vertices: ")
    for i in grafica.vertices():
        print(i)
    print("Aristas: ")
    for i in grafica.edges():
        print(i)
    print("¿Tiene ciclos? "+ str(grafica.has_cycles()))
