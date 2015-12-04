from Graph import Graph
from GraphFormat import *
import os

class GraphReader:

    def __init__(self, archivo):
        #try:
        self.archivo = open(archivo, 'r+')
        if isinstance(archivo, str):
            filename, file_extension = os.path.splitext(archivo)
            self.name = filename
            self.ext = file_extension
            temp = dame_tipo_graph(self.ext)
            print(type(temp))
            self.graph = temp.dame_graph(self.archivo)
        else:
            raise ValueError('A very specific bad thing happened')
        #except:
        #    print("Favor de pasar el archivo como una cadena")

    def get_graph():
        return self.graph
    
