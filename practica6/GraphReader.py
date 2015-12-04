from Graph import Graph
from GraphFormat import *
import os

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
    
