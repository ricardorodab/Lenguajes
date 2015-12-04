from Vertex import Vertex
from Edges import Edge
from TDA import TDA

class Graph(TDA):
    
    def __init__(self, digraph = False ,vert = {}):
        TDA.__init__(self, vert)
        self.digraph = digraph

    def set_directed(value):
        self.digraph = value
        
    def directed():
        return self.digraph
    
    def edges():
        lista = []
        if self.digraph:
            for i in self.v:
                lista += i.neighbours()
        else:
            vecinos_pasados = []
            for i in self.v:
                for j in i.neighbours():
                    if j not in vecinos_pasados:
                        lista += [i.get_arista(j)]
                vecinos_pasados += [i]
        return lista
    
    def une(obj1, obj2, peso = 0):
        nodo1 = self.v[obj1]
        nodo2 = self.v[obj2]
        nodo1.agrega_vecino(nodo2, peso)
        if self.digraph == False:
            nodo2.agrega_vecino(nodo1, peso)

    def separa(obj1, obj2):
        nodo1 = self.v[obj1]
        nodo2 = self.v[obj2]
        nodo1.desconecta(nodo2)
        nodo2.desconecta(nodo1)
