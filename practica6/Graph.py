from Vertex import Vertex
from Edges import Edge
from TDA import TDA

class Graph(TDA):
    
    def __init__(self, digraph = False ,vert = {}):
        TDA.__init__(self, vert)
        self.digraph = digraph

    def dame_graph(self):
        return self

    def set_directed(self,value):
        self.digraph = value
        
    def directed(self):
        return self.digraph
    
    def edges(self):
        lista = []
        if self.digraph:
            for i in self.v.values():
                for j in i.neighbours():
                    lista += [self.v[i.get_elem()].get_arista(j)]
        else:
            vecinos_pasados = []
            for i in self.v.values():
                for j in i.neighbours():
                    if j not in vecinos_pasados:
                        lista += [self.v[i.get_elem()].get_arista(j)]
                vecinos_pasados += [i]
        return lista
            
    
    def une(self,obj1, obj2, peso = 0):
        nodo1 = self.v[obj1]
        nodo2 = self.v[obj2]
        nodo1.agrega_vecino(nodo2, peso)
        if self.digraph == False:
            nodo2.agrega_vecino(nodo1, peso)

    def separa(self,obj1, obj2):
        nodo1 = self.v[obj1]
        nodo2 = self.v[obj2]
        nodo1.desconecta(nodo2)
        nodo2.desconecta(nodo1)

    def has_cycles(self):
        for i in self.v.values():
            ciclo = self.__esta_en_ciclo(i, [],[])
            if ciclo:
                return True
        return False

    def __esta_en_ciclo(self, nodo, pasados, por_hacer):
        vecinos = nodo.neighbours()
        pasados += [nodo]
        for i in nodo.neighbours():
            if i not in pasados:
                if i in por_hacer:
                    return True
                por_hacer += [i]
        if por_hacer == []:
            return False
        nuevo = por_hacer.pop(0)
        return self.__esta_en_ciclo(nuevo,pasados,por_hacer)
