from Nodo import Nodo
from Edges import Edge

class Vertex(Nodo):

    def __init__(self,elem,aristas = {}):
        Nodo.__init__(self,elem)
        self.aristas = aristas
        if aristas == {}:            
            self.grado = 0
        else:
            self.grado = len(aristas)

    def agrega_vecino(nodo,peso):
        #actual = Vertex(self,self.elem,self.aristas)
        arista = Edge(self, nodo, peso)
        self.aristas[nodo] = arista
        self.grado += 1
        return arista

    def desconecta(nodo):
        if self.aristas[nodo] != None:
            self.aristas[nodo] = None
            self.grado -= 1

    def degree():
        return self.grado

    def neighbours():
        vecinos = []
        conex = list(self.aristas.values())
        for i in conex:
            if self == i.tvertex():
                vecinos += [i.svertex()]
            else:
                vecinos += [i.tvertex()]
        return vecinos

    def get_arista(nodo):
        return self.aristas[nodo]
    
    
