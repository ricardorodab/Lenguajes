from Nodo import Nodo
from Edges import Edge

class Vertex(Nodo):

    def __init__(self,elem,aristas):
        Nodo.__init__(self,elem)
        self.aristas = aristas
        if aristas == {}:            
            self.grado = 0
        else:
            self.grado = len(aristas)

    def __str__(self):
        return self.elem
            
    def agrega_vecino(self,nodo,peso):
        #actual = Vertex(self,self.elem,self.aristas)
        arista = Edge(self, nodo, peso)
        self.aristas[nodo] = arista
        self.grado += 1
        

    def desconecta(self,nodo):
        if self.aristas[nodo] != None:
            self.aristas[nodo] = None
            self.grado -= 1

    def degree(self):
        return self.grado

    def neighbours(self):
        vecinos = []
        conex = self.aristas.values()
        for i in conex:
            if self == i.tvertex():
                vecinos += [i.svertex()]
            else:
                vecinos += [i.tvertex()]
        return vecinos

    def get_arista(self,nodo):
        return self.aristas[nodo]
    
    __repr__ = __str__
