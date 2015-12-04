from Vertex import Vertex

class TDA:

    def __init__(self,vert = {}):
        self.v = vert

    def vertices():
        return self.v

    def add(obj):
        nodo = Nodo(obj)
        self.v[obj] = nodo

    def elimina(obj):
        nodo = self.v[obj]
        for i in nodo.neightbours():
            i.desconecta(nodo)
        self.v[obj] = None
