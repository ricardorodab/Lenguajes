from Vertex import Vertex

class TDA:

    def __init__(self,vert = {}):
        self.v = vert

    def vertices(self):
        return list(self.v.values())
#        valores = self.v.keys()
#        return list(valores)

    def add(self, obj):
        nodo = Vertex(obj)
        self.v[obj] = nodo

    def elimina(self, obj):
        nodo = self.v[obj]
        for i in nodo.neightbours():
            i.desconecta(nodo)
        self.v[obj] = None

