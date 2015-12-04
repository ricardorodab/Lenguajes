class Edge():

    def __init__(self,nodoIni = None,nodoFin = None,peso = 0):
        self.ini = nodoIni
        self.fin = nodoFin
        self.peso = peso

    def __str__(self):
        cadena = "[" +self.ini.__str__() + " " + self.fin.__str__() + " " + str(self.peso)+ "]"
        return cadena

    def agregar_fin(self,vertice):
        self.fin = vertice

    def agrega_inicio(self,vertice):
        self.ini = vertice

    def tvertex(self):
        return self.fin

    def svertex(self):
        return self.ini

    def weight(self):
        return self.peso
    
    def set_weight(self,w):
        self.peso = w

    __repr__ = __str__
