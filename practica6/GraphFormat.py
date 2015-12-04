from Graph import Graph
from xml.dom import minidom
import json
import sys

def dame_tipo_graph(ext):
    try:
        if ext == ".xml":
            return GraphXML()
        elif ext == ".csv":
            return GraphCSV()
        elif ext == ".json":
            return GraphJSON()
        else:
            raise ValueError('Formato no valido')
    except:
        print("El tipo de archivo no es correcto")
        print("El archivo debe ser .xml,.json,.csv")
        sys.exit(1)
        
    
class GraphXML(Graph):

    def __init__(self,digraph = False, vert = {}):
        Graph.__init__(self,digraph,vert)
        
    def dame_graph(self,arch):
        try:
            xmldoc = minidom.parse(arch)
            itemlist = xmldoc.getElementsByTagName('graph')
            if int(itemlist[0].attributes['direct'].value) == 1:
                self.digraph = True
            else:
                self.digraph = False                
            itemlist = xmldoc.getElementsByTagName('vertex')
            for s in itemlist:
                self.add(str(s.attributes['label'].value))
            itemlist = xmldoc.getElementsByTagName('edge')
            for j in itemlist:
                source = str(j.attributes['source'].value)
                target = str(j.attributes['target'].value)
                weight = int(j.attributes['weight'].value)
                self.une(source, target, weight)
            return Graph(self.digraph,self.v)
        except:
            print("El archivo XML se encuentra corrupto")
                
class GraphJSON(Graph):

    def __init__(self,digraph = False, vert = {}):
        Graph.__init__(self,digraph,vert)
    

    def dame_graph(self,arch):
        try:
            json_data=open(arch)
            data = json.load(json_data)
            if int(data['direct']) == 1:
                self.digraph = True
            else:
                self.digraph = False
            for s in data['vertices']:
                self.add(s)
            for j in data['edges']:
                self.une(j[0], j[1], j[2])
            json_data.close()
            return Graph(self.digraph,self.v)
        except:
            print("El archivo JSON se encuentra corrupto")
        
class GraphCSV(Graph):

    def __init__(self,digraph = False, vert = {}):
        Graph.__init__(self,digraph,vert)
    

    def dame_graph(self,arch):
        try:
            archivo_lectura = open(arch, 'r+')
            linea = archivo_lectura.readline()
            linea = linea.replace(" ","")
            dir = linea.split('=')[1].lstrip()[0]
            if int(dir) == 1:
                self.digraph = True
            else:
                self.digraph = False
            linea = archivo_lectura.readline()
            while linea:
                linea = linea.replace(" ","")
                linea = linea.split(',')
                if self.is_element(linea[0]) == False:
                    self.add(linea[0])
                if self.is_element(linea[1]) == False:
                    self.add(linea[1])
                self.une(linea[0], linea[1], int(linea[2]))
                linea = archivo_lectura.readline()            
            return Graph(self.digraph,self.v)
        except:
            print("El archivo CSV se encuentra corrupto")
        
