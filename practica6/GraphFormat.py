from Graph import Graph
from xml.dom import minidom

def dame_tipo_graph(ext):
    if ext == ".xml":
        return GraphXML()
    elif ext == ".csv":
        return GraphCSV()
    elif ext == ".json":
        return GraphJSON()
    else:
        raise ValueError('Formato no valido')
    
class GraphXML(Graph):

    def __init__(self,digraph = False, vert = {}):
        Graph.__init__(self,digraph,vert)
        
    def dame_graph(archivo):
        try:
            xmldoc = minidom.parse(archivo)
            itemlist = xmldoc.getElementsByTagName('graph')
            if int(itemlist[0].attributes['direct'].value) == 1:
                self.digraph = True
            else:
                self.digraph = False                
            itemlist = xmldoc.getElementsByTagName('vertex')
            for s in itemlist:
                self.add(srt(s.attributes['label'].value))
            itemlist = xmldoc.getElementsByTagName('edge')
            for j in itemlist:
                source = str(j.atrributes['source'].value)
                target = str(j.attributes['target'].value)
                weight = int(j.attributes['weight'].value)
                self.une(source, target, weight)
            return Graph(self.digraph,self.vertices())
        except:
            print("El archivo XML se encuentra corrupto")
                
