package topoSort;

import java.util.Iterator;

import dataStructures.dictionary.Dictionary;
import dataStructures.dictionary.HashDictionary;
import dataStructures.graph.DiGraph;
import dataStructures.list.ArrayList;
import dataStructures.list.List;
import dataStructures.set.HashSet;
import dataStructures.set.Set;

public class TopologicalSortingDicPar<V> {

	private List<Set<V>> topSort;
	private boolean hasCycle;

	public TopologicalSortingDicPar(DiGraph<V> graph) {

		topSort = new ArrayList<>();
		// dictionary: vertex -> # of pending predecessors
		Dictionary<V, Integer> pendingPredecessors = new HashDictionary<>();
		Set<V> sources = new HashSet<V>();
		
		Iterator<V> i = graph.vertices().iterator();
        V vertex;
        
        while(i.hasNext()) {
            vertex = i.next();
        	pendingPredecessors.insert(vertex, graph.inDegree(vertex)); // Asignamos la cantidad de arcos que apuntan a ese vértice.
        }
        
        int index = 0;
        hasCycle = false;
        
        while(!hasCycle() && !pendingPredecessors.isEmpty()) {
        	for(V vertice : pendingPredecessors.keys() ) {     // Vamos reccoriendo los pendientes por procesar
        		if(pendingPredecessors.valueOf(vertice)==0) { // Seleccionamos los fuentes del diccionario
        			sources.insert(vertice);
        		}
        	}
        	
         if(sources.isEmpty()) { // Si nos quedamos sin fuente es porque hay ciclo.
            hasCycle = true;
          }
         
         Iterator<V> iter = sources.iterator();
         V s;
              
         while(iter.hasNext()) {
             s = iter.next();
             
             pendingPredecessors.delete(s);  // Eliminamos del diccionario de los pendientes.
        	 
             Iterator<V> successors = graph.successors(s).iterator();
             V next;
             
             while(successors.hasNext()) {
                 next = successors.next();
                 pendingPredecessors.insert(next, pendingPredecessors.valueOf(next)-1); // Actualizamos Diccionario.
                          
            }
         }
         
         topSort.insert(index, sources);  // Añadimos fuentes al orden topológico, haciendo uso de conjuntos.
         index++;
         sources = new HashSet<V>();
         
         }
             
     }
	

	public boolean hasCycle() {
		return hasCycle;
	}

	public List<Set<V>> order() {
		return hasCycle ? null : topSort;
	}

}
