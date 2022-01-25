package topoSort;

import java.util.Iterator;

import dataStructures.dictionary.Dictionary;
import dataStructures.dictionary.HashDictionary;
import dataStructures.graph.DiGraph;
import dataStructures.list.ArrayList;
import dataStructures.list.List;
import dataStructures.queue.LinkedQueue;
import dataStructures.queue.Queue;

public class TopologicalSortingDic<V> {

	private List<V> topSort;
	private boolean hasCycle;

	public TopologicalSortingDic(DiGraph<V> graph) {

		topSort = new ArrayList<>();
		// dictionary: vertex -> # of pending predecessors
		Dictionary<V, Integer> pendingPredecessors = new HashDictionary<>();
		Queue<V> sources = new LinkedQueue<>();

		Iterator<V> i = graph.vertices().iterator();
        V vertex;
        
        while(i.hasNext()) {
        	vertex = i.next();
        	pendingPredecessors.insert(vertex, graph.inDegree(vertex)); // Asignamos la cantidad de arcos que apuntan a ese vertice.
        }
        
        int index = 0;
        hasCycle = false;
        
        while(!hasCycle() && !pendingPredecessors.isEmpty()) {
        	for(V ver : pendingPredecessors.keys() ) {      // Vamos reccoriendo los pendientes por procesar
        		if(pendingPredecessors.valueOf(ver)==0) {  // Seleccionamos los fuentes del diccionario.
        			sources.enqueue(ver);
        		}
        	}
        
        if(sources.isEmpty()) { // Si nos quedamos sin fuente es porque hay ciclo.
           hasCycle = true;
        }
        
        while(!sources.isEmpty()) { 
        	pendingPredecessors.delete(sources.first()); // Eliminamos del diccionario de los pendientes los vertices de la fuente
        	topSort.insert(index, sources.first());  // Añadimos fuentes al orden topológico.
        	index++;
        	
        	Iterator<V> successors = graph.successors(sources.first()).iterator();
            V next;
            
            while(successors.hasNext()) {
               next = successors.next();
               pendingPredecessors.insert(next, pendingPredecessors.valueOf(next)-1);  // Restamos fuentes del diccionario.
           
            }
            
            sources.dequeue();
        }
	}
        
}

	public boolean hasCycle() {
		return hasCycle;
	}

	public List<V> order() {
		return hasCycle ? null : topSort;
	}
}
