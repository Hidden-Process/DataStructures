
import dataStructures.graph.WeightedGraph;
import dataStructures.graph.WeightedGraph.WeightedEdge;

import dataStructures.dictionary.Dictionary;
import dataStructures.dictionary.HashDictionary;
import dataStructures.priorityQueue.PriorityQueue;
import dataStructures.priorityQueue.LinkedPriorityQueue;
import dataStructures.set.Set;
import dataStructures.set.HashSet;

public class Kruskal {

	public static <V,W> Set<WeightedEdge<V,W>> kruskal(WeightedGraph<V,W> g) {

		Set<WeightedEdge<V,W>> forest = new HashSet<>();

		PriorityQueue<WeightedEdge<V,W>> PQ = new LinkedPriorityQueue();
		for(WeightedEdge<V,W> edge : g.edges()) PQ.enqueue(edge);

		Dictionary<V,V> dict = new HashDictionary<>();
		for(V vertex : g.vertices()) dict.insert(vertex,vertex);

		while(!PQ.isEmpty()){
			WeightedEdge<V,W> e = PQ.first();
			PQ.dequeue();

			V r1 = representante(e.source(),dict);
			V r2 = representante(e.destination(),dict);

			if(!r1.equals(r2)){
				dict.insert(r2,e.source());
				forest.insert(e);
			}
		}
		return forest;
	}

	private static <V> V representante(V src, Dictionary<V,V> dict){
		V v = src;
		V r = dict.valueOf(v);

		while(!v.equals(r)){
			v = r;
			r = dict.valueOf(v);
		}

		return r;
	}

	// Sólo para evaluación continua / only for part time students
	public static <V,W> Set<Set<WeightedEdge<V,W>>> kruskals(WeightedGraph<V,W> g) {

		// COMPLETAR
		
		return null;
	}
}
