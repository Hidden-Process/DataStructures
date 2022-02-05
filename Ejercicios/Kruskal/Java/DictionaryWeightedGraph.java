package dataStructures.graph;

import java.util.Iterator;

import dataStructures.dictionary.Dictionary;
import dataStructures.dictionary.HashDictionary;

import dataStructures.set.Set;
import dataStructures.set.HashSet;
import dataStructures.tuple.Tuple2;

public class DictionaryWeightedGraph<V, W extends Comparable<? super W>> implements WeightedGraph<V, W> {

    static class WE<V1, W1 extends Comparable<? super W1>> implements WeightedEdge<V1, W1> {

		V1 src, dst;
        W1 wght;

        WE(V1 s, V1 d, W1 w) {
            src = s;
            dst = d;
            wght = w;
        }

        public V1 source() {
            return src;
        }

        public V1 destination() {
            return dst;
        }

        public W1 weight() {
            return wght;
        }

        public String toString() {
            return "WE(" + src + "," + dst + "," + wght + ")";
        }

		public int hashCode() {
			
			int code = 17;
            int a = 31;
            code =  code * a + wght.hashCode();
            code = code * a + src.hashCode() + dst.hashCode();

			return code;
		}

		public boolean equals(Object obj) {
			
			if(! (obj instanceof WeightedEdge<?,?>)) return false;

            WeightedEdge<V1,W1> we = (WeightedEdge<V1, W1>) obj;
            return(src.equals(we.source()) && dst.equals(we.destination()) || src.equals(we.destination())
            && dst.equals(we.source()) && wght.equals(we.weight()));

		}
		
		public int compareTo(WeightedEdge<V1, W1> o) {
			return wght.compareTo(o.weight());
		}
    }

    /**
     * Each vertex is associated to a dictionary containing associations
     * from each successor to its weight
     */
    protected Dictionary<V, Dictionary<V, W>> graph;

    public DictionaryWeightedGraph() {
        graph = new HashDictionary<>();
    }


    public void addVertex(V v) {
        if(!graph.isDefinedAt(v))  graph.insert(v,new HashDictionary <V,W>());
    }

    public void addEdge(V src, V dst, W w) {
        if(!graph.isDefinedAt(src) || !graph.isDefinedAt(dst)) throw new GraphException("El vertice src o dst no está en el grafo");
        Dictionary <V,W> dictS = graph.valueOf(src);
        Dictionary <V,W> dictD = graph.valueOf(dst);
        dictS.insert(dst,w);
        dictD.insert(src,w);
    }

    public Set<Tuple2<V, W>> successors(V v) {
        if(!graph.isDefinedAt(v)) throw new GraphException("El vértice no está definido en el grafo");
        Dictionary<V,W> edges = graph.valueOf(v);
        Set<Tuple2<V,W>> set = new HashSet<>();
        for(Tuple2<V,W> tuple : edges.keysValues()) set.insert(tuple);
        return set;
    }


    public Set<WeightedEdge<V, W>> edges() {
    	Set<WeightedEdge<V,W>> allEdges = new HashSet<>();
        for(Tuple2<V,Dictionary<V,W>> tuple : graph.keysValues()){
            V src = tuple._1();;
            for(Tuple2<V,W> dict : tuple._2().keysValues()){
                allEdges.insert(new WE<>(src,dict._1(),dict._2()));
            }
        }
        return allEdges;
    }


    /** DON'T EDIT ANYTHING BELOW THIS COMMENT **/


    public Set<V> vertices() {
        Set<V> vs = new HashSet<>();
        for (V v : graph.keys())
            vs.insert(v);
        return vs;
    }


    public boolean isEmpty() {
        return graph.isEmpty();
    }

    public int numVertices() {
        return graph.size();
    }


    public int numEdges() {
        int num = 0;
        for (Dictionary<V, W> d : graph.values())
            num += d.size();
        return num / 2;
    }


    public String toString() {
        String className = getClass().getSimpleName();
        String s = className + "(vertices=(";

        Iterator<V> it1 = vertices().iterator();
        while (it1.hasNext())
            s += it1.next() + (it1.hasNext() ? ", " : "");
        s += ")";

        s += ", edges=(";
        Iterator<WeightedEdge<V, W>> it2 = edges().iterator();
        while (it2.hasNext())
            s += it2.next() + (it2.hasNext() ? ", " : "");
        s += "))";

        return s;
    }
}
