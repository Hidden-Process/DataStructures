package dataStructures.graph;

import dataStructures.list.*;
import dataStructures.set.*;

import java.util.Iterator;

public class EulerianCycle<V> {
    private List<V> eCycle;

    @SuppressWarnings("unchecked")
    public EulerianCycle(Graph<V> g) {
        Graph<V> graph = (Graph<V>) g.clone();
        eCycle = eulerianCycle(graph);
    }

    public boolean isEulerian() {
        return eCycle != null;
    }

    public List<V> eulerianCycle() {
        return eCycle;
    }

    // J.1
    private static <V> boolean isEulerian(Graph<V> g) {
       if(g.isEmpty() || g.numVertices() == 1) return true;
       if(g.numVertices() == 2) return false;

       boolean flag = true;
       Iterator<V> vertices = g.vertices().iterator();

       while(vertices.hasNext() && flag){
           V v = vertices.next();
           if(g.degree(v) % 2 != 0) flag = false;
        }
        return flag;
    }

    // J.2
    private static <V> void remove(Graph<V> g, V v, V u) {
        g.deleteEdge(v,u);
        Iterator <V> it = g.vertices().iterator();
        while(it.hasNext()){
            V vertice = it.next();
            if(g.degree(vertice) == 0) g.deleteVertex(vertice);
        }
    }

    // J.3
    private static <V> List<V> extractCycle(Graph<V> g, V v0) {
        List <V> ciclo = new LinkedList<>();
        ciclo.append(v0);
        V v = v0;
        boolean flag = false;
        while(!flag){
            V u = g.successors(v).iterator().next();
            remove(g,v,u);
            ciclo.append(u);
            v = u;
            flag = v.equals(v0);
        }
        return ciclo;
    }

    // J.4
    private static <V> void connectCycles(List<V> xs, List<V> ys) {
        if (xs.isEmpty()) {
            for (V vertice : ys) xs.append(vertice);
        } else {
            V v = ys.get(0);
            int pos = 0;
            Iterator<V> it = xs.iterator();

            while (it.hasNext() && !it.next().equals(v)) pos++;
            xs.remove(pos);

            for (V vertice : ys) {
                xs.insert(pos, vertice);
                pos++;
            }
        }
    }

    // J.5
    private static <V> V vertexInCommon(Graph<V> g, List<V> cycle) {
        Set<V> vertices = g.vertices();
        Iterator<V> it = cycle.iterator();

        V v = null;
        boolean encontrado = false;

        while(it.hasNext() && !encontrado){
            V elem = it.next();
            if(vertices.isElem(elem)){
                v = elem;
                encontrado = true;
            }
        }
        return v;
    }

    // J.6
    private static <V> List<V> eulerianCycle(Graph<V> g) {
        if(!isEulerian(g)) return null;

        V v = g.vertices().iterator().next();
        List <V>  euleriano = new LinkedList<>();
        while(!g.isEmpty()){
        List<V> parcial = extractCycle(g,v);
        connectCycles(euleriano,parcial);
        if(!g.isEmpty()) v = vertexInCommon(g,euleriano);
        }

        return euleriano;
    }
}
