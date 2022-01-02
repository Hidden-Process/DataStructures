package dataStructures.set;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import dataStructures.dictionary.AVLDictionary;
import dataStructures.dictionary.Dictionary;
import dataStructures.list.ArrayList;
import dataStructures.list.List;

public class DisjointSetDictionary<T extends Comparable<? super T>> implements DisjointSet<T> {

    private Dictionary<T, T> dic;

    /**
     * Inicializa las estructuras necesarias.
     */
    public DisjointSetDictionary() {
        dic = new AVLDictionary<>();
    }

    /**
     * Devuelve {@code true} si el conjunto no contiene elementos.
     */
    @Override
    public boolean isEmpty() {
        return dic.isEmpty();
    }

    /**
     * Devuelve {@code true} si {@code elem} es un elemento del conjunto.
     */
    @Override
    public boolean isElem(T elem) {
        return dic.isDefinedAt(elem);
    }

    /**
     * Devuelve el numero total de elementos del conjunto.
     */

    @Override
    public int numElements() {
        return dic.size();
    }

    /**
     * Agrega {@code elem} al conjunto. Si {@code elem} no pertenece al
     * conjunto, crea una nueva clase de equivalencia con {@code elem}. Si
     * {@code elem} pertencece al conjunto no hace nada.
     */
    @Override
    public void add(T elem) {
        if(!isElem(elem)){
            dic.insert(elem, elem);
        }
    }

    /**
     * Devuelve el elemento canonico (la raiz) de la clase de equivalencia la
     * que pertenece {@code elem}. Si {@code elem} no pertenece al conjunto
     * devuelve {@code null}.
     */
    private T root(T elem) {
        if(!isElem(elem)) return null;

        T node = elem;

        while(!dic.valueOf(node).equals(node)) node = dic.valueOf(node);

        return node;
    
    }

    /**
     * Devuelve {@code true} si {@code elem} es el elemento canonico (la raiz)
     * de la clase de equivalencia a la que pertenece.
     */

    private boolean isRoot(T elem) {
        return (dic.valueOf(elem).equals(elem));
    }

    /**
     * Devuelve {@code true} si {@code elem1} y {@code elem2} estan en la misma
     * clase de equivalencia.
     */
    @Override
    public boolean areConnected(T elem1, T elem2) {
        if(!(isElem(elem1) && isElem(elem2))) return false;
        else return(root(elem1).equals(root(elem2)));
    }

    /**
     * Devuelve una lista con los elementos pertenecientes a la clase de
     * equivalencia en la que esta {@code elem}. Si {@code elem} no pertenece al
     * conjunto devuelve la lista vacia.
     */
    @Override
    public List<T> kind(T elem) {
        List<T> list = new ArrayList<>();

       if(isElem(elem)){
           for(T key : dic.keys()){
               if(areConnected(key, elem)) list.append(key);
           }
       }

       return list;

    }

    /**
     * Une las clases de equivalencias de {@code elem1} y {@code elem2}. Si
     * alguno de los dos argumentos no esta en el conjunto lanzara una excepcion
     * {@code IllegalArgumenException}.
     */
    @Override
    public void union(T elem1, T elem2) {
        if(!(isElem(elem1) && isElem(elem2))) {
            throw new IllegalArgumentException("Alguno de los argumento no está en el conjunto");
        }

        T r1 = root(elem1);
        T r2 = root(elem2);

        if(r1.compareTo(r2) > 0) dic.insert(r1,r2);
        else                     dic.insert(r2,r1);
    }

    /**
     * Aplana la estructura de manera que todos los elementos se asocien
     * directamente con su representante canonico.
     */
    @Override
    public void flatten() {
        for(T elem : dic.keys()){
            if(!isRoot(elem)){
                T raiz = root(elem);
                dic.delete(elem);
                dic.insert(elem, raiz);
            }
        }
    }

    /**
     * Devuelve una lista que contiene las clases de equivalencia del conjunto
     * como listas.
     */
    @Override
    public List<List<T>> kinds() {
       
            Set<T> keys = new HashSet<>();
            for(T elem: dic.keys()) {
                keys.add(elem);
            }
            
            T canon = null;

            List<List<T>> res = new ArrayList<>();
    
            while(!keys.isEmpty()) {
                Iterator<T> it = keys.iterator();
                List<T> one = new ArrayList<>();
                
                while(it.hasNext()) {
   
                    T current = it.next();
   
                    if(isRoot(current) && canon == null) {
                        canon = current;
                        one.append(current);
                        
                    }else {
                        if(one.isEmpty()) {
                            one.append(current);
                            one.append(root(current));
                            canon = root(current);
                            
                        }else {
                            if(root(current).equals(canon)) {
                                one.append(current);
                            }
                        }
                    }
                }
                
                res.append(one);
                for(T aur : one) {
                    keys.remove(aur);
                }
                canon = null;
            }
            return res;
        }
    
    /**
     * Devuelve una representacion del conjunto como una {@code String}.
     */
    @Override
    public String toString() {
        return "DisjointSetDictionary(" + dic.toString() + ")";
    }
}
