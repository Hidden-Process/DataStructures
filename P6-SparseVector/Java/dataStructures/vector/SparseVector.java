package dataStructures.vector;

import java.util.Iterator;
import java.util.NoSuchElementException;

public class SparseVector<T> implements Iterable<T> {

    public interface Tree<T> {

        T get(int sz, int i);

        Tree<T> set(int sz, int i, T x);
    }

    // Unif Implementation

    public static class Unif<T> implements Tree<T> {

        private T elem;

        public Unif(T e) {
            elem = e;
        }

        @Override
        public T get(int sz, int i) {
            return elem;
        }

        @Override
        public Tree<T> set(int sz, int i, T x) {
           if(elem.equals(x)) return this;
           else if(sz == 1)   return new Unif<T>(x);
           else {
               Node<T> node;
               if(i < sz/2) node = new Node<>(set(sz/2, i, x),this);
               else         node = new Node<>(this, set(sz / 2, i - (sz/2) , x));
               return node;
           }
        }

        @Override
        public String toString() {
            return "Unif(" + elem + ")";
        }
    }

    // Node Implementation

    public static class Node<T> implements Tree<T> {

        private Tree<T> left, right;

        public Node(Tree<T> l, Tree<T> r) {
            left = l;
            right = r;
        }

        @Override
        public T get(int sz, int i) {
            if(i < sz/2) return left.get(sz/2, i);
            else         return right.get(sz/2, i - (sz/2));
        }

        @Override
        public Tree<T> set(int sz, int i, T x) {
            if(i < sz/2) left  = left.set(sz/2, i, x);
            else         right = right.set(sz/2, i - (sz/2), x);

            simplify();

            return this;
        }

        public Tree<T> simplify() {
            if(left instanceof Unif<?> && right instanceof Unif<?> && left.get(1, 0) == right.get(1, 0)){
                return left;
            }
            return this;
        }

        @Override
        public String toString() {
            return "Node(" + left + ", " + right + ")";
        }
    }

    // SparseVector Implementation

    private int size;
    private Tree<T> root;

    public SparseVector(int n, T elem) {
        if(n < 0) throw new VectorException("Error: Tamaño del vector negativo");
        size = (int) Math.pow(2, n);
        root = new Unif<T>(elem);
    }

    public int size() {
        return size;
    }

    public T get(int i) {
       if(i  <0 || i > (size-1)) throw new VectorException("Error: Indice no válido");
       return root.get(size, i);
    }

    public void set(int i, T x) {
        if(i  <0 || i > (size-1)) throw new VectorException("Error: Indice no válido");
        root = root.set(size, i, x);
    }

    @Override
    public Iterator<T> iterator() {
        return new SparseVectorIterator();
    }

    private class SparseVectorIterator implements Iterator<T>{

        int ind;

        public SparseVectorIterator(){
            ind = 0;
        }

        @Override
        public boolean hasNext(){
            return ind < size;
        }

        @Override
        public T next(){
            if(!hasNext()) throw new NoSuchElementException();
            T elem = root.get(size, ind);
            ind++;
            return elem;
        }
    }

    @Override
    public String toString() {
        return "SparseVector(" + size + "," + root + ")";
    }
}
