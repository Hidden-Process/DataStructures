package dataStructures.doubleEndedQueue;

public class LinkedDoubleEndedQueue<T> implements DoubleEndedQueue<T> {

    private static class Node<E> {
        private E elem;
        private Node<E> next;
        private Node<E> prev;

        public Node(E x, Node<E> nxt, Node<E> prv) {
            elem = x;
            next = nxt;
            prev = prv;
        }
    }

    private Node<T> first, last;

    /**
     *  Invariants:
     *  if queue is empty then both first and last are null
     *  if queue is non-empty:
     *      * first is a reference to first node and last is ref to last node
     *      * first.prev is null
     *      * last.next is null
     *      * rest of nodes are doubly linked
     */

    /**
     * Complexity: O(1)
     */
    public LinkedDoubleEndedQueue() {
        first = null;
        last = null;
    }

    /**
     * Complexity: O(1)
     */
    @Override
    public boolean isEmpty() {
        return first == null && last == null;
    }

    /**
     * Complexity: O(1)
     */
    @Override
    public void addFirst(T x) {
        Node <T> node = new Node<>(x, first, null);
        if(isEmpty()) {
            first = node;
            last = node;
        } else {
            first.prev = node;
            first = node;
        }
    }

    /**
     * Complexity: O(1)
     */
    @Override
    public void addLast(T x) {
        Node <T> node = new Node<>(x,last,null);
        if(isEmpty()){
            first = node;
            last = node;
        } else {
            last.prev = node;
            last = node;
        }

    }

    /**
     * Complexity: O(1)
     */
    @Override
    public T first() {
        return first.elem;
    }

    /**
     * Complexity: O(1)
     */
    @Override
    public T last() {
        return last.elem;
    }

    /**
     * Complexity: O(1)
     */
    @Override
    public void deleteFirst() {
        if(first == last) {
            first = null;
            last = null;
        } else {
            first = first.next;
            first.prev = null;
        }
    }

    /**
     * Complexity: O(1)
     */
    @Override
    public void deleteLast() {
        if(first == last){
            first = null;
            last = null;
        } else {
            last = last.next;
            last.prev = null;
        }
    }

    /**
     * Returns representation of queue as a String.
     */
    @Override
    public String toString() {
    String className = getClass().getName().substring(getClass().getPackage().getName().length()+1);
        String s = className+"(";
        for (Node<T> node = first; node != null; node = node.next)
            s += node.elem + (node.next != null ? "," : "");
        s += ")";
        return s;
    }
}
