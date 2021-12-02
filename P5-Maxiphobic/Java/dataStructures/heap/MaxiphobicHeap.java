package dataStructures.heap;

/**
 * Heap implemented using maxiphobic heap-ordered binary trees.
 * @param <T> Type of elements in heap.
 */

public class MaxiphobicHeap<T extends Comparable<? super T>> implements	Heap<T> {

	private static class Tree<E> {
		private E elem;
		private int size;
		private Tree<E> left;
		private Tree<E> right;
	}

	private static int size(Tree<?> heap) {
		return heap == null ? 0 : heap.size;
	}

	private static <T extends Comparable<? super T>> Tree<T> merge(Tree<T> h1,	Tree<T> h2) {
		if (h1 == null)
			return h2;
		if (h2 == null)
			return h1;

		T x1 = h1.elem;
		T x2 = h2.elem;

		if(x1.compareTo(x2)<=0) return node(x1,h1.left,h1.right,h2);
		else return node(x2,h1,h2.left,h2.right);

	}

	private static <T extends Comparable<? super T>> Tree<T> node(T x, Tree<T> h1, Tree<T> h2, Tree<T> h3){
		int w1 = size(h1);
		int w2 = size(h2);
		int w3 = size(h3);

		Tree<T> tree = new Tree<>();

		tree.elem = x;
		tree.size = w1 + w2 + w3 + 1;

		if(w1 >= w2 && w1 >= w3){
			tree.left = h1;
			tree.right = merge(h2,h3);
		} else if( w2 >= w1 && w2 >= w3){
			tree.left = h2;
			tree.right = merge(h1,h3);
		} else {
			tree.left = h3;
			tree.right = merge(h1,h2);
		}

		return tree;
	}

	private Tree<T> root;

	// copies a tree
	private static <T extends Comparable<? super T>> Tree<T> copy(Tree<T> h) {
		if (h==null)
			return null;
		else {
			Tree<T> h1 = new Tree<>();
			h1.elem = h.elem;
			h1.size = h.size;
			h1.left = copy(h.left);
			h1.right = copy(h.right);			
			return h1;		
		}
	}

	/**
	 * Creates an empty Maxiphobic Heap.
	 * <p>Time complexity: O(1)
	 */
	public MaxiphobicHeap() {
		root = null;
	}

	/**
	 * Copy constructor for Maxiphobic Heap. 
	 * <p>Time complexity: O(n)
	 */	
	public MaxiphobicHeap(MaxiphobicHeap<T> h) {
		root = copy(h.root);
	}
	

	/**
	 * {@inheritDoc}
	 * <p>Time complexity: O(1)
	 */
	public boolean isEmpty() {
		return root == null;
	}

	/**
	 * {@inheritDoc}
	 * <p>Time complexity: O(1)
	 */
	public int size() {
		return root == null ? 0 : root.size;
	}

	/**
	 * {@inheritDoc}
	 * <p>Time complexity: O(1)
	 * @throws <code>EmptyHeapException</code> if heap stores no element.
	 */
	public T minElem() {
		if (isEmpty())
			throw new EmptyHeapException("minElem on empty heap");
		else
			return root.elem;
	}

	/**
	 * {@inheritDoc}
	 * <p>Time complexity: O(log n)
	 * @throws <code>EmptyHeapException</code> if heap stores no element.
	 */
	public void delMin() {
		if (isEmpty())
			throw new EmptyHeapException("delMin on empty heap");
		else
			root = merge(root.left, root.right);
	}

	/**
	 * {@inheritDoc}
	 * <p>Time complexity: O(log n)
	 */
	public void insert(T value) {
		Tree<T> newHeap = new Tree<T>();
		newHeap.elem = value;
		newHeap.size = 1;
		newHeap.left = null;
		newHeap.right = null;

		root = merge(root, newHeap);
	}

	public void clear() {
		root = null;
	}

	private static String toStringRec(Tree<?> tree) {
		return tree == null ? "null" : "Node<" + toStringRec(tree.left) + ","
				+ tree.elem + "," + toStringRec(tree.right) + ">";
	}

	/**
	 * Returns representation of heap as a String.
	 */
  @Override public String toString() {
    String className = getClass().getName().substring(getClass().getPackage().getName().length()+1);

  	return className+"("+toStringRec(this.root)+")";
  }

}
