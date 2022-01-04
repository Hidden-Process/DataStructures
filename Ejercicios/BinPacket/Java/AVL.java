import dataStructures.list.List;
import dataStructures.list.ArrayList;
import java.util.Iterator;


class Bin {
    private int remainingCapacity; // capacity left for this bin
    private List<Integer> weights; // weights of objects included in this bin

    public Bin(int initialCapacity) {
        remainingCapacity = initialCapacity;
        weights = new ArrayList<>();
    }

    // returns capacity left for this bin
    public int remainingCapacity() {
        return remainingCapacity;
    }

    // adds a new object to this bin
    public void addObject(int weight) throws IllegalArgumentException {
        if(weight > remainingCapacity){
            throw new IllegalArgumentException ("The object is too heavy for the bin");
        }

        weights.append(weight);
        remainingCapacity -= weight;
    }

    // returns an iterable through weights of objects included in this bin
    public Iterable<Integer> objects() {

      return new Iterable<Integer>() {

           @Override
            public Iterator<Integer> iterator(){

                return new Iterator<Integer>() {

                    int numElem= 0;

                    @Override
                    public boolean hasNext(){
                        return numElem < weights.size();
                    }

                    @Override
                    public Integer next(){
                       int elem = weights.get(numElem);
                       numElem++;
                       return elem;
                    }
                };
            }
        };
    }

    public String toString() {
        String className = getClass().getSimpleName();
        StringBuilder sb = new StringBuilder(className);
        sb.append("(");
        sb.append(remainingCapacity);
        sb.append(", ");
        sb.append(weights.toString());
        sb.append(")");
        return sb.toString();
    }
}

// Class for representing an AVL tree of bins
public class AVL {
    static private class Node {
        Bin bin; // Bin stored in this node
        int height; // height of this node in AVL tree
        int maxRemainingCapacity; // max capacity left among all bins in tree rooted at this node
        Node left, right; // left and right children of this node in AVL tree

        // recomputes height of this node
        void setHeight() {
          height = 1 +  Math.max(height(left), height(right));
        }

        // recomputes max capacity among bins in tree rooted at this node
        void setMaxRemainingCapacity() {
            maxRemainingCapacity = Math.max(maxRemainingCapacity(left), 
            Math.max(maxRemainingCapacity(right), bin.remainingCapacity()));

        }

        // left-rotates this node. Returns root of resulting rotated tree
        Node rotateLeft() {
            Node x = this.right;
            Node r1 = x.left;
            this.right = r1;
            this.setHeight();
            this.setMaxRemainingCapacity();

            x.left = this;
            x.setHeight();
            x.setMaxRemainingCapacity();

            return x;
        }
    }

    private static int height(Node node) {
        return (node == null) ? 0 : node.height;
    }

    private static int maxRemainingCapacity(Node node) {
        return (node == null) ? 0 : node.maxRemainingCapacity;
    }

    private Node root; // root of AVL tree

    public AVL() {
        this.root = null;
    }

    // adds a new bin at the end of right spine.
    public void addNewBin(Bin bin) {
        root = addNewBin(root, bin);
    }
    
    private static Node addNewBin(Node node, Bin bin) {
        if(node == null){
            node = new Node();
            node.bin = bin;
            node.height = 1;
            node.maxRemainingCapacity = bin.remainingCapacity();
            node.left = null;
            node.right = null;
        } else {
            node.right = addNewBin(node.right, bin);
            if(height(node.right) - height(node.left) > 1) node = node.rotateLeft();
            else {
                node.setHeight();
                node.setMaxRemainingCapacity();
            }
        }
        return node;       
    }

    // adds an object to first suitable bin. Adds
    // a new bin if object cannot be inserted in any existing bin
    public void addFirst(int initialCapacity, int weight) throws Exception  {
        if(maxRemainingCapacity(root) < weight  || root == null){
            Bin bin = new Bin(initialCapacity);
            bin.addObject(weight);
            addNewBin(bin);
        } else addFirst(root,weight);
    }

    private void addFirst(Node node, int weight) throws Exception{
        if(maxRemainingCapacity(node.left) >= weight) addFirst(node.left, weight);
        else if (node.bin.remainingCapacity() >= weight) node.bin.addObject(weight);
        else addFirst(node.right, weight);
        node.setMaxRemainingCapacity();
    }

    public void addAll(int initialCapacity, int[] weights) throws Exception {
        for(int w : weights) addFirst(initialCapacity, w);
    }

    public List<Bin> toList() {
        return toList(root);
    }

    private List<Bin> toList(Node node){
        if(node == null) return new ArrayList<>();
        else {
            List<Bin> bins = toList(node.left);
            bins.append(node.bin);
            for(Bin b : toList(node.right)) bins.append(b);
            return bins;
        }
    } 

    public String toString() {
        String className = getClass().getSimpleName();
        StringBuilder sb = new StringBuilder(className);
        sb.append("(");
        stringBuild(sb, root);
        sb.append(")");
        return sb.toString();
    }

    private static void stringBuild(StringBuilder sb, Node node) {
        if(node==null)
            sb.append("null");
        else {
            sb.append(node.getClass().getSimpleName());
            sb.append("(");
            sb.append(node.bin);
            sb.append(", ");
            sb.append(node.height);
            sb.append(", ");
            sb.append(node.maxRemainingCapacity);
            sb.append(", ");
            stringBuild(sb, node.left);
            sb.append(", ");
            stringBuild(sb, node.right);
            sb.append(")");
        }
    }
}

class LinearBinPacking {
    public static List<Bin> linearBinPacking(int initialCapacity, List<Integer> weights) {
       /* Para cada objeto iteramos sobre los cubos disponibles si cabe lo metemos ahi
          Si no cabe creamos un nuevo cubo con ese objeto y lo añadimos a la lista. */
        List <Bin> bins = new ArrayList<>();
        for(int w : weights){
            Iterator <Bin> it = bins.iterator();
            boolean fit = false;
            while(!fit && it.hasNext()){
                Bin bin = it.next();
                if(bin.remainingCapacity() >= w ){
                    bin.addObject(w);
                    fit = true;
                }
            }
            if(!fit){
                Bin bin = new Bin(initialCapacity);
                bin.addObject(w);
                bins.append(bin);
            }
        }
        return bins;
    }
	
	public static Iterable<Integer> allWeights(Iterable<Bin> bins) {
        List <Integer> w = new ArrayList<>();
        for(Bin b : bins){
            for(int obj : b.objects()){
                w.append(obj);
            }
        }
        return w;		
	}
}