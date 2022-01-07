import dataStructures.dictionary.AVLDictionary;
import dataStructures.dictionary.Dictionary;
import dataStructures.list.LinkedList;
import dataStructures.list.List;
import dataStructures.priorityQueue.BinaryHeapPriorityQueue;
import dataStructures.priorityQueue.PriorityQueue;
import dataStructures.set.AVLSet;
import dataStructures.set.Set;
import dataStructures.tuple.Tuple2;

public class Huffman {

    // Exercise 1
    public static Dictionary<Character, Integer> weights(String s) {
    	Dictionary <Character, Integer> dict = new AVLDictionary<>();
        Set <Character> set = new AVLSet<>();
        char [] input = s.toCharArray();

        for (char c : input) set.insert(c);
        for (char c : set)   dict.insert(c, frecuency(c, input));
        return dict;
    }

    private static int frecuency(Character c, char [] input){
        int frecuency = 0;
        for (char ch : input){
            if(c.equals(ch)) frecuency++;
        }
        return frecuency;
    }

    // Exercise 2.a
    public static PriorityQueue<WLeafTree<Character>> huffmanLeaves(String s) {
    	PriorityQueue <WLeafTree<Character>> pq = new BinaryHeapPriorityQueue<>();
        Dictionary <Character, Integer> dict = weights(s);
        WLeafTree<Character> tree;
        for(Tuple2 <Character, Integer> t : dict.keysValues()){
            tree = new WLeafTree<Character>(t._1(), t._2());
            pq.enqueue(tree);
        }
    	return pq;
    }

    // Exercise 2.b
    public static WLeafTree<Character> huffmanTree(String s) {
        if(weights(s).size() < 2) throw new HuffmanException("The String must have at least 2 symbols");
        PriorityQueue <WLeafTree<Character>> pq = huffmanLeaves(s);
        WLeafTree <Character> first = pq.first();
        pq.dequeue();
        WLeafTree <Character> tree = new WLeafTree<>(first, pq.first());
        pq.dequeue();
        while(!pq.isEmpty()){
            WLeafTree <Character> temp = tree;
            tree = new WLeafTree<>(temp, pq.first());
            pq.dequeue();
        }
    	return tree;
    }

    // Exercise 3.a
    public static Dictionary<Character, List<Integer>> joinDics(Dictionary<Character, List<Integer>> d1, Dictionary<Character, List<Integer>> d2) {
        Dictionary <Character, List<Integer>> dict = new AVLDictionary<>();
        for(Tuple2 <Character, List<Integer>> t : d1.keysValues()) dict.insert(t._1(), t._2());
        for(Tuple2 <Character, List<Integer>> t : d2.keysValues()) dict.insert(t._1(), t._2());
    	return dict;
    }

    // Exercise 3.b
    public static Dictionary<Character, List<Integer>> prefixWith(int i, Dictionary<Character, List<Integer>> d) {
        Dictionary<Character, List<Integer>> dict = new AVLDictionary<>();
        List <Integer> list = new LinkedList<>();
        for(Character c : d.keys()){
            list = d.valueOf(c);
            list.prepend(i);
            dict.insert(c, list);
        }
    	return dict;
    }

    // Exercise 3.c
    public static Dictionary<Character, List<Integer>> huffmanCode(WLeafTree<Character> ht) {
        Dictionary<Character, List<Integer>> dict = new AVLDictionary<>();
        if(ht.isLeaf()) {
            List <Integer> list = new LinkedList<>();
            dict.insert(ht.elem(), list);
        } else dict = joinDics(prefixWith(0, huffmanCode(ht.leftChild())), prefixWith(1, huffmanCode(ht.rightChild())));
        return dict;
    }

    // Exercise 4
    public static List<Integer> encode(String s, Dictionary<Character, List<Integer>> hc) {
        List <Integer> code = new LinkedList<>();
        for(Character c : s.toCharArray()){
            List <Integer> tmp = hc.valueOf(c);
            for(Integer val : tmp) code.append(val);
        }
    	return code;
    }

    // Exercise 5
    public static String decode(List<Integer> bits, WLeafTree<Character> ht) {
        String msg = "";
        WLeafTree <Character> tree = ht;

        for(Integer bit : bits){
            if(bit == 0) tree = tree.leftChild();
            else         tree = tree.rightChild();
            
            if(tree.isLeaf()){
                msg += tree.elem();
                tree = ht;
            }
        }
    	return msg;
    }
}
