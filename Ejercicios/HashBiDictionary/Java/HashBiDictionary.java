package dataStructures.dictionary;

import dataStructures.list.List;
import dataStructures.list.ArrayList;
import dataStructures.set.AVLSet;
import dataStructures.set.Set;
import dataStructures.tuple.Tuple2;
import java.util.Iterator;
import java.util.NoSuchElementException;

public class HashBiDictionary<K,V> implements BiDictionary<K,V>{
	private Dictionary<K,V> bKeys;
	private Dictionary<V,K> bValues;
	
	public HashBiDictionary() {
		bKeys   = new HashDictionary<>();
		bValues = new HashDictionary<>();
	}
	
	public boolean isEmpty() {
		return bKeys.isEmpty();
	}
	
	public int size() {
		return bKeys.size();
	}
	
	public void insert(K k, V v) {
		if(bKeys.isDefinedAt(k)) bKeys.delete(k);
		if(bValues.isDefinedAt(v)) bValues.delete(v);
		bKeys.insert(k, v);
		bValues.insert(v, k);
	}
	
	public V valueOf(K k) {
		return bKeys.valueOf(k);
	}
	
	public K keyOf(V v) {
		return bValues.valueOf(v);
	}
	
	public boolean isDefinedKeyAt(K k) {
		return bKeys.isDefinedAt(k);
	}
	
	public boolean isDefinedValueAt(V v) {
		return bValues.isDefinedAt(v);
	}
	
	public void deleteByKey(K k) {
		if(!bKeys.isDefinedAt(k)) throw new NoSuchElementException("Key doesn't exist");
		bValues.delete(bKeys.valueOf(k));
		bKeys.delete(k);
	}
	
	public void deleteByValue(V v) {
		if(!bValues.isDefinedAt(v)) throw new NoSuchElementException("Value doesn't exist");
		bKeys.delete(bValues.valueOf(v));
		bValues.delete(v);
	}
	
	public Iterable<K> keys() {
		return bKeys.keys();
	}
	
	public Iterable<V> values() {
		return bValues.keys();
	}
	
	public Iterable<Tuple2<K, V>> keysValues() {
		return bKeys.keysValues();
	}

	private static <K,V extends Comparable<? super V>> boolean isInjectiveDict(Dictionary<K, V> dict) {
		Set<V> values = new AVLSet<>();
		Iterator<V> it = dict.values().iterator();

		boolean flag = true;
		while(it.hasNext() && flag){
			V val = it.next();
			if(values.isElem(val)) flag = false;
			else values.insert(val);
		}
		return flag;
	}
	
	public static <K,V extends Comparable<? super V>> BiDictionary<K, V> toBiDictionary(Dictionary<K,V> dict) {
		BiDictionary<K,V> biDic = new HashBiDictionary<>();
		if(isInjectiveDict(dict)){
			for(Tuple2<K,V> tupla : dict.keysValues()) biDic.insert(tupla._1(), tupla._2());
			return biDic;
		} else throw new IllegalArgumentException("The given dictionary is not injective");
		
	}

	public <W> BiDictionary<K, W> compose(BiDictionary<V,W> bdic) {
		BiDictionary<K,W> biDic = new HashBiDictionary<>();
		for(Tuple2<K,V> t : this.keysValues()){
			if(bdic.isDefinedKeyAt(t._2())){
				biDic.insert(t._1(), bdic.valueOf(t._2()));
			}
		}
		return biDic;
	}
		
	public static <K extends Comparable<? super K>> boolean isPermutation(BiDictionary<K,K> bd) {
		Set<K> keys   = new AVLSet<>();
		Set<K> values = new AVLSet<>();

		boolean permutation = true;

		for(Tuple2<K,K> t : bd.keysValues()){
			keys.insert(t._1());
			values.insert((t._2()));
		}

		Iterator<K> s1 = keys.iterator();
		while(s1.hasNext() && permutation){
			K k = s1.next();
			if(!values.isElem(k)) permutation = false;
		}
		return permutation;
	}
	
	public static <K extends Comparable<? super K>> List<K> orbitOf(K k, BiDictionary<K,K> bd) {
		if(!isPermutation(bd)) throw new IllegalArgumentException("The given Bidictionary is not a permutation");
		
		List<K> list = new ArrayList<>();
		list.append(k);

		K key = k;

		boolean orbit = false;
		while(!orbit){
			list.append(bd.valueOf(key));
			key = bd.valueOf(key);
			if(bd.valueOf(key).equals(k)) orbit = true;
		}
		return list;
	}
	
	public static <K extends Comparable<? super K>> List<List<K>> cyclesOf(BiDictionary<K,K> bd) {
		if(!isPermutation(bd)) throw new IllegalArgumentException("The given Bidictionary is not a permutation");
		
		List <List<K>> cycles = new ArrayList<>();
		List <K> subcycles = new ArrayList<>();

		for(K key : bd.keys()) subcycles.append(key);

		while(!subcycles.isEmpty()){
			K key = subcycles.get(0);
			cycles.append(orbitOf(key, bd));
			for(int i = 0; i < subcycles.size(); i++){
				for(int j=0; j<cycles.size();j++){
					for(int k=0; k<cycles.get(j).size();k++){
						if(subcycles.get(i).equals(cycles.get(j).get(k))){
							subcycles.remove(i);
						} 
					}
				}
			} 
		}
		return cycles;
	}
		
	@Override
	public String toString() {
		return "HashBiDictionary [bKeys=" + bKeys + ", bValues=" + bValues + "]";
	}
}
