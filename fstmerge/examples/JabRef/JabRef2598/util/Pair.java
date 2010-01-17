package net.sf.jabref.util; 

import java.util.*; 

import net.sf.jabref.Util; 


public  class  Pair <P, V> {
	

	
	public P p;

	

	
	public V v;

	

	
	public Pair(P p, V v) {
		this.p = p;
		this.v = v;
	}


	

	
	public static <P extends Comparable<P>, V> Comparator<Pair<P, V>> pCompare() {
		return new Comparator<Pair<P, V>>() {
			public int compare(Pair<P, V> arg0, Pair<P, V> arg1) {
				return arg0.p.compareTo(arg1.p);
			}
		};
	}


	

	
	public static <P> Comparator<? super Pair<P, ?>> pCompare(
		final Comparator<P> comp) {
		return new Comparator<Pair<P, ?>>() {
			public int compare(Pair<P, ?> arg0, Pair<P, ?> arg1) {
				return comp.compare(arg0.p, arg1.p);
			}
		};
	}


	

	
	public Pair<V, P> flip() {
		return new Pair<V, P>(v, p);
	}


	

	
	public static <P, V> List<Pair<V, P>> flipList(List<Pair<P, V>> list) {
		LinkedList<Pair<V, P>> result = new LinkedList<Pair<V, P>>();
		for (Pair<P, V> pair : list)
			result.add(pair.flip());
		return result;
	}


	

	
	public static <P extends Comparable<P>, V> List<Pair<P, Set<V>>> disjointPartition(
		List<Pair<P, V>> list) {

		List<Pair<P, Set<V>>> result = new LinkedList<Pair<P, Set<V>>>();

		Comparator<Pair<P, V>> c = Pair.pCompare();
		Collections.sort(list, Collections.reverseOrder(c));

		Iterator<Pair<P, V>> i = list.iterator();

		Set<V> vs;

		if (i.hasNext()) {
			Pair<P, V> first = i.next();
			P last = first.p;
			vs = new HashSet<V>();
			vs.add(first.v);

			while (i.hasNext()) {
				Pair<P, V> next = i.next();
				if (last.compareTo(next.p) == 0) {
					vs.add(next.v);
				} else {
					result.add(new Pair<P, Set<V>>(last, vs));
					vs = new HashSet<V>();
					last = next.p;
					vs.add(next.v);
				}
			}
			result.add(new Pair<P, Set<V>>(last, vs));
		}
		return result;

	}


	

	
	public static <V extends Comparable<V>> Comparator<? super Pair<?, V>> vCompare() {
		return new Comparator<Pair<?, V>>() {
			public int compare(Pair<?, V> arg0, Pair<?, V> arg1) {
				return arg0.v.compareTo(arg1.v);
			}
		};
	}


	

	
	public static <V> Comparator<? super Pair<?,V>> vCompare(
		final Comparator<V> vComp) {
		return new Comparator<Pair<?, V>>() {
			public int compare(Pair<?, V> arg0, Pair<?, V> arg1) {
				return vComp.compare(arg0.v, arg1.v);
			}
		};
	}


	

	
	public static <P, V> List<Pair<P, V>> zip(List<P> ps, List<V> vs) {

		List<Pair<P, V>> result = new LinkedList<Pair<P, V>>();

		Iterator<P> pI = ps.iterator();
		Iterator<V> vI = vs.iterator();

		while (pI.hasNext()) {
			V nextV = (vI.hasNext() ? vI.next() : null);
			result.add(new Pair<P, V>(pI.next(), nextV));
		}
		while (vI.hasNext()) {
			result.add(new Pair<P, V>(null, vI.next()));
		}
		return result;
	}


	

	
	public static <P> List<P> pList(List<? extends Pair<P, ?>> list) {

		List<P> result = new LinkedList<P>();

		for (Pair<P, ?> pair : list) {
			result.add(pair.p);
		}
		return result;
	}


	

	
	public static <V> List<V> vList(List<? extends Pair<?, V>> list) {

		List<V> result = new LinkedList<V>();

		for (Pair<?, V> pair : list) {
			result.add(pair.v);
		}
		return result;
	}


	

	
	
	public static <V> Iterator<V> iteratorV(final Iterator<? extends Pair<?, V>> iterator){
		return new Iterator<V>(){
			public boolean hasNext() {
				return iterator.hasNext();
			}

			public V next() {
				return iterator.next().v;
			}

			public void remove() {
				iterator.remove();
			}
		};
	}


	
	
	
	public static <V> Iterable<V> iterableV(final Iterable<? extends Pair<?, V>> iterable){
		return new Iterable<V>(){
			public Iterator<V> iterator() {
				return iteratorV(iterable.iterator());
			}
		};
	}


	
	
	
	public static <P> Iterator<P> iteratorP(final Iterator<? extends Pair<P,?>> iterator){
		return new Iterator<P>(){
			public boolean hasNext() {
				return iterator.hasNext();
			}

			public P next() {
				return iterator.next().p;
			}

			public void remove() {
				iterator.remove();
			}
		};
	}


	
	
	
	public static <P> Iterable<P> iterableP(final Iterable<? extends Pair<P,?>> iterable){
		return new Iterable<P>(){
			public Iterator<P> iterator() {
				return iteratorP(iterable.iterator());
			}
		};
	}


	
	
	
	public String toString() {
		return new StringBuffer().append('<').append(p).append(',').append(v)
			.append('>').toString();
	}


	

	@Override
	public int hashCode() {
		return (this.p == null ? 0 : this.p.hashCode())
			| (this.v == null ? 0 : this.v.hashCode());
	}


	

	
	public boolean equals(Object o) {
		if (!(o instanceof Pair))
			return false;

		Pair<?, ?> other = (Pair<?, ?>) o;
		return Util.equals(this.p, other.p) && Util.equals(this.v, other.v);
	}



}
