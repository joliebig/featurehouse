package net.sf.jabref.util; 


public  class  TypeOr <S, T> {
	

	public S s;

	

	public T t;

	

	public TypeOr(S s, T t) {
		if (!(s == null ^ t == null))
			throw new IllegalArgumentException("Either s or t need to be null");

		this.s = s;
		this.t = t;
	}


	

	public boolean isS() {
		return s != null;
	}


	

	public boolean isT() {
		return t != null;
	}



}
