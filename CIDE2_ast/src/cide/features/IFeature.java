package cide.features;

import java.io.Serializable;

public interface IFeature extends Serializable, Comparable<IFeature> {

	public abstract long getId();

	public abstract String toString();

	public abstract int compareTo(IFeature o);

	public abstract boolean equals(Object obj);

	public abstract int hashCode();

}