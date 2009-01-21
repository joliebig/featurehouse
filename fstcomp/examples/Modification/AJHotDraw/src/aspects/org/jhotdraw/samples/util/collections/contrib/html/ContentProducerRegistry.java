
package org.jhotdraw.contrib.html; 
import java.io.Serializable; 
import java.net.URL; 
import java.util.Hashtable; 
import java.util.Iterator; 
import java.util.Map; 
public  class  ContentProducerRegistry  implements Serializable {
		private Hashtable fContentProducers = new Hashtable();

		private transient ContentProducerRegistry fParent;

		private static ContentProducerRegistry fDefaultRegistry =	new ContentProducerRegistry(null);

		static {	fDefaultRegistry.registerContentProducer(URL.class, new URLContentProducer());	}

		public ContentProducerRegistry() {	setParent(fDefaultRegistry);	}

		public ContentProducerRegistry(ContentProducerRegistry parent) {	setParent(parent);	}

		public void setAutonomous() {	setParent(null);	}

		public boolean isAutonomous() {	return (getParent() == null);	}

		public void setParent(ContentProducerRegistry newParent) {	fParent = newParent;	}

		public ContentProducerRegistry getParent() {	return fParent;	}

		public static ContentProducer registerDefaultContentProducer(Class targetClass, ContentProducer producer) {	return fDefaultRegistry.registerContentProducer(targetClass, producer);	}

		public static void unregisterDefaultContentProducer(Class targetClass, ContentProducer producer) {	fDefaultRegistry.unregisterContentProducer(targetClass, producer);	}

		public static ContentProducer getDefaultContentProducer(Class targetClass) {	return fDefaultRegistry.getContentProducer(targetClass);	}

		public static ContentProducer getExactDefaultContentProducer(Class targetClass) {	return fDefaultRegistry.getExactContentProducer(targetClass);	}

		public ContentProducer registerContentProducer(Class targetClass, ContentProducer producer) {	ContentProducer previousProducer = getContentProducer(targetClass);	fContentProducers.put(targetClass, producer);	return previousProducer;	}

		public void unregisterContentProducer(Class targetClass, ContentProducer producer) {	ContentProducer currentProducer = getContentProducer(targetClass);	if (currentProducer == producer) {	fContentProducers.remove(targetClass);	}	}

		public ContentProducer getContentProducer(Class targetClass) {	ContentProducer producer = getExactContentProducer(targetClass);	if (producer != null) {	return producer;	}	return getSuperClassContentProducer(targetClass, null);	}

		public ContentProducer getExactContentProducer(Class targetClass) {	ContentProducer producer = (ContentProducer)fContentProducers.get(targetClass);	if (producer != null) {	return producer;	}	if (!this.isAutonomous()) {	return getParent().getExactContentProducer(targetClass);	}	return null;	}

		protected ContentProducer getSuperClassContentProducer(Class targetClass, Class closestClass) {	Map.Entry entry = null;	Class entryClass = null;	ContentProducer closestProducer = null;	Iterator iter = fContentProducers.entrySet().iterator();	while (iter.hasNext()) {	entry = (Map.Entry)iter.next();	entryClass = (Class)entry.getKey();	if (entryClass.isAssignableFrom(targetClass)) {	if (closestClass != null && closestClass.isAssignableFrom(entryClass)) {	closestClass = entryClass;	closestProducer = (ContentProducer)entry.getValue();	}	}	}	if (!this.isAutonomous()) {	ContentProducer parentProducer =	getParent().getSuperClassContentProducer(targetClass, closestClass);	if (parentProducer != null) {	closestProducer = parentProducer;	}	}	return closestProducer;	}


}
