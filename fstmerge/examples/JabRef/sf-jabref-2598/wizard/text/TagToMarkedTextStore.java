







package net.sf.jabref.wizard.text ;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.ListIterator;

import javax.swing.text.StyledDocument;


public class TagToMarkedTextStore
{
  private class TMarkedStoreItem
  {
    int start ;
    int end ;
  }

  private HashMap<String, LinkedList<TMarkedStoreItem>> tagMap ;

  public TagToMarkedTextStore()
  {
    tagMap = new HashMap<String, LinkedList<TMarkedStoreItem>>(10) ;
  }

  
	public void appendPosition(String tag, int start, int end) {
		LinkedList<TMarkedStoreItem> ll = tagMap.get(tag);
		if (ll == null) {
			ll = new LinkedList<TMarkedStoreItem>();
			tagMap.put(tag, ll);
		}

		TMarkedStoreItem item = new TMarkedStoreItem();
		ll.add(item);
		item.end = end;
		item.start = start;
	}

  
	public void insertPosition(String tag, int start, int end) {
		LinkedList<TMarkedStoreItem> ll = tagMap.get(tag);

		if (ll == null) {
			ll = new LinkedList<TMarkedStoreItem>();
			tagMap.put(tag, ll);
		} else {
			ll.clear();
		}

		TMarkedStoreItem item = new TMarkedStoreItem();
		ll.add(item);
		item.end = end;
		item.start = start;
	}

  
	public void setStyleForTag(String tag, String style, StyledDocument doc) {
		LinkedList<TMarkedStoreItem> ll = tagMap.get(tag);

		if (ll != null) {
			
			for (ListIterator<TMarkedStoreItem> lIt = ll.listIterator(); lIt
				.hasNext();) {
				Object du2 = lIt.next();
				if (du2 != null) {
					TMarkedStoreItem item = (TMarkedStoreItem) du2;
					doc.setCharacterAttributes(item.start, item.end
						- item.start, doc.getStyle(style), true);
				}
			}
		}
	}

}
