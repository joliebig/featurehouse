









package net.sf.jabref.wizard.text ;

import java.util.*;
import javax.swing.* ;
import javax.swing.text.*;


public class TagToMarkedTextStore
{
  private class TMarkedStoreItem
  {
    int start ;
    int end ;
  }

  private HashMap tagMap ;

  public TagToMarkedTextStore()
  {
    tagMap = new HashMap(10) ;
  }

  
  public void appendPosition(String tag, int start, int end)
  {
    LinkedList ll ;
    Object dummy = tagMap.get(tag) ;
    if (dummy == null)
    {
      ll = new LinkedList() ;
      tagMap.put(tag, ll) ;
    }
    else
    {
      ll = (LinkedList) dummy ;
    }

    TMarkedStoreItem item = new TMarkedStoreItem() ;
    ll.add(item);
    item.end = end ;
    item.start = start ;
  }

  
  public void insertPosition(String tag, int start, int end)
  {
    LinkedList ll ;
    Object dummy = tagMap.get(tag) ;
    if (dummy == null)
    {
      ll = new LinkedList() ;
      tagMap.put(tag, ll) ;
    }
    else
    {
      ll = (LinkedList) dummy ;
      ll.clear();
    }

    TMarkedStoreItem item = new TMarkedStoreItem() ;
    ll.add(item);
    item.end = end ;
    item.start = start ;
  }

  
  public void setStyleForTag(String tag, String style, StyledDocument doc)
  {
    Object dummy = tagMap.get(tag) ;
    if (dummy != null)
    {
      LinkedList ll = (LinkedList) dummy ;

      
      for (ListIterator lIt = ll.listIterator() ; lIt.hasNext() ; )
      {
        Object du2 = lIt.next() ;
        if (du2 != null)
        {
          TMarkedStoreItem item = ( TMarkedStoreItem ) du2 ;
          doc.setCharacterAttributes( item.start, item.end - item.start,
                                      doc.getStyle( style ), true ) ;
        }
      }
    }
  }


}
