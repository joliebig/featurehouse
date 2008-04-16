package com.horstmann.violet;

public class ObjectDiagramGraph
{
   static
   {
      FieldNode f = new FieldNode();
      MultiLineString fn = new MultiLineString();
      fn.setText("name");
      f.setName(fn);
      MultiLineString fv = new MultiLineString();
      fv.setText("value");
      f.setValue(fv);
      NODE_PROTOTYPES[1] = f;
   }
}





