package com.horstmann.violet;

public class UseCaseDiagramGraph
{
   static
   {
      ClassRelationshipEdge includeRel =
         new ClassRelationshipEdge();
      includeRel.setBentStyle(BentStyle.STRAIGHT);
      includeRel.setLineStyle(LineStyle.DOTTED);
      includeRel.setEndArrowHead(ArrowHead.V);
      includeRel.setMiddleLabel("\u00ABinclude\u00BB");
      EDGE_PROTOTYPES[2] = includeRel;
   }
}





