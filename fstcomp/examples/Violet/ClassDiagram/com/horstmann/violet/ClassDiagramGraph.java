/*
Violet - A program for editing UML diagrams.

Copyright (C) 2002 Cay S. Horstmann (http://horstmann.com)

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

package com.horstmann.violet;

import java.awt.geom.Point2D;

import com.horstmann.violet.framework.Edge;
import com.horstmann.violet.framework.Graph;
import com.horstmann.violet.framework.Node;

/**
   A UML class diagram.
*/
public class ClassDiagramGraph extends Graph
{

   public boolean connect(Edge e, Point2D p1, Point2D p2)
   {
      Node n1 = findNode(p1);
      Node n2 = findNode(p2);
      // if (n1 == n2) return false;
      return super.connect(e, p1, p2);
   }

   public Node[] getNodePrototypes()
   {
      return NODE_PROTOTYPES;
   }

   public Edge[] getEdgePrototypes()
   {
      return EDGE_PROTOTYPES;
   }

   protected static final Node[] NODE_PROTOTYPES = new Node[4];

   protected static final Edge[] EDGE_PROTOTYPES = new Edge[7];

}
