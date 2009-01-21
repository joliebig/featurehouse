
package org.jhotdraw.util; 
import java.util.*; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.standard.*; 
import java.awt.*; 
public  class  GraphLayout  extends FigureChangeAdapter {
		public double LENGTH_FACTOR=1.0;

		public double REPULSION_STRENGTH=0.5;

		public double REPULSION_LIMIT=200.0;

		int REPULSION_TYPE=0;

		public double SPRING_STRENGTH=0.1;

		public double TORQUE_STRENGTH=0.25;

		public double FRICTION_FACTOR=0.75;

		private Hashtable nodes = new Hashtable(), edges = new Hashtable();

		public GraphLayout() {}

		private GraphNode getGraphNode(Figure node) { return (GraphNode)nodes.get(node);	}

		private double len(Figure edge) { return ((Double)edges.get(edge)).doubleValue()*LENGTH_FACTOR;	}

		public void addNode(Figure node) { nodes.put(node, new GraphNode(node)); node.addFigureChangeListener(this);	}

		public void addEdge(ConnectionFigure edge, int addlen) { Dimension d1 = edge.getStartConnector().owner().size(); Dimension d2 = edge.getEndConnector().owner().size(); int len = Math.max(d1.width,d1.height)/2 +	Math.max(d2.width,d2.height)/2 + addlen; edges.put(edge, new Double(len));	}

		public synchronized void relax() { if (nodes == null) return; Enumeration edgeEnum = edges.keys(); while (edgeEnum.hasMoreElements()) { ConnectionFigure e = (ConnectionFigure)edgeEnum.nextElement(); double targetlen = len(e); GraphNode from = getGraphNode(e.getStartConnector().owner()); GraphNode to = getGraphNode(e.getEndConnector().owner()); double vx = to.x - from.x; double vy = to.y - from.y; double len = Math.sqrt(vx * vx + vy * vy); if (len>0) { double f = SPRING_STRENGTH * (targetlen - len) / len; double dx = f * vx; double dy = f * vy; double phi=Math.atan2(vx,vy); double dir=-Math.sin(4*phi); dx += TORQUE_STRENGTH*vy*dir/len; dy += -TORQUE_STRENGTH*vx*dir/len; to.dx += dx; to.dy += dy; from.dx += -dx; from.dy += -dy;	}	}	Enumeration nodeEnum1 = nodes.elements();	while (nodeEnum1.hasMoreElements()) {	GraphNode n1 = (GraphNode)nodeEnum1.nextElement();	double dx = 0;	double dy = 0;	Enumeration nodeEnum2 = nodes.elements();	while (nodeEnum2.hasMoreElements()) { GraphNode n2 = (GraphNode)nodeEnum2.nextElement(); if (n1 == n2) { continue; } double vx = n1.x - n2.x; double vy = n1.y - n2.y; double lensqr = vx * vx + vy * vy; double len = Math.sqrt(lensqr); if (len == 0) { dx += REPULSION_STRENGTH * Math.random(); dy += REPULSION_STRENGTH * Math.random(); } else if (len < REPULSION_LIMIT) { vx=vx/REPULSION_LIMIT; vy=vy/REPULSION_LIMIT; len=len/REPULSION_LIMIT; double f=0; switch (REPULSION_TYPE) { case 0: f = 0.5 * (1 - len) / len;	break; case 1: f = 1 - len;	break; case 2: f = 2 * (1 - len) * (1 - len);	break; }	f *= REPULSION_STRENGTH;	dx += f * vx;	dy += f * vy;	} } n1.dx += dx; n1.dy += dy; } Enumeration nodeEnum = nodes.keys(); while (nodeEnum.hasMoreElements()) { Figure node = (Figure)nodeEnum.nextElement(); GraphNode n = getGraphNode(node); if (!Boolean.TRUE.equals(node.getAttribute(FigureAttributeConstant.LOCATION))) {	n.x += Math.max(-5, Math.min(5, n.dx));	n.y += Math.max(-5, Math.min(5, n.dy));	Point c = node.center();	node.moveBy((int)Math.round(n.x)-c.x,	(int)Math.round(n.y)-c.y);	if (n.x < 0) {	n.x = 0;	}	if (n.y < 0) {	n.y = 0;	} } n.dx *= FRICTION_FACTOR; n.dy *= FRICTION_FACTOR; }	}

		synchronized public void figureChanged(FigureChangeEvent e) { if (nodes!=null) { Figure node = e.getFigure(); if (nodes.containsKey(node)) { getGraphNode(node).update(); } }	}

		public void remove() { if (nodes!=null) { Enumeration nodeEnum = nodes.keys(); while (nodeEnum.hasMoreElements()) {	Figure node = (Figure)nodeEnum.nextElement();	node.removeFigureChangeListener(this); } nodes = null; edges = null; }	}


} 
 
class  GraphNode {
		double x=0.0, y=0.0;

		double dx=0.0;

		double dy=0.0;

		final Figure node;

		GraphNode(Figure newNode) { node = newNode; update();	}

		void update() { Point p = node.center(); if (Math.abs(p.x - Math.round(x))>1 || Math.abs(p.y - Math.round(y))>1) { x = p.x; y = p.y; }	}


}
