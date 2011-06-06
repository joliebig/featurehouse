

package com.lowagie.rups.view.itext.treenodes;

import java.util.List;

import org.dom4j.Attribute;
import org.dom4j.Branch;
import org.dom4j.Comment;
import org.dom4j.Document;
import org.dom4j.Element;
import org.dom4j.Namespace;
import org.dom4j.Node;
import org.dom4j.ProcessingInstruction;
import org.dom4j.Text;

import com.lowagie.rups.view.icons.IconFetcher;
import com.lowagie.rups.view.icons.IconTreeNode;

public class XdpTreeNode extends IconTreeNode {

    
    private static final long serialVersionUID = -6431790925424045933L;

    
    public XdpTreeNode(Node node) {
        super(null, node);
        if (node instanceof Element) {
            Element element = (Element)node;
            addChildNodes(element.attributes());
        }
        if (node instanceof Branch) {
            Branch branch = (Branch) node;
            addChildNodes(branch.content());
        }
        if (node instanceof Attribute) {
            icon = IconFetcher.getIcon("attribute.png");
            return;
        }
        if (node instanceof Text) {
            icon = IconFetcher.getIcon("text.png");
            return;
        }
        if (node instanceof ProcessingInstruction) {
            icon = IconFetcher.getIcon("pi.png");
            return;
        }
        if (node instanceof Document) {
            icon = IconFetcher.getIcon("xfa.png");
            return;
        }
        icon = IconFetcher.getIcon("tag.png");
    }

    private void addChildNodes(List<Node> list) {
        for (Node n: list) {
            if (n instanceof Namespace) continue;
            if (n instanceof Comment) continue;
            this.add(new XdpTreeNode(n));
        }
    }

    public Node getNode() {
        return (Node)getUserObject();
    }
    
    public String toString() {
        Node node = getNode();
        if (node instanceof Element) {
            Element e = (Element)node;
            return e.getName();
        }
        if (node instanceof Attribute) {
            Attribute a = (Attribute)node;
            StringBuffer buf = new StringBuffer();
            buf.append(a.getName());
            buf.append("=\"");
            buf.append(a.getValue());
            buf.append("\"");
            return buf.toString();
        }
        if (node instanceof Text) {
            Text t = (Text)node;
            return t.getText();
        }
        if (node instanceof ProcessingInstruction) {
            ProcessingInstruction pi = (ProcessingInstruction)node;
            StringBuffer buf = new StringBuffer("<?");
            buf.append(pi.getName());
            buf.append(" ");
            buf.append(pi.getText());
            buf.append("?>");
            return buf.toString();
        }
        if (node instanceof Document) {
            return "XFA Document";
        }
        return getNode().toString();
    }
}