class iff{
    public String toXMLString() {
        return "<iff>\n\t"+right.toXMLString()+"\n"+left.toXMLString()+"\t</iff>";
        //return "(" + left + " iff " + right + ")";
    }
}