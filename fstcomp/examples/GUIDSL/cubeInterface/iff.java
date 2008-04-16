//created on: Thu Oct 13 18:43:19 CDT 2005
class iff{
    public String toXMLString() {
        return "<iff>\n\t"+right.toXMLString()+"\n"+left.toXMLString()+"\t</iff>";
        //return "(" + left + " iff " + right + ")";
    }
}