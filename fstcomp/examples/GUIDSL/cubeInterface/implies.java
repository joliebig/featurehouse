//created on: Thu Oct 13 18:42:26 CDT 2005

class implies {
    public String toXMLString() {
        return "<implies>\n\t"+right.toXMLString()+"\n"+left.toXMLString()+"\t</implies>";
    }
}