class implies {
    public String toXMLString() {
        return "<implies>\n\t"+right.toXMLString()+"\n"+left.toXMLString()+"\t</implies>";
    }
}