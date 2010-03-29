class or{
    public String toXMLString() {
        StringBuffer str=new StringBuffer();
        Object obj[] = children().toArray();

        str.append("<or>");
        for(int i=0;i< obj.length;i++){
            str.append( ((node)obj[i]).toXMLString());
        }
        str.append("</or>");

        return str.toString();
    }

}