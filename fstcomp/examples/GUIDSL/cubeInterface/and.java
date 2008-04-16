//created on: Thu Oct 13 18:41:00 CDT 2005

class and{
    public String toXMLString() {
        StringBuffer str=new StringBuffer();
        Object obj[] = children().toArray();

        str.append("<and>");
        for(int i=0;i< obj.length;i++){
            str.append( ((node)obj[i]).toXMLString());
        }
        str.append("</and>");

        return str.toString();

    }
}