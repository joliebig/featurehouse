//created on: Tue Jan 24 13:02:40 CST 2006

class onlyone{
    public String toXMLString() {
        StringBuffer str=new StringBuffer();
        Object obj[] = children().toArray();

        str.append("<onlyone>");
        str.append(left.toXMLString() + "\n"+ right.toXMLString());
        str.append("</onlyone>");

        return str.toString();

    }
}