//created on: Thu Oct 13 18:37:45 CDT 2005

class choose1{
    public String toXMLString() {
        StringBuffer str=new StringBuffer();
        Object obj[] = children().toArray();

        str.append("<choose1>");
        array2String( children().toArray(), "," );
        str.append("</choose1>");

        return str.toString();

    }
}