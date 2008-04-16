
class ESList {
     // Get the constraints encoded as XML
     // This will need to be modified add full XML support.
     public static String getCTableXML() {
          StringBuffer str=new StringBuffer();

          str.append("<constraints>\n");
          int cnt = CTable.size();
          for (int i = 0; i<cnt; i++) {
            node n = ( node ) CTable.get(i);
            str.append("<constraint>"+n.toXMLString()+"</constraint>\n");
          }

          str.append("</constraints>\n");

         return str.toString();
    }

}