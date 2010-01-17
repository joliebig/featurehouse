

package net.sf.jabref; 

public  class  BibtexString {
	

    String _name, _content, _id;

	

    public BibtexString(String id, String name, String content) {
	_id = id;
	_name = name;
	_content = content;
    }


	

    public String getId() {
	return _id;
    }


	

    public void setId(String id) {
	_id = id;
    }


	

    public String getName() {
	return _name;
    }


	

    public void setName(String name) {
	_name = name;
    }


	

    public String getContent() {
	return ((_content == null) ? "" : _content);
    }


	

    public void setContent(String content) {
	_content = content;
    }


	

    public Object clone() {
      return new BibtexString(_id, _name, _content);
    }



}
