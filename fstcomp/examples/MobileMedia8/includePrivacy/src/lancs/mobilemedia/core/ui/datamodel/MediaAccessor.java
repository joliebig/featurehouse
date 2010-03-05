package lancs.mobilemedia.core.ui.datamodel;
public abstract class MediaAccessor {
  protected String password_label;
  public void addPassword(  String albumname,  String passwd){
    try {
      passwordRS=RecordStore.openRecordStore("mpp-" + albumname,true);
      passwordRS.addRecord(passwd.getBytes(),0,passwd.getBytes().length);
      passwordRS.closeRecordStore();
    }
 catch (    RecordStoreException e) {
    }
  }
  public String getPassword(  String albumname){
    String password=null;
    try {
      passwordRS=RecordStore.openRecordStore("mpp-" + albumname,false);
      if (passwordRS != null) {
        password=new String(passwordRS.getRecord(1));
      }
    }
 catch (    RecordStoreException e) {
    }
    return password;
  }
}
