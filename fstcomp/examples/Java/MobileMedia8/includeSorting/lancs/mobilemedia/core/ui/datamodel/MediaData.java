package lancs.mobilemedia.core.ui.datamodel;
public class MediaData {
  private int numberOfViews=0;
  /** 
 * [EF] Added in the scenario 02 
 */
  public void increaseNumberOfViews(){
    this.numberOfViews++;
  }
  /** 
 * [EF] Added in the scenario 02 
 * @return the numberOfViews
 */
  public int getNumberOfViews(){
    return numberOfViews;
  }
  /** 
 * @param views
 */
  public void setNumberOfViews(  int views){
    this.numberOfViews=views;
  }
}
