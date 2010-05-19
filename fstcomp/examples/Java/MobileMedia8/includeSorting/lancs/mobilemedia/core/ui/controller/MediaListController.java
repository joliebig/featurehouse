package lancs.mobilemedia.core.ui.controller;
public class MediaListController {
  /** 
 * @param images
 * @param pos1
 * @param pos2
 */
  private void exchange(  MediaData[] medias,  int pos1,  int pos2){
    MediaData tmp=medias[pos1];
    medias[pos1]=medias[pos2];
    medias[pos2]=tmp;
  }
  /** 
 * Sorts an int array using basic bubble sort
 * @param numbers the int array to sort
 */
  public void bubbleSort(  MediaData[] medias){
    System.out.print("Sorting by BubbleSort...");
    for (int end=medias.length; end > 1; end--) {
      for (int current=0; current < end - 1; current++) {
        if (medias[current].getNumberOfViews() > medias[current + 1].getNumberOfViews()) {
          exchange(medias,current,current + 1);
        }
      }
    }
    System.out.println("done.");
  }
  protected void hook33(  boolean sort,  MediaData[] medias){
    if (sort) {
      bubbleSort(medias);
    }
    original(sort,medias);
  }
}
