package lancs.mobilemedia.core.ui.controller;
public class MediaController {
  private void incrementCountViews(  String selectedImageName){
    try {
      MediaData image=getAlbumData().getMediaInfo(selectedImageName);
      image.increaseNumberOfViews();
      updateMedia(image);
      System.out.println("<* BaseController.handleCommand() *> Image = " + selectedImageName + "; # views = "+ image.getNumberOfViews());
    }
 catch (    ImageNotFoundException e) {
      Alert alert=new Alert("Error","The selected photo was not found in the mobile device",null,AlertType.ERROR);
      Display.getDisplay(midlet).setCurrent(alert,Display.getDisplay(midlet).getCurrent());
    }
catch (    InvalidImageDataException e) {
      Alert alert=new Alert("Error","The image data is not valid",null,AlertType.ERROR);
      alert.setTimeout(5000);
    }
catch (    PersistenceMechanismException e) {
      Alert alert=new Alert("Error","It was not possible to recovery the selected image",null,AlertType.ERROR);
      alert.setTimeout(5000);
    }
  }
@MethodObject static class MediaController_handleCommand {
    protected void hook27(){
      if (label.equals("Sort by Views")) {
        _this.showMediaList(_this.getCurrentStoreName(),true,false);
        ScreenSingleton.getInstance().setCurrentScreenName(Constants.IMAGELIST_SCREEN);
        throw new ReturnBoolean(true);
      }
      original();
    }
  }
}
