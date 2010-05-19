package lancs.mobilemedia.core.ui.screens;
import java.io.InputStream;
import javax.microedition.lcdui.Command;
import javax.microedition.lcdui.Display;
import javax.microedition.lcdui.Graphics;
import javax.microedition.lcdui.game.GameCanvas;
import javax.microedition.media.Manager;
import javax.microedition.media.Player;
import javax.microedition.media.control.VideoControl;
import lancs.mobilemedia.core.ui.MainUIMidlet;
import lancs.mobilemedia.core.ui.controller.AbstractController;
import de.ovgu.cide.jakutil.*;
public class PlayVideoScreen extends GameCanvas {
  private Player player=null;
  private VideoControl videoControl=null;
  private Command start=new Command("Start",Command.EXIT,1);
  private Command back=new Command("Back",Command.ITEM,1);
  private Command stop=new Command("Stop",Command.ITEM,1);
  private Display display=null;
  private boolean error=false;
  private int dy=2;
  public void startVideo(){
    if (error)     return;
    try {
      player.start();
    }
 catch (    Exception e) {
      e.printStackTrace();
    }
  }
  public void stopVideo(){
    try {
      if (player != null)       player.stop();
    }
 catch (    Exception e) {
      e.printStackTrace();
    }
  }
  public PlayVideoScreen(  MainUIMidlet midlet,  InputStream storedVideo,  String type,  AbstractController controller){
    super(false);
    display=Display.getDisplay(midlet);
    this.addCommand(start);
    this.addCommand(stop);
    this.addCommand(back);
    this.hook56();
    try {
      this.hook57();
      this.hook58(storedVideo,type);
      player.realize();
      videoControl=(VideoControl)player.getControl("VideoControl");
    }
 catch (    Exception e) {
      System.out.println("Error ao criar o player:");
      e.printStackTrace();
    }
    try {
      this.setCommandListener(controller);
      System.out.println("Crio os comandos e vai iniciar o display");
      videoControl.initDisplayMode(VideoControl.USE_DIRECT_VIDEO,this);
    }
 catch (    Exception e) {
      System.out.println("Error criar or controler" + e.getMessage());
    }
    int halfCanvasWidth=this.getWidth();
    int halfCanvasHeight=this.getHeight();
    try {
      videoControl.setDisplayFullScreen(false);
      videoControl.setDisplaySize(halfCanvasWidth - 10,halfCanvasHeight - 10);
      videoControl.setDisplayLocation(5,5);
    }
 catch (    Exception e) {
      System.out.println("Error ao definir a tela");
      e.printStackTrace();
    }
  }
  public void paint(  Graphics g){
    g.setColor(0xffffff);
    g.fillRect(0,0,getWidth(),getHeight());
    flushGraphics();
  }
  public void keyPressed(  int keyCode){
    int gameAction=getGameAction(keyCode);
    int y=videoControl.getDisplayY();
    if (gameAction == UP) {
      y-=dy;
    }
 else     if (gameAction == DOWN) {
      y+=dy;
    }
    videoControl.setDisplayLocation(videoControl.getDisplayX(),y);
    repaint();
  }
  public void setVisibleVideo(){
    display.setCurrent(this);
    videoControl.setVisible(true);
  }
  protected void hook56(){
  }
  protected void hook57() throws Exception {
  }
  protected void hook58(  InputStream storedVideo,  String type) throws Exception {
  }
}
