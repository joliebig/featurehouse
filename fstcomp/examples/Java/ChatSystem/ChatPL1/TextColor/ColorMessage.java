

import java.awt.Color;

public class ColorMessage extends TextMessage{

	private String color;
	
	public ColorMessage(String content){
		super(content);
		color=new String("Schwarz");
	}
	
	
	public void setColor(String color){
		this.color=color;
	}
	
	public String getColor(){
		return color;
	}
	
	
}