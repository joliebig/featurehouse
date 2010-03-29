
public class TankManager {

	protected Vector toolMenge = new Vector();
		
	public void komponenteMalen() {
		original();
		if (mapReady) {
			gruppeMalen(toolMenge);
			}
	}

	public void toolsLaden() {
		
	}

	public void toolInit(int type) {
		if (Math.abs(random.nextInt() % 300) == 0) {
			int x = Math.abs(random.nextInt() % 59);
			int y = Math.abs(random.nextInt() % 59);
			Tool tool = new Tool(this, x, y, type);
			if (!tool.stossenGegen(metalwall) && !tool.stossenGegen(waterwall)
					&& toolMenge.size() < 3) {
				toolMenge.addElement(tool);
			}
		}
	}
	
	public void addEnemyTank() {
		original();
		toolsLaden();
	}

}