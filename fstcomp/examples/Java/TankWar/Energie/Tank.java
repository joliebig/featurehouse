
public class Tank {

	protected void toolBehandeln(int toolType) {
		original(toolType);
		switch (toolType) {
		case 373:// 255,0,0
			this.energie += 10;
			break;
		}
	}

}