	public static class UndoActivity extends SendToBackCommand.UndoActivity {
		public UndoActivity(DrawingView newDrawingView) {
			super(newDrawingView);
		}

		protected void sendToCommand(Figure f) {
			getDrawingView().drawing().bringToFront(f);
		}
	}
