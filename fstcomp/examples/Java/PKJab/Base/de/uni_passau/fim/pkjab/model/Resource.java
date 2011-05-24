package de.uni_passau.fim.pkjab.model;

import de.uni_passau.fim.pkjab.util.UserState;


	public class Resource {
		private final String resource;
		private UserState state = UserState.OFFLINE;
		private int priority = 0;
		private String text = "";
		
		public Resource(String resource) {
			if (resource == null) {
				throw new IllegalArgumentException();
			}
			this.resource = resource;
		}

		public UserState getState() {
			return state;
		}

		public void setState(UserState state) {
			if (state == null) {
				throw new IllegalArgumentException();
			}
			this.state = state;
		}

		public int getPriority() {
			return priority;
		}

		public void setPriority(int priority) {
			this.priority = priority;
		}

		public String getText() {
			return text;
		}

		public void setText(String text) {
			this.text = text;
		}

		public String getResource() {
			return resource;
		}
	}