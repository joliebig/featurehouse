package composer.rules;

public enum CompositionKeyword {
	FINAL_CONTRACT("\\final_contract", 1), FINAL_METHOD("\\final_method", 0), CUMULATIVE_CONTRACT(
			"\\cumulative_contract", 2), CONSECUTIVE_CONTRACT(
			"\\consecutive_contract", 3), CONJUNCTIVE_CONTRACT(
			"\\conjunctive_contract", 4);
	private String label;
	private int rank;

	private CompositionKeyword(String label, int rank) {
		this.label = label;
		this.rank = rank;
	}

	public String getLabel() {
		return label;
	}

	public void setLabel(String label) {
		this.label = label;
	}

	public int getRank() {
		return rank;
	}

	public void setRank(int rank) {
		this.rank = rank;
	}

	@Override
	public String toString() {
		return this.label + " Rank: " + this.rank;
	}
	
	public static CompositionKeyword getCompositionKeyword(String label) {
		for (CompositionKeyword key : values()) {
	        if (key.getLabel().equals(label)) {
	            return key;
	        }
	    }
		
		return null;
	}
}
