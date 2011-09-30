class AspectJCollector {
	protected boolean inter_type_method_flag = false;
	protected boolean inter_type_constructor_flag = false;
	protected boolean inter_type_field_flag = false;
	protected boolean aspect_variable_flag = false;
	protected boolean advice_flag = false;
	protected boolean method_flag = false;
	protected boolean constructor_flag = false;
	protected boolean aspect_flag = false;
	protected boolean class_flag = false;
	protected boolean interface_flag = false;
	private boolean tmp_aspect_flag = false;
	private boolean tmp_class_flag = false;
	private boolean tmp_interface_flag = false;
    protected int nested_block_count = 0;

    public void Block() throws ParseException {
        nested_block_count++;
        super.Block();
        nested_block_count--;
    }
    
    public void MethodDeclaration() throws ParseException {
		method_flag = true;
		super.MethodDeclaration();
		method_flag = false;
    }
    
    public void ConstructorDeclaration() throws ParseException {
		constructor_flag = true;
		super.ConstructorDeclaration();
		constructor_flag = false;
    }

    public void AdviceDeclaration() throws ParseException {
		advice_flag = true;
		super.AdviceDeclaration();
		advice_flag = false;
    }
	
	public void AspectVariableDeclarator() throws ParseException {
		aspect_variable_flag = true;
		super.AspectVariableDeclarator();
		aspect_variable_flag = false;
	}
	
    public void BasicTypePatternDot() throws ParseException {
    	if(aspect_variable_flag == true)
			inter_type_field_flag = true;
		super.BasicTypePatternDot();
    }
    
    public void InterTypeMethodDeclaration() throws ParseException {
		inter_type_method_flag = true;
		super.InterTypeMethodDeclaration();
		inter_type_method_flag = false;
    }
    
    public void InterTypeConstructorDeclaration() throws ParseException {
		inter_type_constructor_flag = true;
		super.InterTypeConstructorDeclaration();
		inter_type_constructor_flag = false;
    }
    
    public void AspectBody() throws ParseException {
    	saveState();
    	aspect_flag = true;
    	class_flag = false;
    	interface_flag = false;
    	super.AspectBody();
		recoverState();
    }
    
    public void ClassBody() throws ParseException {
    	saveState();
    	aspect_flag = false;   	
    	class_flag = true;
    	interface_flag = false;
    	super.ClassBody();
    	recoverState();
    }
    
    public void InterfaceMemberDeclaration() throws ParseException {
    	saveState();
    	aspect_flag = false; 
    	class_flag = false; 
    	interface_flag = true;
    	super.InterfaceMemberDeclaration();
		recoverState();
    }
    
    private void saveState() {
    	tmp_aspect_flag = aspect_flag;
    	tmp_class_flag = class_flag;
    	tmp_interface_flag = interface_flag;
    }
    
    private void recoverState() {
    	aspect_flag = tmp_aspect_flag;
    	class_flag = tmp_class_flag;
    	interface_flag = tmp_interface_flag;
    }
    
}
