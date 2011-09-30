class AspectJCollector {
    public void InterfaceMemberDeclaration() throws ParseException {
        int begin = this.token.beginLine;
        super.InterfaceMemberDeclaration();
        int end = this.token.endLine;
        stats.interface_loc += end - begin + 1;
    }

    public void UnmodifiedInterfaceDeclaration() throws ParseException {
        super.UnmodifiedInterfaceDeclaration();
        stats.interface_count++;        
    }
    
    public void InterfaceDeclaration() throws ParseException {
        super.InterfaceDeclaration();
        stats.top_interface_count++;        
    }

    public void NestedInterfaceDeclaration() throws ParseException {
        super.NestedInterfaceDeclaration();
        stats.nested_interface_count++;        
    }
}
