public   class  Graph {
	
    public void  run__wrappee__Base  ( Vertex s )
    {
     	System.out.println("Base");
    }

	
    // Executes Connected Components
    public void run( Vertex s )
    {
	     	System.out.println("Connected");
        ConnectedComponents( );
        run__wrappee__Base( s );
    }

	

    public void ConnectedComponents( ) 
    {
        GraphSearch( new RegionWorkSpace( ) );
    }


}
