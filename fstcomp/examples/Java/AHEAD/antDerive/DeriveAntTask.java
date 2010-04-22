

import java.io.File;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DirectoryScanner;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.ExecuteJava;
import org.apache.tools.ant.taskdefs.MatchingTask;
import org.apache.tools.ant.types.Commandline;
import org.apache.tools.ant.types.Path;

/**
 * Ant task to derive a set of target files from a given set of source
 * files.  The derivation is done by invoking {@link Main#main(String[])}.
 *
 * @layer<antDerive>
 */
    
public class DeriveAntTask extends MatchingTask {

    /**
     * Adds a given argument to the argument list.
     *
     * @layer<antDerive>
     */
    public Commandline.Argument createArg() {
        return command.createArgument() ;
    }

    /**
     * Adds a path for source directory search.
     *
     * @layer<antDerive>
     */
    public Path createSrc() {

        if ( sourcePath == null )
            sourcePath = new Path( getProject() ) ;

        return sourcePath.createPath() ;
    }

    /**
     * Invokes {@link Main#main(String[])} for each source file.
     *
     * @layer<antDerive>
     */
    public void execute() throws BuildException {

        if ( sourcePath == null || sourcePath.size() < 1 )
            throw new BuildException( "srcdir attribute must be set",
                            getLocation() ) ;

        createArg().setValue( listFiles ? "-verbose" : "-quiet" ) ;
        String[] extraArgs = command.getArguments() ;

        int errors = 0 ;

        String[] sourceDirs = sourcePath.list() ;
        for ( int dirIndex = 0; dirIndex < sourceDirs.length; ++dirIndex ) {

            File directory = getProject().resolveFile( sourceDirs [dirIndex] ) ;
            if ( listFiles )
                log( "directory \"" + directory + '"', Project.MSG_INFO ) ;

            if ( ! directory.exists() )
                throw new BuildException( "source directory \""
                                                + directory.getPath()
                                                + "\" does not exist",
                                                getLocation() ) ;

            DirectoryScanner scanner = getDirectoryScanner( directory ) ;
            String[] files = scanner.getIncludedFiles() ;

            if ( files.length > 0 ) {
                for ( int n = 0 ; n < files.length ; ++n )
                    files[n] = new File( directory,files[n] ).toString() ;
                processFiles( files, extraArgs ) ;

            }
            else
                if ( errorIfNoFiles ) {
                    throw new BuildException( "no input files given",getLocation() ) ;

                }
        }

        if ( errors > 0 ) {
            String errorMessage =
                            "derivation failed in "
                            + errors
                            + " source "
                            + ( errors == 1 ? "directory" : "directories" ) ;
            if ( failOnError )
                throw new BuildException( errorMessage, getLocation() ) ;
            else
                log( errorMessage, Project.MSG_ERR ) ;
        }
    }

    /**
     * A <code>main</code> method causes a wrapper class to be generated
     * at the top level of the enclosing package.  As a result, this task
     * will be available as a top-level (<em>not</em> inner) class within
     * the enclosing package.
     *
     * @layer<antDerive>
     */
    public static void main( String[] args ) {
        throw new UnsupportedOperationException( "invoking \"DeriveAntTask.main\" is not supported" ) ;
    }

    /**
     * Processes a given array of source file names with a specified
     * array of command-line options.
     *
     * @layer<antDerive>
     */
    public boolean processFiles( String[] sources, String[] options )
    throws BuildException {

        // Form total argument list by concatenating sources with options:
        //
        String[] args = new String [sources.length + options.length] ;
        System.arraycopy( sources,0, args,0, sources.length ) ;
        System.arraycopy( options,0, args,sources.length, options.length ) ;

        try {
            Main.main( args ) ;
            return true ;

        }
        catch ( BuildException exception ) {
            throw exception ;

        }
        catch ( Exception exception ) {
            log( exception.toString(), Project.MSG_ERR ) ;
            return false ;
        }
    }

    /**
     * If true and if errors occur during derivation, aborts the enclosing
     * build at the end of the task.
     *
     * @layer<antDerive>
     */
    public void setFailonerror( boolean fail ) {
        failOnError = fail ;
    }

    /**
     * If true, list the source files to be handed to the tool.
     *
     * @layer<antDerive>
     */
    public void setListfiles( boolean list ) {
        listFiles = list ;
    }

    /**
     * If true, throws a build error if no source files are found for the
     * tool.  If false and there are no source files, this task just exits
     * without invoking the tool.
     *
     * @layer<antDerive>
     */
    public void setErrorifnofiles( boolean errorIfNoFiles ) {
        this.errorIfNoFiles = errorIfNoFiles ;
    }

    /**
     * An alternative way to specify a source search path.
     * @see #createSrc()
     *
     * @layer<antDerive>
     */
    public void setSrcdir( Path srcDir ) {

        if ( sourcePath == null )
            sourcePath = srcDir ;
        else
            sourcePath.append( srcDir ) ;
    }

    /**
     * Specifies a timeout on the time to derive from a given file.
     *
     * @layer<antDerive>
     */
    public void setTimeout( Long interval ) {
        timeOut = interval ;
    }

    protected Commandline command = new Commandline() ;
    private boolean errorIfNoFiles = true ;
    private boolean failOnError = false ;
    private boolean listFiles = false ;
    private Path sourcePath = null ;
    private Long timeOut = null ;

}
