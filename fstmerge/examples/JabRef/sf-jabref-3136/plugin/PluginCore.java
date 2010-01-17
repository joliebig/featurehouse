package net.sf.jabref.plugin;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import net.sf.jabref.Globals;
import net.sf.jabref.plugin.util.Util;

import org.java.plugin.ObjectFactory;
import org.java.plugin.PluginManager;
import org.java.plugin.PluginManager.PluginLocation;
import org.java.plugin.boot.DefaultPluginsCollector;
import org.java.plugin.registry.PluginDescriptor;
import org.java.plugin.standard.StandardPluginLocation;
import org.java.plugin.util.ExtendedProperties;


public class PluginCore {

    static PluginManager singleton;

    static File userPluginDir = new File(System.getProperty("user.home")+"/.jabref/plugins");

    static PluginLocation getLocationInsideJar(String context, String manifest) {
        URL jar = PluginCore.class
            .getResource(Util.joinPath(context, manifest));

        if (jar == null) {
            return null;
        }
        String protocol = jar.getProtocol().toLowerCase();
        try {
            if (protocol.startsWith("jar")) {
                return new StandardPluginLocation(new URL(jar.toExternalForm()
                    .replaceFirst("!(.*?)$", Util.joinPath("!", context))), jar);
            } else if (protocol.startsWith("file")) {
                File f = new File(jar.toURI());
                return new StandardPluginLocation(f.getParentFile(), manifest);
            }
        } catch (URISyntaxException e) {
            return null;
        } catch (MalformedURLException e) {
            return null;
        }
        return null;
    }

    static PluginManager initialize() {
        
        Logger.getLogger("org.java.plugin").setLevel(Level.WARNING);

        Logger log = Logger.getLogger(PluginCore.class.getName());

        ObjectFactory objectFactory = ObjectFactory.newInstance();

        PluginManager result = objectFactory.createManager();

        
        
        try {
            DefaultPluginsCollector collector = new DefaultPluginsCollector();
            ExtendedProperties ep = new ExtendedProperties();


            List<File> directoriesToSearch = new LinkedList<File>();
            directoriesToSearch.add(new File("./src/plugins"));
            directoriesToSearch.add(new File("./plugins"));
            directoriesToSearch.add(userPluginDir);

            try {
                File parent = new File(PluginCore.class.getProtectionDomain()
                    .getCodeSource().getLocation().toURI()).getParentFile();
            
                if (!parent.getCanonicalFile().equals(
                    new File(".").getCanonicalFile())) {
                    directoriesToSearch.add(new File(parent, "/src/plugins"));
                    directoriesToSearch.add(new File(parent, "/plugins"));
                }
            } catch (Exception e) {
                
            }

            StringBuilder sb = new StringBuilder();
            for (File directory : directoriesToSearch) {
                
                
                if (directory.exists()) {
                    if (sb.length() > 0)
                        sb.append(',');
                    sb.append(directory.getPath());
                }
            }

            ep.setProperty("org.java.plugin.boot.pluginsRepositories", sb
                .toString());
            collector.configure(ep);

            Collection<PluginLocation> plugins = collector
                .collectPluginLocations();

            
            String[] jarLocationsToSearch = new String[] {
                "/plugins/net.sf.jabref.core/",
                "/plugins/net.sf.jabref.export.misq/"};
                
            
            for (String jarLocation : jarLocationsToSearch) {
                PluginLocation location = getLocationInsideJar(jarLocation,
                    "plugin.xml");
                if (location != null)
                    plugins.add(location);
            }

            if (plugins.size() <= 0) {
                log
                    .warning(Globals
                        .lang("No plugins were found in the following folders:") +
                        "\n  " +
                        Util.join(directoriesToSearch
                            .toArray(new String[directoriesToSearch.size()]),
                            "\n  ", 0, directoriesToSearch.size()) +
                        "\n" +
                        Globals.lang("and inside the JabRef-jar:") +
                        "\n  " +
                        Util.join(jarLocationsToSearch, "\n  ", 0,
                            jarLocationsToSearch.length) +
                        "\n" +
                        Globals
                            .lang("At least the plug-in 'net.sf.jabref.core' should be there."));
            } else {
                result.publishPlugins(plugins.toArray(new PluginLocation[] {}));

                Collection<PluginDescriptor> descs = result.getRegistry()
                    .getPluginDescriptors();

                sb = new StringBuilder();
                sb.append(Globals.lang("Found %0 plugin(s)", String
                    .valueOf(descs.size())) +
                    ":\n");

                for (PluginDescriptor p : result.getRegistry()
                    .getPluginDescriptors()) {
                    sb.append("  - ").append(p.getId()).append(" (").append(
                        p.getLocation()).append(")\n");
                }
                log.info(sb.toString());
            }

        } catch (Exception e) {
            log
                .severe(Globals
                    .lang("Error in starting plug-in system. Starting without, but some functionality may be missing.") +
                    "\n" + e.getLocalizedMessage());
        }
        return result;
    }

    public static PluginManager getManager() {
        if (singleton == null) {
            singleton = PluginCore.initialize();
        }

        return singleton;
    }
}
