To compile the AHEAD tools, two libraries have to be in the classpath: 
jakarta.jar (same directory) and ant.jar (has to be installed externally),  
for example, javac -cp jakarta.jar:/usr/share/ant/lib/ant.jar MixinComp/*.java.

To run a tool, jakarta.jar has to be in the classpath, for example,
cd MixinComp && java -cp .:../jakarta.jar Main.

