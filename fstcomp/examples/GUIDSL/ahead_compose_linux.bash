yes | rm -r guidsl
composer --equation guidsl.expression
jak2java guidsl/*.jak
javac -cp .:../Jakarta/jakarta.jar:../Jakarta/jdom.jar:../Jakarta/sat4j.jar guidsl/*.java