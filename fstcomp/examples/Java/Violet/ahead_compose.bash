yes | rm -r violet
composer --equation violet.expression
jak2java violet/com/horstmann/violet/*.jak
jak2java violet/com/horstmann/violet/framework/*.jak
javac -cp . violet/com/horstmann/violet/framework/*.java violet/com/horstmann/violet/*.java