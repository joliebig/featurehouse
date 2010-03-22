package com.horstmann.violet;

import com.horstmann.violet.framework.VersionChecker;

/**
 * A program for editing UML diagrams.
 */

public class UMLEditor
{

   private static final String JAVA_VERSION = "1.4";

   public static void main(String[] args)
   {
      VersionChecker checker = new VersionChecker();
      checker.check(JAVA_VERSION);

      original(args);
   }

}