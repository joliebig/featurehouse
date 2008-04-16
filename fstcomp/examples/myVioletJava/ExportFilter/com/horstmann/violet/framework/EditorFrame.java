package com.horstmann.violet.framework;

public class EditorFrame {

    EditorFrame(Class appClass) {
         exportFilter = new ExtensionFilter(
         editorResources.getString("files.image.name"), 
         editorResources.getString("files.image.extension"));
         imageExtensions = editorResources.getString("files.image.extension");
    }
}