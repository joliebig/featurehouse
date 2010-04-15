class Actions {

       
    /**
     *THIS IS FOR SAVE ACTION, SaveAs ACTION has saveAs() method
     *If we want to save a new text, then we want to know 
     *if the text was saved befor or not.
     *If the text wasn't be saved befor, then we will use saveAs()
     *If the text was be save befor, then we will use save()
     */
    public void savE(){
        /**
         *if String fileName is null, then using saveAs().
         *Because there was no file was be saved
         */
        if(fileName == null){
            saveAs();	
        }
        /**
         *if String fileName has a String (file name), then using save().
         *Because we want to save the new text in the same file.
         *if the user want to save the all text (old & new text) in a new file, ->
         *he can presses SaveAs button (@saveAs())
         */
        else{
            save();
        }
    }

    /**
     *THIS IS THE WAY FOR SAVING THE TEXT IN THE SAME FILE
     */
    public void save(){
        //initializing 'fout' to write all text in the selected file
        try{
            PrintWriter fout = new PrintWriter(new FileWriter(jfc.getSelectedFile()));
            //for getting the text from the text area
            fileContent = n.getTextArea().getText();
            //using StringTokenizer for the 'fileContent' String
            StringTokenizer st=new StringTokenizer(fileContent,System.getProperty("line.separator"));
            while(st.hasMoreTokens()){
                //write the string (text) in the selected file
                fout.println(st.nextToken());
            }
            //closing fout
            fout.close();
        }
        catch(IOException ioe){
            System.err.println("I/O Error on Save");
        }
        n.setTitle(jfc.getSelectedFile().getName() + " - JAVA Notepad");
    }
    /**
     *THIS IS THE WAY FOR SAVING THE TEXT IN A NEW FILE
     */	
    public void saveAs(){
        //filter the kind of files, we want only TXT file
        filter.addExtension("txt");
        //to set a description for the file (TXT)
        filter.setDescription("TXT Documents");
        //setting the FileFilter to JFileChooser
        jfc.setFileFilter(filter);
        returnVal = jfc.showSaveDialog(n);
        if(returnVal == JFileChooser.APPROVE_OPTION){
            //initializing the PrintWriter, to save the text in a new file
            PrintWriter fout = null;
            try{
                fout = new PrintWriter(new FileWriter(jfc.getSelectedFile() + ".txt"));
                //getting the text from the text area
                fileContent = n.getTextArea().getText();
                //getting the name of the selected file
                fileName = jfc.getSelectedFile().getPath();
                //using StringTokenizer for the 'fileContent' String
                StringTokenizer st=new StringTokenizer(fileContent,System.getProperty("line.separator"));
                while(st.hasMoreTokens()){
                    //write the string (text) in the selected file 
                    fout.println(st.nextToken());
                }
                //closing 'fout'
                fout.close();
            }
            catch(IOException ioe){
                System.err.println ("I/O Error on Save");
            }
        }
        n.setTitle(jfc.getSelectedFile().getName() + " - JAVA Notepad");
    }

    public void exiT(){
        /**
         *if the text area isn't empty & if the text area hasn't a text which
         *not saved befor (fileContent != null)
         */
        if(!n.getTextArea().getText().equals("") && !n.getTextArea().getText().equals(fileContent)){
            //if there was no file opened or saved
            if(fileName == null){
                /**
                 *this method has 3 options (1 = YES, 2 = NO, 3 = Cancel)
                 *this is an option pop up to the user, for saving the old text or not
                 */
                option = JOptionPane.showConfirmDialog(null,"Do you want to save the changes ??");
                /**
                 *if the user click on YES button,
                 *then the program will be saved the text & close the program
                 */
                if(option == 0){
                    //for saving the text into new file
                    saveAs();
                    //for closing the program
                    original();
                }
                /**
                 *if the user click on NO button,
                 *then the program will be closed without save the text
                 */
                if(option == 1){
                    //for closing the program
                	original();
                }
            }
            //if there was a text which was be opend or saved		
            else{
                /**
                 *this method has 3 options (1 = YES, 2 = NO, 3 = Cancel)
                 *this is an option pop up to the user, for saving the old text or not
                 */
                option = JOptionPane.showConfirmDialog(null,"Do you want to save the changes ??");
                /**
                 *if the user click on YES button,
                 *then the program will be saved the text & close the program
                 */
                if(option == 0){
                    //for saving the text into the same file
                    save();
                    //for closing the program
                    original();
                }
                /**
                 *if the user click on NO button,
                 *then the program will be closed without save the text
                 */
                if(option == 1){
                    //for closing the program
                	original();
                }
            }
        }
        /**
         *if the text was be opened or saved before but wasn't be changed,
         *and we want to close the program. This option will be actived
         */	
        else{
            //for closing the program
        	original();
        }
    }


    /**
     *If we want to write a new text, first we want to know if the text area is empty or not,
     *second we want to kwnow if the text was saved or not.
     *If the text area isn't empty & the text wasn't saved befor this time, then the program display ->
     *for the user an option for saving the text in a new file or in the same file
     */
    public void neW(){
        /**
         *if the text area isn't empty & if the text area hasn't 
         *a text not saved befor (fileContent != null)
         */
        if(!n.getTextArea().getText().equals("") && !n.getTextArea().getText().equals(fileContent)){
            /**
             *here, we have 2 thing, first if the text wasn't be opened or saved befor
             *second if the text was be opened or saved
             */
            //if there was no file opened or saved
            if(fileName == null){
                /**
                 *this method has 3 options (1 = YES, 2 = NO, 3 = Cancel)
                 *this is an option pop up to the user, for saving the old text or not
                 */
                option = JOptionPane.showConfirmDialog(null,"Do you want to save the changes ??");
                //if the user click on YES button, 
                //then the program will save the text & make a new text area
                if(option == 0){
                    //to save the text into a new file
                    saveAs();
                    //to create new getTextArea() after saving the old getTextArea()
                    original();
                }
                //if the user click on NO button
                if(option == 1){
                    //to create new getTextArea() without saving the old getTextArea()
                	original();
                }
            }
            //if there was a text which was be opend or saved
            else{
                /**
                 *this method has 3 options (1 = YES, 2 = NO, 3 = Cancel)
                 *this is an option pop up to the user, for saving the old text or not
                 */
                option = JOptionPane.showConfirmDialog(null,"Do you want to save the changes ??");
                /**
                 *if the user click on YES button, 
                 *then the program will save the text into the old file &
                 *make a new text area,,,
                 */
                if(option == 0){
                    save();
                    //to create new getTextArea() after saving the old getTextArea()
                    original();
                }
                //if the user click on NO button
                if(option == 1){
                    //to create new getTextArea() without saving the old getTextArea()
                	original();
                }
            }
        }
        /**
         *if the text was be opened or saved before but wasn't be changed,
         *and we want to write a new text,,, this option will be actived
         */
        else{
        	original();
        }
    }

    public void opeN(){
        /**
         *if the text area isn't empty & if the text area hasn't a text which
         *not saved befor (fileContent != null)
         */
        if(!n.getTextArea().getText().equals("") && !n.getTextArea().getText().equals(fileContent)){
            /**
             *here, we have 2 thing, first if the text wasn't be opened or saved befor
             *second if the text was be opened or saved
             */
            //if there was no file opened or saved
            if(fileName == null){
                /**
                 *this method has 3 options (1 = YES, 2 = NO, 3 = Cancel)
                 *this is an option pop up to the user, for saving the old text or not
                 */
                option = JOptionPane.showConfirmDialog(null,"Do you want to save the changes ??");
                //if the user click on YES button, 
                //then the program will be saved the text & opened a new documents
                if(option == 0){
                    //for saving the text in a new file
                    saveAs();
                    //for opening the new documents
                    original();
                }
                /**
                 *if the user click on NO button, 
                 *the program will be opened the new documents
                 */
                if(option == 1){
                    //for opening the new documents
                	original();
                }
            }
            //if there was a text which was be opend or saved
            else{
                /**
                 *this method has 3 options (1 = YES, 2 = NO, 3 = Cancel)
                 *this is an option pop up to the user, for saving the old text or not
                 */
                option = JOptionPane.showConfirmDialog(null,"Do you want to save the changes ??");
                /**
                 *if the user click on YES button, 
                 *then the program will save the text into the same file &
                 *open the new documents
                 */
                if(option == 0){
                    //for saving the text into the same file
                    save();
                    //for opening the new documents
                    original();
                }
                /**
                 *if the user click on NO button,
                 *the program will be opened the new documents
                 */
                if(option == 1){
                    //for opening the new documents
                	original();
                }
            }
        }
        /**
         *if the text was be opened or saved before but wasn't be changed,
         *and we want to open a new document,,, this option will be actived
         */		
        else{
            //for opening the new documents
        	original();
        }
    }

}
