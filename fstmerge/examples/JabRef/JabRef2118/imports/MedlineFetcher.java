package net.sf.jabref.imports; 
import java.util.ArrayList; 
import java.net.*; 

import javax.swing.*; 

import java.awt.*; 
import java.awt.event.ActionListener; 
import javax.xml.parsers.SAXParserFactory; 
import javax.xml.parsers.SAXParser; 
import java.awt.event.ActionEvent; 
import java.awt.event.FocusAdapter; 
import java.awt.event.FocusEvent; 
import java.util.Iterator; 
import java.util.regex.Pattern; 
import java.util.regex.Matcher; 

import net.sf.jabref.*; 
import net.sf.jabref.undo.NamedCompound; 
import net.sf.jabref.undo.UndoableInsertEntry; 
import java.io.*; 
import net.sf.jabref.HelpAction; 
import net.sf.jabref.gui.ImportInspectionDialog; 
import java.io.BufferedReader; 
import java.io.IOException; 
import java.io.InputStreamReader; 
import java.net.HttpURLConnection; 
import java.net.MalformedURLException; 
import java.net.URL; 



public  class  MedlineFetcher  extends SidePaneComponent implements  Runnable , 
        ImportInspectionDialog.CallBack {
	

    
    public  class  SearchResult {
		
        public int count;

		
        public int retmax;

		
        public int retstart;

		
        public String ids = "";

		
    

		
        public SearchResult()
            {
                count = 0;
                retmax = 0;
                retstart = 0;
            }


		

        public void addID(String id)
        {

        idList.add(id);
                if(!ids.equals(""))
                    ids += ","+id;
                else
                    ids = id;
            }


		
    public ArrayList<String> idList = new ArrayList<String>();


	}

	
    final int PACING = 20;

	
    final int MAX_TO_FETCH = 10;

	
    boolean keepOn = true;

	
    String idList;

	
    JTextField tf = new JTextField();

	
    JPanel pan = new JPanel();

	
    GridBagLayout gbl = new GridBagLayout();

	
    GridBagConstraints con = new GridBagConstraints();

	
    MedlineFetcher ths = this;

	
    AuthorDialog authorDialog;

	
    JFrame jFrame;

	 
    JButton go = new JButton(Globals.lang("Fetch")),
        helpBut = new JButton(GUIGlobals.getImage("helpSmall"));

	
    HelpAction help;

	

    public MedlineFetcher(SidePaneManager p0) {
        super(p0, GUIGlobals.getIconUrl("medline"), Globals.lang("Fetch Medline"));

        help = new HelpAction(Globals.helpDiag, GUIGlobals.medlineHelp, "Help");
        helpBut.addActionListener(help);
        helpBut.setMargin(new Insets(0,0,0,0));
        tf.setPreferredSize(new Dimension(1,tf.getPreferredSize().height));
        
        
        JPanel main = new JPanel();
            main.setLayout(gbl);
        con.fill = GridBagConstraints.BOTH;
        
        con.gridwidth = GridBagConstraints.REMAINDER;
        con.weightx = 1;
        con.weighty = 1;
        con.fill = GridBagConstraints.BOTH;
        gbl.setConstraints(tf, con);
        main.add(tf);
        con.weighty = 0;
        con.gridwidth = 1;
        gbl.setConstraints(go, con);
        main.add(go);
        con.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(helpBut, con);
        main.add(helpBut);
        ActionListener listener = new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    (new Thread(ths)).start(); 
                }
            };
        main.setBorder(BorderFactory.createEmptyBorder(1,1,1,1));
        add(main, BorderLayout.CENTER);
        go.addActionListener(listener);
        tf.addActionListener(listener);
        tf.addFocusListener(new FocusAdapter() {
            public void focusGained(FocusEvent event) {
                if (!event.isTemporary() && (tf.getText().length()>0)) {
                    tf.selectAll();
                }
            }
        });
    }


	

    public JTextField getTextField() {
        return tf;
    }


	

    public void fetchById() {
        
        
        Pattern p = Pattern.compile("\\d+[,\\d+]*");
        
        Matcher m = p.matcher( idList );
        if ( m.matches() ) {
            panel.frame().output(Globals.lang("Fetching Medline by ID..."));

            ArrayList<BibtexEntry> bibs = fetchMedline(idList);
            if ((bibs != null) && (bibs.size() > 0)) {
                
                
                
                tf.setText("");
                
                

        panel.frame().addImportedEntries(panel, bibs, null, false, this);

        

                
            } else
                panel.output(Globals.lang("No Medline entries found."));
        } else {
            JOptionPane.showMessageDialog(panel.frame(),Globals.lang("Please enter a semicolon or comma separated list of Medline IDs (numbers)."),Globals.lang("Input error"),JOptionPane.ERROR_MESSAGE);
        }
    }


	






  public static ArrayList<BibtexEntry> fetchMedline(String id)
  {
    ArrayList<BibtexEntry> bibItems=null;
    try {

      String baseUrl = "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&retmode=xml&rettype=citation&id=" + id;

      URL url = new URL( baseUrl );
      HttpURLConnection data = (HttpURLConnection)url.openConnection();


       


        
        SAXParserFactory parserFactory = SAXParserFactory.newInstance();
        
        parserFactory.setValidating(true);
        parserFactory.setNamespaceAware(true);

        
        SAXParser parser = parserFactory.newSAXParser();   
        MedlineHandler handler = new MedlineHandler();
        

        parser.parse( data.getInputStream(), handler);
        
        
        bibItems = handler.getItems();

    }
    catch(javax.xml.parsers.ParserConfigurationException e1){}
    catch(org.xml.sax.SAXException e2){}
    catch(java.io.IOException e3){}
    return bibItems;
}


	

   public void run() {

        idList = tf.getText().replace(';', ',');

        
        
        Pattern p1 = Pattern.compile("\\d+[,\\d+]*"),
            p2 = Pattern.compile(".+[,.+]*");

         Matcher m1 = p1.matcher( idList ),
             m2 = p2.matcher( idList );
         if ( m1.matches() ) {
             panel.frame().output(Globals.lang("Fetching Medline by id ..."));
             idList = tf.getText().replace(';', ',');
             fetchById();
             
         }
         else if ( m2.matches() ) {
            panel.frame().output(Globals.lang("Fetching Medline by term ..."));

            
            
            String searchTerm = setupTerm(idList); 
            SearchResult result = getIds(searchTerm ,0,1); 
            
            if (result.count == 0) {
                JOptionPane.showMessageDialog(panel.frame(), Globals.lang("No references found"));
                return;
            }
            String question =
                Globals.lang("References found")+": "
                + Integer.toString(result.count)+"  "
                + Globals.lang("Number of references to fetch?");
            String strCount =
                JOptionPane.showInputDialog(question,
                                            Integer.toString(result.count));

            
            if((strCount == null) || strCount.equals(""))
                return;
            int count;
        try {
            count = Integer.parseInt(strCount);
        } catch (NumberFormatException ex) {
            panel.output("");
            return;
        }

        ImportInspectionDialog diag = new ImportInspectionDialog(panel.frame(), panel,
                BibtexFields.DEFAULT_INSPECTION_FIELDS, Globals.lang("Fetch Medline"), false);
        Util.placeDialog(diag, panel.frame());
         diag.setDefaultSelected(false); 

             
        diag.setVisible(true);
        keepOn = true;
         diag.addCallBack(new ImportInspectionDialog.CallBack() {
             public void done(int entriesImported) {
                 if (entriesImported > 0) {
                 panel.output(Globals.lang("Medline entries fetched")+": "+entriesImported);
                 panel.markBaseChanged();
             } else
                 panel.output(Globals.lang("No Medline entries found."));
            }

             public void cancelled() {
                 panel.output(Globals.lang("%0 import cancelled.", "Medline"));
             }


             public void stopFetching() {
                
                keepOn = false;
             }
         });
            for (int jj = 0; jj < count; jj+=PACING) {
            if (!keepOn)
                break;
                    
                    result = getIds(searchTerm,jj,PACING);

            

            final ArrayList<BibtexEntry> bibs = fetchMedline(result.ids);
            if (!keepOn)
                break;
            diag.addEntries(bibs);
            diag.setProgress(jj+PACING, count);
            }
         diag.entryListComplete();
         }
   }


	
    public String setupTerm(String in){
        Pattern part1=Pattern.compile(", ");
        Pattern part2=Pattern.compile(",");
        Pattern part3=Pattern.compile(" ");
        Matcher matcher;
        matcher=part1.matcher(in);
        in=matcher.replaceAll("\\+AND\\+");
        matcher=part2.matcher(in);
        in=matcher.replaceAll("\\+AND\\+");
        matcher=part3.matcher(in);
        in=matcher.replaceAll("+");

        return in;
    }


	

    
    public SearchResult getIds(String term, int start,int pacing){
        String baseUrl="http://eutils.ncbi.nlm.nih.gov/entrez/eutils";
        String medlineUrl = baseUrl
            +"/esearch.fcgi?db=pubmed&retmax="
            +Integer.toString(pacing)
            +"&retstart="+Integer.toString(start)
            +"&term=";
        Pattern idPattern=Pattern.compile("<Id>(\\d+)</Id>");
        Pattern countPattern=Pattern.compile("<Count>(\\d+)<\\/Count>");
        Pattern retMaxPattern=Pattern.compile("<RetMax>(\\d+)<\\/RetMax>");
        Pattern retStartPattern=Pattern.compile("<RetStart>(\\d+)<\\/RetStart>");
        Matcher idMatcher;
        Matcher countMatcher;
        Matcher retMaxMatcher;
        Matcher retStartMatcher;
        boolean doCount = true;
        SearchResult result = new SearchResult();
        
        try{
            URL ncbi = new URL(medlineUrl+term);
            
            BufferedReader in =
                new BufferedReader
                (new InputStreamReader
                 ( ncbi.openStream()));
            String inLine;
            while ((inLine=in.readLine())!=null){

                
                idMatcher=idPattern.matcher(inLine);
                if (idMatcher.find()){
                    result.addID(idMatcher.group(1));
                }
                retMaxMatcher=retMaxPattern.matcher(inLine);
                if (idMatcher.find()){
                    result.retmax=Integer.parseInt(retMaxMatcher.group(1));
                }
                retStartMatcher=retStartPattern.matcher(inLine);
                if (retStartMatcher.find()){
                    result.retstart=Integer.parseInt(retStartMatcher.group(1));
                }
                countMatcher=countPattern.matcher(inLine);
                if (doCount && countMatcher.find()){
                    result.count=Integer.parseInt(countMatcher.group(1));
                    doCount = false;
                }
            }

        }
        catch (MalformedURLException e) {     
            System.out.println("bad url");
            e.printStackTrace();
        }
        catch (IOException e) {               
            System.out.println("connection failed");
            e.printStackTrace();

        }
        return result;
    }


	

    public String[] getTitles(String[] idArrayList) {
      String[] titles = new String[Math.min(MAX_TO_FETCH, idArrayList.length)];
        String temp;
        for (int i=0; i<Math.min(MAX_TO_FETCH, idArrayList.length); i++){
            temp=getOneCitation(idArrayList[i]);
            titles[i]=getVitalData(temp);
        }
        return titles;
    }


	

        
    public String getOneCitation(String id){
        String baseUrl="http://eutils.ncbi.nlm.nih.gov/entrez/eutils";
        String retrieveUrl = baseUrl+"/efetch.fcgi?db=pubmed&retmode=xml&rettype=citation&id=";
        StringBuffer sb=new StringBuffer();
        try{
            URL ncbi = new URL(retrieveUrl+id);
            BufferedReader in =
                new BufferedReader
                (new InputStreamReader
                 ( ncbi.openStream()));
            String inLine;
            while ((inLine=in.readLine())!=null){

                sb.append(inLine);
            }

        }
        catch (MalformedURLException e) {     
            System.out.println("bad url");
            e.printStackTrace();
        }
        catch (IOException e) {               
            System.out.println("connection failed");
            e.printStackTrace();

        }
        return sb.toString();
    }


	

        
    public String getVitalData(String sb){
        StringBuffer result=new StringBuffer();
        Pattern articleTitle=Pattern.compile("<ArticleTitle>(.+)</ArticleTitle>");
        Matcher matcher;
        matcher=articleTitle.matcher(sb);
        if (matcher.find())
        result.append("Title: ").append(matcher.group(1));

        
        
        
        return result.toString();
    }


	

    
    
    
    public void done(int entriesImported) {
        panel.output(Globals.lang("Medline entries fetched")+": "+entriesImported);
    }


	

    public void cancelled() {
        panel.output(Globals.lang("%0 import cancelled.", "Medline"));
    }


	


    
    
    
    public void stopFetching() {
        
    }



}
