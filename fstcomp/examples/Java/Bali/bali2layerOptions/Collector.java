layer baseArgs;

refines class Collector{

    /**
     * Collects class names that are to be generated as
     * Jak files
     */
    public void collectClasses(StringBuffer className) {
        baliRules.collectClasses(className) ;
    }

}