class AspectJCollector {
    static protected CommentRemover comment_remover = new CommentRemover();
	boolean parseString(StringBuffer buf) {
        comment_remover.Run(buf);
		return original(buf);
    }
}
