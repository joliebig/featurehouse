<xsl:stylesheet version="1.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns:cpp="http://www.sdml.info/srcML/cpp"
	xmlns:srcml="http://www.sdml.info/srcML/src"
>

<xsl:template match="srcml:comment">
	<!-- do nothing here -->
</xsl:template>

<xsl:template match="*">
	<xsl:copy>
		<xsl:apply-templates />
	</xsl:copy>
</xsl:template>

</xsl:stylesheet>

