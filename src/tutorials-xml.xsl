<?xml version="1.0"?>

<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:param name="tutxmls" />
  <xsl:param name="tutpaths" />

  <xsl:template name="output-tutlinks">
    <xsl:param name="xmls" />
    <xsl:param name="paths" />
    <xsl:variable name="xml" select="substring-before($xmls, ',')" />
    <xsl:variable name="xmls" select="substring-after($xmls, ',')" />
    <xsl:variable name="path" select="substring-before($paths, ',')" />
    <xsl:variable name="paths" select="substring-after($paths, ',')" />

    <tutorial>
      <xml><xsl:value-of select="$xml" /></xml>
      <path><xsl:value-of select="$path" /></path>
    </tutorial>

    <xsl:if test="$xmls">
      <xsl:call-template name="output-tutlinks">
        <xsl:with-param name="xmls" select="$xmls" />
        <xsl:with-param name="paths" select="$paths" />
      </xsl:call-template>
    </xsl:if>
  </xsl:template>

  <xsl:template match="tutorials">
    <xsl:copy>
      <xsl:call-template name="output-tutlinks">
        <xsl:with-param name="xmls" select="concat($tutxmls,',')" />
        <xsl:with-param name="paths" select="concat($tutpaths,',')" />
      </xsl:call-template>      
    </xsl:copy>
  </xsl:template>

  <xsl:template match="*|@*">
    <xsl:copy>
      <xsl:apply-templates />
    </xsl:copy>
  </xsl:template>

</xsl:stylesheet>
