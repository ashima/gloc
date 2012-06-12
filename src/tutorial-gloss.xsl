<?xml version="1.0"?>

<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:param name="gloss" />
  <xsl:output method="text" />

  <xsl:template match="gloss">
    <xsl:if test="ancestor::section/@id = $gloss">
      <xsl:value-of select="." /><xsl:text>
</xsl:text>
    </xsl:if>
  </xsl:template>

  <xsl:template match="*|@*">
    <xsl:apply-templates select="*" />
  </xsl:template>
</xsl:stylesheet>
