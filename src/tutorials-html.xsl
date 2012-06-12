<?xml version="1.0"?>

<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:import href="doc.xsl" />

  <xsl:param name="xml" />

  <xsl:output method="html" />

  <xsl:template match="tutorial">
    <li>
      <a href="{path}"><xsl:value-of select="document(xml)//title" /></a>
      <details open="open">
        <summary>Description</summary>
        <p><xsl:copy-of select="document(xml)//summary/node()" /></p>
      </details>
    </li>
  </xsl:template>

  <xsl:template match="ul[@id='tutlist']">
    <xsl:copy>
      <xsl:apply-templates select="document($xml)/tutorials/tutorial" />
    </xsl:copy>
  </xsl:template>

  <xsl:template match="body">
    <xsl:copy>
      <xsl:apply-templates />
      <xsl:call-template name="footer" />
    </xsl:copy>
  </xsl:template>

</xsl:stylesheet>
