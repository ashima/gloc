<?xml version="1.0"?>

<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:import href="doc.xsl" />

  <xsl:param name="name" />

  <xsl:output method="html" />

  <xsl:template match="@*|node()" mode="code">
    <xsl:apply-templates select="@*|node()" mode="code" />
  </xsl:template>

  <xsl:template match="gloss">
    <pre type="x-shader/x-webglsl">
      <xsl:value-of select="." />
    </pre>
  </xsl:template>

  <xsl:template match="gloss" mode="code">
    <script type="text/json" id="{@id}">
      <xsl:value-of select="document(concat($name,'/',@id,'.xml'))//json" />
    </script>
  </xsl:template>

  <xsl:template match="js">
    <pre type="text/javascript">
      <xsl:value-of select="." />
    </pre>
  </xsl:template>

  <xsl:template match="js" mode="code">
    <script type="text/javascript">
      <xsl:value-of select="." />
    </script>
  </xsl:template>

  <xsl:variable name="title" select="'glo(|c|l) Tutorial : '" />

  <xsl:template match="title">
    <title><xsl:value-of select="$title" /><xsl:value-of select="." /></title>
  </xsl:template>

  <xsl:template match="tutorial">
    <html>
      <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
        <link rel="stylesheet" href="../gloc.css" type="text/css" />
        <script type="text/javascript" src="http://ashimaarts.com/gloc/1.0/glol.js"></script>
        <style type="text/css">
          article { margin: 2em; }
        </style>
        <xsl:apply-templates select="head/*" />
      </head>
      <body>
        <article>
          <h1><xsl:value-of select="$title" /><xsl:value-of select="head/title" /></h1>
          <xsl:apply-templates select="body/*" />
        </article>
        <xsl:call-template name="footer" />
        <xsl:apply-templates select="body//gloss" mode="code" />
        <xsl:apply-templates select="body//js" mode="code" />
      </body>
    </html>
  </xsl:template>

</xsl:stylesheet>
