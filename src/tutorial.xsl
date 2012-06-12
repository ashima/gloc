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
    <xsl:variable name="id" select="../@id" />
    <script type="x-shader/x-webglsl" id="{$id}.glsl">
      <xsl:value-of select="." />
    </script>
    <nav class="resource">
      <a href="{$id}.glsl" title="{$id}.glsl">glsl</a> |
      <a href="{$id}.glo" title="gloc -c --dissolve {$id}.glsl -o {$id}.glo">glo</a> |
      <a href="{$id}.xml" title="gloc --xml --dissolve {$id}.glsl -o {$id}.xml">xml</a>
    </nav>
    <script type="text/javascript">
    var fid = "<xsl:value-of select="concat($id,'.glsl')" />";
    var s = document.getElementById(fid);
    var feditor = CodeMirror(function (elt) {
        s.parentNode.insertBefore(elt,s);
    }, {
        value: s.innerText,
        lineNumbers: true,
        mode: "text/x-csrc",
        keyMap: "emacs",
        readOnly: true,
        lineWrapping: true
    });
    </script>
  </xsl:template>

  <xsl:template match="gloss" mode="code">
    <script type="text/json" id="{../@id}.glo"><xsl:value-of select="document(concat($name,../@id,'.xml'))//json" /></script>
  </xsl:template>

  <xsl:template match="js">
    <script type="text/javascript" id="{@id}">
      <xsl:value-of select="." />
    </script>
    <script type="text/javascript">
    var fid = "<xsl:value-of select="@id" />";
    var s = document.getElementById(fid);
    var feditor = CodeMirror(function (elt) {
        s.parentNode.insertBefore(elt,s);
    }, {
        value: s.innerText,
        lineNumbers: true,
        mode: "javascript",
        keyMap: "emacs",
        readOnly: true,
        lineWrapping: true
    });
    </script>
  </xsl:template>

  <xsl:template match="js" mode="code">
    <script type="text/javascript">
      <xsl:value-of select="." />
    </script>
  </xsl:template>

  <xsl:variable name="title"> with <em>glol</em></xsl:variable>

  <xsl:template match="title">
    <title><xsl:value-of select="." /><xsl:value-of select="$title" /></title>
  </xsl:template>

  <xsl:template match="section/h2">
    <xsl:copy>
      <a class="anchor" href="#{../@id}">#</a>
      <xsl:apply-templates />
    </xsl:copy>
  </xsl:template>

  <xsl:template match="summary">
    <p><xsl:apply-templates /></p>
  </xsl:template>

  <xsl:template match="gltoc">
    <xsl:element name="{@type}">
      <xsl:for-each select="//section/h2">
        <li><a href="#{../@id}"><xsl:value-of select="." /></a></li>
      </xsl:for-each>
    </xsl:element>
  </xsl:template>

  <xsl:template match="head/author|head/summary|head/history" />

  <xsl:template match="tutorial">
    <html>
      <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
        
        <script src="../../glocode/CodeMirror2/lib/codemirror.js"></script>
        <script src="../../glocode/CodeMirror2/mode/clike/clike.js"></script>
        <script src="../../glocode/CodeMirror2/mode/javascript/javascript.js"></script>
        <script src="../../glocode/CodeMirror2/keymap/emacs.js"></script>
        <link rel="stylesheet" type="text/css" href="../../glocode/CodeMirror2/lib/codemirror.css" />

        <link rel="stylesheet" href="../../gloc.css" type="text/css" />
        <script type="text/javascript" src="http://ashimaarts.com/gloc/1.0/glol.js"></script>
        <script type="text/javascript" src="https://raw.github.com/ashima/webgl-engine/master/awe0.js"></script>
        <script type="text/javascript">
          function error(msg) {
            var status = document.getElementById("status");
            status.style.color = "red";
            status.innerText = msg;
            status.style.display = "block";
          }

          function makePerspective(fovy, aspect, near, far) {
            var tp = near * Math.tan(fovy * Math.PI / 360.0);
            var bt = -tp;
            var lt = bt * aspect;
            var rt = tp * aspect;

            var X =    2*near/(rt-lt);
            var Y =    2*near/(tp-bt);
            var A =  (rt+lt)/(rt-lt);
            var B =  (tp+bt)/(tp-bt);
            var C = -(far+near)/(far-near);
            var D = -2*far*near/(far-near);

            return [X, 0, 0, 0,
                    0, Y, 0, 0,
                    A, B, C, -1,
                    0, 0, D, 0 ] ;
          }
        </script>
        <style type="text/css">
          h1 { font-size: 2em; }
          h2 { font-size: 1.2em; }
          article { margin: 2em; }
          nav { margin: 1em 0; }
          nav.resource { font-size: 80%; margin-top: 0; }
          section > h2 { margin: 0; }
          h2 > .anchor { margin-right: 1em; color: #ccc; }
          .byline { font-weight: normal; color: #999; position: relative; top: -0.5em; }
          .author { font-style: italic; color: #999; }
          .summary { font-style: italic; }
          #status { position: fixed; background-color: #ccc;
                    border-top: 1px solid black;
                    border-left: 1px solid black;
                    bottom: 0; right: 0; width: 30%; height: 40px;
                    display: none; overflow: auto; resize: both }
        </style>
        <xsl:apply-templates select="head/*" />
        <xsl:apply-templates select="body//gloss" mode="code" />
      </head>
      <body>
        <article>
          <h1>
            <xsl:value-of select="head/title" /><xsl:copy-of select="$title" />
          </h1>
          <span class="byline">by
          <a class="author" href="{head/author/@href}">
            <xsl:value-of select="head/author" />
          </a>
          </span>
          <nav><a href="..">Tutorial Index</a> | <a href="example.html">Just the facts</a></nav>
          <p class="summary">
            <xsl:copy-of select="head/summary" />
          </p>
          <xsl:apply-templates select="body/*" />
          <details>
            <summary>Revision History</summary>
            <xsl:for-each select="head/history/revision">
              <p><xsl:value-of select="@date" /></p>
              <ul class="revision">
                <xsl:for-each select="change">
                  <xsl:copy-of select="node()" />
                </xsl:for-each>
              </ul>
            </xsl:for-each>
          </details>
        </article>
        <xsl:call-template name="footer" />
        <div id="status" />
      </body>
    </html>
  </xsl:template>

</xsl:stylesheet>
