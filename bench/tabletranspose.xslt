<!--

https://stackoverflow.com/questions/4410084/transpose-swap-x-y-axes-in-html-table

-->


<xsl:stylesheet version="1.0"
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
 <xsl:output omit-xml-declaration="yes"/>
 <xsl:template match="table">
   <table>
     <xsl:for-each select="tr[1]/*">
      <xsl:variable name="vRowPos" select="position()"/>
      <tr>
       <xsl:for-each select="/table/tr">
        <xsl:variable name="vColPos" select="position()"/>
        <xsl:copy-of select="/table/tr[$vColPos]/*[$vRowPos]"/>
       </xsl:for-each>
      </tr>
     </xsl:for-each>
   </table>
 </xsl:template>
</xsl:stylesheet>

