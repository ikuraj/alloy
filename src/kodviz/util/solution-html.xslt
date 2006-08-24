<?xml version="1.0" encoding="iso-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="html" version="1.0" indent="yes" standalone="yes"
  omit-xml-declaration="no"></xsl:output>

    <xsl:template match="/">
        <xsl:apply-templates select="solution/module"/>
    </xsl:template>

    <xsl:template match="solution/module">
        <h1>
        <xsl:value-of select="@name"/>
        </h1>
        <xsl:apply-templates select="sig"/>
        <hr/>
    </xsl:template>

    <xsl:template match="sig">
        <h2>
        <xsl:value-of select="@name"/>
        </h2>
        <xsl:for-each select="atom">
            <xsl:sort select="@name"/>
            <xsl:value-of select="@name"/>, 
        </xsl:for-each>
        <xsl:for-each select="field">
            <xsl:sort select="@name"/>
            <h3>
            <xsl:value-of select="@name"/>
            : 
            <xsl:value-of select="type"/>
            </h3>
            <table border="1" cellspacing="3" cellpadding="5">
            <xsl:for-each select="tuple">
                <tr>
                <xsl:for-each select="atom">
                    <td> <xsl:value-of select="@name"/> </td>
                </xsl:for-each>
                </tr>
            </xsl:for-each>
            </table>
        </xsl:for-each>
    </xsl:template>


</xsl:stylesheet>

