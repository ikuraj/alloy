<?xml version="1.0" encoding="iso-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="xml" version="1.0" indent="yes" standalone="no"
  omit-xml-declaration="no"></xsl:output>

    <xsl:template match="/">
        <solution>
        <xsl:apply-templates/>
        </solution>
    </xsl:template>

    <xsl:template match="module|sig|field">
        <xsl:variable name="n" select="translate(@name,'/[]','___')"/>
        <xsl:element name="{$n}">
        <xsl:apply-templates/>
        </xsl:element>
    </xsl:template>

    <xsl:template match="type|atom">
        <xsl:copy-of select="."/>
    </xsl:template>

    <xsl:template match="tuple">
        <tuple>
        <xsl:apply-templates/>
        </tuple>
    </xsl:template>

</xsl:stylesheet>

