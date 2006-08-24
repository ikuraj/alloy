package kodviz.xml;

public class PaletteXMLParserDbg {
	public static void chk(Object o) throws PaletteXMLParserError {
		chk(o, null);
	}
	public static void chk(Object o, String error_msg)
	throws PaletteXMLParserError {
		chk(o != null, error_msg);
	}
	public static void chk(boolean b) throws PaletteXMLParserError {
		chk(b, null);
	}
	public static void chk(boolean b, String error_msg)
	throws PaletteXMLParserError {
		if (!b) {
			if (error_msg == null)
				throw new PaletteXMLParserError();
			else
				throw new PaletteXMLParserError(error_msg);
		}
	}
}
