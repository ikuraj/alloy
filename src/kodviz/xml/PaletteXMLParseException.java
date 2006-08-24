package kodviz.xml;

@SuppressWarnings("serial")
public class PaletteXMLParseException extends Exception
{
	public PaletteXMLParseException() {
		super("Detected a malformed element");
	}
	
	public PaletteXMLParseException(String msg) {
		super(msg);
	}
}
