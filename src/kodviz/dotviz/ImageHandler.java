package kodviz.dotviz;

import java.awt.Image;
import java.awt.Toolkit;
import java.awt.image.BufferedImage;
import java.net.URL;

/**
 * ImageHandler simply handles all of the images used for icons in the gui.
 * This should probably be taken out and replaced with the stuff already in
 * place (i.e. the loadImage method in alloy.gui.NewAlloyGUI) when we integrate
 * this gui into the other Alloy stuff.
 */
abstract class ImageHandler {

    /**
     * Returns an image specified by the given path.  The path should be relative
     * to newalloyviz.
     */
    static Image loadImage(String pathName) {
	if (pathName != null) {
	    
	    URL url = ImageHandler.class.getClassLoader().getResource(pathName);

	    if (url != null) {
		return Toolkit.getDefaultToolkit().createImage(url);
	    }
	}
	return new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB);
    }
}
