/*
 * Alloy Analyzer
 * Copyright (c) 2002 Massachusetts Institute of Technology
 *
 * The Alloy Analyzer is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * The Alloy Analyzer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with the Alloy Analyzer; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

package kodviz.util;

import java.awt.Image;
import java.awt.Toolkit;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

/**
 * Manages resources (files) loaded from the .jar.  Files needed by
 * your application can be included in the jar, so the entire application
 * is one file.  You can then call methods of this class to extract
 * these files to a temporary directory and get a full path to them.
 * This class keeps track of which resources have already been
 * extracted to temporary files, and won't extract the resource
 * more than once.
 * <p>
 * The above description relates to those resources that must be
 * saved in separate disk files before they can be used -- such
 * as executables.  Other resources, such as images, can be
 * used directly from the .jar.  This class also provides
 * methods to access a given resource as a stream or as a
 * URL.
 */
@SuppressWarnings("unchecked")
public class ResourceManager {
	/** Names of resources already extracted
	from the .jar and put into the temporary directory during this
	run.  {@link Map} from the {@link String} resource id of the process to the
	{@link File} temporary file to which the resource has been extracted. */
	private static Map _extractedResources = new HashMap();

	/////////////
	// Methods //
	/////////////

	/** Extract the executable binary  stored as a resource in the jar, to
	the temporary directory, if not done yet.  Return the full pathname
	to the resource file. */
	public static File getExecutableResource(String resourceName_)
		throws IOException {
		File binFile = getResource(resourceName_);
		// make it executable, except on windows where it should already be executable
		if (!FileUtil.runningOnWindows()) {
			try {
				final String[] cmdArray = {"chmod", "u+x", binFile.getName()};
				Runtime.getRuntime().exec(cmdArray, null, binFile.getParentFile()).waitFor();
			} catch (InterruptedException e_) {
			}
		}
		return binFile;
	}

	/** Extract the resource stored as a resource in the jar, to
	the temporary directory, if not done yet.  Return the full pathname
	to the resource file. */
	public static File getResource(String resourceName_) throws IOException {
		File tmpFile = (File) _extractedResources.get(resourceName_);
		
		// this resource has been previously fetched: return it
		if (tmpFile != null)
			return tmpFile;
			
		// this resource has not been previously fetched, so do so now
		Dbg.info("Resource name: " + resourceName_);
		final InputStream binStream =
			new BufferedInputStream(
				ResourceManager.class.getClassLoader().getResourceAsStream(
					resourceName_));
		tmpFile = TmpFiles.createHandleToATempFile(resourceName_);
		_extractedResources.put(resourceName_, tmpFile);
		final FileOutputStream tmpFileOutputStream = new FileOutputStream(tmpFile);
		byte[] b = new byte[16384];
		while (true) {
			int numRead = binStream.read(b);
			if (numRead == -1)
				break;
			tmpFileOutputStream.write(b, 0, numRead);
		}
		tmpFileOutputStream.close();
		return tmpFile;
	}

	/**
	 * Return an input stream for reading a specified resource.
	 */
	public static InputStream getResourceAsStream(String resourceName_) {
		return new BufferedInputStream(
			ResourceManager.class.getClassLoader().getResourceAsStream(
				resourceName_));
	}

	/** Return a URL for accessing a given resource. */
	public static URL getResourceURL(String resourceName_) {
		return ResourceManager.class.getClassLoader().getResource(
			resourceName_);
	}

	/** Load an image resource. Returns null if can't load image. */
	public static Image getImage(String resourceName_) {
		URL url = ResourceManager.getResourceURL(resourceName_);

		if (url == null)
			return null;
		else
			return Toolkit.getDefaultToolkit().createImage(url);
	}
}
