package imapscraper.utils;
/*
 * @licensing
 * http://wrongnotes.blogspot.com/2007/09/javamail-parsing-made-easy.html
 * no license info
 */

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.*;
import javax.mail.*;

public class MimeUtils {

	public static Map<String,Part> findMimeTypes(Part p, String... mimeTypes) {
		Map<String,Part> parts = new HashMap<String,Part>();
		findMimeTypesHelper(p, parts, mimeTypes);
		return parts;
	}

	// a little recursive helper function that actually does all the work.
	public static void findMimeTypesHelper(Part p, Map<String,Part> parts, String... mimeTypes) {
		try {
			if (p.isMimeType("multipart/*")) {
				Multipart mp = (Multipart) p.getContent();
				for (int i = 0; i < mp.getCount(); i++) {
					findContentTypesHelper(mp.getBodyPart(i), parts, mimeTypes);
				}
			} else {
				for (String mimeType : mimeTypes) {
					if (p.isMimeType(mimeType) && !parts.containsKey(mimeType)) {
						parts.put(mimeType, p);
					}
				}
			}
		} catch (Exception ex) {
			//logger.warn(p.getContentType(), ex);
		}
	}

	private static void findContentTypesHelper(Part p, Map<String,Part> contentTypes,
			String... mimeTypes) throws MessagingException, IOException {
		try {
			if (p.isMimeType("multipart/*")) {
				Multipart mp = (Multipart) p.getContent();
				for (int i = 0; mp != null && i < mp.getCount(); i++) {
					findContentTypesHelper(mp.getBodyPart(i), contentTypes,
							mimeTypes);
				}
			} else {
				for (String mimeType : mimeTypes) {
					if (p.isMimeType(mimeType)) {
						contentTypes.put(mimeType, p);
					}
				}
			}
		} catch (UnsupportedEncodingException ex) {
			//logger.warn(p.getContentType(), ex);
		}
	}
}