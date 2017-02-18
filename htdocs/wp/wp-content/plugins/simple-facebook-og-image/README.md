Simple Wordpress OG Image
========================

Add og:image (Open Graph) tag to posts so Facebook can display proper image. 

The image is determined by these rules:   
1. Is there a cached image that was already set? If yes, use it. (This means no extra calls are made i.e. no performance penalty)   
2. Is there a featured image? If yes, use it.   
3. If there isn't a featured image use first images (all images since 1.2 version) from the post instead of only one. That can be turn on/off from plugin settings.
4. If there isn't an image in the post, check is there a default image. If yes, use it.   
5. If there isn't no tag will appear.   
