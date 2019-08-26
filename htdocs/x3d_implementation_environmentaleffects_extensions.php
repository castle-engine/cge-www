<!-- TODO: format as proper HTML, link. -->
<pre>

ImageBackground : X3DBackgroundNode {
  SFColorRGBA [in,out] color     1 1 1 1
  SFNode      [in,out] texture   NULL                    # [X3DTextureNode]
  MFVec2f     [in,out] texCoords [ 0 0, 1 0, 1 1, 0 1 ]
}

- Texture

  Texture displayed in a full-screen quad.
  Only 2D single-texture nodes are supported now.
  If you leave this as @nil, the image background has no visible effect.

- Color
  Color that is multiplied by the texture color. Opaque white by default.

  Note that the alpha component of this color matters
  (just like the alpha channel of the texture in @link(Texture)).
  If the resulting image is partially-transparent,
  it will be mixed with black color.

- TexCoords

  Texture coordinates of the full-screen quad.
  By default they use the whole texture area:

  @orderedList(
    @item Vector2(0, 0),
    @item Vector2(1, 0),
    @item Vector2(1, 1),
    @item Vector2(0, 1)
  )

  It is undefined what happens if there are not exactly 4 items.

</pre>
